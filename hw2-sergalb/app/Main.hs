{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (Identity)
import Control.Monad.State (runState)
import qualified Data.ByteString.Char8 as C (ByteString, hGetContents, hPut, unpack)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map.Lazy as Map (Map (..), elems, empty, insert, keys, (!))
import Data.Maybe (fromJust)
import FileManager (FMTree (..), FSContext (..), FileManagerT, addDir, addFile, addPathSuffix,
                    changeDirectory, findInTree, getFileContent, initFS, makeRelative, remove,
                    saveFs, writeToFile)
import Options.Applicative (CommandFields (..), Mod (..), Parser (..), command, defaultPrefs,
                            execParserPure, fullDesc, getParseResult, handleParseResult, header,
                            help, helper, hsubparser, info, infoOption, long, metavar, overFailure,
                            progDesc, strArgument)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist,
                         getCurrentDirectory, getPermissions, listDirectory, makeAbsolute, readable,
                         removeDirectoryRecursive, renamePath, setCurrentDirectory,
                         withCurrentDirectory)
import System.FilePath (pathSeparator)
import System.IO (IOMode (..), hFlush, stdout, withFile)
import VersionControlSystem (getRevisions, initVCS, printRevisions, revisionByIndex,
                             splitedPathToPath, vcsAdd, vcsUpdate)

data Command
  = Cd FilePath
  | Ls
  | AddFile FilePath
  | AddDir FilePath
  | Rm FilePath
  | Cat FilePath
  | Write FilePath C.ByteString
  | Find String
  | VcsInit
  | VcsAdd FilePath
  | VcsUpdate FilePath C.ByteString
  | VcsHistory FilePath
  | Revision FilePath String
  | Exit
  deriving (Show)

newtype Opts =
  Opts
    { optCommand :: Command
    }
  deriving (Show)

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  initialFS <- initFS currentDirectory
  untilExit initialFS currentDirectory

untilExit :: FSContext -> FilePath -> IO ()
untilExit context@(FSContext tree relative) realCurrentDir = do
  putStr $ realCurrentDir ++ [pathSeparator] ++ splitedPathToPath relative ++ ">"
  hFlush stdout
  command <- getLine
  let splitedCommand =
        if '"' `elem` command
          then let (x:xs:xss) = (splitOn "\"" command)
                in init (splitOn " " x) ++ [xs]
          else splitOn " " command  
  let maybeCommand = getParseResult $ overFailure id $ execParserPure defaultPrefs optsParser splitedCommand
  case maybeCommand of
    Just cmd ->
      case cmd of
        Cd way -> performAction context (changeDirectory way) realCurrentDir
        Ls -> listDirectoryContent context >> untilExit context realCurrentDir
        AddFile name -> performAction context (addFile name) realCurrentDir
        AddDir name -> performAction context (addDir name) realCurrentDir
        Rm name -> performAction context (remove name) realCurrentDir
        Cat name -> printActionRes print context (getFileContent name) realCurrentDir
        Write name text -> performAction context (writeToFile name text) realCurrentDir
        Find name -> printActionRes printFindRes context (findInTree name) realCurrentDir
        VcsInit -> performAction context initVCS realCurrentDir
        VcsAdd name -> performAction context (vcsAdd name) realCurrentDir
        VcsUpdate path comment -> performAction context (vcsUpdate path comment) realCurrentDir
        VcsHistory path -> printActionRes printRevisions context (getRevisions path) realCurrentDir
        Revision path index -> printActionRes print context (revisionByIndex path index) realCurrentDir
        Exit -> saveFs tree
    _ -> (putStrLn $ "uncnown command " ++ command ++ " use -h to help") >> untilExit context realCurrentDir

printActionRes :: (Show a, Show e) => (a -> IO ()) -> FSContext -> FileManagerT e a -> FilePath -> IO ()
printActionRes printer context fileManager realCurrentDir =
  let evaluating = runState (runExceptT fileManager) context
   in case evaluating of
        (Right res, newContext) -> printer res >> untilExit newContext realCurrentDir
        (Left exception, _)     -> print exception >> untilExit context realCurrentDir

performAction :: Show e => FSContext -> FileManagerT e a -> FilePath -> IO ()
performAction context fileManager realCurrentDir =
  let res = runState (runExceptT fileManager) context
   in case res of
        (Right _, newContext) -> untilExit newContext realCurrentDir
        (Left exception, _)   -> print exception >> untilExit context realCurrentDir

printFindRes :: Maybe FilePath -> IO ()
printFindRes (Just path) = print path
printFindRes Nothing     = print "Couldn't find file in current directory"

listDirectoryContent :: FSContext -> IO ()
listDirectoryContent (FSContext (Directory _ children permissions) relative) =
  if readable permissions
    then case relative of
           (x:xs) -> listDirectoryContent (FSContext ((Map.!) children x) xs)
           []     -> mapM_ (putStrLn . makeRelative) $ Map.keys children
    else print "access denied for read directory"
listDirectoryContent (FSContext File {} (x:xs)) = print "unexpected error incorrect relative state"
listDirectoryContent (FSContext (File name _ _) []) = print name

--for debug
printAllTree :: Int -> FMTree -> IO ()
printAllTree lvl tree = do
  replicateM_ lvl (putStr "    ")
  case tree of
    (Directory name children _) -> do
      putStrLn $ name ++ " D"
      foldMap (printAllTree (lvl + 1)) (Map.elems children)
    (File name _ _) -> putStrLn $ name ++ " F"

optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> progDesc "file-manager" <> header "file-manager - simpe tool for work wth file system")

versionOption :: Parser (a -> a)
versionOption = infoOption "0.0" (long "version" <> help "Show version")

programOptions :: Parser Command
programOptions =
  hsubparser
    (cdCommand <> lsCommand <> addFileCommand <> addDirCommand <> removeCommand <> catCommand <> writeCommand <>
     exitCommand <>
     findCommand <>
     vcsInitCommand <>
     vcsAddCommand <>
     vcsUpdateCommand <>
     vcsHistoryCommand <>
     revisionByIndexCommand)

cdCommand :: Mod CommandFields Command
cdCommand = command "cd" (info cdOptions (progDesc "Change directory"))

cdOptions :: Parser Command
cdOptions = Cd <$> strArgument (metavar "PATH" <> help "Path to new directory")

lsCommand :: Mod CommandFields Command
lsCommand = command "ls" (info (pure Ls) (progDesc "show contents of current directory"))

addFileCommand :: Mod CommandFields Command
addFileCommand = command "create-file" (info addFileOptions (progDesc "add file to current directory"))

addFileOptions :: Parser Command
addFileOptions = AddFile <$> strArgument (metavar "NAME" <> help "name of new file")

addDirCommand :: Mod CommandFields Command
addDirCommand = command "mkdir" (info addDirOptions (progDesc "add new directory to current directory"))

addDirOptions :: Parser Command
addDirOptions = AddDir <$> strArgument (metavar "NAME" <> help "name of new directory")

removeCommand :: Mod CommandFields Command
removeCommand = command "rm" (info removeOptions (progDesc "remove file or directory in current directory"))

removeOptions :: Parser Command
removeOptions = Rm <$> strArgument (metavar "NAME" <> help "name of removed file or dir")

catCommand :: Mod CommandFields Command
catCommand = command "cat" (info catOptions (progDesc "show content of file"))

catOptions :: Parser Command
catOptions = Cat <$> strArgument (metavar "NAME" <> help "name of file for show")

writeCommand :: Mod CommandFields Command
writeCommand = command "write" (info writeOptions (progDesc "write specified text to file"))

writeOptions :: Parser Command
writeOptions =
  Write <$> strArgument (metavar "PATH" <> help "name of file for write") <*>
  strArgument (metavar "STRING" <> help "text for write")

exitCommand :: Mod CommandFields Command
exitCommand = command "exit" (info (pure Exit) (progDesc "exit from file-manger and save state"))

findCommand :: Mod CommandFields Command
findCommand =
  command "find" (info findOptions (progDesc "find file with specified name in subdirectory of current directory"))

findOptions :: Parser Command
findOptions = Find <$> strArgument (metavar "NAME" <> help "name of file for find")

vcsInitCommand :: Mod CommandFields Command
vcsInitCommand = command "vcs-init" (info (pure VcsInit) (progDesc "init vcs in current directory"))

vcsAddCommand :: Mod CommandFields Command
vcsAddCommand = command "vcs-add" (info vcsAddOptions (progDesc "add file or directory under version control"))

vcsAddOptions :: Parser Command
vcsAddOptions = VcsAdd <$> strArgument (metavar "PATH" <> help "relative PATH to file or directory to add")

vcsUpdateCommand :: Mod CommandFields Command
vcsUpdateCommand = command "vcs-update" (info vcsUpdateOptions (progDesc "update revision of file"))

vcsUpdateOptions :: Parser Command
vcsUpdateOptions =
  VcsUpdate <$> strArgument (metavar "PATH" <> help "name of file for update revision") <*>
  strArgument (metavar "STRING" <> help "comment for new revision")

vcsHistoryCommand :: Mod CommandFields Command
vcsHistoryCommand =
  command "vcs-history" (info vcsHistoryOptions (progDesc "show all revisions of file in version control"))

vcsHistoryOptions :: Parser Command
vcsHistoryOptions = VcsHistory <$> strArgument (metavar "PATH" <> help "relative PATH to file to show")

revisionByIndexCommand :: Mod CommandFields Command
revisionByIndexCommand = command "vcs-revision" (info revisionByIndexOptions (progDesc "get revision of file by index"))

revisionByIndexOptions :: Parser Command
revisionByIndexOptions =
  Revision <$> strArgument (metavar "PATH" <> help "name of file for update revision") <*>
  strArgument (metavar "STRING" <> help "comment for new revision")
