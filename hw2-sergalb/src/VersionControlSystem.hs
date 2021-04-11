{-# LANGUAGE OverloadedStrings #-}

module VersionControlSystem
  ( getRevisions
  , initVCS
  , printRevisions
  , revisionByIndex
  , splitedPathToPath
  , vcsAdd
  , vcsUpdate
  ) where

import Control.Exception (Exception)
import Control.Monad.Except (Except, ExceptT, catchError, lift, runExcept, runExceptT, throwError)
import Control.Monad.State (State, StateT, get, modify, put, runState)
import qualified Data.ByteString.Char8 as C (ByteString, lines, putStrLn, unlines)
import Data.Foldable (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Lazy as Map (Map (..), delete, elems, empty, insert, lookup, member, size)
import FileManager (FMTree (..), FSContext (..), FileManagerT, addDir, addInner, addPathSuffix,
                    allPermissions, changeFS, getFmTree)
import FSException (FSException (..))
import System.Directory (Permissions (..), emptyPermissions, executable, readable,
                         setOwnerExecutable, setOwnerReadable, setOwnerWritable, writable)
import System.FilePath (pathSeparator)


-- | Create folder 'vcsDirName' in current directory if none of the parents are under version control
-- may fail with 
--  * 'InitException'
--  * 'IncorrectRelativeState'
initVCS :: FileManagerT FSException ()
initVCS = do
  context <- get
  vcs <- getVcs context
  case vcs of
    Just tree -> throwError $ InitException "current directory is already under vcs"
    Nothing ->
      addDir vcsDirName `catchError` (\_ -> throwError $ InitException "error while creating directory for vcs")

-- | Take current directory
-- go up by parents, search for 'vcsDirName'
-- return @Just FSContext@, represent closest parent under version control.
-- Nothing iif none of the parents are under version control
-- may fail with 'IncorrectRelativeState'
getVcs :: FSContext -> FileManagerT FSException (Maybe FSContext)
getVcs (FSContext (Directory _ children _) []) =
  return $ fmap (\tree -> FSContext tree [vcsDirName]) (Map.lookup vcsDirName children)
getVcs (FSContext _ []) = return Nothing
getVcs context@(FSContext tree relative) = do
  curTree <- getFmTree context [readable]
  case curTree of
    res@(Directory _ children _) ->
      if Map.member vcsDirName children
        then return $ Just (FSContext res (relative ++ [vcsDirName]))
        else getVcs $ FSContext tree (init relative)
    _ -> throwError $ IncorrectRelativeState "find .vcs in file"


-- | Take path from current directory to added 'FMTree'
-- Find 'FMTree' by path
-- Find nearest parent under vcs (may be current Directory)
-- if get file by path, add it under vcs
-- if get directory by path, add it and all subdiretory of it recursively under vcs 
-- may fail with
--  * 'VcsDoesNotInitialized'
--  * any exception from 'getFmTree'
vcsAdd :: FilePath -> FileManagerT FSException ()
vcsAdd path = do
  context@(FSContext tree relative) <- get
  added <- getFmTree (FSContext tree (relative ++ splitOn [pathSeparator] path)) [readable]
  let addedForVcs = convertTreeToVcsTree added
  maybeVcs <- getVcs context
  case maybeVcs of
    Nothing -> throwError VcsDoesNotInitialized
    Just (FSContext vcs vcsRel) -> do
      -- vcsRel is path to initialized directory + ".vcs"
      -- therefore if remove prefix |vcsRel - 1|, we get path from initialized to added
      let pathFromInitialized = drop (length vcsRel - 1) relative
      updated <-
        changeFS
          (addPathSuffix (splitedPathToPath pathFromInitialized) path)
          (FSContext tree vcsRel)
          (addInner addedForVcs)
      put $ FSContext updated relative

-- | Take
--  * Path from current directory to added file
--  * Comment for revision
-- Find file by path
-- Find nearest parent under vcs (may be current Directory)
-- add new revision of file under vcs  
-- may fail with
--  * 'VcsDoesNotInitialized'
--  * any exception from 'getFmTree'
vcsUpdate :: FilePath -> C.ByteString -> FileManagerT FSException ()
vcsUpdate path comment = do
  context@(FSContext tree relative) <- get
  added <- getFmTree (FSContext tree (relative ++ splitOn [pathSeparator] path)) []
  maybeVcs <- getVcs context
  case maybeVcs of
    Nothing -> throwError VcsDoesNotInitialized
    Just (FSContext vcs vcsRel) ->
      case added of
        (File _ content _) -> do
          let pathFromInitialized = drop (length vcsRel - 1) relative
          mdr <- getFmTree (FSContext tree (vcsRel ++ pathFromInitialized ++ splitByPathSeparator path)) []
          updated <- addRevision mdr content comment
          uup <-
            changeFS
              (addPathSuffix (splitedPathToPath pathFromInitialized) path)
              (FSContext tree vcsRel)
              (addInner updated)
          put $ FSContext uup relative
        _ -> throwError $ FileNotFound "given path isn't file"

-- | Take path from current directory to file
-- Find file by path.
-- Find revisions of this file in version controll
-- return 'Directory' with revisions and comments
-- may fail with any exception from 'getVcs'
getRevisions :: FilePath -> FileManagerT FSException FMTree
getRevisions path = do
  context@(FSContext tree relative) <- get
  maybeVcs <- getVcs context
  case maybeVcs of
    Nothing -> throwError VcsDoesNotInitialized
    Just (FSContext vcs vcsRel) -> do
      let pathFromInitialized = drop (length vcsRel - 1) relative
      getFmTree (FSContext tree (vcsRel ++ pathFromInitialized ++ splitByPathSeparator path)) []
      
-- | Take 'Directory' with revisions and comments and print it in format
--  may fail with any exception from 'getVcs'
printRevisions :: FMTree -> IO ()
printRevisions (Directory name children _) = do
  putStrLn "revisions of file name"
  let mbComments = Map.lookup commentsFileName children
  case mbComments of
    (Just (File _ commentsText _)) -> do
      let comments = C.lines commentsText
      mapM_ printRevision (zip (Map.elems $ Map.delete commentsFileName children) comments)
    Nothing -> putStrLn "couldn't find comments of revision"
  where
    printRevision :: (FMTree, C.ByteString) -> IO ()
    printRevision (File number content _, comment) = do
      putStr $ "Revision number " ++ show number ++ ". with comment: "
      C.putStrLn comment
      C.putStrLn content
    printRevision _ = putStrLn "couldn't show revision"
printRevisions _ = putStrLn "couldn't find revisions of file"

-- | Take 
--  * Path from current directory to file
--  * Index of revision
-- return content of revision with specified index
--  * 'RevisionDoesntExist'
--  * IncorrectRelativeState
--  * any exception from 'getRevisions'
revisionByIndex :: FilePath -> String -> FileManagerT FSException C.ByteString
revisionByIndex path index = do
  revisions <- getRevisions path
  case revisions of
    (Directory name children _) ->
      let maybeRevision = Map.lookup index children
       in case maybeRevision of
            (Just (File _ content _)) -> return content
            _ -> throwError $ RevisionDoesntExist $ "couldn't find revision of file " ++ name ++ " with index " ++ index
    _ -> throwError $ IncorrectRelativeState "couldn't find revisions"

addRevision :: FMTree -> C.ByteString -> C.ByteString -> FileManagerT FSException FMTree
addRevision (Directory name children _) newContent comment = do
  let number = show $ Map.size children - 1
  let maybeComments = Map.lookup commentsFileName children
  case maybeComments of
    (Just (File _ oldComments _)) -> do
      let withRev = Map.insert number (File number newContent allPermissions) children
      let newComments = File commentsFileName (C.unlines (C.lines oldComments ++ [comment])) allPermissions
      return $ Directory name (Map.insert commentsFileName newComments withRev) allPermissions
    Nothing -> throwError $ IncorrectRelativeState "couldn't find directory with revisions 1"
addRevision _ _ _ = throwError $ IncorrectRelativeState "couldn't find directory with revisions"

splitByPathSeparator :: FilePath -> [FilePath]
splitByPathSeparator = splitOn [pathSeparator]

convertTreeToVcsTree :: FMTree -> FMTree
convertTreeToVcsTree (Directory name children permissions) =
  Directory name (fmap convertTreeToVcsTree children) permissions
convertTreeToVcsTree (File name content _) =
  let withRev = Map.insert "0" (File "0" content allPermissions) Map.empty
      commentFile = File commentsFileName "init" allPermissions
   in Directory name (Map.insert commentsFileName commentFile withRev) allPermissions

splitedPathToPath :: [FilePath] -> FilePath
splitedPathToPath = foldl' addPathSuffix ""

vcsDirName :: String
vcsDirName = ".vcs"

commentsFileName :: String
commentsFileName = "comments"
