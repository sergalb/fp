{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module FileManager
  ( FMTree(..)
  , FSContext(..)
  , FileManagerT
  , addDir
  , addFile
  , addPathSuffix
  , changeDirectory
  , findInTree
  , getFileContent
  , initFS
  , makeRelative
  , remove
  , saveFs
  , writeToFile
  , getFmTree
  , changeFS
  , addInner
  , allPermissions
  , getName
  ) where

import Control.Monad.Except (Except, ExceptT, lift, runExcept, runExceptT, throwError)
import Control.Monad.State (State, StateT, get, modify, put, runState)
import qualified Data.ByteString.Char8 as C (ByteString, hGetContents, hPut)
import Data.Foldable (asum, foldl')
import Data.List.Split (splitOn)
import qualified Data.Map.Lazy as Map (Map (..), delete, elems, empty, insert, lookup, (!))
import FSException (FSException (..))
import System.Directory (Permissions (..), canonicalizePath, createDirectoryIfMissing,
                         doesDirectoryExist, emptyPermissions, executable, getCurrentDirectory,
                         getPermissions, listDirectory, readable, removeDirectoryRecursive,
                         renamePath, setOwnerExecutable, setOwnerReadable, setOwnerWritable,
                         withCurrentDirectory, writable)
import System.FilePath (pathSeparator)
import System.IO (IOMode (..), withFile)

-- | Representation of File System like tree
-- with directory-nodes and file-leafs
data FMTree
  = Directory FilePath (Map.Map FilePath FMTree) Permissions
  | File FilePath C.ByteString Permissions
  deriving (Show, Eq)

-- | Context of evaluating, used in 'FileMangerT'
data FSContext =
  FSContext
    { tree     :: FMTree -- Representation of uploaded file system
    , relative :: [FilePath] -- Path from root of 'tree' to current user's place
    }
  deriving (Show, Eq)

-- | Evaluating Monad
type FileManagerT e a = (ExceptT e (State FSContext)) a

-- | Take absolute path to directory
-- upload it in 'FMTree'
-- return uploaded tree and empty relative path
initFS :: FilePath -> IO FSContext
initFS absolutePath = do
  isDirectory <- doesDirectoryExist absolutePath
  permissions <- getPermissions absolutePath
  let relativePath = makeRelative absolutePath
  if isDirectory
    then do
      innerOfDir <- listDirectory absolutePath
      innerMap <- foldl' (addInner absolutePath) (return Map.empty) innerOfDir
      let res = Directory relativePath innerMap permissions
      return $ FSContext res []
    else do
      content <- withFile absolutePath ReadMode C.hGetContents
      return $ FSContext (File relativePath content permissions) []
  where
    addInner :: FilePath -> IO (Map.Map FilePath FMTree) -> FilePath -> IO (Map.Map FilePath FMTree)
    addInner pathPref mapIO elemPath = do
      map <- mapIO
      FSContext elem _ <- initFS (pathPref ++ [pathSeparator] ++ elemPath)
      return $ Map.insert elemPath elem map

-- | Take relative path from current dir to other (must be represented in uploaded tree)
-- move current relative path to it
-- can perform "." and ".."
changeDirectory :: FilePath -> FileManagerT FSException ()
changeDirectory way = do
  (FSContext tree relativePath) <- get
  let pathInTree = pathInTreeByRequest relativePath way
  exist <- checkFilePathExist $ FSContext tree pathInTree
  if exist
    then put $ FSContext tree pathInTree
    else throwError $ PathDoesntExist way

-- | Take current place as [FilePath] and path to move on
-- return moved place as [FilePath] -- relative from tree root
pathInTreeByRequest :: [FilePath] -> FilePath -> [FilePath]
pathInTreeByRequest relative way =
  let splited = relative ++ splitOn [pathSeparator] way
   in reduceDotAndDoubleDot splited []
  where
    reduceDotAndDoubleDot :: [FilePath] -> [FilePath] -> [FilePath]
    reduceDotAndDoubleDot (x:xs:xss) res =
      case xs of
        "."  -> reduceDotAndDoubleDot xss (res ++ [x])
        ""   -> reduceDotAndDoubleDot xss (res ++ [x])
        ".." -> reduceDotAndDoubleDot xss res
        _    -> reduceDotAndDoubleDot (xs : xss) (res ++ [x])
    reduceDotAndDoubleDot [x] res =
      case x of
        "." -> res
        "" -> res
        ".." ->
          if null res
            then res
            else init res
        _ -> res ++ [x]
    reduceDotAndDoubleDot [] res = res

-- | Take context with tree and current relative path
-- return true if path exist, false otherwise
-- The operation can fail with 'NoAccessRights'
checkFilePathExist :: FSContext -> FileManagerT FSException Bool
checkFilePathExist (FSContext (Directory name map rights) (x:xs)) =
  if readable rights
    then let child = Map.lookup x map
          in case child of
               Just childTree -> checkFilePathExist $ FSContext childTree xs
               Nothing        -> return False
    else throwError $ NoAccessRights name
checkFilePathExist (FSContext (Directory name _ _) []) = return True
checkFilePathExist (FSContext (File name _ _) []) = return True
checkFilePathExist (FSContext (File name _ _) (x:xs)) = return False

-- | Take path to added file
-- create file with specified by path name
-- add file to tree
-- may fail with
--  * NoAccessRights
--  * IncorrectRelativeState
addFile :: FilePath -> FileManagerT FSException ()
addFile path =
  let file = File (makeRelative path) "" allPermissions
   in add file path

-- | Take path to added directory
-- create directory with specified by path name
-- add directory to tree
-- may fail with
--  * NoAccessRights
--  * IncorrectRelativeState
addDir :: FilePath -> FileManagerT FSException ()
addDir path =
  let dir = Directory (makeRelative path) Map.empty allPermissions
   in add dir path

-- | Take added file/directory and path to it
-- add it to tree
-- may fail with
--  * NoAccessRights
--  * IncorrectRelativeState
add :: FMTree -> FilePath -> FileManagerT FSException ()
add newTreePart path = do
  context@(FSContext tree relative) <- get
  newTree <- changeFS path context (addInner newTreePart)
  put $ FSContext newTree relative

-- | Take
--  * added 'FMTree', may both file, directory
--  * path from current place to added
--  * tree, which represent current place. Must be 'Directory'
-- move by path and add given 'FMtree' to it
-- return tree, which represent current place with added 'FMtree'
-- may fail with
--  * NoAccessRights
--  * IncorrectRelativeState
addInner :: FMTree -> FilePath -> FMTree -> FileManagerT FSException FMTree
addInner _ [] _ = throwError $ IncorrectRelativeState "empty name for added file/directory"
addInner newTreePart path directory@Directory {} = do
  let splited = splitOn [pathSeparator] path
  process splited directory
  where
    process :: [FilePath] -> FMTree -> FileManagerT FSException FMTree
    process [x] dir@(Directory dirName children permissions) =
      if writable permissions
        then return (Directory dirName (Map.insert x newTreePart children) permissions)
        else throwError $ NoAccessRights $ "can't write to directory " ++ dirName
    process (x:xs) dir@(Directory dirName children permissions) = do
      let maybeChild = Map.lookup x children
      case maybeChild of
        (Just innerDir@Directory {}) ->
          if writable permissions
            then do
              newChild <- process xs innerDir
              return $ Directory dirName (Map.insert x newChild children) permissions
            else throwError $ NoAccessRights $ "can't delete file or directory " ++ dirName
        _ -> do
          newChild <- process xs (Directory x Map.empty allPermissions)
          return $ Directory dirName (Map.insert x newChild children) permissions
    process _ _ = throwError $ IncorrectRelativeState ""
addInner _ _ (File name _ _) = throwError $ IncorrectRelativeState $ "attempt to add file into file " ++ name

-- | Take path to deleted 'FMTree'
-- file should be in current place
-- delete it from tree
-- may fail with
--  * NoAccessRights
--  * FileNotFound
--  * IncorrectRelativeState
remove :: FilePath -> FileManagerT FSException ()
remove path = do
  context@(FSContext _ relative) <- get
  newTree <- changeFS path context removeInner
  put $ FSContext newTree relative

-- | Take
--  * deleted 'FMTree', may both file, directory
--  * name of file/directory to delete. It must be in current tree
--  * current tree, which represent current place. Must be 'Directory'
-- find 'FMTree' in current tree by name and delete it
-- return current tree without deleted 'FMTree'
-- may fail with
--  * NoAccessRights
--  * FileNotFound
--  * IncorrectRelativeState
removeInner :: FilePath -> FMTree -> FileManagerT FSException FMTree
removeInner name (Directory dirName children permissions) = do
  let maybeChild = Map.lookup name children
  case maybeChild of
    (Just child) ->
      if writable $ getPermissionsByTree child
        then return (Directory dirName (Map.delete name children) permissions)
        else throwError $ NoAccessRights $ "can't delete file or directory " ++ dirName
    Nothing -> throwError $ FileNotFound name
removeInner name _ =
  throwError $ IncorrectRelativeState $ "attempt to delete file from file or from current dir " ++ name

-- | Take name of file, represented as FMTree (must be 'File')
-- file should be in current place
-- return content of given file
-- may fail with
--  * NoAccessRights
--  * FileNotFound
--  * IncorrectRelativeState
getFileContent :: FilePath -> FileManagerT FSException C.ByteString
getFileContent name = do
  FSContext tree relative <- get
  file <- getFmTree (FSContext tree (pathInTreeByRequest relative name)) [readable]
  case file of
    (File _ content _) -> return content
    _ -> throwError $ IncorrectRelativeState $ "error while searching file: " ++ name

-- | Take path to file for write and text to write. File represented as FMTree (must be 'File')
-- file should be in current place
-- rewrite file with given text
-- may fail with
--  * NoAccessRights
--  * FileNotFound
--  * IncorrectRelativeState
writeToFile :: FilePath -> C.ByteString -> FileManagerT FSException ()
writeToFile name text = do
  context@(FSContext _ relative) <- get
  newTree <- changeFS name context (writeToFileInner text)
  put $ FSContext newTree relative

-- | Take
--  * Text for write in file.
--  * Name of file to write in. It must be in current tree.
--  * Tree, which represent current place. Must be 'Directory'.
-- find 'FMTree' in current tree by name and rewrite it
-- return current tree with rewrited file
-- may fail with
--  * NoAccessRights
--  * FileNotFound
--  * IncorrectRelativeState
writeToFileInner :: C.ByteString -> FilePath -> FMTree -> FileManagerT FSException FMTree
writeToFileInner text name (Directory dirName children permissions) = do
  let maybeChild = Map.lookup name children
  case maybeChild of
    (Just child) ->
      if writable $ getPermissionsByTree child
        then return (Directory dirName (Map.insert name (File name text allPermissions) children) permissions)
        else throwError $ NoAccessRights $ "can't rewrite file " ++ dirName
    Nothing -> throwError $ FileNotFound name
writeToFileInner _ name _ = throwError $ IncorrectRelativeState $ "attempt to move from file " ++ name

-- | Take name of file or directory
-- found it in subdirectory of current directory
-- directory traverse in lexicographical order (as DFS)
-- return 'Just FilePath' if file was founded
-- 'Nothing' otherwise
findInTree :: String -> FileManagerT FSException (Maybe FilePath)
findInTree name = do
  context@(FSContext tree relative) <- get
  currentDir <- getFmTree context []
  return $ findInTreeInner "" name currentDir

-- | Take
--  * Path of current place from search start.
--  * Name of file/directory to find.
--  * Tree, which represent current place. Must be 'Directory'
-- get all subdirectories/files. Search it for given name. Stops if found.
-- return 'Just FilePath' if file was founded
-- 'Nothing' otherwise
findInTreeInner :: FilePath -> String -> FMTree -> Maybe FilePath
findInTreeInner pref name (Directory _ children _) =
  let maybeChild = Map.lookup name children
   in case maybeChild of
        Just child -> Just (addPathSuffix pref name)
        Nothing -> asum [findInTreeInner (addPathSuffix pref (getName tree)) name tree | tree <- Map.elems children]
findInTreeInner _ _ _ = Nothing

-- | Function to perform all requests which change uploaded tree
-- Take
--  * Path from current directory to requested by user.
--  * Context included tree and current place. tree must be 'Directory'
--  * Function which perform directory from current place
-- Get down from root to current directory. Call given function with current directory and path taked from user.
-- Rebuild all tree with changed current tree.
-- return changed tree
-- may fail with
--  * 'IncorrectRelativeState'
--  * 'FileNotFound'
--  * any 'FSException' from given function
changeFS ::
     FilePath -> FSContext -> (FilePath -> FMTree -> FileManagerT FSException FMTree) -> FileManagerT FSException FMTree
changeFS path (FSContext (Directory dirName children permissions) (x:xs)) updater = do
  let maybeChild = Map.lookup x children
  case maybeChild of
    Just child -> do
      tree <- changeFS path (FSContext child xs) updater
      return $ Directory dirName (Map.insert x tree children) permissions
    Nothing -> throwError $ FileNotFound $ "couldn't find file/directory " ++ x ++ " in directory " ++ dirName
changeFS path context@(FSContext tree []) updater = updater path tree
changeFS _ _ _ = throwError $ IncorrectRelativeState "attempt to work with file like with directory"

-- | Take
--  * Context included tree and current place. tree must be 'Directory'
--  * List of permissions to check.
-- Get down from root to current directory.
-- return currentTree
-- may fail with
--  * 'IncorrectRelativeState'
--  * 'FileNotFound'
getFmTree :: FSContext -> [Permissions -> Bool] -> FileManagerT FSException FMTree
getFmTree (FSContext (Directory name children permissions) (x:xs)) checkPermissions =
  let maybeChild = Map.lookup x children
   in case maybeChild of
        (Just child) ->
          if all (\checker -> checker $ getPermissionsByTree child) checkPermissions
            then getFmTree (FSContext child xs) checkPermissions
            else throwError $
                 NoAccessRights $
                 "hasn't rights to handle: " ++ x ++ " permisions: " ++ (show $ getPermissionsByTree child)
        Nothing -> throwError $ FileNotFound $ "couldn't find file " ++ x ++ " in directory " ++ name
getFmTree (FSContext fmTree []) _ = return fmTree

-- | Take tree, which represent all file system
-- create temporary directory and upload tree to it
-- atomically replace old directory with uploaded
saveFs :: FMTree -> IO ()
saveFs tree = do
  oldCur <- getCurrentDirectory
  newCur <- canonicalizePath $ addPathSuffix oldCur ".."
  case tree of
    (Directory name children permissions) ->
      withCurrentDirectory
        oldCur
        (do saveFsInner newCur (Directory ".tmp" children permissions)
            removeDirectoryRecursive oldCur
            renamePath (addPathSuffix newCur ".tmp") oldCur)
    _ -> print "Something go wrong current directory isn't directory"

saveFsInner :: FilePath -> FMTree -> IO ()
saveFsInner pathPref (Directory path children permissions) = do
  let absolute = addPathSuffix pathPref path
  createDirectoryIfMissing True absolute
  foldMap (saveFsInner absolute) (Map.elems children)
saveFsInner pathPref (File name text _) = withFile (addPathSuffix pathPref name) WriteMode (`C.hPut` text)

getName :: FMTree -> FilePath
getName (Directory name _ _) = name
getName (File name _ _)      = name

getPermissionsByTree :: FMTree -> Permissions
getPermissionsByTree (Directory _ _ permissions) = permissions
getPermissionsByTree (File _ _ permissions)      = permissions

allPermissions :: Permissions
allPermissions = setOwnerReadable True $ setOwnerWritable True $ setOwnerExecutable True emptyPermissions

addPathSuffix :: FilePath -> FilePath -> FilePath
addPathSuffix pref relative =
  if pref == ""
    then relative
    else pref ++ [pathSeparator] ++ relative

makeRelative :: FilePath -> FilePath
makeRelative path = last $ splitOn [pathSeparator] path
