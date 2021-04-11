{-# LANGUAGE Rank2Types #-}

module Task5FileSystemLens where

import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid (First)
import Lens.Micro
  ( Getting
  , Lens'
  , Traversal'
  , (%~)
  , (&)
  , (.~)
  , (^..)
  , (^?)
  , _1
  , _2
  , lens
  , traversed
  )
import System.Directory (doesDirectoryExist, doesPathExist, listDirectory)
import System.FilePath (pathSeparator, takeFileName)

data FS
  = Dir
      { _name :: FilePath
      , _contents :: [FS]
      }
  | File
      { _name :: FilePath
      }
  deriving (Show)

type Prism' s a
   = forall f. Applicative f =>
                 (a -> f a) -> s -> f s

-- | Take absolute path to directory
-- upload it in 'FS'
-- return uploaded tree
initFS :: FilePath -> IO FS
initFS absolutePath = do
  pathExist <- doesPathExist absolutePath
  isDirectory <- doesDirectoryExist absolutePath
  let file = takeFileName absolutePath
  if not pathExist
    then error $ "file: '" ++ absolutePath ++ "' not found"
    else if isDirectory
           then do
             innerOfDir <- listDirectory absolutePath
             dirContents <-
               foldl' (addInner absolutePath) (return []) innerOfDir
             return $ Dir file dirContents
           else do
             return $ File file
  where
    addInner :: FilePath -> IO [FS] -> FilePath -> IO [FS]
    addInner pathPref dirContentIO elemPath = do
      dirContent <- dirContentIO
      element <- initFS (pathPref ++ [pathSeparator] ++ elemPath)
      return $ element : dirContent

name :: Lens' FS FilePath
name = lens (_name) (\fs newName -> fs {_name = newName})

fileNamePrism :: Traversal' FS FilePath
fileNamePrism = _File

dirNamePrism :: Traversal' FS FilePath
dirNamePrism = _Dir . _1

contents :: Traversal' FS [FS]
contents = _Dir . _2

-- |type Prism' s a = forall f . Applicative f => (a -> f a) -> s -> f s
prism :: (a -> s) -> (s -> Maybe a) -> Prism' s a
prism set get =
  \func obj ->
    case (get obj) of
      Just r -> set <$> func r
      Nothing -> pure obj

_Dir :: Prism' FS (FilePath, [FS])
_Dir = prism set get
  where
    get :: FS -> Maybe (FilePath, [FS])
    get (Dir _name _contents) = Just (_name, _contents)
    get _ = Nothing
    set :: (FilePath, [FS]) -> FS
    set (_name, _contents) = Dir _name _contents

_File :: Prism' FS FilePath
_File = prism set get
  where
    get :: FS -> Maybe FilePath
    get (File _name) = Just _name
    get _ = Nothing
    set :: FilePath -> FS
    set _name = File _name

(^?.) :: Monoid a => s -> Getting (First a) s a -> a
s ^?. l = fromMaybe mempty (s ^? l)

{-# INLINE (^?.) #-}
infixl 8 ^?.

-- lens :: (s -> a) -> (s -> a -> s) -> (a -> f a) -> s -> f s
lens' :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens' get set = \f s -> set s <$> f (get s)

subdirs :: FS -> [(FilePath, [FS])]
subdirs fs = fs ^.. contents . traversed . _Dir

dirName :: FS -> Maybe FilePath
dirName fs = fs ^? (_Dir . _1)

fileName :: FS -> FilePath
fileName fs = fs ^?. _File

setRootNameToOsRoot :: FS -> FS
setRootNameToOsRoot fs = fs & _Dir . _1 .~ "/"

appendRootName :: FS -> FilePath -> FS
appendRootName fs suffix = fs & _Dir . _1 %~ (++ suffix)

fileNames :: FS -> [FilePath]
fileNames fs = fs ^.. contents . traversed . _File
