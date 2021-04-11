{-# LANGUAGE Rank2Types #-}

module Task7ChangeFS where

import Lens.Micro (Traversal', filtered, traversed, (%~), (&), (.~), (^.),
                   (^..), (^?), (<&>))
import System.FilePath (takeBaseName)
import Task5FileSystemLens (FS (..), Prism', contents, dirNamePrism, name, prism, _File)
import Task6FSTraverse (cd)

changeFileFormats :: FilePath -> FS -> FS
changeFileFormats newExtension = contents . traversed . _File %~ (\file -> takeBaseName file ++ "." ++ newExtension)

getAllNamesRecursively :: Traversal' FS FilePath
getAllNamesRecursively = rec . traversed
  where
    rec :: Prism' FS [FilePath]
    rec = prism set get
    get :: FS -> Maybe [FilePath]
    get (File _name) = Just [_name]
    get dir          = Just $ ((dir ^. name)) : (dir ^.. contents . traversed . rec . traversed)
    set :: [FilePath] -> FS
    set _name = undefined

removeEmptyDir :: FilePath -> FS -> FS
removeEmptyDir directoryName fs =
  fs & contents .~ fs ^.. contents . traversed .
  (filtered (\n -> not ((n ^. dirNamePrism == directoryName) && (null (n ^. contents)))))

move :: FilePath -> Prism' FS FS
move newDir = prism set get
                where
                  get :: FS -> Maybe FS
                  get dir@(Dir oldName _) = (dir ^? cd newDir) <&> (name .~ (oldName ++ "/" ++ newDir))
                  get _ = Nothing
                  set :: FS -> FS
                  set fs = fs

getPath :: Traversal' FS FilePath
getPath = dirNamePrism