{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Task6FSTraverse where

import Lens.Micro (Traversal', (^.), filtered, traversed)

import Task5FileSystemLens (FS, contents, dirNamePrism, fileNamePrism, name)

cd :: FilePath -> Traversal' FS FS
cd directoryName =
  contents . traversed . (filtered (\n -> n ^. dirNamePrism == directoryName))

ls :: Traversal' FS FilePath
ls = contents . traversed . name

file :: FilePath -> Traversal' FS FS
file fName = contents . traversed . filtered (\f -> f ^. fileNamePrism == fName)
