{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FSTests
  ( fileSystemTests
  ) where

import Data.Maybe (isJust, isNothing)

import Control.Monad.Except (runExceptT)
import Control.Monad.State.Lazy (runState)
import qualified Data.ByteString.Char8 as C (ByteString)
import Data.Char (ord)
import Data.Either (isLeft)
import Data.Map (Map, empty, fromList, lookup)
import FileManager (FMTree (..), FSContext (..), FileManagerT, addDir, addFile, addPathSuffix,
                    allPermissions, changeDirectory, getName, remove)
import FSException (FSException (..))
import System.Directory (Permissions (..), emptyPermissions, executable, readable, writable)
import Test.Tasty
import Test.Tasty.HUnit

fileSystemTests :: TestTree
fileSystemTests = testGroup "File System tests" [changeDirectoryTests, addTests, removeTests]

changeDirectoryTests :: TestTree
changeDirectoryTests =
  testGroup
    "changeDirectory"
    [ testCase "move one" $ performCorrect [] "c" (contextByRelative ["c"]) changeDirectory
    , testCase "move by .." $ performCorrect ["c"] ".." (contextByRelative []) changeDirectory
    , testCase "2 move" $ performCorrect [] (addPathSuffix "c" "gg") (contextByRelative ["c", "gg"]) changeDirectory
    , testCase "complecated" $
      performCorrect
        ["c"]
        (addPathSuffix "." $ addPathSuffix "gg" $ addPathSuffix ".." "gg")
        (contextByRelative ["c", "gg"])
        changeDirectory
    , testCase "path doesn't exist" $ performIncorrect [] "q" changeDirectory
    ]

addTests :: TestTree
addTests =
  testGroup
    "add tests"
    [ testCase "add dir q" $ performCorrect [] "q" (addInRoot (Directory "q" empty allPermissions)) addDir
    , testCase "add file e" $ performCorrect [] "e" (addInRoot (File "e" "" allPermissions)) addFile
    , testCase "add from c to c" $ performCorrect ["c"] "e" (addInC (File "e" "" allPermissions) ["c"]) addFile
    , testCase "add from root to c" $
      performCorrect [] (addPathSuffix "c" "e") (addInC (File "e" "" allPermissions) []) addFile
    ]

removeTests :: TestTree
removeTests =
  testGroup
    "remove tests"
    [ testCase "remove dir from root" $ performCorrect [] "c" (removedC) remove
    , testCase "remove file from root" $ performCorrect [] "b.txt" (removedB) remove
    , testCase "remove from inner dir" $ performCorrect ["c"] "gg" (removedGG) remove
    , testCase "failed - path not found" $ performIncorrect [] "q" remove
    ]

performCorrect :: [FilePath] -> FilePath -> FSContext -> (FilePath -> FileManagerT FSException a) -> Assertion
performCorrect start path end evaluating =
  let st = contextByRelative start
      moved = evaluating path
      res = getContext st moved
   in (Right $ end) @=? res

performIncorrect :: [FilePath] -> FilePath -> (FilePath -> FileManagerT FSException a) -> Assertion
performIncorrect start path evaluating =
  let st = contextByRelative start
      moved = evaluating path
      res = getContext st moved
   in assertBool "" $ isLeft res

getContext :: FSContext -> FileManagerT FSException a -> Either FSException FSContext
getContext context fileManager =
  let res = runState (runExceptT fileManager) context
   in case res of
        (Right _, newContext) -> Right newContext
        (Left exception, _)   -> Left exception

contextByRelative :: [FilePath] -> FSContext
contextByRelative rel =
  FSContext
    { tree =
        Directory
          "a"
          (fromList
             [ ("b.txt", File "b.txt" "some content" (allPermissions))
             , ( "c"
               , Directory
                   "c"
                   (fromList
                      [ ("cg", Directory "cg" (fromList []) (allPermissions))
                      , ("gg", Directory "gg" (fromList [("empty", File "empty" "" (allPermissions))]) (allPermissions))
                      ])
                   (allPermissions))
             ])
          (allPermissions)
    , relative = rel
    }

addInRoot :: FMTree -> FSContext
addInRoot tree =
  FSContext
    { tree =
        Directory
          "a"
          (fromList
             [ ("b.txt", File "b.txt" "some content" (allPermissions))
             , (getName tree, tree)
             , ( "c"
               , Directory
                   "c"
                   (fromList
                      [ ("cg", Directory "cg" (fromList []) (allPermissions))
                      , ("gg", Directory "gg" (fromList [("empty", File "empty" "" (allPermissions))]) (allPermissions))
                      ])
                   (allPermissions))
             ])
          (allPermissions)
    , relative = []
    }

addInC :: FMTree -> [FilePath] -> FSContext
addInC tree rel =
  FSContext
    { tree =
        Directory
          "a"
          (fromList
             [ ("b.txt", File "b.txt" "some content" (allPermissions))
             , ( "c"
               , Directory
                   "c"
                   (fromList
                      [ ("cg", Directory "cg" (fromList []) (allPermissions))
                      , ("gg", Directory "gg" (fromList [("empty", File "empty" "" (allPermissions))]) (allPermissions))
                      , (getName tree, tree)
                      ])
                   (allPermissions))
             ])
          (allPermissions)
    , relative = rel
    }

removedC :: FSContext
removedC =
  FSContext
    { tree = Directory "a" (fromList [("b.txt", File "b.txt" "some content" (allPermissions))]) (allPermissions)
    , relative = []
    }

removedB :: FSContext
removedB =
  FSContext
    { tree =
        Directory
          "a"
          (fromList
             [ ( "c"
               , Directory
                   "c"
                   (fromList
                      [ ("cg", Directory "cg" (fromList []) (allPermissions))
                      , ("gg", Directory "gg" (fromList [("empty", File "empty" "" (allPermissions))]) (allPermissions))
                      ])
                   (allPermissions))
             ])
          (allPermissions)
    , relative = []
    }

removedGG :: FSContext
removedGG =
  FSContext
    { tree =
        Directory
          "a"
          (fromList
             [ ("b.txt", File "b.txt" "some content" (allPermissions))
             , ("c", Directory "c" (fromList [("cg", Directory "cg" (fromList []) (allPermissions))]) (allPermissions))
             ])
          (allPermissions)
    , relative = ["c"]
    }
