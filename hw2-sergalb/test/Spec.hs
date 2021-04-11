import Test.Tasty

import FSTests (fileSystemTests)

main :: IO ()
main =
  defaultMain $
  testGroup "tests for hw 1" [fileSystemTests]
