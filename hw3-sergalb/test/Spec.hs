import Test.Tasty

import Task1GeometryTests (task1Tests) 
import Task4HalyavaScriptTests (task4Tests)

main :: IO ()
main =
  defaultMain $
  testGroup "tests for hw 3" [task1Tests, task4Tests]
