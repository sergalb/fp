{-# LANGUAGE ScopedTypeVariables #-}

module Task4HalyavaScriptTests
  ( task4Tests
  ) where

import Control.Parallel.Strategies
import Task1Geometry
import Task4HalyavaScript
import Test.Tasty
import Test.Tasty.HUnit

whileFun :: (HalyavaAST expr) => Int -> expr Int
whileFun =
  sFun1 $ \a res ->
      sWhile
        ((valToRhs a) @> (literal 1))
        (res @= (literal 2))

ifFun :: (HalyavaAST expr) => Int -> expr Int
ifFun =
  sFun1 $ \a res ->
    sIf ((valToRhs a) @> literal (17)) (res @= (literal 1)) (res @= (literal 2))

sequntialyAssign :: (HalyavaAST expr) => Int -> expr Int
sequntialyAssign =
  sFun1 $ \a res ->
      (res @= (literal 1)) #
      (res @= (literal 2)) 


complexFun :: (HalyavaAST expr) => Int -> expr Int
complexFun =
  sFun1 $ \a res ->
    sVar 0 $ \accum ->
      ((accum @= (literal 1)) # (res @= (literal 0))) #
      sWhile
        ((valToRhs a) @> (eRead accum))
        ((accum @= eRead accum) # (res @= eRead res))

complexRes :: String
complexRes =
  "function(v0) {\nv1 = 0;\nv2 = 0;\nv2 = 1;\nv1 = 0;\nwhile (\"v0\" > \"v2\") {\nv2 = \"v2\";\nv1 = \"v1\";\n}\nreturn v1;\n}"

ifRes :: String
ifRes =
  "function(v0) {\nv1 = 0;\nif (\"v0\" > 17) {\nv1 = 1;;\nv1 = 2;;\n\nreturn v1;\n}"
  
whileRes :: String
whileRes = "function(v0) {\nv1 = 0;\nwhile (\"v0\" > 1) {\nv1 = 2;\n}\nreturn v1;\n}"

sequantialyAssignRes :: String
sequantialyAssignRes = "function(v0) {\nv1 = 0;\nv1 = 1;\nv1 = 2;\nreturn v1;\n}"

  
intFuncToStr :: (Int -> Printer Int) -> String
intFuncToStr f = toString (f 0) 0

task4Tests :: TestTree
task4Tests = testGroup "Task 4 tests" [printingTests]

printingTests :: TestTree
printingTests =
  testGroup
    "complex"
    [ testCase "while function" $
      assertEqual "uncorrect printing" whileRes (intFuncToStr whileFun)
    , testCase "if function" $
      assertEqual "uncorrect printing" ifRes (intFuncToStr ifFun)
    , testCase "sequantialyAssign function" $
          assertEqual "uncorrect printing" sequantialyAssignRes (intFuncToStr sequntialyAssign)
    , testCase "complex function" $
      assertEqual "uncorrect printing" complexRes (intFuncToStr complexFun)
    ]
