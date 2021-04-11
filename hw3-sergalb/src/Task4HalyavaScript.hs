{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Task4HalyavaScript where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)

data Var a =
  Var
    { _var     :: a,
      _varName :: String
    }

data Val a =
  Val
    { _val     :: a
    , _valName :: String
    }

makeLenses ''Var
makeLenses ''Val


instance Show (Val a) where
  show = (^. valName)

class MyMonoid a where
  myEmpty :: a

-- todo what? i get error duplicate instance
--instance Monoid a => MyMonoid a where
--  myEmpty :: a
--  myEmpty = mempty
instance MyMonoid Int where
  myEmpty :: Int
  myEmpty = (0 :: Int)

data RHS b where
  ShowableRHS :: Show a => a -> RHS b

instance Show (RHS v) where
  show :: RHS v -> String
  show (ShowableRHS a) = show a


class HalyavaAST expr where
  sFun1 :: (Show a, Show b, MyMonoid b) => (Val a -> Var b -> expr b) -> a -> expr b
  sVar :: Show a => a -> (Var a -> expr b) -> expr b
  sIf :: expr Bool -> expr a -> expr a -> expr a
  sWhile :: expr Bool -> expr a -> expr a
  (@=) :: Var a -> RHS a -> expr a
  (#) :: expr a -> expr b -> expr b
  (@>) :: Ord a => RHS a -> RHS a -> expr Bool

newtype Printer a =
  Printer
    { toString :: (Int) -> String
    }

instance HalyavaAST Printer where
  sFun1 :: (Show a, Show b, MyMonoid b) => (Val a -> Var b -> Printer b) -> a -> Printer b
  sFun1 (fun :: (Val a -> Var b -> Printer b))  value =
    Printer $ \_ ->
      "function(v0) {" ++
      lineSeparator ++
      "v1 = " ++ (show (myEmpty :: b)) ++ ";" ++ lineSeparator ++
      (toString (fun (Val value "v0") (Var myEmpty "v1")) 2) ++
      lineSeparator ++ "return v1;" ++ lineSeparator ++ "}"

  sVar :: Show a => a -> (Var a -> Printer b) -> Printer b
  sVar val varFunc =
    Printer $ \identifier ->
      "v" ++
      (show identifier) ++
      " = " ++
      (show val) ++ ";"  ++ lineSeparator ++
      (toString
         (varFunc $ Var val ("v" ++ show identifier))
         (identifier + 1))
  var @= val =
    Printer $ \_ ->
      var ^. varName ++ " = " ++ (show val) ++ ";"
  sIf cond thenExpr elseExpr =
    Printer $ \identifier ->
      "if (" ++
      (toString cond identifier) ++
      ") {" ++
      lineSeparator ++
      (toString thenExpr identifier) ++
      ";" ++
      lineSeparator ++ (toString elseExpr identifier) ++ ";" ++ lineSeparator
  sWhile cond expr =
    Printer $ \identifier ->
      "while (" ++
      (toString cond identifier) ++
      ") {" ++
      lineSeparator ++
      (toString expr identifier)
      ++ lineSeparator ++ "}"
  a # b = Printer $ \identifier -> (toString a identifier) ++ lineSeparator ++ (toString b identifier)
  a @> b = Printer $ \_ -> (show a) ++ " > " ++ (show b)

eRead :: Var a -> RHS a
eRead (Var (_ :: a) name) = (ShowableRHS name :: RHS a)

literal :: Show a => a -> RHS a
literal v = ShowableRHS v

valToRhs :: Val a -> RHS a
valToRhs (Val _ n) = ShowableRHS n


lineSeparator :: String
lineSeparator = "\n"
