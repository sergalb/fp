{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
module FSException
  ( FSException(..)
  ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)


data FSException =
    PathDoesntExist String
  | NoAccessRights String
  | IncorrectRelativeState String
  | FileNotFound String
  | InitException String
  | VcsDoesNotInitialized
  | RevisionDoesntExist String
  deriving (Show, Typeable, Exception, Eq)
