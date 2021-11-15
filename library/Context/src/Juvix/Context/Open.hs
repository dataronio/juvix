{-# LANGUAGE DeriveAnyClass #-}

module Juvix.Context.Open where

import qualified Data.Aeson as A
import Juvix.Library

data T = Explicit | Implicit deriving (Show, Read, Eq, Generic, NFData)

instance A.ToJSON T where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON T where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data TName a = TName
  { open :: T,
    name :: a
  }
  deriving (Show, Read, Eq, Generic, NFData)

instance (A.ToJSON a) => A.ToJSON (TName a) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.FromJSON a) => A.FromJSON (TName a) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})
