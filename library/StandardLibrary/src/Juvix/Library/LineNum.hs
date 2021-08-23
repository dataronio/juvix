{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Juvix.Library.LineNum where

import Control.Lens
import qualified Data.Aeson as A
import Juvix.Library

data T = T {tLine :: Int, tCol :: Int} deriving (Show, Eq, Ord, Data, Generic)

instance A.ToJSON T where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON T where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

makeLensesWith camelCaseFields ''T
