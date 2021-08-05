{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Juvix.Library.LineNum where

import Control.Lens
import Data.Hashable ()
import Juvix.Library

data T = T {tLine :: Int, tCol :: Int}
  deriving (Show, Eq, Ord, Data, Generic)

instance Hashable T where
  hash (T {tLine, tCol}) = hash (hash tLine, hash tCol)

makeLensesWith camelCaseFields ''T
