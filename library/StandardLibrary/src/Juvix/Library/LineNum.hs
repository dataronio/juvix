{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Juvix.Library.LineNum where

import Control.Lens
import Juvix.Library
import Data.Hashable()


data T = T {tLine :: Int, tCol :: Int}
  deriving (Show, Eq, Ord, Generic)

instance Hashable T where
  hash (T {tLine, tCol}) = hash (hash tLine, hash tCol)

makeLensesWith camelCaseFields ''T
