-- |
--
-- A Recursive list represents the concept of a list with a chance to nest.
-- This allows us to get a tree like structure, where a pipeline step can contain more steps.
module Juvix.BerlinPipeline.RecursiveList where

import Juvix.Library

data T
