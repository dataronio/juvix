{-# LANGUAGE UndecidableInstances #-}

-- |
-- - This file defines the main ADT for the Juvix front end language.
-- - This ADT corresponds to the BNF laid out [[https://juvix.readthedocs.io/en/latest/compiler/frontend/s-expression-syntax.html#bnf-syntax][here]].
-- - Later a trees that grow version of this will be implemented, so
--   infix functions can better transition across syntax
-- - Note :: The names for the types in =ArrowData= are stored in the
--           =ArrowGen= and not in =NamedType=
module Juvix.Parsing.Types
  ( module Juvix.Parsing.Types,
    module Juvix.Parsing.Types.Base,
    Header (..),
  )
where

import Juvix.Parsing.Types.Base

-- Unwrap the header from the rest of the definitions
extractTopLevel :: Header topLevel -> [topLevel]
extractTopLevel (Header _ tops) = tops
extractTopLevel (NoHeader tops) = tops
