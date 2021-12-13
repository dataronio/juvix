{-# LANGUAGE UndecidableInstances #-}

module Juvix.Sexp.Structure
  ( to,
    from,
  )
where

import Juvix.Library (Maybe)
import qualified Juvix.Sexp as Sexp

to :: Sexp.Serialize a => Sexp.T -> Maybe a
to = Sexp.deserialize

from :: Sexp.Serialize a => a -> Sexp.T
from = Sexp.serialize
