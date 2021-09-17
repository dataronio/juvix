module Juvix.Sexp.Structure
  ( Structure,
    to,
    from,
  )
where

import Juvix.Library (Maybe)
import qualified Juvix.Sexp as Sexp

class Structure a where
  to :: Sexp.T -> Maybe a
  from :: a -> Sexp.T
