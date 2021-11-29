-- | Module for predefined constants
module Juvix.Backends.LLVM.Codegen.Constants where

import Juvix.Library hiding (Type)
import qualified LLVM.AST.Type as Type

portLength :: Num p => p
portLength = 32

i4 :: Type.Type
i4 = Type.IntegerType 4

i8 :: Type.Type
i8 = Type.IntegerType 8

-- | For now, "int" in our Haskell LLVM code is an alias for 64-bit
-- LLVM integers.  We might at some point want to make this architecture-
-- specific.
int :: Type.Type
int = Type.IntegerType 64

double :: Type.Type
double = Type.FloatingPointType Type.DoubleFP
