-- | Provides a mechanism for defining Sum types
-- - Has the code to encode a sum type via what is defined by the user or
--   what is defined to create the interaction net system.
module Juvix.Backends.LLVM.Codegen.Types.Sum where

import qualified Juvix.Backends.LLVM.Codegen.Constants as Constants
import Juvix.Backends.LLVM.Codegen.Types.Shared
import Juvix.Library hiding (Type)
import qualified Juvix.Library.HashMap as Map
import LLVM.AST
import LLVM.AST.Type
import qualified LLVM.AST.Type as Type
import qualified Prelude as Prelude (error)

-----------------------------------------------------------------------------------------
-- Types
-----------------------------------------------------------------------------------------

type Size = Int

-- | Data needed to make a Variant
data VariantInfo = Variant
  { size :: Size,
    name :: Symbol,
    typ' :: Type
  }

-----------------------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------------------

-- TODO ∷ Optimize this using hacker's delight

-- | 'sumSize' takes a list of variants and creates the array type to hold the
-- largest variant
sumSize :: [VariantInfo] -> Type
sumSize variants
  -- Assume 8 is the smallest allocation type
  | largest `mod` 16 == 0 = ArrayType (divLarg 16) i16
  | largest `mod` 08 == 0 = ArrayType (divLarg 08) i8
  | otherwise = ArrayType (divLarg 4) Constants.i4
  where
    largest = maximumDef 0 (size <$> variants)
    divLarg = div (fromIntegral largest)

-- TODO ∷ optimize this using bit-wise functions
-- Think hacker's delight
-- Also should we allow smaller than i8?

-- | 'tagSize' takes a list of variants and figures out what the size of the tag should be
tagSize :: [VariantInfo] -> Type
tagSize variants
  | len < pow2 4 = Constants.i4
  | len < pow2 8 = i8
  | len < pow2 16 = i16
  | len < pow2 32 = i32
  | otherwise = i64
  where
    pow2 :: Int -> Int
    pow2 = (2 ^)
    len = length variants

-- | grabs the integer length from an IntegerType Type
tagSizeIntExn :: Type -> Word32
tagSizeIntExn (Type.IntegerType i) = i
tagSizeIntExn _ = Prelude.error "unsupported operation"

createVariantName :: Symbol -> Symbol -> Symbol
createVariantName sumName varName = sumName <> "-" <> varName

-----------------------------------------------------------------------------------------
-- Functions for updating the symbol table and variant table for new variants
-----------------------------------------------------------------------------------------

sumPack :: Bool
sumPack = True

createSum :: [VariantInfo] -> Type
createSum variants =
  StructureType
    { isPacked = sumPack,
      elementTypes =
        [tag, arrSize]
    }
  where
    tag = tagSize variants
    arrSize = sumSize variants

-- | updates the type of the variant, to properly have the tag
updateVariant :: Type -> VariantInfo -> VariantInfo
updateVariant tagType (Variant s n t) =
  Variant s n (StructureType packed fields)
  where
    (packed, fields) = case t of
      StructureType p ele -> (p, tagType : ele)
      _ -> (sumPack, [tagType, t])

-- | 'insertSums' creates a sum type, and inserts the new types into the symbol table
-- and the variant table for all the newly created variants
insertSums ::
  Symbol ->
  [VariantInfo] ->
  SymbolTable ->
  VariantToType ->
  TypeTable ->
  (SymbolTable, VariantToType, TypeTable)
insertSums sumName variants symTbl varTbl typTbl = (newSymTbl, newVarTbl, newTypTbl)
  where
    sum' = createSum variants
    tag' = tagSizeIntExn (tagSize variants)
    typTbl' = Map.insert sumName sum' typTbl
    symTbl' =
      Map.insert sumName (LocalReference sum' (mkName (unintern sumName))) symTbl
    newVarTbl =
      fst $
        foldr
          ( \(Variant {name = n}) (tbl, offset) ->
              ( Map.insert
                  (createVariantName sumName n)
                  ( S
                      { sum' = sumName,
                        offset = offset,
                        tagSize' = tag'
                      }
                  )
                  tbl,
                succ offset
              )
          )
          (varTbl, 0)
          variants
    newTypTbl =
      foldr
        ( \(Variant _s n t) tbl ->
            Map.insert (createVariantName sumName n) t tbl
        )
        typTbl'
        variants
    newSymTbl =
      foldr
        ( \(Variant _s n t) tbl ->
            let name = createVariantName sumName n
                operand = LocalReference t (mkName (unintern name))
             in Map.insert name operand tbl
        )
        symTbl'
        variants
