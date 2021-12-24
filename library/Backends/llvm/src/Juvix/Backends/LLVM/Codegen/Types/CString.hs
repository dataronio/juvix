-- | Defines a type for \00 terminated strings
module Juvix.Backends.LLVM.Codegen.Types.CString
  ( CString,
    fromText,
    length,
    encodeUtf8,
  )
where

import Data.Hashable
import qualified Data.Text as Text
import Juvix.Library hiding (encodeUtf8, length)
import qualified Juvix.Library as J
import qualified Prelude

newtype CString = CString Text
  deriving (Eq, Ord, Semigroup, Monoid, Hashable)

instance Show CString where
  show = Prelude.show . unCString

instance ConvertText Text CString where
  toS = fromText

fromText :: Text -> CString
fromText = CString

unCString :: CString -> Text
unCString (CString t) = t <> "\00"

-- | @length@ takes into account the terminating character.
length :: CString -> Int
length (CString t) = Text.length t + 1

encodeUtf8 :: CString -> ByteString
encodeUtf8 = J.encodeUtf8 . unCString
