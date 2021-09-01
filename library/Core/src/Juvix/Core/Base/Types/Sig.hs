{-# LANGUAGE UndecidableInstances #-}

module Juvix.Core.Base.Types.Sig
  ( Sig (..),
    Sigs,
  )
where

import qualified Data.Aeson as A
import Data.HashMap.Strict (HashMap)
import qualified Juvix.Core.Base.Types.Base as Core
import Juvix.Library hiding (show)
import qualified Juvix.Library.NameSymbol as NameSymbol

---------------------
-- Core Signatures --
---------------------

type Sigs ext primTy primVal =
  HashMap Core.GlobalName (Sig ext primTy primVal)

data Sig ext primTy primVal
  = DataSig
      { sigDataType :: !(Core.Term ext primTy primVal),
        sigDataCons :: [NameSymbol.T]
      }
  | ConSig
      { sigConType :: !(Maybe (Core.Term ext primTy primVal))
      }
  | ValSig
      { sigValUsage :: !Core.GlobalUsage,
        sigValType :: !(Core.Term ext primTy primVal)
      }
  deriving (Generic)

instance (A.ToJSON ty, A.ToJSON val, Core.CoreAll A.ToJSON ext ty val) => A.ToJSON (Sig ext ty val) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.FromJSON ty, A.FromJSON val, Core.CoreAll A.FromJSON ext ty val) => A.FromJSON (Sig ext ty val) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

deriving instance
  ( Eq primTy,
    Eq primVal,
    Core.TermAll Eq ext primTy primVal,
    Core.ElimAll Eq ext primTy primVal
  ) =>
  Eq (Sig ext primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    Core.TermAll Show ext primTy primVal,
    Core.ElimAll Show ext primTy primVal
  ) =>
  Show (Sig ext primTy primVal)

deriving instance
  ( Data ext,
    Data primTy,
    Data primVal,
    Core.TermAll Data ext primTy primVal,
    Core.ElimAll Data ext primTy primVal
  ) =>
  Data (Sig ext primTy primVal)
