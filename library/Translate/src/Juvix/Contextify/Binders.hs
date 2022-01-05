-- | @Juvix.Contextify.Binders@ covers matching binders and the users
-- patterns for means of processing. See examples of how partial
-- serialization works in Test.Contextify.Binders
module Juvix.Contextify.Binders where

import Juvix.Library hiding (Type, on)
import qualified Juvix.Sexp as Sexp
import qualified Juvix.Sexp.Structure.Parsing as Structure
import qualified Juvix.Sexp.Structure.Transition as Structure

--------------------------------------------------------------------------------
-- Binder Declarations
--------------------------------------------------------------------------------

data BinderPlus a
  = Other a
  | Lambda
      { binderPlusArgs :: Sexp.B (BinderPlus a),
        binderPlusBody :: Sexp.B (BinderPlus a)
      }
  | Declaim
      { binderPlusClaim :: Sexp.B (BinderPlus a),
        binderPlusBody :: Sexp.B (BinderPlus a)
      }
  | LetMatch
      { binderPlusName :: Symbol,
        binderPlusArgMatch :: [ArgBody (BinderPlus a)],
        binderPlusBody :: Sexp.B (BinderPlus a)
      }
  | Primitive {binderPlusTerm :: Sexp.B (BinderPlus a)}
  | LetType
      { binderPlusNameAndSig :: Sexp.B (BinderPlus a),
        binderPlusArgs :: Sexp.B (BinderPlus a),
        binderPlusBody :: Sexp.B (BinderPlus a),
        binderPlusRest :: Sexp.B (BinderPlus a)
      }
  | LetHandler (LetHandler (BinderPlus a))
  | LambdaCase (LambdaCase (BinderPlus a))
  | Case (Case (BinderPlus a))
  | OpenIn
      { binderPlusModu :: Sexp.B (BinderPlus a),
        binderPlusBody :: Sexp.B (BinderPlus a)
      }
  | Type (Type (BinderPlus a))
  deriving (Show, Eq, Generic)

-- we can't handle this derivation of list* automatically
data Case a
  = Case' (Sexp.B a) [DeconBody a]
  deriving (Show, Generic, Eq)

-- we can't handle this derivation of list* automatically
data LambdaCase a
  = LambdaCase' [DeconBody a]
  deriving (Show, Generic, Eq)

-- we can't handle this derivation of list* automatically
data LetHandler a
  = LetHandler' (Sexp.B a) (LetOp a) [LetRet a]
  deriving (Show, Generic, Eq)

-- we can't handle this derivation of list* automatically
data Type a
  = Type' (Sexp.B a) (Sexp.B a) (Sexp.B a)
  deriving (Show, Generic, Eq)

data LetOp a = Defop
  { letOpName :: Sexp.B a,
    letOpArgs :: Sexp.B a,
    letOpBody :: Sexp.B a
  }
  deriving (Show, Generic, Eq)

data LetRet a = Defret
  { letRetArg :: Sexp.B a,
    letRetBody :: Sexp.B a
  }
  deriving (Show, Generic, Eq)

data ArgBody a = ArgBody
  { argBodyArgs :: Sexp.B a,
    argBodyBody :: Sexp.B a
  }
  deriving (Show, Generic, Eq)

data DeconBody a = DeconBody
  { deconBodyPat :: Sexp.B a,
    deconBodyBody :: Sexp.B a
  }
  deriving (Show, Generic, Eq)

argToDecon :: ArgBody a -> DeconBody a
argToDecon (ArgBody a b) = DeconBody a b

deconToArg :: DeconBody a -> ArgBody a
deconToArg (DeconBody a b) = ArgBody a b

--------------------------------------------------------------------------------
-- Derivation No Changes
--------------------------------------------------------------------------------

instance Sexp.DefaultOptions (LetRet a)
instance Sexp.Serialize a => Sexp.Serialize (LetRet a)

instance Sexp.DefaultOptions (LetOp a)
instance Sexp.Serialize a => Sexp.Serialize (LetOp a)

--------------------------------------------------------------------------------
-- Main Derivation
--------------------------------------------------------------------------------

instance Sexp.DefaultOptions (BinderPlus a)

-- We give an explicit derivation to namely make the @Other@
-- constructor not take a spot and win any ambiguities
instance Sexp.Serialize a => Sexp.Serialize (BinderPlus a) where
  serialize (Other a) =
    Sexp.serialize a
  serialize (Case case') =
    Sexp.serialize case'
  serialize (LambdaCase case') =
    Sexp.serialize case'
  serialize (LetHandler handler) =
    Sexp.serialize handler
  serialize other =
    Sexp.serializeOpt (Sexp.defaultOptions @(BinderPlus ())) other

  deserialize xs =
    -- we want to try deserializing the generic first, as it may
    -- overlap, in which we want it to win
    (Other <$> Sexp.deserialize @a xs)
      <|> (Case <$> Sexp.deserialize @(Case (BinderPlus a)) xs)
      <|> (LambdaCase <$> Sexp.deserialize @(LambdaCase (BinderPlus a)) xs)
      <|> (LetHandler <$> Sexp.deserialize @(LetHandler (BinderPlus a)) xs)
      <|> Sexp.deserializeOpt (Sexp.defaultOptions @(BinderPlus ())) xs

--------------------------------------------------------------------------------
-- No Constructor Derivations
--------------------------------------------------------------------------------

instance Sexp.Serialize a => Sexp.Serialize (ArgBody a) where
  serialize (ArgBody args body) =
    Sexp.list [Sexp.serialize args, Sexp.serialize body]
  deserialize (Sexp.List [a1, a2]) =
    ArgBody <$> Sexp.deserialize a1 <*> Sexp.deserialize a2
  deserialize _ = Nothing

instance Sexp.Serialize a => Sexp.Serialize (DeconBody a) where
  serialize (DeconBody args body) =
    Sexp.list [Sexp.serialize args, Sexp.serialize body]
  deserialize (Sexp.List [a1, a2]) =
    DeconBody <$> Sexp.deserialize a1 <*> Sexp.deserialize a2
  deserialize _ = Nothing

--------------------------------------------------------------------------------
-- Group By 2 Derivations
--------------------------------------------------------------------------------

instance {-# OVERLAPPING #-} Sexp.Serialize a => Sexp.Serialize [ArgBody a] where
  serialize = Sexp.unGroupBy2 . toStarList (Sexp.serialize @(ArgBody _))
  deserialize = fromStarList (Sexp.deserialize @(ArgBody _)) . Sexp.groupBy2

--------------------------------------------------------------------------------
-- list* derivations
--------------------------------------------------------------------------------

instance Sexp.Serialize a => Sexp.Serialize (LetHandler a) where
  serialize (LetHandler' name sexp argbody) =
    Sexp.listStar
      [ Sexp.atom Structure.nameHandler,
        Sexp.serialize name,
        Sexp.serialize sexp,
        Sexp.serialize argbody
      ]

  -- old auto generated deserialize by hand... need to update it to
  -- generate this!
  deserialize form
    | Structure.isHandler form =
      case form of
        _name Sexp.:> name Sexp.:> letOp Sexp.:> letRet ->
          LetHandler'
            <$> Sexp.deserialize name
            <*> Sexp.deserialize @(LetOp a) letOp
            <*> Sexp.deserialize @[LetRet a] letRet
        _ -> Nothing
    | otherwise = Nothing

-- custom instance for case, as star style functions aren't derivable
-- yet
instance Sexp.Serialize a => Sexp.Serialize (Case a) where
  serialize (Case' sexp argbody) =
    Sexp.listStar
      [Sexp.atom Structure.nameCase, Sexp.serialize sexp, Sexp.serialize argbody]

  -- old auto generated deserialize by hand... need to update it to
  -- generate this!
  deserialize form
    | Structure.isCase form =
      case form of
        _name Sexp.:> sexp1 Sexp.:> argsBody ->
          Case'
            <$> Sexp.deserialize sexp1 <*> Sexp.deserialize @[DeconBody a] argsBody
        _ -> Nothing
    | otherwise = Nothing

-- custom instance for case, as star style functions aren't derivable
-- yet
instance Sexp.Serialize a => Sexp.Serialize (LambdaCase a) where
  serialize (LambdaCase' decons) =
    Sexp.listStar
      [Sexp.atom Structure.nameLambdaCase, Sexp.serialize decons]

  -- old auto generated deserialize by hand... need to update it to
  -- generate this!
  deserialize form
    | Structure.isCase form =
      case form of
        _name Sexp.:> decons ->
          LambdaCase' <$> Sexp.deserialize @[DeconBody a] decons
        _ -> Nothing
    | otherwise = Nothing

instance Sexp.Serialize a => Sexp.Serialize (Type a) where
  serialize (Type' nameAndSig args body) =
    Sexp.listStar
      [ Sexp.atom Structure.nameType,
        Sexp.serialize nameAndSig,
        Sexp.serialize args,
        Sexp.serialize body
      ]

  -- old auto generated deserialize by hand... need to update it to
  -- generate this!
  deserialize form
    | Structure.isCase form =
      case form of
        _nameType Sexp.:> sexp1 Sexp.:> sexp2 Sexp.:> sexp3 ->
          Type' <$> Sexp.deserialize sexp1 <*> Sexp.deserialize sexp2 <*> Sexp.deserialize sexp3
        _ -> Nothing
    | otherwise = Nothing

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

toStarList :: (t -> Sexp.B a) -> [t] -> Sexp.B a
toStarList f (x : xs) =
  f x Sexp.:> toStarList f xs
toStarList _ [] = Sexp.Nil

fromStarList :: (Sexp.B b -> Maybe a) -> Sexp.B b -> Maybe [a]
fromStarList f (x Sexp.:> xs) =
  (:) <$> f x <*> fromStarList f xs
fromStarList _ Sexp.Nil = Just []
fromStarList _ _ = Nothing
