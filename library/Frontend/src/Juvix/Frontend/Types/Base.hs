{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- - This file defines the main ADT for the Juvix front end language.
-- - This ADT corresponds to the bnf laid out [[https://github.com/heliaxdev/juvix/blob/develop/doc/Frontend/syntax.org][here]].
-- - Later a trees that grow version of this will be implemented, so
--   infix functions can better transition across syntax
-- - Note :: The names for the types in =ArrowData= are stored in the
--           =ArrowGen= and not in =NamedType=
module Juvix.Frontend.Types.Base where

import qualified Data.Aeson as A
import Juvix.Library hiding (Data, Handler, Product, Sum, Type)
import qualified Juvix.Library.Usage as Usage

type ConstructorName = NameSymb

type NameSymb = NonEmpty Symbol

type ModuleName = NameSymb

-- we will want an include... but this will have to be deferred to a context phase
data TopLevel
  = Type Type
  | ModuleOpen ModuleOpen
  | Signature Signature
  | Module Module
  | Function Function
  | Handler Handler
  | Effect Effect
  | Declaration Declaration
  | TypeClass
  | TypeClassInstance
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON TopLevel where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON TopLevel where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------
newtype Declaration
  = Infixivity InfixDeclar
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Declaration where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Declaration where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data InfixDeclar
  = NonAssoc Symbol Natural
  | AssocL Symbol Natural
  | AssocR Symbol Natural
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON InfixDeclar where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON InfixDeclar where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data Type = Typ
  -- Was a usage but can't alias for now
  { typeUsage :: Maybe Expression,
    typeName' :: !Symbol,
    typeArgs :: [Symbol],
    typeForm :: Data
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Type where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Type where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- 'Data' is the data declaration in the Juvix language
data Data
  = Arrowed
      { dataArrow :: Expression,
        dataAdt' :: Adt
      }
  | NonArrowed
      { dataAdt :: Adt
      }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Data where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Data where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

--------------------------------------------------
-- Arrows
--------------------------------------------------
data NamedType = NamedType'
  { nameRefineName :: !Name,
    namedRefineRefine :: Expression
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON NamedType where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON NamedType where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- TODO ∷ change TypeName to TypeNameModule
data TypeRefine = TypeRefine
  { typeRefineName :: Expression,
    typeRefineRefinement :: Expression
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON TypeRefine where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON TypeRefine where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

--------------------------------------------------
-- Types Misc
--------------------------------------------------
data Name
  = Implicit !Symbol
  | Concrete !Symbol
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Name where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Name where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data ArrowSymbol
  = ArrowUse Usage.T
  | -- Was a usage but can't alias for now
    ArrowExp Expression
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON ArrowSymbol where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON ArrowSymbol where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- Was a new type
-- TODO ∷ finish this type!
newtype UniverseExpression
  = UniverseExpression Symbol
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON UniverseExpression where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON UniverseExpression where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

--------------------------------------------------
-- ADTs
--------------------------------------------------
-- The types for ADT are not the most
-- constraining, however are setup to have the
-- least amount of boilerplate in the latter
-- stages as possible
-- a Sum of length one should not have a record
-- [type Address = Foo {abc : Int}]
-- this form should be considered illegal unless we wish to permit
-- named records along with unnamed records.
-- Ι suspect in the future this will instead be used for Enum
-- Subsets with refined information
data Adt
  = Sum (NonEmpty Sum)
  | Product Product
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Adt where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Adt where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data Sum = S
  { sumConstructor :: !Symbol,
    sumValue :: !(Maybe Product)
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Sum where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Sum where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- for when a product is without a sum
-- only a record can apply
-- a sum of only one is a named product
data Product
  = Record !Record
  | Arrow Expression
  | ADTLike [Expression]
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Product where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Product where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data Record = Record''
  { recordFields :: NonEmpty NameType,
    recordFamilySignature :: Maybe Expression
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Record where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Record where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data NameType = NameType'
  { nameTypeSignature :: Expression,
    nameTypeName :: !Name
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON NameType where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON NameType where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

--------------------------------------------------------------------------------
-- Effect Handlers
--------------------------------------------------------------------------------

data Effect = Eff
  { effName :: Symbol,
    effOps :: [Signature]
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Effect where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Effect where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data Operation = Op (FunctionLike Expression)
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Operation where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Operation where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

--  A 'Handler', as implemented here, is a set of functions that implement
-- (at least) one `Effect` interface.
-- However, Handlers are NOT scoped, meaning that they can't be defined
-- defined within another function. We CAN fix that, but it requires
-- us to make some choices, it's wise to have Witch up and running first.
data Handler
  = Hand Symbol [Operation]
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Handler where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Handler where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

--------------------------------------------------------------------------------
-- Functions And Modules
--------------------------------------------------------------------------------
-- was a newtype
-- 'Function' is a normal signature with a name arguments and a body
-- that may or may not have a guard before it
newtype Function
  = Func (FunctionLike Expression)
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Function where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Function where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- 'Module' is like function, however it allows multiple top levels
newtype Module
  = Mod (FunctionLike (NonEmpty TopLevel))
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Module where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Module where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data ModuleE = ModE
  { moduleEBindings :: FunctionLike (NonEmpty TopLevel),
    moduleEBody :: Expression
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON ModuleE where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON ModuleE where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- 'FunctionLike' is the generic version for both modules and functions
data FunctionLike a = Like
  { functionLikedName :: Symbol,
    functionLikeArgs :: [Arg],
    functionLikeBody :: GuardBody a
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON a => A.ToJSON (FunctionLike a) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON a => A.FromJSON (FunctionLike a) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- 'GuardBody' determines if a form is a guard or a body
data GuardBody a
  = Body a
  | Guard (Cond a)
  deriving (Show, Read, Eq, Generic)

instance (A.ToJSON a) => A.ToJSON (GuardBody a) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance (A.FromJSON a) => A.FromJSON (GuardBody a) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

newtype ModuleOpen
  = Open ModuleName
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON ModuleOpen where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON ModuleOpen where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data ModuleOpenExpr = OpenExpress
  { moduleOpenExprModuleN :: ModuleName,
    moduleOpenExprExpr :: Expression
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON ModuleOpenExpr where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON ModuleOpenExpr where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- Very similar to name, but match instead of symbol
data Arg
  = ImplicitA MatchLogic
  | ConcreteA MatchLogic
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Arg where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Arg where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

newtype Cond a
  = C (NonEmpty (CondLogic a))
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON a => A.ToJSON (Cond a) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON a => A.FromJSON (Cond a) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data CondLogic a = CondExpression
  { condLogicPred :: Expression,
    condLogicBody :: a
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON a => A.ToJSON (CondLogic a) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON a => A.FromJSON (CondLogic a) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

--------------------------------------------------------------------------------
-- Signatures
--------------------------------------------------------------------------------
data Signature = Sig
  { signatureName :: Symbol,
    -- Was a usage but can't alias for now
    signatureUsage :: Maybe Expression,
    signatureArrowType :: Expression,
    signatureConstraints :: [Expression]
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Signature where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Signature where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

--------------------------------------------------------------------------------
-- Type Classes
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

-- TODO ∷ add <expression> : <expression> <refine>?
-- to the parser
data Expression
  = Cond (Cond Expression)
  | Constant Constant
  | Let Let
  | ModuleE ModuleE
  | LetType LetType
  | Match Match
  | Name NameSymb
  | OpenExpr ModuleOpenExpr
  | Lambda Lambda
  | Application Application
  | Primitive Primitive
  | List List
  | Tuple Tuple
  | Block Block
  | Infix Infix
  | ExpRecord ExpRecord
  | Do Do
  | -- Added due to merge
    ArrowE ArrowExp
  | NamedTypeE NamedType
  | RefinedE TypeRefine
  | UniverseName UniverseExpression
  | Parened Expression
  | DeclarationE DeclarationExpression
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Expression where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Expression where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data DeclarationExpression
  = DeclareExpression Declaration Expression
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON DeclarationExpression where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON DeclarationExpression where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data Primitive
  = Prim NameSymb
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Primitive where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Primitive where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data List
  = ListLit [Expression]
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON List where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON List where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data Tuple
  = TupleLit [Expression]
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Tuple where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Tuple where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data ArrowExp = Arr'
  { arrowExpLeft :: Expression,
    -- Was a usage but can't alias for now
    arrowExpUsage :: Expression,
    arrowExpRight :: Expression
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON ArrowExp where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON ArrowExp where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data Constant
  = Number Numb
  | String String'
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Constant where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Constant where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data Numb
  = Integer' Integer
  | Double' Double
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Numb where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Numb where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

newtype String'
  = Sho Text
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON String' where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON String' where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

newtype Block = Bloc
  {blockExpr :: Expression}
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Block where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Block where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data Lambda = Lamb
  { lambdaArgs :: NonEmpty MatchLogic,
    lambdaBody :: Expression
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Lambda where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Lambda where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data Application = App
  { applicationName :: Expression,
    applicationArgs :: NonEmpty Expression
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Application where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Application where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- Was a newtype but extensible adds fields
newtype Do
  = Do'' (NonEmpty DoBody)
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Do where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Do where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- promote this to a match!!!
data DoBody = DoBody
  { doBodyName :: Maybe Symbol,
    doBodyExpr :: Expression
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON DoBody where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON DoBody where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- TODO ∷ we need includes in here as well!
-- Was a newtype but extensible adds fields
data ExpRecord = ExpressionRecord
  { expRecordFields :: NonEmpty (NameSet Expression)
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON ExpRecord where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON ExpRecord where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

data Let = Let'
  { letBindings :: FunctionLike Expression,
    letBody :: Expression
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Let where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Let where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data LetType = LetType''
  { letTypeBindings :: Type,
    letTypeBody :: Expression
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON LetType where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON LetType where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- TODO ∷ have letSig

--------------------------------------------------
-- Symbol Binding
--------------------------------------------------

data Infix = Inf
  { infixLeft :: Expression,
    infixOp :: NameSymb,
    infixRight :: Expression
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Infix where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Infix where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

--------------------------------------------------
-- Matching
--------------------------------------------------

data Match = Match''
  { matchOn :: Expression,
    matchBindigns :: NonEmpty MatchL
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON Match where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON Match where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data MatchL = MatchL
  { matchLPattern :: MatchLogic,
    matchLBody :: Expression
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON MatchL where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON MatchL where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

-- TODO ∷ add literals to the match
data MatchLogic = MatchLogic
  { matchLogicContents :: MatchLogicStart,
    matchLogicNamed :: Maybe Symbol
  }
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON MatchLogic where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON MatchLogic where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data MatchLogicStart
  = MatchCon ConstructorName [MatchLogic]
  | MatchName Symbol
  | MatchConst Constant
  | MatchRecord (NonEmpty (NameSet MatchLogic))
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON MatchLogicStart where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON MatchLogicStart where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data NameSet t
  = Punned NameSymb
  | NonPunned NameSymb t
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON t => A.ToJSON (NameSet t) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON t => A.FromJSON (NameSet t) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

data Header topLevel
  = Header NameSymb [topLevel]
  | NoHeader [topLevel]
  deriving (Show, Read, Eq, Generic)

instance A.ToJSON t => A.ToJSON (Header t) where
  toJSON = A.genericToJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})

instance A.FromJSON t => A.FromJSON (Header t) where
  parseJSON = A.genericParseJSON (A.defaultOptions {A.sumEncoding = A.ObjectWithSingleField})
