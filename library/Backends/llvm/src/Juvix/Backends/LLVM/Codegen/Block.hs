-- |
-- - Has the code necessary to generate LLVM Code
module Juvix.Backends.LLVM.Codegen.Block where

import Data.ByteString.Short hiding (length)
import Juvix.Backends.LLVM.Codegen.Types as Types
import Juvix.Backends.LLVM.Codegen.Types.Shared
import Juvix.Library hiding (Type, local)
import qualified Juvix.Library.HashMap as Map
import LLVM.AST
import qualified LLVM.AST as AST
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.IntegerPredicate as IntPred
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import qualified LLVM.AST.ParameterAttribute as ParameterAttribute
import qualified LLVM.AST.Type as Type
import Prelude (String)

--------------------------------------------------------------------------------
-- Codegen Operations
--------------------------------------------------------------------------------

fresh :: (HasState "count" b m, Enum b) => m b
fresh = do
  i <- get @"count"
  put @"count" (succ i)
  pure i

resetCount :: (HasState "count" s m, Num s) => m ()
resetCount = put @"count" 0

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

createBlocks ::
  ( HasState "blocks" (Map.HashMap Name BlockState) m,
    HasThrow "err" Errors m
  ) =>
  m [BasicBlock]
createBlocks = do
  sortedBlocks <- sortBlocks . Map.toList <$> get @"blocks"
  traverse makeBlock sortedBlocks

makeBlock :: HasThrow "err" Errors f => (Name, BlockState) -> f BasicBlock
makeBlock (l, BlockState i s t) = maketerm t >>| BasicBlock l (reverse s)
  where
    maketerm (Just x) = pure x
    maketerm Nothing = throw @"err" (BlockLackingTerminator i)

sortBlocks :: [(a, BlockState)] -> [(a, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

entryBlockName :: IsString p => p
entryBlockName = "entry"

emptyCodegen :: Types.CodegenState
emptyCodegen =
  Types.CodegenState
    { Types.currentBlock = mkName entryBlockName,
      Types.blocks = Map.empty,
      Types.symTab = Map.empty,
      Types.count = 0,
      Types.names = Map.empty,
      Types.blockCount = 1,
      Types.moduleAST = emptyModule "juvix-module",
      Types.debug = 0
    }

execEnvState :: Codegen a -> SymbolTable -> CodegenState
execEnvState (Types.CodeGen m) a =
  execState (runExceptT m) (emptyCodegen {Types.symTab = a})

evalEnvState :: Codegen a -> SymbolTable -> Either Errors a
evalEnvState (Types.CodeGen m) a =
  evalState (runExceptT m) (emptyCodegen {Types.symTab = a})

runEnvState :: Codegen a -> SymbolTable -> (Either Errors a, CodegenState)
runEnvState (Types.CodeGen m) a =
  runState (runExceptT m) (emptyCodegen {Types.symTab = a})

--------------------------------------------------------------------------------
-- Module Level
--------------------------------------------------------------------------------

emptyModule :: ShortByteString -> Module
emptyModule label = AST.defaultModule {moduleName = label}

-- TODO :: Figure out the semantics of a function like this.
inModule :: HasState "moduleAST" Module m => Module -> m ()
inModule = undefined

addDefn :: HasState "moduleDefinitions" [Definition] m => Definition -> m ()
addDefn d = modify @"moduleDefinitions" (<> [d])

addType :: HasState "moduleDefinitions" [Definition] m => Name -> Type -> m ()
addType name typ =
  addDefn (TypeDefinition name (Just typ))

defineGen ::
  HasState "moduleDefinitions" [Definition] m =>
  Bool ->
  Type ->
  Symbol ->
  [(Type, Name)] ->
  [BasicBlock] ->
  m Operand
defineGen isVarArgs retty label argtys body = do
  addDefn $
    GlobalDefinition $
      functionDefaults
        { Global.parameters = params,
          -- Figure out which is best!
          Global.callingConvention = CC.Fast,
          Global.returnType = retty,
          Global.basicBlocks = body,
          Global.name = internName label
        }
  return $
    ConstantOperand $
      C.GlobalReference
        (Types.pointerOf (FunctionType retty (fst <$> argtys) isVarArgs))
        (internName label)
  where
    params = ((\(ty, nm) -> Parameter ty nm []) <$> argtys, isVarArgs)

define,
  defineVarArgs ::
    HasState "moduleDefinitions" [Definition] m =>
    Type ->
    Symbol ->
    [(Type, Name)] ->
    [BasicBlock] ->
    m Operand
define = defineGen False
defineVarArgs = defineGen True

-- | registerFunction is useful for making functions properly recursive
registerFunction :: HasState "symTab" SymbolTable m => Type -> [(Type, b)] -> Name -> m ()
registerFunction retty argtys label =
  assign (nameToSymbol label) $
    ConstantOperand $
      C.GlobalReference
        (Types.pointerOf (FunctionType retty (fst <$> argtys) False))
        label

makeFunction ::
  ( HasThrow "err" Errors m,
    HasState "blockCount" Int m,
    HasState "blocks" (Map.T Name.Name BlockState) m,
    HasState "count" Word m,
    HasState "currentBlock" Name.Name m,
    HasState "names" Names m,
    HasState "symTab" Types.SymbolTable m
  ) =>
  Symbol ->
  [(Type.Type, Name.Name)] ->
  m ()
makeFunction name args = do
  entry <- addBlock name
  _ <- setBlock entry
  traverse_
    (\(typ, nam) -> assign (nameToSymbol nam) (local typ nam))
    args

-- TODO ∷ Current can't call this recursively, as how the clearing of
-- just have the environment that needs to change change locally,
-- should be easy to add
-- symTab works... please instead just pop the arguments off the
-- stack, and when we locally let name, have a with name primitive.
defineFunctionGen ::
  Types.Define m => Bool -> Type -> Symbol -> [(Type, Name)] -> m a -> m Operand
defineFunctionGen bool retty name args body = do
  functionOperand <-
    withLocalArgumentBlocks $ do
      makeFunction name args
      registerFunction retty args (internName name)
      body
      blockName <- createBlocks
      defineGen bool retty name args blockName
  assign name functionOperand
  pure functionOperand

defineFunction,
  defineFunctionVarArgs ::
    Define m => Type -> Symbol -> [(Type, Name)] -> m a -> m Operand
defineFunction = defineFunctionGen False
defineFunctionVarArgs = defineFunctionGen True

withLocalArgumentBlocks ::
  ( HasState "blocks" (Map.HashMap k v) m,
    HasState "count" s1 m,
    HasState "currentBlock" Name m,
    HasState "symTab" s2 m,
    Num s1
  ) =>
  m b ->
  m b
withLocalArgumentBlocks op = do
  -- Reserve old Environment
  ----------------------------------------
  oldSymTab <- get @"symTab"
  oldBlocks <- get @"blocks"
  oldCount' <- get @"count"
  oldBlockName <- get @"currentBlock"
  -- Prepare Environment
  ----------------------------------------
  resetCount
  put @"blocks" Map.empty
  -- Do op
  ----------------------------------------
  ret <- op
  -- restore order
  ----------------------------------------
  --we should do smart symtab accounting
  put @"symTab" oldSymTab
  put @"blocks" oldBlocks
  put @"count" oldCount'
  setBlock oldBlockName
  -- return
  ----------------------------------------
  return ret

--------------------------------------------------------------------------------
-- Unique Name gen
--------------------------------------------------------------------------------

generateUniqueName :: NewBlock m => Symbol -> m Name
generateUniqueName = fmap internName . generateUniqueSymbol

generateUniqueSymbol :: NewBlock m => Symbol -> m Symbol
generateUniqueSymbol bname = do
  nms <- get @"names"
  let (qname, supply) = uniqueName bname nms
  put @"names" supply
  return qname

--------------------------------------------------------------------------------
-- Block Stack
--------------------------------------------------------------------------------

entry :: (HasState "currentBlock" Name m) => m Name
entry = get @"currentBlock"

getBlock :: (HasState "currentBlock" Name m) => m Name
getBlock = entry

-- TODO ∷ hack make a proper algorithm later!
addBlockNumber :: NewBlock m => Symbol -> Int -> m Name
addBlockNumber bname number = do
  bls <- get @"blocks"
  name <- generateUniqueName bname
  let new = emptyBlock number
  put @"blocks" (Map.insert name new bls)
  return name

addBlock :: NewBlock m => Symbol -> m Name
addBlock bname = do
  bls <- get @"blocks"
  ix <- get @"blockCount"
  name <- generateUniqueName bname
  let new = emptyBlock ix
  put @"blocks" (Map.insert name new bls)
  put @"blockCount" (succ ix)
  return name

setBlock :: HasState "currentBlock" Name m => Name -> m ()
setBlock bName = put @"currentBlock" bName

modifyBlock ::
  ( HasState "blocks" (Map.T Name v) m,
    HasState "currentBlock" Name m
  ) =>
  v ->
  m ()
modifyBlock new = do
  active <- get @"currentBlock"
  modify @"blocks" (Map.insert active new)

current ::
  ( HasState "blocks" (Map.T Name b) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) =>
  m b
current = do
  c <- get @"currentBlock"
  b <- get @"blocks"
  case Map.lookup c b of
    Just x -> return x
    Nothing -> throw @"err" (NoSuchBlock (show c))

externf :: Externf m => Name -> m Operand
externf name = getvar (nameToSymbol name)

nameToSymbol :: Name -> Symbol
nameToSymbol (UnName n) = (intern (filter (/= '\"') (show n)))
nameToSymbol (Name n) = (intern (filter (/= '\"') (show n)))

local :: Type -> Name -> Operand
local = LocalReference

instr :: RetInstruction m => Type -> Instruction -> m Operand
instr typ ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk {stack = (ref := ins) : i})
  pure (local typ ref)

unnminstr ::
  ( HasState "blocks" (Map.T Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) =>
  Instruction ->
  m ()
unnminstr ins = do
  blk <- current
  let i = stack blk
  modifyBlock (blk {stack = (Do ins) : i})

terminator ::
  ( HasState "blocks" (Map.T Name BlockState) m,
    HasState "currentBlock" Name m,
    HasThrow "err" Errors m
  ) =>
  Named Terminator ->
  m (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk {term = Just trm})
  return trm

--------------------------------------------------------------------------------
-- External linking
--------------------------------------------------------------------------------

external :: (HasState "moduleDefinitions" [Definition] m) => Type -> String -> [(Type, Name)] -> m Operand
external retty label argtys = do
  addDefn $
    GlobalDefinition $
      functionDefaults
        { Global.parameters = ((\(ty, nm) -> Parameter ty nm []) <$> argtys, False),
          Global.callingConvention = CC.Fast, -- TODO: Do we always want this?
          Global.returnType = retty,
          Global.basicBlocks = [],
          Global.name = mkName label,
          Global.linkage = L.External
        }
  return $
    ConstantOperand $
      C.GlobalReference
        (Types.pointerOf (FunctionType retty (fst <$> argtys) False))
        (mkName label)

externalVar :: (HasState "moduleDefinitions" [Definition] m) => Type -> String -> [(Type, Name)] -> m Operand
externalVar retty label argtys = do
  addDefn $
    GlobalDefinition $
      functionDefaults
        { Global.parameters =
            ( ( \(ty, nm) ->
                  Parameter
                    ty
                    nm
                    [ParameterAttribute.NoAlias, ParameterAttribute.NoCapture]
              )
                <$> argtys,
              True
            ),
          Global.callingConvention = CC.C, -- TODO: Do we always want this?
          Global.returnType = retty,
          Global.basicBlocks = [],
          Global.name = mkName label,
          Global.linkage = L.External
        }
  return $
    ConstantOperand $
      C.GlobalReference
        (Types.pointerOf (FunctionType retty (fst <$> argtys) True))
        (mkName label)

--------------------------------------------------------------------------------
-- Printing facility
--------------------------------------------------------------------------------

definePrintf :: External m => m ()
definePrintf = do
  let name = "printf"
  op <- externalVar Type.i32 name [(Types.pointerOf Type.i8, "n")]
  assign (intern name) op

printf :: Call m => [Operand] -> m Operand
printf args = do
  printf <- externf "printf"
  instr Type.i32 $ callConvention CC.C printf (emptyArgs args)

cString :: [Char] -> C.Constant
cString str = C.Array Type.i8 (C.Int 8 . fromIntegral . ord <$> terminatedStr)
  where
    terminatedStr = str <> "\00"

cStringPointer :: RetInstruction m => [Char] -> m Operand
cStringPointer str = do
  t <- alloca (Type.ArrayType len Type.i8)
  store t (Operand.ConstantOperand vec)
  pure t
  where
    vec = cString str
    len = fromIntegral (length str + 1)

printCString :: Call m => [Char] -> [Operand] -> m Operand
printCString str args = do
  str <- cStringPointer str
  ptrIn <-
    getElementPtr $
      Types.Minimal
        { Types.type' = Types.pointerOf Type.i8,
          Types.address' = str,
          Types.indincies' = constant32List [0, 0]
        }
  printf (ptrIn : args)

--------------------------------------------------------------------------------
-- Memory management
--------------------------------------------------------------------------------

-- malloc & free need to be defined once and then can be called normally with `externf`

defineMalloc :: External m => m ()
defineMalloc = do
  let name = "malloc"
  op <- external (Types.pointerOf Type.i8) name [(Types.size_t, "size")]
  assign (intern name) op

defineFree :: External m => m ()
defineFree = do
  let name = "free"
  op <- external voidTy name [(Types.pointerOf Type.i8, "type")]
  assign (intern name) op

malloc :: Call m => Integer -> Type -> m Operand
malloc size type' = do
  malloc <- externf "malloc"
  i8Ptr <-
    instr (Types.pointerOf Type.i8) $
      callConvention
        CC.Fast
        malloc
        (emptyArgs [Operand.ConstantOperand (C.Int Types.size_t_int size)])
  bitCast i8Ptr type'

free :: Call m => Operand -> m ()
free thing = do
  free <- externf "free"
  casted <- bitCast thing (Types.pointerOf Type.i8)
  unnminstr (callConvention CC.Fast free (emptyArgs [casted]))

--------------------------------------------------------------------------------
-- Integer Operations
--------------------------------------------------------------------------------

sdiv,
  udiv,
  add,
  sub,
  mul ::
    RetInstruction m => Type -> Operand -> Operand -> m Operand
sdiv t a b =
  instr
    t
    SDiv
      { exact = False,
        operand0 = a,
        operand1 = b,
        metadata = []
      }
udiv t a b =
  instr
    t
    UDiv
      { exact = False,
        operand0 = a,
        operand1 = b,
        metadata = []
      }
add t a b =
  instr
    t
    Add
      { -- no signed warp
        nsw = True,
        -- no unSigned warp
        nuw = True,
        operand0 = a,
        operand1 = b,
        metadata = []
      }
sub t a b =
  instr
    t
    Sub
      { -- no signed warp
        nsw = True,
        -- no unSigned warp
        nuw = True,
        operand0 = a,
        operand1 = b,
        metadata = []
      }
mul t a b =
  instr
    t
    Mul
      { -- no signed warp
        nsw = True,
        -- no unSigned warp
        nuw = True,
        operand0 = a,
        operand1 = b,
        metadata = []
      }

icmp ::
  RetInstruction m => IntPred.IntegerPredicate -> Operand -> Operand -> m Operand
icmp iPred op1 op2 = instr Type.i1 $ ICmp iPred op1 op2 []

--------------------------------------------------------------------------------
-- Floating Point Operations
--------------------------------------------------------------------------------

-- Floating Point operations

fdiv,
  fadd,
  fsub,
  fmul ::
    RetInstruction m => Type -> Operand -> Operand -> m Operand
fdiv t a b = instr t $ FDiv noFastMathFlags a b []
fadd t a b = instr t $ FAdd noFastMathFlags a b []
fsub t a b = instr t $ FSub noFastMathFlags a b []
fmul t a b = instr t $ FMul noFastMathFlags a b []

--------------------------------------------------------------------------------
-- Control Flow
--------------------------------------------------------------------------------

ret :: Instruct m => Operand -> m (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

retNull :: Instruct m => m (Named Terminator)
retNull = terminator $ Do $ Ret Nothing []

cbr :: Instruct m => Operand -> Name -> Name -> m (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

br :: Instruct m => Name -> m (Named Terminator)
br val = terminator $ Do $ Br val []

phi :: RetInstruction m => Type -> [(Operand, Name)] -> m Operand
phi ty incoming = instr ty $ Phi ty incoming []

switch :: Instruct m => Operand -> Name -> [(C.Constant, Name)] -> m (Named Terminator)
switch val default' dests = terminator $ Do $ Switch val default' dests []

generateIf ::
  ( RetInstruction m,
    HasState "blockCount" Int m,
    HasState "names" Names m
  ) =>
  Type ->
  Operand ->
  m Operand ->
  m Operand ->
  m Operand
generateIf ty cond tr fl = do
  ifThen <- addBlock "if.then"
  ifElse <- addBlock "if.else"
  ifExit <- addBlock "if.exit"
  -- %entry
  ------------------
  test <- icmp IntPred.EQ cond (ConstantOperand (C.Int 1 1))
  _ <- cbr test ifThen ifElse
  -- if.then
  ------------------
  setBlock ifThen
  t <- tr
  _ <- br ifExit
  ifThen <- getBlock
  -- if.else
  ------------------
  setBlock ifElse
  f <- fl
  _ <- br ifExit
  ifElse <- getBlock
  -- if.exit
  ------------------
  setBlock ifExit
  phi ty [(t, ifThen), (f, ifElse)]

--------------------------------------------------------------------------------
-- Effects
--------------------------------------------------------------------------------

emptyArgs :: Functor f => f a1 -> f (a1, [a2])
emptyArgs = fmap (\x -> (x, []))

callConvention ::
  CC.CallingConvention ->
  Operand ->
  [(Operand, [ParameterAttribute.ParameterAttribute])] ->
  Instruction
callConvention convention fn args =
  Call
    { functionAttributes = [],
      tailCallKind = Nothing,
      callingConvention = convention,
      returnAttributes = [],
      function = Right fn,
      arguments = args,
      metadata = []
    }

callVoid ::
  RetInstruction m =>
  Operand ->
  [(Operand, [ParameterAttribute.ParameterAttribute])] ->
  m ()
callVoid fn args =
  unnminstr $
    Call
      { functionAttributes = [],
        tailCallKind = Nothing,
        callingConvention = CC.Fast,
        returnAttributes = [],
        function = Right fn,
        arguments = args,
        metadata = []
      }

call ::
  RetInstruction m =>
  Type ->
  Operand ->
  [(Operand, [ParameterAttribute.ParameterAttribute])] ->
  m Operand
call typ fn args =
  instr typ $
    Call
      { functionAttributes = [],
        tailCallKind = Nothing,
        callingConvention = CC.Fast,
        returnAttributes = [],
        function = Right fn,
        arguments = args,
        metadata = []
      }

-- TODO :: is the pointerOf on the ty needed
-- the LLVM8 testing on newKledi shows it being the same type back
-- however that would be incorrect?!
alloca :: RetInstruction m => Type -> m Operand
alloca ty = instr (pointerOf ty) $ Alloca ty Nothing 0 []

load :: RetInstruction m => Type -> Operand -> m Operand
load typ ptr = instr typ $ Load False ptr Nothing 0 []

store :: Instruct m => Operand -> Operand -> m ()
store ptr val = unnminstr $ Store False ptr val Nothing 0 []

--------------------------------------------------------------------------------
-- Casting Operations
--------------------------------------------------------------------------------

bitCast :: RetInstruction m => Operand -> Type -> m Operand
bitCast op typ = instr typ $ BitCast op typ []

ptrToInt :: RetInstruction m => Operand -> Type -> m Operand
ptrToInt op typ = instr typ $ AST.PtrToInt op typ []

trunc :: RetInstruction m => Operand -> Type -> m Operand
trunc op typ = instr typ $ Trunc op typ []

--------------------------------------------------------------------------------
-- Pointer Operations
--------------------------------------------------------------------------------

-- | 'getElementPtr' gets an index of a struct or an array as a pointer
-- Takes a minimal data type to emulate named arguments
getElementPtr :: RetInstruction m => MinimalPtr -> m Operand
getElementPtr (Minimal address indices type') =
  instr type' $
    GetElementPtr
      { inBounds = True,
        metadata = [],
        address = address,
        indices = indices
      }

loadElementPtr :: RetInstruction m => MinimalPtr -> m Operand
loadElementPtr minimal = do
  ptr <-
    getElementPtr
      (minimal {Types.type' = pointerOf (Types.type' minimal)})
  load (Types.type' minimal) ptr

constant32List :: Functor f => f Integer -> f Operand
constant32List = fmap (ConstantOperand . C.Int 32)

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: (HasState "symTab" SymbolTable m) => Symbol -> Operand -> m ()
assign var x = do
  modify @"symTab" (Map.insert var x)

getvar ::
  ( HasState "symTab" SymbolTable m,
    HasThrow "err" Errors m
  ) =>
  Symbol ->
  m Operand
getvar var = do
  syms <- get @"symTab"
  case Map.lookup var syms of
    Just x ->
      return x
    Nothing ->
      throw @"err"
        ( VariableNotInScope $
            "Local variable not in scope:"
              <> "\n syms: "
              <> show syms
              <> "\n var: "
              <> show var
        )

internName :: Symbol -> Name
internName = mkName . unintern
