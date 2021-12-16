-- |
-- - Has the code necessary to generate LLVM Code, and gives a small
--   DSL in order to effectively do so.
--
-- An example of using the library to define an llvm function from
-- haskell may look like
--
-- @
--   defineLink :: (Define m, Debug m) => m Operand.Operand
--   defineLink = Block.defineFunction Type.void "link" args $
--     do
--       aux1 <- auxiliary1
--       aux2 <- auxiliary2
--       node1 <- Block.externf "node_1"
--       node2 <- Block.externf "node_1"
--       Types.debugLevelOne $ do
--         _ <- Block.printCString "Executing Link rule \n" []
--         _ <- Block.printCString "Calling Link on \n" []
--         Debug.printNodePort node1 aux1
--         Debug.printNodePort node2 aux2
--       Ops.setPort ("node_1", "port_1") ("node_2", "port_2")
--       Types.debugLevelOne $ do
--         _ <- Block.printCString "Calling Link on \n" []
--         Debug.printNodePort node1 aux2
--         Debug.printNodePort node2 aux1
--       Ops.setPort ("node_2", "port_2") ("node_1", "port_1")
--       Block.retNull
--     where
--       args =
--         [ (nodePointer, "node_1"),
--           (numPortsPointer, "port_1"),
--           (nodePointer, "node_2"),
--           (numPortsPointer, "port_2")
--         ]
-- @
--
--
-- Here we define a function called "link" that takes 4 arguments, and
-- generates code that will link two nodes together. Since we are
-- writing an llvm function, we have to `Block.externf` the given
-- arguments to be able to refer to them in the LLVM code.
--
-- Using the code to compile IR terms coming from some abstract
-- machine looks a bit different, we can see it in this example here
--
-- @
--   compileLam ty captures arguments body
--    | length captures == 0 = do
--       let (llvmArgty, llvmRetty) =
--             functionTypeLLVM ty
--           llvmArgNames =
--             fmap (Block.internName . NameSymbol.toSymbol) arguments
--           llvmArguments =
--             zip llvmArgty llvmArgNames
--       -- time to generate unique names
--       lamName <- Block.generateUniqueSymbol "lambda"
--       Block.defineFunction llvmRetty lamName llvmArguments $
--         do
--           bod <- compileTerm body
--           Block.ret bod
--    | otherwise =
--       throw @"err" (Types.UnsupportedOperation "closures are not supported")
-- @
--
-- In this example instead of having to `Block.externf` the given
-- arguments, we can rely on resolution to properly handle
-- that. However, we can see effective use of the library with the
-- `Block.generateUniqueSymbol` function and the
-- `Block.defineFunction` function. The first gives us an unique to
-- compile again, while the latter setups up the environment to
-- compile a new top level declaration.
module Juvix.Backends.LLVM.Codegen.Block
  ( -- * Codegen operations
    emptyCodegen,
    execEnvState,
    evalEnvState,
    runEnvState,

    -- * Module Level
    addType,
    defineFunction,
    defineFunctionVarArgs,

    -- * Unique Name Generation
    generateUniqueName,
    generateUniqueSymbol,

    -- * Block Stack
    getBlock,
    addBlock,
    setBlock,
    externf,

    -- * External linking
    external,
    externalVar,

    -- * Printing functionality
    definePrintf,
    printf,
    globalString,
    cString,
    cStringPointer,
    printCString,

    -- * memory mangement
    defineMalloc,
    defineFree,
    malloc,
    mallocType,
    free,

    -- * Operations

    -- ** Integer Operations
    sdiv,
    udiv,
    add,
    sub,
    mul,
    icmp,

    -- ** Null
    Juvix.Backends.LLVM.Codegen.Block.null,

    -- ** Floating Point Operations
    fdiv,
    fadd,
    fsub,
    fmul,

    -- ** Control Flow Operations
    ret,
    retNull,
    cbr,
    phi,
    switch,
    generateIf,

    -- ** Calling Operations
    emptyArgs,
    callVoid,
    call,

    -- ** Allocation Operations
    alloca,
    load,
    store,

    -- ** Casting Operations
    bitCast,
    ptrToInt,
    trunc,

    -- ** Pointer operations
    getElementPtr,
    unsafeGetElementPtr,
    loadElementPtr,
    constant32List,

    -- * Misc
    internName,
  )
where

import Data.ByteString.Short hiding (length)
import Juvix.Backends.LLVM.Codegen.Types as Types
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
      Types.typTab = Map.empty,
      Types.varTab = Map.empty,
      Types.count = 0,
      Types.names = Map.empty,
      Types.blockCount = 1,
      Types.moduleAST = emptyModule "juvix-module",
      Types.debug = 0
    }

execEnvState :: Codegen a -> SymbolTable -> CodegenState
execEnvState c = snd . runEnvState c

evalEnvState :: Codegen a -> SymbolTable -> Either Errors a
evalEnvState c = fst . runEnvState c

runEnvState :: Codegen a -> SymbolTable -> (Either Errors a, CodegenState)
runEnvState (Types.CodeGen m) a =
  runState (runExceptT m) (emptyCodegen {Types.symTab = a})

--------------------------------------------------------------------------------
-- Module Level
--------------------------------------------------------------------------------

-- Rethink Module level API to be robust in switching to other LLVΜmodules.

emptyModule :: ShortByteString -> Module
emptyModule label = AST.defaultModule {moduleName = label}

addDefn :: HasState "moduleDefinitions" [Definition] m => Definition -> m ()
addDefn d = modify @"moduleDefinitions" (<> [d])

-- | @addType@ adds the given name as a type definition with the given
-- type
--
-- @
-- register = do
--   ...
--   Codegen.addType Codegen.portTypeName Defs.portType
--   ...
--
-- Defs.portType :: Type.Type
-- Defs.portType = Codegen.portType Types.eacPointer
--
-- Codegen.portTypeNameRef :: Type
-- Codegen.portTypeNameRef = Type.NamedTypeReference portTypeName
--
-- Codegen.portTypeName :: IsString p => p
-- Codegen.portTypeName = "graph_port"
--
-- Codegen.portType :: Type -> Type
-- Codegen.portType nodePtr =
--   StructureType
--     { isPacked = True,
--       elementTypes =
--         [ nodePtr, -- the pointer to the other node
--           numPortsNameRef -- the offset from the base of the node where the port is
--         ]
--     }
-- @
addType :: HasState "moduleDefinitions" [Definition] m => Name -> Type -> m ()
addType name typ =
  addDefn (TypeDefinition name (Just typ))

defineGen ::
  HasState "moduleDefinitions" [Definition] m =>
  -- | determines if the function is varargs or not
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
      _ <- body
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

-- TODO :: create function given local bindings, reserves them in the
-- environment

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

-- | getBlock - useful when trying to get the current block or φ nodes
--
-- @
-- do
--   primaryCase <- Codegen.addBlock "case.primary"
--   continueComp <- Codegen.addBlock "case.continue"
--   currentBlock <- Codegen.getBlock
--   _ <- Codegen.cbr isPrimary primaryCase continueComp
--   …
--   Codegen.phi eacListPtrType [(eacList, currentBlock), (newList, primaryCase)]
-- @
getBlock :: (HasState "currentBlock" Name m) => m Name
getBlock = entry

-- | @addBlock@ generates a new block. This is used every time a
-- jump/decision/goto point is needed. For example:
--
-- @
-- do
--  appCase <- Codegen.addBlock "switch.app"
--  lamCase <- Codegen.addBlock "switch.lam"
--  ...
--  _term <-
--    Codegen.switch
--     tag
--     defCase -- this should never happen
--     [ (Types.app, appCase),
--       (Types.lam, lamCase),
--       (Types.era, eraCase),
--       (Types.dup, dupCase)
--     ]
--  …
--  Codegen.setBlock appCase
--  ... code related to appCase ...
-- @
addBlock :: NewBlock m => Symbol -> m Name
addBlock bname = do
  bls <- get @"blocks"
  ix <- get @"blockCount"
  name <- generateUniqueName bname
  let new = emptyBlock ix
  put @"blocks" (Map.insert name new bls)
  put @"blockCount" (succ ix)
  return name

-- | @setBlock@ used to set a block, useful for branches... see
-- @addBlock@'s documentation for more detail in practical use
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

-- | @externf@ - From a given @Name@ get the operand it's associated with.
-- Useful for both custom LLVM functions in wh:
-- defineLink = Block.defineFunction Type.void "link" args $
--
-- @
--  do
--    node1 <- Block.externf "node_1"
--    node2 <- Block.externf "node_2"
--    ... computation with node1 and node2 ...
--  where
--    args =
--      [ (nodePointer, "node_1"),
--        (numPortsPointer, "port_1"),
--        (nodePointer, "node_2"),
--        (numPortsPointer, "port_2")
--      ]
-- @
--
-- And for variable lookup inside a compiler:
--
-- @
-- compileVar sym = do
--   Block.externf (Block.internName (NameSymbol.toSymbol sym))
-- @
externf :: Externf m => Name -> m Operand
externf name = getvar (nameToSymbol name)

nameToSymbol :: Name -> Symbol
nameToSymbol = intern . filter (/= '\"') . strName
  where
  strName nm = case nm of
    UnName n -> show n
    Name n -> show n

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

-- | @external@ allows for us to declare foreign WASM/LLVM code as our
-- own code and call it.
external ::
  (HasState "moduleDefinitions" [Definition] m) =>
  -- | retty is the return type
  Type ->
  -- | name of the function
  String ->
  -- | Argument name and types to the function
  [(Type, Name)] ->
  m Operand
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

-- | @externalVar@ Like @external@ except that we pass extra parameter
-- flags to the argument list of the @external@ function, and maintain
-- the varargs calling convention
externalVar ::
  (HasState "moduleDefinitions" [Definition] m) =>
  -- | retty is the return type
  Type ->
  -- | name of the function
  String ->
  -- | Argument name and types to the function
  [(Type, Name)] ->
  m Operand
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

-- | @definePrintf@ defines the printf function to the environment
definePrintf :: External m => m ()
definePrintf = do
  let name = "printf"
  op <- externalVar Type.i32 name [(Types.pointerOf Type.i8, "n")]
  assign (intern name) op

-- | @printf@ calls the printf function on a list of operands
-- including the cString and the list of operands it operates over
printf :: Call m => [Operand] -> m Operand
printf args = do
  printf <- externf "printf"
  instr Type.i32 $ callConvention CC.C printf (emptyArgs args)

-- | @globalString@ creates a globally-defined string constant and
-- returns a pointer to it.
globalString ::
  ( RetInstruction m,
    HasState "moduleDefinitions" [Definition] m
  ) =>
  String ->
  Name ->
  m Operand
globalString str name = do
  addDefn $
    GlobalDefinition $
      globalVariableDefaults
        { Global.name = name,
          Global.initializer = Just (cString str),
          Global.type' = ArrayType (fromIntegral (length str + 1)) Type.i8
        }
  getElementPtr $
    Types.Minimal
      { Types.type' = Types.pointerOf Type.i8,
        Types.address' =
          ConstantOperand $
            C.GlobalReference
              (Types.pointerOf $ ArrayType (fromIntegral (length str + 1)) Type.i8)
              name,
        Types.indincies' = constant32List [0, 0]
      }

-- | @cString@ given a haskell string, get the LLVM C style
-- representation of the function. The result is a constant string in
-- the generated LLVM
cString :: String -> C.Constant
cString str = C.Array Type.i8 (C.Int 8 . fromIntegral . ord <$> terminatedStr)
  where
    terminatedStr = str <> "\00"

-- | @cStringPointer@ like @cString@ however gives a pointer to a
-- string, resulting in a non constant string to manipulate.
cStringPointer :: RetInstruction m => String -> m Operand
cStringPointer str = do
  t <- alloca (Type.ArrayType len Type.i8)
  store t (Operand.ConstantOperand vec)
  pure t
  where
    vec = cString str
    len = fromIntegral (length str + 1)

-- | @printCString@ given a Haskell String and a list of operands to
-- the printf string, call printf on the string.
--
-- @
--   Block.printCString "Allocating node %p \n" [nodePtr]
-- @
printCString :: Call m => String -> [Operand] -> m Operand
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

-- malloc & free need to be defined once and then can be called
-- normally with `externf`

-- | @defineMalloc@ defines the malloc function
defineMalloc :: External m => m ()
defineMalloc = do
  let name = "malloc"
  op <- external (Types.pointerOf Type.i8) name [(Type.i32, "size")]
  assign (intern name) op

-- | @defineFree@ defines the free function
defineFree :: External m => m ()
defineFree = do
  let name = "free"
  op <- external voidTy name [(Types.pointerOf Type.i8, "type")]
  assign (intern name) op

-- | @malloc@ takes the size and the resulting type of the @malloc@. the
-- size parameter represents the amount of memory needing to be
-- allocated, and the resulting type will be used to @bitCast@ the
-- allocation into the specified type
malloc :: Call m => Integer -> Type -> m Operand
malloc size resultType = do
  malloc <- externf "malloc"
  i8Ptr <-
    instr (Types.pointerOf Type.i8) $
      callConvention
        CC.Fast
        malloc
        (emptyArgs [Operand.ConstantOperand (C.Int 32 size)])
  bitCast i8Ptr resultType

-- Does the sizeof code actually work!?

-- | @mallocType@ mallocs the given type
mallocType :: Call m => Type -> m Operand
mallocType resultType = do
  malloc <- externf "malloc"
  i8Ptr <-
    instr (Types.pointerOf Type.i8) $
      callConvention
        CC.Fast
        malloc
        (emptyArgs [Operand.ConstantOperand (C.sizeof resultType)])
  bitCast i8Ptr (Types.pointerOf resultType)

-- | @free@ frees the given operand
free :: Call m => Operand -> m ()
free thing = do
  free <- externf "free"
  casted <- bitCast thing (Types.pointerOf Type.i8)
  unnminstr (callConvention CC.Fast free (emptyArgs [casted]))

-- * Operations are functions that are callable on the LLVM backend

-- * that modify the current block with the given operation

--------------------------------------------------------------------------------
-- Integer Operations
--------------------------------------------------------------------------------

-- |
--
--   [@sdiv@] calls the sdiv function for integer numbers
--   [@udiv@] calls the udiv function for integer numbers
--   [@add@] calls the add function for integer numbers
--   [@mul@] calls the mul function for integer numbers
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

-- | @icmp@ given some Integer predicate and two values, compute if
-- the predicate holds true over them.
-- @
--   cmp <- Block.icmp IntPred.EQ nodeInt otherNodeInt
-- @
icmp ::
  RetInstruction m => IntPred.IntegerPredicate -> Operand -> Operand -> m Operand
icmp iPred op1 op2 = instr Type.i1 $ ICmp iPred op1 op2 []

--------------------------------------------------------------------------------
-- Floating Point Operations
--------------------------------------------------------------------------------

-- Floating Point operations

-- |
--
--   [@fdiv@] calls the fsdiv function for floating point style numbers
--   [@fadd@] calls the fudiv function for floating point style numbers
--   [@fsub@] calls the fadd function for floating point style numbers
--   [@fmul@] calls the fmul function for floating point style numbers
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
-- Null
--------------------------------------------------------------------------------

null :: Type -> Operand
null ty = do
  Operand.ConstantOperand (C.Null ty)

--------------------------------------------------------------------------------
-- Control Flow
--------------------------------------------------------------------------------

-- | @ret@ returns the given instruction from the current block
ret :: Instruct m => Operand -> m (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

-- | @retNull@ returns void from the current block
retNull :: Instruct m => m (Named Terminator)
retNull = terminator $ Do $ Ret Nothing []

-- | @cbr@ takes a condition, and two block names. If the condition is
-- true, then control is transferred to the fist block name, otherwise
-- control is transferred to the second block name
--
-- @
-- nullCase <- Codegen.addBlock "empty.list"
-- carExists <- Codegen.addBlock "car.check"
-- nullCheck <- Types.checkNull eacLPtr
-- _ <- Codegen.cbr nullCheck carExists nullCase
-- @
cbr :: Instruct m => Operand -> Name -> Name -> m (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

-- | @br@ is like @cbr@ but is an unconditional transfer of control,
-- thus always jumping to the given block name
--
-- @
-- _ <- Codegen.br extCase
-- @
br :: Instruct m => Name -> m (Named Terminator)
br val = terminator $ Do $ Br val []

-- | @phi@ is the essential LLVM idea for selecting values between
-- different blocks. φ in our case takes a type to return, and a list
-- of @Operand@s with their block @Name@s. If the computation came
-- from the given block, then that @Operand@ is chosen for the return
-- result.
--
-- @
-- do
--   primaryCase <- Codegen.addBlock "case.primary"
--   continueComp <- Codegen.addBlock "case.continue"
--   currentBlock <- Codegen.getBlock
--   _ <- Codegen.cbr isPrimary primaryCase continueComp
--   …
--   Codegen.phi eacListPtrType [(eacList, currentBlock), (newList, primaryCase)]
-- @
phi :: RetInstruction m => Type -> [(Operand, Name)] -> m Operand
phi ty incoming = instr ty $ Phi ty incoming []

-- | @switch@ is a case statement in LLVM.
--
-- @
-- do
--  appCase <- Codegen.addBlock "switch.app"
--  lamCase <- Codegen.addBlock "switch.lam"
--  ...
--  _term <-
--    Codegen.switch
--     tag
--     defCase -- this should never happen
--     [ (Types.app, appCase),
--       (Types.lam, lamCase),
--       (Types.era, eraCase),
--       (Types.dup, dupCase)
--     ]
--  …
--  Codegen.setBlock appCase
--  ... code related to appCase ...
-- @
switch :: Instruct m => Operand -> Name -> [(C.Constant, Name)] -> m (Named Terminator)
switch val default' dests = terminator $ Do $ Switch val default' dests []

-- | @generateIf@ is a clean layer over @cbr@ if one just wants to
-- emulate a higher level if expression
--
-- @
-- Block.generateIf typ tag largeBranch smallBranch
-- @
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

-- | @emptyArgs@ appends an empty attribute list to the arguments for
-- function calls
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

-- | @callVoid@ calls a function on the given arguments with the void
-- return type
--
-- @
-- Block.callVoid reduce_until_complete (Block.emptyArgs [ptr])
-- @
callVoid ::
  RetInstruction m =>
  -- | The function operand
  Operand ->
  -- | The argument operands along with an attribute list
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

-- | @call@ takes a return type, a function, and the arguments to call
-- it on.
-- An example of calling the function
--
-- @
-- ptr <- Codegen.call opaqueNetType create_net (Codegen.emptyArgs [])
-- @
--
-- Another Example of calling it on generated code
--
-- @
-- arguments <- traverse compileTerm xs
-- function <- compileTerm f
-- --
-- let -- We should probably get the type from the function itself
--     -- rather than pass in what it should be here
--     functionType = typeToLLVM returnTy
--     -- ignore attributes for now!
--     argsAtrributes = zip arguments (repeat [])
--
-- Block.call functionType function argsAtrributes
-- @
call ::
  RetInstruction m =>
  -- | The return type of the function call
  Type ->
  -- | The function operand
  Operand ->
  -- | The argument operands along with an attribute list
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

-- | @alloca@ calls the alloca function in LLVM. Given the type, LLVM
-- knows enough to allocate that amount of memory
alloca :: RetInstruction m => Type -> m Operand
alloca ty = instr (pointerOf ty) $ Alloca ty Nothing 0 []

-- | @load@ takes a return type and a given operand. @load@ loads the
-- given operand into memory. Useful for GEP calls and from results
-- form allocation
--
-- @
-- portSizeP <- Ops.allocaNumPortNum (fromIntegral $ length mPorts)
-- tagPtr <-
--   Block.getElementPtr $
--     Types.Minimal
--       { Types.type' = Types.numPortsPointer,
--         Types.address' = nodePtr,
--         Types.indincies' = Block.constant32List [0, 0]
--       }
-- portSize <- Block.load numPortsNameRef portSizeP
-- Block.store tagPtr portSize
-- @
load :: RetInstruction m => Type -> Operand -> m Operand
load typ ptr = instr typ $ Load False ptr Nothing 0 []

-- | @store@ stores the second argument, val, into the first arugment,
-- ptr. Note the first argument must be a pointer type.
-- See @load@ for an example call.
store :: Instruct m => Operand -> Operand -> m ()
store ptr val = unnminstr $ Store False ptr val Nothing 0 []

--------------------------------------------------------------------------------
-- Casting Operations
--------------------------------------------------------------------------------

-- | @bitCast@ bitcasts the given operand to the specified type.
bitCast :: RetInstruction m => Operand -> Type -> m Operand
bitCast op typ = instr typ $ BitCast op typ []

-- | @ptrToInt@ Turns a given ptr into the given integer type.
ptrToInt :: RetInstruction m => Operand -> Type -> m Operand
ptrToInt op typ = instr typ $ AST.PtrToInt op typ []

trunc :: RetInstruction m => Operand -> Type -> m Operand
trunc op typ = instr typ $ Trunc op typ []

--------------------------------------------------------------------------------
-- Pointer Operations
--------------------------------------------------------------------------------

-- | @getElementPtr@ gets an index of a struct or an array as a pointer
-- Takes a minimal data type to emulate named arguments
--
-- @
-- do
--   ptrIn <-
--    getElementPtr $
--      Types.Minimal
--        { Types.type' = Types.pointerOf Type.i8,
--          Types.address' = str,
--          Types.indincies' = constant32List [0, 0]
--        }
-- @
getElementPtr :: RetInstruction m => MinimalPtr -> m Operand
getElementPtr (Minimal address indices type') =
  instr type' $
    GetElementPtr
      { inBounds = True,
        metadata = [],
        address = address,
        indices = indices
      }

unsafeGetElementPtr :: RetInstruction m => MinimalPtr -> m Operand
unsafeGetElementPtr (Minimal address indices type') =
  instr type' $
    GetElementPtr
      { inBounds = False,
        metadata = [],
        address = address,
        indices = indices
      }

-- | @loadElementPtr@ acts like a @getElementPtr@ plus a @load@ right
-- after. The Taken @MinimalPtr@ type is updated to reflect this.
--
-- @
-- branchGen variant variantType extraDeref = do
--   casted <- Block.bitCast numPort (Types.pointerOf (Types.variantToType variant))
--   value <-
--     Block.loadElementPtr $
--       Types.Minimal
--         { Types.type' = variantType, -- note not a pointer to the variantType!
--           Types.address' = casted,
--           Types.indincies' = Block.constant32List [0, 1]
--         }
-- @
loadElementPtr :: RetInstruction m => MinimalPtr -> m Operand
loadElementPtr minimal = do
  ptr <-
    getElementPtr
      (minimal {Types.type' = pointerOf (Types.type' minimal)})
  load (Types.type' minimal) ptr

-- | @constant32List@ shorthand for a constant i32 integer list to
-- call @getElementPtr@ with. see @getElementPtr@ or @loadElementPtr@
-- for example usage.
constant32List :: Functor f => f Integer -> f Operand
constant32List = fmap (ConstantOperand . C.Int 32)

--------------------------------------------------------------------------------
-- Sum Type Declarations
--------------------------------------------------------------------------------
argsGen :: [Name.Name]
argsGen = (mkName . ("_" <>) . show) <$> ([1 ..] :: [Integer])

variantCreationName :: Symbol -> Symbol
variantCreationName = (<> "_%func")

-- | Generic logic to create a variant, used in 'createVariantAllocaFunction'
-- and 'createVariantGen'
variantCreation ::
  ( RetInstruction m,
    HasState "typTab" TypeTable m,
    Integral a,
    Foldable t
  ) =>
  Type ->
  Symbol ->
  Word32 ->
  t Operand ->
  a ->
  (Type -> m Operand) ->
  m Operand
variantCreation sumTyp variantName tag args offset allocFn = do
  typTable <- get @"typTab"
  sum <- allocFn sumTyp
  getEle <-
    getElementPtr $
      Minimal
        { Types.type' = Types.pointerOf (Type.IntegerType tag),
          Types.address' = sum,
          Types.indincies' = constant32List [0, 0]
        }
  store
    getEle
    (ConstantOperand (C.Int tag (toInteger offset)))
  -- TODO ∷ remove the ! call here
  let varType = typTable Map.! variantName
  casted <- bitCast sum (Types.pointerOf varType)
  foldM_
    ( \i inst -> do
        ele <-
          getElementPtr $
            Minimal
              { Types.type' = Types.pointerOf (intoStructTypeErr varType i),
                Types.address' = casted,
                Types.indincies' = constant32List [0, i]
              }
        store ele inst
        pure (succ i)
    )
    1 -- not 0, as 0 is reserved for the tag that was set
    args
  pure casted

-- TODO ∷ Remove repeat code!!!
-- TODO ∷ use, so far the createVariant is only used

-- | creates a variant creation definition function
createVariantAllocaFunction ::
  ( Define m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m
  ) =>
  Symbol ->
  [Type] ->
  m Operand
createVariantAllocaFunction variantName argTypes = do
  varTable <- get @"varTab"
  typTable <- get @"typTab"
  case Map.lookup variantName varTable of
    Nothing ->
      throw @"err" (NoSuchVariant (show variantName))
    Just
      ( S
          { sum' = sumName,
            offset = offset,
            tagSize' = tag
          }
        ) ->
        case Map.lookup sumName typTable of
          Nothing ->
            throw @"err" (DoesNotHappen ("type " <> show sumName <> "does not exist"))
          Just sumTyp ->
            let varCName = variantCreationName variantName
                args = zip argTypes argsGen
             in defineFunction sumTyp varCName args $ do
                  argsName <- traverse (externf . snd) args
                  casted <- variantCreation sumTyp variantName tag argsName offset alloca
                  _ <- ret casted
                  createBlocks

createVariantGen ::
  ( RetInstruction m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m,
    Foldable t
  ) =>
  Symbol ->
  t Operand ->
  (Type -> m Operand) ->
  m Operand
createVariantGen variantName args allocFn = do
  varTable <- get @"varTab"
  typTable <- get @"typTab"
  case Map.lookup variantName varTable of
    Nothing ->
      throw @"err" (NoSuchVariant (show variantName))
    Just
      ( S
          { sum' = sumName,
            offset = offset,
            tagSize' = tag
          }
        ) ->
        case Map.lookup sumName typTable of
          Nothing ->
            throw @"err" (DoesNotHappen ("type " <> show sumName <> "does not exist"))
          Just sumTyp ->
            variantCreation sumTyp variantName tag args offset allocFn

-- | Creates a variant by calling alloca.
-- WARNING:  This is unsafe until we can do escape analysis!  Until then,
-- use the mallocVariant.
allocaVariant ::
  ( RetInstruction m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m,
    Foldable t
  ) =>
  Symbol ->
  t Operand ->
  m Operand
allocaVariant variantName args = createVariantGen variantName args alloca

-- | Creates a variant by calling malloc.
mallocVariant ::
  ( Call m,
    HasState "typTab" TypeTable m,
    HasState "varTab" VariantToType m,
    Foldable t
  ) =>
  Symbol ->
  t Operand ->
  Integer ->
  m Operand
mallocVariant variantName args size = createVariantGen variantName args (malloc size)

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

-- | @internName@ interns a Symbol into an LLVM name
internName :: Symbol -> Name
internName = mkName . unintern
