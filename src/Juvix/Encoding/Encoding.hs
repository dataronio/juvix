{-# LANGUAGE NamedFieldPuns #-}
module Juvix.Encoding.Encoding where

import qualified Data.Map.Strict as Map
import           Prelude         (error)

import           Juvix.Library   hiding (Sum, Product)
import           Juvix.Encoding.Types


-- Generic Case expansion ------------------------------------------------------

-- | caseGen takes an Case form and generates the proper expanded form
-- takes a few arguments to determine what to do when there are no arguments to
-- a case and how to combine the different cases
caseGen ∷ ( HasState "constructors" (Map SomeSymbol Bound)    m
          , HasState "adtMap"       (Map SomeSymbol Branches) m
          , HasThrow "err"          Errors                    m
          , HasWriter "missingCases" [SomeSymbol]             m )
        ⇒ Switch
        → (Lambda → Lambda)           -- What to do when there are no arguments?
        → (Lambda → Lambda → Lambda)  -- How does the recursive case work?
        → m Lambda
caseGen (Case _ []) _ _ =
  throw @"err" MatchWithoutCases
caseGen (Case on cases@(C c _ _ :_)) onNoArg onRec = do
  cons ← get @"constructors"
  case cons Map.!? c of
    Nothing              → throw @"err" (NotInAdt c)
    Just Bound {adtName} → do
      adtConstructors ← getOrderedConstructors adtName
      case adtConstructors of
        [] → throw @"err" InvalidAdt
        _ → do
          -- Faster with manual recursion!
          let lambdaFromEnv f t =
                case caseMap Map.!? t of
                  Nothing → do
                    tell @"missingCases" [t]
                    pure (f idL)
                  Just ([], body)   → pure (f $ onNoArg body)
                  Just (args, body) → pure (f (foldr Lambda body args))
              recCase t accLam = lambdaFromEnv (flip onRec accLam) t
              initial t        = lambdaFromEnv identity            t
              butLastadtCon    = reverse (tailSafe (reverse adtConstructors))

          last         ← initial (lastDef (error "doesn't happen") adtConstructors)
          expandedCase ← foldrM recCase last butLastadtCon
          return $ Application on expandedCase
  where
    caseMap = foldr (\ (C s args body) → Map.insert s (args, body)) mempty cases

    getOrderedConstructors adtName = do
      adtMap ← get @"adtMap"
      case adtMap Map.!? adtName of
        Just x  → return x
        Nothing → throw @"err" (AdtNotDefined adtName)

-- Helper for Mendler and Scott encodings --------------------------------------

adtConstructor ∷ ( HasState "adtMap" (Map SomeSymbol [k]) m
                 , HasState "constructors" (Map k Bound)  m
                 , HasThrow "err" Errors                  m
                 , Ord k
                 ) ⇒ k → Lambda → SomeSymbol → m ()
adtConstructor s prod name = do
  modify' @"adtMap" (Map.alter (\case
                                   Nothing → Just [s]
                                   Just x  → Just $ x <> [s]) name)
  cons ← get @"constructors"
  case cons Map.!? s of
    Just _  → throw   @"err" AlreadyDefined
    Nothing → modify' @"constructors" (Map.insert s (Bound prod name))


-- generic lambda helpers ------------------------------------------------------

idL ∷ Lambda
idL = Lambda (someSymbolVal "x") (Value (someSymbolVal "x"))

app ∷ Lambda → Lambda → Lambda
app (Lambda s t) replace = rec' t
  where
    rec' (Value s')
      | s == s'   = replace
      | otherwise = Value s'
    rec' (Lambda s' t)
      | s == s'   = Lambda s' t
      | otherwise = Lambda s' (rec' t)
    rec' (Application t1 t2) = Application (rec' t1) (rec' t2)
app t _replace = t