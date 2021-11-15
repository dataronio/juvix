module Juvix.Witch.CPSTranslation.SexpHelpers where

import Juvix.Library hiding (empty, from, fst, list, snd, trace)
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Sexp as Sexp
import Juvix.Sexp.Structure
import qualified Juvix.Sexp.Structure.CoreNamed as Core
import qualified Juvix.Sexp.Structure.Parsing as Str
import qualified Juvix.Sexp.Structure.Transition as Str

spair :: Sexp.T
spair = var ":pair"

empty :: Sexp.T
empty = Sexp.Nil

do' :: [Str.DoBodyFull] -> Sexp.T
do' x = from . Str.Do . Sexp.list $ Str.fromDoBodyFull <$> x

lam :: Sexp.T -> Sexp.T -> Sexp.T
lam name x = from $ Core.Lam name' x
  where
    (Just name') = Sexp.nameFromT name

app :: Sexp.T -> Sexp.T -> Sexp.T
app x y = from $ Core.App x y

letMatch :: Sexp.T -> [Str.ArgBody] -> Sexp.T -> Sexp.T
letMatch x y z = from $ Str.LetMatch x y z

fun :: Sexp.T -> Sexp.T -> Sexp.T -> Sexp.T
fun x y z = from $ Str.Defun x y z

pair :: Sexp.T -> Sexp.T -> Sexp.T
pair x y = from $ Core.Pair x y

-- multiple argument application
app' :: Sexp.T -> [Sexp.T] -> Sexp.T
app' f args = foldr (\arg acc -> app acc arg) f args

-- multiple argument lambda
lam' :: [Sexp.T] -> Sexp.T -> Sexp.T
lam' args body = foldr (\arg acc -> lam arg acc) body args

var :: NameSymbol.T -> Sexp.T
var x = Sexp.atom x

skip :: Sexp.T
skip = Sexp.Nil

unlist :: Sexp.T -> Sexp.T
unlist (sexp Sexp.:> Sexp.Nil) = sexp
unlist sexp = sexp

fst :: Sexp.T -> Sexp.T
fst var_ = letMatch var_ argbody skip
  where
    argbody = [Str.ArgBody (spair Sexp.:> var "p" Sexp.:> var "q") (var "p")]

snd :: Sexp.T -> Sexp.T
snd var_ = letMatch var_ argbody skip
  where
    argbody = [Str.ArgBody (spair Sexp.:> var "p" Sexp.:> var "q") (var "q")]

let_ :: Core.Binder -> Sexp.T -> Sexp.T
let_ b x = from $ Core.Let b x

binder :: NameSymbol.T -> Sexp.T -> Core.Binder
binder name term = Core.Binder name (var ":omega") term

-- TODO: move to record
triple :: Sexp.T -> Sexp.T -> Sexp.T -> Sexp.T
triple name value cont = from $ Core.Pair name (from $ Core.Pair value cont)
