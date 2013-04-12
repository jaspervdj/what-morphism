--------------------------------------------------------------------------------
-- | GHC Core uses two constructors for let bindings:
--
-- > Rec [g = f, f = g]
--
-- for recursive bindings, and
--
-- > NonRec (g = f)
--
-- for non-recursive bindings. By applying our rewrite rules and converting
-- functions to fold and build-based functions, we often remove actual
-- recursion. This module is responsible for turning 'Rec' constructors into
-- 'NonRec' constructors where possible.
module WhatMorphism.RemoveRec
    ( removeRec
    ) where


--------------------------------------------------------------------------------
import           CoreSyn               (Bind (..), CoreBind, Expr (..))
import qualified Data.Generics.Schemes as Data
import           Data.Typeable         (cast)
import           Var                   (Var)


--------------------------------------------------------------------------------
import           WhatMorphism.SynEq


--------------------------------------------------------------------------------
removeRec :: [CoreBind] -> [CoreBind]
removeRec = concatMap go
  where
    go b@(NonRec _ _) = [b]
    go (Rec [])       = []
    go (Rec bs)       = case break isNonRec' bs of
        (xs, (y : ys)) -> NonRec (fst y) (snd y) : go (Rec (xs ++ ys))
        _              -> [Rec bs]
      where
        isNonRec' (b, e) = isNonRec (map fst bs) b e


--------------------------------------------------------------------------------
isNonRec :: [Var] -> Var -> Expr Var -> Bool
isNonRec binds _ e = null $ contained (map Var binds) e


--------------------------------------------------------------------------------
contained :: [Expr Var] -> Expr Var -> [Expr Var]
contained candidates = Data.everything (++) $ \x -> case cast x of
    Nothing -> []
    Just x' -> filter (.==. x') candidates
