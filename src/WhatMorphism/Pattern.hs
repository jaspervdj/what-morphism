--------------------------------------------------------------------------------
module WhatMorphism.Pattern
    ( toFold
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   ((<|>))
import           Control.Monad         (forM_)
import           CoreSyn
import           Type                  (Type)
import qualified Type                  as Type
import           Var                   (Var)
import qualified Var                   as Var


--------------------------------------------------------------------------------
import           WhatMorphism.Dump
import           WhatMorphism.Expr
import           WhatMorphism.RewriteM


--------------------------------------------------------------------------------
toFold :: Var -> Expr Var -> RewriteM (Expr Var)
toFold f body = toFold' (Var f) body


--------------------------------------------------------------------------------
toFold' :: Expr Var -> Expr Var -> RewriteM (Expr Var)
toFold' f (Lam x body) =
    toFoldOver (\t -> App f (Var t)) x body <|>
    toFold' (App f (Var x)) body
toFold' _ _            = fail "No top-level Lam"


--------------------------------------------------------------------------------
toFoldOver :: (Var -> Expr Var)
           -> Var
           -> Expr Var
           -> RewriteM (Expr Var)
toFoldOver f d (Lam x body)            =
    toFoldOver (\t -> App (f t) (Var x)) d body
toFoldOver f d c@(Case (Var x) _ rTyp alts)
    | x == d                           = do
        forM_ alts $ \(ac, bnds, expr) -> do
            message $ "Rewriting AltCon " ++ dump ac
            message $ "Was: " ++ dump expr
            expr' <- rewriteAlt f d bnds rTyp expr
            message $ "Now: " ++ dump expr'
        return c
    | otherwise                        = fail "Wrong argument destructed"
toFoldOver _ _ _                       = fail "No top-level Case"


--------------------------------------------------------------------------------
rewriteAlt :: (Var -> Expr Var)
           -> Var
           -> [Var]
           -> Type
           -> Expr Var
           -> RewriteM (Expr Var)
rewriteAlt _ _ []       _    body = return body
rewriteAlt f d (t : ts) rTyp body = do
    expr <- rewriteAlt f d ts rTyp body
    if isRecursive
        then liftCoreM $ mkLambda rTyp            (f t)   expr
        else liftCoreM $ mkLambda (Var.varType t) (Var t) expr
  where
    isRecursive = Var.varType t `Type.eqType` Var.varType d
