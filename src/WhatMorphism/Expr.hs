-------------------------------------------------------------------------------
-- | Utilities for manipulating expressions
module WhatMorphism.Expr
    ( subExprs
    , binds
    ) where


--------------------------------------------------------------------------------
import           Coercion (Coercion)
import           CoreSyn
import           Literal  (Literal)
import           Type     (Type)
import           Var      (Id, Var)


--------------------------------------------------------------------------------
subExprs :: Expr b -> [Expr b]
subExprs x = x : go x
  where
    go (Var _)         = []
    go (Lit _)         = []
    go (App f a)       = subExprs f ++ subExprs a
    go (Lam _ e)       = subExprs e
    go (Let b e)       = concatMap subExprs (map snd $ binds b) ++ subExprs e
    go (Case e _ _ as) = concatMap subExprs [a | (_, _, a) <- as]
    go (Cast e _)      = subExprs e
    go (Tick _ e)      = subExprs e
    go (Type _)        = []
    go (Coercion _)    = []


--------------------------------------------------------------------------------
binds :: Bind b -> [(b, Expr b)]
binds (NonRec b e) = [(b, e)]
binds (Rec bs)     = bs


--------------------------------------------------------------------------------
-- | Since we love folds...
foldExpr
    :: (Id -> a)                                    -- ^ Var
    -> (Literal -> a)                               -- ^ Lit
    -> (a -> a -> a)                                -- ^ App
    -> (b -> a -> a)                                -- ^ Lam
    -> (b -> a -> a -> a)                           -- ^ Bind NonRec
    -> ([(b, a)] -> a -> a)                         -- ^ Bind Rec
    -> (a -> b -> Type -> [(AltCon, [b], a)] -> a)  -- ^ Case
    -> (a -> Coercion -> a)                         -- ^ Cast
    -> (Tickish Id -> a -> a)                       -- ^ Tick
    -> (Type -> a)                                  -- ^ Type
    -> (Coercion -> a)                              -- ^ Coercion
    -> Expr b                                       -- ^ Expr to fold over
    -> a                                            -- ^ Result
foldExpr var lit app lam bnrec brec cas cast tick typ coer = go
  where
    go (Var x)         = var x
    go (Lit x)         = lit x
    go (App f x)       = app (go f) (go x)
    go (Lam a x)       = lam a (go x)
    go (Let b e)       = case b of
        NonRec b' e' -> bnrec b' (go e') (go e)
        Rec bs       -> brec [(b', go e') | (b', e') <- bs] (go e)
    go (Case e b t as) = cas (go e) b t [(ac, bs, go e') | (ac, bs, e') <- as]
    go (Cast e c)      = cast (go e) c
    go (Tick t e)      = tick t (go e)
    go (Type t)        = typ t
    go (Coercion c)    = coer c
