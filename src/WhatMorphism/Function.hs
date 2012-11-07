--------------------------------------------------------------------------------
module WhatMorphism.Function
    ( Function (..)
    , fromAppExpr
    , fromBindExpr
    , fromBinds
    ) where


--------------------------------------------------------------------------------
import           CoreSyn
import           Outputable


--------------------------------------------------------------------------------
import           WhatMorphism.Expr


--------------------------------------------------------------------------------
-- | A function call or definition
data Function f a = Function f [a]
    deriving (Show)


--------------------------------------------------------------------------------
instance (Outputable f, Outputable a) => Outputable (Function f a) where
    ppr (Function f as) = ppr f <+> ppr as


--------------------------------------------------------------------------------
-- | This returns a function application
fromAppExpr :: Expr b -> [Function (Expr b) (Expr b)]
fromAppExpr = go []
  where
    go as (App f a) = go (a : as) f
    go as e
        | null as   = []
        | otherwise = [Function e $ reverse as]


--------------------------------------------------------------------------------
-- | This returns a function definition (and the body as well)
fromBindExpr :: Expr b -> [(Function b b, Expr b)]
fromBindExpr (Let b _) = fromBinds b
fromBindExpr _         = []


--------------------------------------------------------------------------------
fromBinds :: Bind b -> [(Function b b, Expr b)]
fromBinds = concatMap (uncurry fromBind) . binds
  where
    fromBind b      = go []
      where
        go as (Lam a e) = go (a : as) e
        go as e
            | null as   = []
            | otherwise = [(Function b (reverse as), e)]
