--------------------------------------------------------------------------------
module WhatMorphism.Function
    ( Function (..)
    , fromAppExpr
    , fromBindExpr
    ) where


--------------------------------------------------------------------------------
import           CoreSyn


--------------------------------------------------------------------------------
import           WhatMorphism.Expr


--------------------------------------------------------------------------------
-- | A function call or definition
data Function f a = Function f [a]
    deriving (Show)


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
-- | This returns a function definition
fromBindExpr :: Expr b -> [Function b b]
fromBindExpr (Let b _) = fromBinds b
fromBindExpr _         = []


--------------------------------------------------------------------------------
fromBinds :: Bind b -> [Function b b]
fromBinds = concatMap (uncurry fromBind) . binds
  where
    fromBind b      = go []
      where
        go as (Lam a e) = go (a : as) e
        go as _
            | null as   = []
            | otherwise = [Function b $ reverse as]
