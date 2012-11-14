--------------------------------------------------------------------------------
module WhatMorphism.Function
    ( Function (..)
    , fromAppExpr
    , toAppExpr
    , fromBindExpr
    , fromBinds

    , toNamedFunction
    , varArgs
    , mapFunction
    , mapArgs
    , replaceArg
    , checkRecCall
    ) where


--------------------------------------------------------------------------------
import           CoreSyn
import           Outputable
import           Var               (Var)


--------------------------------------------------------------------------------
import           WhatMorphism.Expr


--------------------------------------------------------------------------------
-- | A function call or definition
data Function f a = Function
    { functionTerm :: f
    , functionArgs :: [a]
    } deriving (Show)


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
toAppExpr :: Function (Expr b) (Expr b) -> Expr b
toAppExpr (Function f []) = foldr (flip App) f $ reverse as


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


--------------------------------------------------------------------------------
toNamedFunction :: Function (Expr Var) a -> [Function Var a]
toNamedFunction (Function (Var v) as) = [Function v as]
toNamedFunction _                     = []


--------------------------------------------------------------------------------
varArgs :: Function f (Expr Var) -> [Function f Var]
varArgs (Function f as) = case mapM toVar as of
    Nothing  -> []
    Just as' -> [Function f as']


--------------------------------------------------------------------------------
mapFunction :: (a -> b) -> Function a a -> Function b b
mapFunction g (Function f as) = Function (g f) (map g as)


--------------------------------------------------------------------------------
mapArgs :: (a -> b) -> Function f a -> Function f b
mapArgs g (Function f as) = Function f $ map g as


--------------------------------------------------------------------------------
replaceArg :: Int -> a -> Function f a -> Function f a
replaceArg idx a (Function f as) =
    let (xs, ys) = splitAt idx as
    in Function f $ xs ++ [a] ++ drop 1 ys


--------------------------------------------------------------------------------
-- | Check the arguments of a recursive call. They should all be the same except
-- for the one at some index which we previously determined.
--
-- If this is the case, we return the new variable at that index.
checkRecCall :: Int -> Function Var Var -> Function Var Var -> [Var]
checkRecCall dIndex (Function def dargs) (Function call cargs)
    -- Should have the same name
    | def /= call = []
    | otherwise   = go 0 dargs cargs
  where
    go i (x : xs) (y : ys)
        -- Should be the same
        | i /= dIndex      = if x == y then go (i + 1) xs ys else []
        -- Can be different, return called arg
        | xs == ys         = [y]
        -- But not if other arguments are different
        | otherwise        = []
    -- Different list lengths etc
    go _ _        _        = []
