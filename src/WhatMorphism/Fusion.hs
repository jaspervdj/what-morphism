--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module WhatMorphism.Fusion
    ( foldFoldFusion
    , listFoldSpec
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   (pure, (<$>), (<*>))
import           Control.Monad.State   (StateT, evalStateT, get, modify)
import           Control.Monad.Trans   (lift)
import           CoreSyn
import           Data.Maybe            (maybeToList)
import qualified PrelNames             as PrelNames
import           Type                  (Type)
import           Var                   (Var)
import qualified Var                   as Var


--------------------------------------------------------------------------------
import           WhatMorphism.Dump
import           WhatMorphism.Expr
import           WhatMorphism.RewriteM
import           WhatMorphism.SynEq


--------------------------------------------------------------------------------
foldFoldFusion :: Expr Var -> RewriteM (Expr Var)
foldFoldFusion = rewriteBranch [] $ \expr -> do
    let mspec = listFoldSpec expr
    modify $ \s -> maybeToList mspec ++ s
    numSpecs <- length <$> get
    case mspec of
        Nothing -> return ()
        Just s  -> lift $ message $ "foldFoldFusion: " ++ dump s
    return expr


--------------------------------------------------------------------------------
data FoldSpec = FoldSpec
    { foldFunction   :: Var
    , foldReturnType :: Type
    , foldAlgebra    :: [Expr Var]
    , foldDestroys   :: Expr Var
    }


--------------------------------------------------------------------------------
instance Dump FoldSpec where
    dump (FoldSpec f r a d) =
        "(FoldSpec " ++ unwords [dump f, dump r, dump a, dump d] ++ ")"


--------------------------------------------------------------------------------
isFusable :: FoldSpec -> FoldSpec -> Bool
isFusable fs1 fs2 =
    foldFunction fs1 .==. foldFunction fs2 &&
    foldDestroys fs1 .==. foldDestroys fs2


--------------------------------------------------------------------------------
listFoldSpec :: Expr Var -> Maybe FoldSpec

listFoldSpec (App (App (App (App (App (Var foldrVar) _) rTyp) cons) nilF) d)
    | Var.varName foldrVar /= PrelNames.foldrName = Nothing
    | otherwise                                   = do
        rTyp' <- fromType rTyp
        return FoldSpec
            { foldFunction   = foldrVar
            , foldReturnType = rTyp'
            , foldAlgebra    = [cons, nilF]
            , foldDestroys   = d
            }
  where
    fromType (Type t) = Just t
    fromType _        = Nothing

listFoldSpec _                                      = Nothing


--------------------------------------------------------------------------------
rewriteBranch :: forall s.
                 s                                           -- ^ Initial state
              -> (Expr Var -> StateT s RewriteM (Expr Var))  -- ^ Rewrite
              -> Expr Var                                    -- ^ Input
              -> RewriteM (Expr Var)                         -- ^ Result
rewriteBranch initial f expr = evalStateT (go expr) initial
  where
    -- Rewrite with a resetted state
    local :: Expr Var -> StateT s RewriteM (Expr Var)
    local x = lift $ evalStateT (go x) initial

    -- Note how we use local for Lam and Case
    go :: Expr Var -> StateT s RewriteM (Expr Var)
    go (Var x)           = f (Var x)
    go (Lit x)           = f (Lit x)
    go (App x y)         = f =<< App <$> go x <*> go y
    go (Lam x y)         = f =<< Lam x <$> local y
    go (Let x y)         = f =<< Let <$> withBinds x (\_ e -> f e) <*> go y
    go (Case x y t alts) = f =<<
        Case <$> go x <*> pure y <*> pure t <*> mapM goAlt alts
    go (Cast x c)        = f =<< Cast <$> go x <*> pure c
    go (Tick t y)        = f =<< Tick <$> pure t <*> go y
    go (Type t)          = f (Type t)
    go (Coercion c)      = f (Coercion c)

    -- This is always local
    goAlt :: Alt Var -> StateT s RewriteM (Alt Var)
    goAlt (ac, bs, expr) = do
        expr' <- local expr
        return (ac, bs, expr')
