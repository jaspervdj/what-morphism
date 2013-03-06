--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module WhatMorphism.Fusion
    ( foldFoldFusion
    , isListFold
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   ((<$>), (<*>))
import           Control.Monad.State   (StateT, evalStateT)
import           Control.Monad.Trans   (lift)
import           CoreSyn
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
foldFoldFusion = undefined


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
isListFold :: Expr Var -> Maybe FoldSpec

isListFold (App (App (App (App (App (Var foldrVar) _) rTyp) cons) nilF) d)
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

isListFold _                                      = Nothing


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
    go (Var x)   = f (Var x)
    go (Lit x)   = f (Lit x)
    go (App x y) = f =<< App <$> go x <*> go y
    go (Lam x y) = f =<< Lam x <$> local y
    go (Let x y) = f =<< Let <$> withBinds x (\_ e -> f e) <*> go y
