--------------------------------------------------------------------------------
module WhatMorphism.Fusion
    ( fusePass
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   (pure, (<$>), (<*>))
import           Control.Monad         (unless)
import           Control.Monad.Error   (catchError)
import           CoreSyn               (Bind (..), CoreBind, Expr (..))
import qualified CoreSyn               as CoreSyn
import           Data.Maybe            (fromMaybe)
import qualified MkCore                as MkCore
import qualified Type                  as Type
import           Var                   (Var)
import qualified Var                   as Var


--------------------------------------------------------------------------------
import           WhatMorphism.Dump
import           WhatMorphism.Expr
import           WhatMorphism.RewriteM


--------------------------------------------------------------------------------
fusePass :: [CoreBind] -> RewriteM [CoreBind]
fusePass = mapM $ \b -> withBinds b $ \v expr -> do
    expr' <- fuse [] expr
    return (v, expr')
  where


--------------------------------------------------------------------------------
fuse :: [(Var, Expr Var)] -> Expr Var -> RewriteM (Expr Var)
fuse _ (Var x) = return (Var x)
fuse _ (Lit x) = return (Lit x)
fuse env (App e1 e2) = do
    e1' <- fuse env e1
    e2' <- fuse env e2
    fuseAppUntilFail env (App e1' e2')
fuse env (Lam x y) = Lam x <$> fuse env y
fuse env (Let (NonRec v b) e) = Let (NonRec v b) <$> fuse ((v, b) : env) e
fuse env (Let bs e) = Let bs <$> fuse env e
fuse env (Case e b t alts) = do
    e'    <- fuse env e
    alts' <- mapM fuse' alts
    return $ Case e' b t alts'
  where
    fuse' (ac, bs, ae) = do
        ae' <- fuse env ae
        return (ac, bs, ae')
fuse env (Cast e c) = Cast <$> fuse env e <*> pure c
fuse env (Tick t e) = Tick t <$> fuse env e
fuse _ (Type t) = return (Type t)
fuse _ (Coercion c) = return (Coercion c)


--------------------------------------------------------------------------------
fuseAppUntilFail :: [(Var, Expr Var)] -> Expr Var -> RewriteM (Expr Var)
fuseAppUntilFail env expr = catchError
    (fuseApp env expr >>= \expr' ->
        fuseAppUntilFail env expr')
    (\err -> do
        message $ "fusePass: " ++ err
        return expr)


--------------------------------------------------------------------------------
fuseApp :: [(Var, Expr Var)] -> Expr Var -> RewriteM (Expr Var)
fuseApp env expr@(App _ _) = case CoreSyn.collectArgs expr of
    (Var fold, fArgs) -> do
        fReg <- isRegisteredFold fold
        unless fReg $ fail $ "Not a registered fold: " ++ dump fold
        let foldTy                   = Var.varType fold
            (foldForAllTys, foldTy') = Type.splitForAllTys foldTy
            (foldArgTys, _foldReTy)  = Type.splitFunTys foldTy'
            foldConcreteReTy         = last $ take (length foldForAllTys) fArgs
        unless (length foldForAllTys + length foldArgTys == length fArgs) $
            fail "Incorrect arity..."
        let buildExpr = inEnv $ last fArgs
        case CoreSyn.collectArgs buildExpr of
            (Var build, bArgs) -> do
                bReg <- isRegisteredBuild build
                unless bReg $ fail "Not a registered build"
                let (_bExtraArgs, bLambda) = (init bArgs, last bArgs)
                {-
                bForAllArgs <- forM bExtraArgs $ \a -> case a of
                    Type t -> return t
                    _      -> fail "bExtraArgs not a type"
                -}
                module' <- rewriteModule
                important $ "WhatMorphismResult: Fusion: " ++ dump module'
                return $ MkCore.mkCoreApps bLambda $
                    foldConcreteReTy :
                    drop (length foldForAllTys) (init fArgs)
            _ -> fail "No fold inside build"
    _ -> fail "No var app"
  where
    inEnv :: Expr Var -> Expr Var
    inEnv (Var v) = fromMaybe (Var v) $ lookup v env
    inEnv x       = x

fuseApp _ _ = fail "No App"
