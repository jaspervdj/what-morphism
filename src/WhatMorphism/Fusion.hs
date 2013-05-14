--------------------------------------------------------------------------------
module WhatMorphism.Fusion
    ( fusePass
    ) where


--------------------------------------------------------------------------------
import           Control.Monad         (unless)
import           Control.Monad.Error   (catchError)
import           CoreSyn               (CoreBind, Expr (..))
import qualified CoreSyn               as CoreSyn
import qualified Data.Generics         as Data
import           Data.Typeable         (cast)
import qualified MkCore                as MkCore
import qualified Type                  as Type
import           Unsafe.Coerce         (unsafeCoerce)
import           Var                   (Var)
import qualified Var                   as Var


--------------------------------------------------------------------------------
import           WhatMorphism.Dump
import           WhatMorphism.Expr
import           WhatMorphism.RewriteM


--------------------------------------------------------------------------------
fusePass :: [CoreBind] -> RewriteM [CoreBind]
fusePass = mapM $ \b -> withBinds b $ \v expr -> do
    expr' <- Data.everywhereM
        (\me -> case cast me of
            Nothing -> return me
            Just e  -> unsafeCoerce $ catchError (fuse e) $ \err -> do
                message $ "fusePass: " ++ err
                return e)
        expr
    return (v, expr')


--------------------------------------------------------------------------------
fuse :: Expr Var -> RewriteM (Expr Var)
fuse expr@(App _ _) = case CoreSyn.collectArgs expr of
    (Var fold, fArgs) -> do
        fReg <- isRegisteredFold fold
        unless fReg $ fail $ "Not a registered fold: " ++ dump fold
        let foldTy                   = Var.varType fold
            (foldForAllTys, foldTy') = Type.splitForAllTys foldTy
            (foldArgTys, _foldReTy)  = Type.splitFunTys foldTy'
            foldConcreteReTy         = last $ take (length foldForAllTys) fArgs
        unless (length foldForAllTys + length foldArgTys == length fArgs) $
            fail "Incorrect arity..."
        let buildExpr = last fArgs
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
fuse _ = fail "No App"
