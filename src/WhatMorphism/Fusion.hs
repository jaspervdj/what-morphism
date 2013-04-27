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
import           WhatMorphism.Expr
import           WhatMorphism.RewriteM


--------------------------------------------------------------------------------
fusePass :: [CoreBind] -> RewriteM [CoreBind]
fusePass = mapM $ \b -> withBinds b $ \_ ->
    Data.everywhereM $ \e -> case cast e of
        Just e' -> unsafeCoerce $ catchError (fuse e') (const $ return e')
        Nothing -> return e


--------------------------------------------------------------------------------
fuse :: Expr Var -> RewriteM (Expr Var)
fuse expr@(App _ _) = case CoreSyn.collectArgs expr of
    (Var fold, fArgs) -> do
        fReg <- isRegisteredFold fold
        unless fReg $ fail "Not a registered fold"
        let foldTy                   = Var.varType fold
            (foldForAllTys, foldTy') = Type.splitForAllTys foldTy
            (foldArgTys, foldReTy)   = Type.splitFunTys foldTy'
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
                return $ MkCore.mkCoreApps bLambda $
                    (Type foldReTy) :
                    drop (length foldForAllTys) (init fArgs)
            _ -> fail "No fold inside build"

    _ -> fail "No var app"
fuse _ = fail "No App"
