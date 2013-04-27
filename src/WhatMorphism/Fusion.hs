--------------------------------------------------------------------------------
module WhatMorphism.Fusion
    ( fusePass
    ) where


--------------------------------------------------------------------------------
import           Control.Monad         (forM, unless)
import           Control.Monad.Error   (catchError)
import           CoreSyn               (CoreBind, Expr (..))
import qualified CoreSyn               as CoreSyn
import qualified Data.Generics         as Data
import qualified Data.Generics.Schemes as Data
import           Data.Typeable         (cast)
import qualified MkCore                as MkCore
import qualified Outputable            as Outputable
import           Type                  (Type)
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
        foldDesTyCon <- liftMaybe "No TyCon App" $
            Type.tyConAppTyCon_maybe (last foldArgTys)
        unless (length foldForAllTys + length foldArgTys == length fArgs) $
            fail "Incorrect arity..."
        let buildExpr = last fArgs
        liftCoreM $
            Outputable.pprTrace "fArgs" (Outputable.ppr fArgs) $ return ()
        liftCoreM $
            Outputable.pprTrace "foldForAllTys" (Outputable.ppr foldForAllTys) $ return ()
        liftCoreM $
            Outputable.pprTrace "foldArgTys" (Outputable.ppr foldArgTys) $ return ()
        liftCoreM $
            Outputable.pprTrace "relevant args" (Outputable.ppr $
                    drop (length foldForAllTys) (init fArgs)
                ) $ return ()
        case CoreSyn.collectArgs buildExpr of
            (Var build, bArgs) -> do
                bReg <- isRegisteredBuild build
                unless bReg $ fail "Not a registered build"
                let (bExtraArgs, bLambda) = (init bArgs, last bArgs)
                bForAllArgs <- forM bExtraArgs $ \a -> case a of
                    Type t -> return t
                    _      -> fail "bExtraArgs not a type"

                liftCoreM $
                    Outputable.pprTrace "foldDesTyCon" (Outputable.ppr $
                            foldDesTyCon
                        ) $ return ()
                liftCoreM $
                    Outputable.pprTrace "bForAllArgs" (Outputable.ppr $
                            bForAllArgs
                        ) $ return ()
                bLambda' <- specialize
                    (Type.mkTyConApp foldDesTyCon bForAllArgs) bLambda
                liftCoreM $
                    Outputable.pprTrace "bLambda" (Outputable.ppr $
                            bLambda'
                        ) $ return ()
                return $ MkCore.mkCoreApps bLambda $
                    (Type foldReTy) :
                    drop (length foldForAllTys) (init fArgs)
            _ -> fail "No fold inside build"

    _ -> fail "No var app"
fuse _ = fail "No App"


--------------------------------------------------------------------------------
-- | Specialize a function in the form of
--
-- > \b :: AnyK a1 a2 ... -> foo :: b
--
-- with a concrete type, so we get
--
-- > \b :: a1 a2 ... -> foo :: [Int]
specialize :: Type -> Expr Var -> RewriteM (Expr Var)
specialize ty (Lam b expr) = return $ Data.everywhere sub expr
  where
    bTy   = Type.mkTyVarTy b
    sub x = case cast x of
        Nothing                 -> x
        Just t
            | Type.eqType bTy t -> unsafeCoerce ty
            | otherwise         -> x
specialize _ _ = fail $ "Can't specialize stuff like this."
