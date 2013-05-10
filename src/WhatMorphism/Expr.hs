-------------------------------------------------------------------------------
-- | Utilities for manipulating expressions
module WhatMorphism.Expr
    ( subExprs
    , subExprsInBranch
    , count
    , replaceExpr
    , substExpr
    , substTy
    , toVar
    , mkLambda
    , freshVar
    , binds
    , withBinds
    , withBindsEverywhere
    , foldExpr
    , guessFunctionReturnType
    , getDataCons
    , idBaseName
    ) where


--------------------------------------------------------------------------------
import           Coercion                   (Coercion)
import           Control.Monad              (forM, liftM)
import           Control.Monad.State.Strict (State, modify, runState)
import qualified CoreFVs                    as CoreFVs
import           CoreMonad                  (CoreM)
import qualified CoreSubst                  as CoreSubst
import           CoreSyn
import           Data.Data                  (Data)
import qualified Data.Generics              as Data
import           Data.Typeable              (cast)
import           DataCon                    (DataCon)
import           Literal                    (Literal)
import qualified Name                       as Name
import qualified OccName                    as OccName
import qualified SrcLoc                     as SrcLoc
import qualified TyCon                      as TyCon
import           Type                       (Type)
import qualified Type                       as Type
import qualified UniqSupply                 as Unique
import qualified Unique                     as Unique
import           Unsafe.Coerce              (unsafeCoerce)
import           Var                        (Id, Var)
import qualified Var                        as Var
import qualified VarEnv                     as VarEnv


--------------------------------------------------------------------------------
import           WhatMorphism.SynEq


--------------------------------------------------------------------------------
subExprs :: Expr b -> [Expr b]
subExprs x = x : go x
  where
    go (Var _)         = []
    go (Lit _)         = []
    go (App f a)       = subExprs f ++ subExprs a
    go (Lam _ e)       = subExprs e
    go (Let b e)       = concatMap subExprs (map snd $ binds b) ++ subExprs e
    go (Case e _ _ as) = subExprs e ++ concatMap subExprs [a | (_, _, a) <- as]
    go (Cast e _)      = subExprs e
    go (Tick _ e)      = subExprs e
    go (Type _)        = []
    go (Coercion _)    = []


--------------------------------------------------------------------------------
subExprsInBranch :: Expr b -> [Expr b]
subExprsInBranch x = x : go x
  where
    go (Var _)          = []
    go (Lit _)          = []
    go (App f a)        = subExprs f ++ subExprs a
    go (Lam _ e)        = subExprs e
    go (Let b e)        = concatMap subExprs (map snd $ binds b) ++ subExprs e
    go (Case e _ _ _as) = subExprs e
    go (Cast e _)       = subExprs e
    go (Tick _ e)       = subExprs e
    go (Type _)         = []
    go (Coercion _)     = []


--------------------------------------------------------------------------------
-- | Just count occurences
count :: Expr Var -> Expr Var -> Int
count needle = Data.everything (+) $ \x -> case cast x of
    Nothing -> 0
    Just x' -> if x' .==. needle then 1 else 0


--------------------------------------------------------------------------------
-- | Count replacements
replaceExpr :: (Expr Var -> Maybe (Expr Var)) -> Expr Var -> (Expr Var, Int)
replaceExpr f = flip runState 0 . state
  where
    state :: Expr Var -> State Int (Expr Var)
    state = Data.everywhereM $ \x -> case cast x of
        Nothing -> return x
        Just y  -> case f y of
            Nothing -> return x
            Just y' -> modify (+ 1) >> return (unsafeCoerce y')


--------------------------------------------------------------------------------
mkEasySubst :: [(Var, Expr Var)] -> CoreSubst.Subst
mkEasySubst env = CoreSubst.extendSubstList
    (CoreSubst.setInScope CoreSubst.emptySubst inScopeSet) env
  where
    inScopeSet = foldr
        (\v s -> VarEnv.delInScopeSet s v)
        (foldr
            (\e s -> VarEnv.extendInScopeSetSet s (CoreFVs.exprFreeVars e))
            VarEnv.emptyInScopeSet (map snd env))
        (map fst env)


--------------------------------------------------------------------------------
substExpr :: [(Var, Expr Var)] -> Expr Var -> Expr Var
substExpr env = CoreSubst.substExpr (error "Ignored SDoc") (mkEasySubst env)


--------------------------------------------------------------------------------
-- TODO Use TvSubst shizzle
substTy :: [(Type.TyVar, Type)] -> Type -> Type
substTy env = CoreSubst.substTy (mkEasySubst [(v, Type a) | (v, a) <- env])


--------------------------------------------------------------------------------
binds :: Bind b -> [(b, Expr b)]
binds (NonRec b e) = [(b, e)]
binds (Rec bs)     = bs


--------------------------------------------------------------------------------
withBinds :: Monad m => Bind b -> (b -> Expr b -> m (Expr b)) -> m (Bind b)
withBinds (NonRec b e) f = liftM (NonRec b) $ f b e
withBinds (Rec bs)     f = liftM Rec $ forM bs $ \(b, e) -> do
    e' <- f b e
    return (b, e')


--------------------------------------------------------------------------------
withBindsEverywhere :: (Data b, Monad m)
                    => (Bind b -> m (Bind b)) -> Bind b -> m (Bind b)
withBindsEverywhere f = Data.everywhereM $ \b -> case cast b of
    Just cb -> f cb >>= \cb' -> return (unsafeCoerce cb')
    Nothing -> return b


--------------------------------------------------------------------------------
toVar :: Expr Var -> Maybe Var
toVar (Var v) = Just v
toVar _       = Nothing


--------------------------------------------------------------------------------
-- | Remove an expression by creating a lambda
--
-- If we float out @x@, this:
--
-- > foo x + x
--
-- Becomes something like:
--
-- > (\tmp -> foo tmp + tmp) x
mkLambda :: Type -> Expr Var -> Expr Var -> CoreM (Expr Var, Bool)
mkLambda typ needle haystack = do
    tmp <- freshVar "wm" typ

    let check n
            | n .==. needle = Just (Var tmp)
            | otherwise     = Nothing
        (haystack', repl)   = replaceExpr check haystack

    return $ if repl > 0
        then (Lam tmp haystack', True)
        else (haystack, False)


--------------------------------------------------------------------------------
-- | Generate a fresh variable
freshVar :: String -> Type -> CoreM Var
freshVar prefix typ = do
    unique <- Unique.getUniqueM
    let occn = OccName.mkVarOcc $ prefix ++ "_" ++ show (Unique.getKey unique)
        name = Name.mkInternalName unique occn SrcLoc.noSrcSpan
        -- var  = Var.mkLocalVar IdInfo.VanillaId name typ IdInfo.vanillaIdInfo
        var  = Var.mkCoVar name typ
    return var


--------------------------------------------------------------------------------
-- | Since we love folds...
-- TODO: Delete?
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
foldExpr var lit app lam bnrec brec cas cast' tick typ coer = go
  where
    go (Var x)         = var x
    go (Lit x)         = lit x
    go (App f x)       = app (go f) (go x)
    go (Lam a x)       = lam a (go x)
    go (Let b e)       = case b of
        NonRec b' e' -> bnrec b' (go e') (go e)
        Rec bs       -> brec [(b', go e') | (b', e') <- bs] (go e)
    go (Case e b t as) = cas (go e) b t [(ac, bs, go e') | (ac, bs, e') <- as]
    go (Cast e c)      = cast' (go e) c
    go (Tick t e)      = tick t (go e)
    go (Type t)        = typ t
    go (Coercion c)    = coer c


--------------------------------------------------------------------------------
guessFunctionReturnType :: Type -> Type
guessFunctionReturnType = snd . Type.splitFunTys . snd . Type.splitForAllTys


--------------------------------------------------------------------------------
-- | This should return the datacons in the correct order!
getDataCons :: Type -> Either String [DataCon]
getDataCons typ = case Type.splitTyConApp_maybe typ of
    Nothing      -> Left "getDataCons: type is no TyConApp?"
    Just (tc, _) -> case TyCon.tyConDataCons_maybe tc of
        Nothing  -> Left "No DataCon's found"
        Just dcs -> Right dcs


--------------------------------------------------------------------------------
idBaseName :: Id -> String
idBaseName = OccName.occNameString . Name.nameOccName . Var.varName
