--------------------------------------------------------------------------------
module WhatMorphism.Fold
    ( foldPass
    , toFold
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>), (<|>))
import           Control.Monad          (forM, mplus, when)
import           Control.Monad          (unless)
import           Control.Monad.Error    (catchError)
import qualified CoreFVs                as CoreFVs
import           CoreSyn
import           Data.List              (find)
import qualified DataCon                as DataCon
import qualified MkCore                 as MkCore
import           Type                   (Type)
import qualified Type                   as Type
import           Var                    (Var)
import qualified Var                    as Var
import qualified VarSet                 as VarSet


--------------------------------------------------------------------------------
import           WhatMorphism.Dump
import           WhatMorphism.Expr
import           WhatMorphism.RemoveRec
import           WhatMorphism.RewriteM


--------------------------------------------------------------------------------
foldPass :: [CoreBind] -> RewriteM [CoreBind]
foldPass = fmap removeRec . mapM foldPass'
  where
    foldPass' = withBindsEverywhere $ \cb -> withBinds cb $ \f e -> do
        reg    <- isRegisteredFoldOrBuild f
        detect <- isDetectMode
        case reg of
            True -> return (f, e)
            _    -> do
                important $ "====== toFold: " ++ dump f
                flip catchError (report f e) $ do
                    e' <- toFold f e
                    unless detect $ registerForInlining f e'
                    important $ "Created fold."
                    return (setInlineInfo f, e')
    report f e err = do
        message $ "====== Error: " ++ err
        return (f, e)


--------------------------------------------------------------------------------
toFold :: Var -> Expr Var -> RewriteM (Expr Var)
toFold f body = do
    message $ "Starting with: " ++ dump body
    toFold' f (Var f) id body


--------------------------------------------------------------------------------
toFold' :: Var
        -> Expr Var
        -> (Expr Var -> Expr Var)
        -> Expr Var
        -> RewriteM (Expr Var)
toFold' f ef mkF (Lam x body) =
    toFoldOver f (\t -> App ef (Var t)) (\e -> mkF (Lam x e)) x body <|>
    toFold' f (App ef (Var x)) (\e -> mkF (Lam x e)) body
toFold' _ _  _   _            = fail "No top-level Lam"


--------------------------------------------------------------------------------
toFoldOver :: Var
           -> (Var -> Expr Var)
           -> (Expr Var -> Expr Var)
           -> Var
           -> Expr Var
           -> RewriteM (Expr Var)
toFoldOver f ef mkF d (Lam x body) =
    toFoldOver f (\t -> App (ef t) (Var x)) (\e -> mkF (Lam x e)) d body
toFoldOver f ef mkF d (Case (Var x) caseBinder rTyp alts)
    | x == d                    = do
        when (isUnliftedType rTyp) $
            fail "Cannot deal with unlifted fold result types"

        alts' <- forM alts $ \alt@(ac, bnds, expr) -> do
            message $ "Rewriting AltCon " ++ dump ac
            message $ "Was: " ++ dump expr
            -- Left-hand side of the case alternative. We can replace x and the
            -- caseBinder by this, this helps us recognize folds in some cases.
            lhs <- altLhs x alt
            let env    = [(x, lhs), (caseBinder, lhs)]
                expr' = substExpr env expr
            (expr'', rec) <- rewriteAlt ef d bnds rTyp expr'
            message $ "Now: " ++ dump expr''
            -- Note how the case binder cannot appear in the result, since it is
            -- a synonym for `x`.
            assertWellScoped (caseBinder : x : bnds) expr''
            return ((ac, expr''), rec)
        -- fold <- mkListFold d rTyp alts'

        module' <- rewriteModule
        when (or $ map snd alts') $
            case Type.splitTyConApp_maybe (Var.varType d) of
                Nothing      -> return ()
                Just (tc, _) -> important $ "WhatMorphismResult: Fold: " ++
                    dump module' ++ "." ++ dump f ++ ", " ++ dump tc

        detect <- isDetectMode
        if detect
            then return (Var f) -- Worst. Hack. Ever.
            else mkF <$> mkFold d rTyp (map fst alts')
    | otherwise                 = fail "Wrong argument destructed"
toFoldOver _ _ _ _ _            = fail "No top-level Case"


--------------------------------------------------------------------------------
altLhs :: Var -> Alt Var -> RewriteM (Expr Var)
altLhs x (ac, bs, _) = case ac of
    LitAlt l   -> return (Lit l)
    DEFAULT    -> return (Var x)
    DataAlt dc -> do
        xTyArgs <- liftMaybe "Destructed Var is no TyCon..." $ do
            (_, as) <- Type.splitTyConApp_maybe (Var.varType x)
            return as
        return $ MkCore.mkCoreApps
            (Var (DataCon.dataConWrapId dc))
            (map Type xTyArgs ++ map Var bs)


--------------------------------------------------------------------------------
mkFold :: Var                   -- ^ Destructed thingy
       -> Type                  -- ^ Return type
       -> [(AltCon, Expr Var)]  -- ^ Case bodies
       -> RewriteM (Expr Var)   -- ^ Resulting expression
mkFold d rTyp alts = do
    fold   <- registeredFold (Var.varType d)
    conses <- liftEither $ getDataCons (Var.varType d)
    fargs  <- mapM getAlt conses
    message $ "Conses: " ++ dump conses
    message $ "Our registered fold is: " ++ dump fold
    message $ "Of the type: " ++ dump (Var.varType fold)
    important $ "WhatMorphismResult: Fold using: " ++ dump fold
    return $ MkCore.mkCoreApps (Var fold) $
        -- Type arguments to destroyed thingy
        (map Type dTyArgs) ++
        -- Return type
        [Type rTyp] ++
        -- Algebra
        fargs ++
        -- Destroyed thingy
        [Var d]
  where
    getAlt dataCon = liftMaybe ("No alt found for " ++ dump dataCon) $
        lookup (DataAlt dataCon) alts `mplus`
        lookup DEFAULT alts

    dTyArgs = case Type.splitTyConApp_maybe (Var.varType d) of
        Just (_, tys) -> tys
        _             -> []


--------------------------------------------------------------------------------
-- This is (deprecated?) a version of 'mkFold' specific to [] lists.
{-
mkListFold :: Var
           -> Type
           -> [(AltCon, Expr Var)]
           -> RewriteM (Expr Var)
mkListFold d rTyp alts = do
    elemTyp <- getElemTyp
    consF   <- getAlt TysWiredIn.consDataCon
    nilF    <- getAlt TysWiredIn.nilDataCon
    liftCoreM $ MkCore.mkFoldrExpr elemTyp rTyp consF nilF (Var d)
  where
    getElemTyp = case Type.splitTyConApp_maybe (Var.varType d) of
        Nothing                            -> fail "Not working on a TyConApp"
        Just (tcon, [elemTyp])
            | tcon == TysWiredIn.listTyCon -> return elemTyp
            | otherwise                    -> fail "Not working on listTyCon"
        Just _                             -> fail "Weird listTyCon?!"

    getAlt dataCon = liftMaybe ("No alt found for " ++ dump dataCon) $
        lookup (DataAlt dataCon) alts
-}


--------------------------------------------------------------------------------
rewriteAlt :: (Var -> Expr Var)
           -> Var
           -> [Var]
           -> Type
           -> Expr Var
           -> RewriteM (Expr Var, Bool)  -- ^ Rewriten expr, any recursive
rewriteAlt _  _ []       _    body = return (body, False)
rewriteAlt ef d (t : ts) rTyp body = do
    (expr, rec)   <- rewriteAlt ef d ts rTyp body
    (expr', rec') <- liftCoreM $ if isRecursive
        then mkLambda rTyp            (ef t)  expr
        else mkLambda (Var.varType t) (Var t) expr

    return (expr', rec || (rec' && isRecursive))
  where
    isRecursive = Var.varType t `Type.eqType` Var.varType d


--------------------------------------------------------------------------------
-- | We don't actually do any scoping, we just have a list of vars which can't
-- appear anymore.
assertWellScoped :: [Var] -> Expr Var -> RewriteM ()
assertWellScoped vars body = case find (`VarSet.elemVarSet` varSet) vars of
    Just v -> fail $ "Not well-scoped: " ++ dump v ++ " still appears"
    _      -> return ()
  where
    varSet = CoreFVs.exprFreeVars body
