--------------------------------------------------------------------------------
module WhatMorphism.Fold
    ( foldPass
    , toFold
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    (pure, (<$>), (<*>))
import           Control.Monad          (foldM, forM, mplus, unless, when)
import           Control.Monad.Error    (catchError)
import           Control.Monad.Reader   (ReaderT, ask, local, runReaderT)
import           Control.Monad.Trans    (lift)
import           Control.Monad.Writer   (WriterT, runWriterT, tell)
import qualified CoreFVs                as CoreFVs
import           CoreSyn
import           Data.List              (find)
import           Data.Monoid            (Monoid (..))
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
data ArgInfo
    = TypeArg
    | ScrutineeArg
    | OtherArg
    deriving (Show)


--------------------------------------------------------------------------------
toFold :: Var -> Expr Var -> RewriteM (Expr Var)
toFold f body = do
    let (args, body') = CoreSyn.collectBinders body
    (x, caseBinder, reTy, alts) <- case body' of
        Case (Var x) caseBinder reTy alts -> return (x, caseBinder, reTy, alts)
        _                                 -> fail "No top-level case"

    when (isUnliftedType reTy) $
        fail "Cannot deal with unlifted fold result types"

    let argInfo a
            | Var.isTyVar a   = TypeArg
            | Var.isTcTyVar a = TypeArg
            | a == x          = ScrutineeArg
            | otherwise       = OtherArg

        argInfos  = [(a, argInfo a) | a <- args]
        otherArgs = [a | (a, OtherArg) <- argInfos]
        altReTy   = Type.mkFunTys (map Var.varType otherArgs) reTy

    let foldRead = FoldRead f (Var.varType x) reTy altReTy argInfos []
    (alts', rec) <-
        flip runFold foldRead $ forM alts $ \alt@(ac, bnds, _) -> do
            expr' <- rewriteAlt x caseBinder alt
            -- Note how the case binder cannot appear in the result, since it is
            -- a synonym for `x`. Also note how non-recursive bindings *will*
            -- still appear in the `expr'`, but since we have something like:
            --
            --     sum (x : xs) = x + sum xs
            --
            -- ~~>
            --
            --     (\x rec -> x + rec)
            --
            -- The `x` will be shadowed by the lambda binder, so it won't appear
            -- as a free variable!
            assertWellScoped (caseBinder : x : bnds) expr'
            return (ac, expr')

    let isDegenerateFold = not $ unAnyBool rec
    module' <- rewriteModule
    when (not isDegenerateFold) $
        case Type.splitTyConApp_maybe (Var.varType x) of
            Nothing      -> return ()
            Just (tc, _) -> important $ "WhatMorphismResult: Fold: " ++
                dump module' ++ "." ++ dump f ++ ", " ++ dump tc

    detect <- isDetectMode
    if (detect || isDegenerateFold)
        then fail $ "Not changing (degenerate=" ++ show isDegenerateFold ++ ")"
        else do
            expr' <- mkFold x altReTy alts'
            return $ MkCore.mkCoreLams
                args
                (MkCore.mkCoreApps expr' (map Var otherArgs))


--------------------------------------------------------------------------------
newtype AnyBool = AnyBool {unAnyBool :: Bool} deriving (Eq, Show)


--------------------------------------------------------------------------------
instance Monoid AnyBool where
    mempty                          = AnyBool False
    mappend (AnyBool x) (AnyBool y) = AnyBool (x || y)


--------------------------------------------------------------------------------
type FoldWrite = AnyBool


--------------------------------------------------------------------------------
data FoldRead = FoldRead
    { foldVar        :: Var
    , foldDeTy       :: Type
    , foldReTy       :: Type
    , foldAltReTy    :: Type
    , foldArgInfos   :: [(Var, ArgInfo)]
    , foldRecBinders :: [(Var, Var)]
    }


--------------------------------------------------------------------------------
type Fold a = ReaderT FoldRead (WriterT FoldWrite RewriteM) a


--------------------------------------------------------------------------------
runFold :: Fold a -> FoldRead -> RewriteM (a, AnyBool)
runFold f fr = runWriterT (runReaderT f fr)


--------------------------------------------------------------------------------
liftRewriteM :: RewriteM a -> Fold a
liftRewriteM = lift . lift


--------------------------------------------------------------------------------
rewriteAlt :: Var -> Var -> (AltCon, [Var], Expr Var) -> Fold (Expr Var)
rewriteAlt x caseBinder alt@(_, bnds, expr) = do
    deTy     <- foldDeTy <$> ask
    altReTy  <- foldAltReTy <$> ask
    argInfos <- foldArgInfos <$> ask
    lamArgs  <- forM bnds $ \b -> do
        let rec = Type.eqType (Var.varType b) deTy
        arg <- if rec
            then liftRewriteM (liftCoreM $ freshVar "rec" altReTy)
            else return b
        return (arg, rec)

    -- Left-hand side of the case alternative. We can replace x and the
    -- caseBinder by this, this helps us recognize folds in some cases.
    lhs <- altLhs x alt
    let env        = [(x, lhs), (caseBinder, lhs)]
        expr'      = substExpr env expr
        recBinders = [(b, a) | (b, (a, True)) <- zip bnds lamArgs]

    expr'' <- local
        (\r -> r {foldRecBinders = recBinders})
        (rewriteAltBody expr')

    return $ MkCore.mkCoreLams
        (map fst lamArgs ++ [a | (a, OtherArg) <- argInfos])
        expr''


--------------------------------------------------------------------------------
rewriteAltBody :: Expr Var -> Fold (Expr Var)

rewriteAltBody e@(Var _) = return e

rewriteAltBody e@(Lit _) = return e

-- TODO: real work here!
rewriteAltBody expr@(App _ _) = do
    f          <- foldVar <$> ask
    argInfos   <- foldArgInfos <$> ask
    recBinders <- foldRecBinders <$> ask

    let (app, args) = CoreSyn.collectArgs expr
    case app of
        (Var v)
            | v == f    -> do
                unless (length args == length argInfos) $
                    fail "Wrong arg length"

                (args', recSubTerm) <- foldM
                        (\(as, rst) (arg, (_, info)) -> case (arg, info) of
                            (Var a, ScrutineeArg) ->
                                case lookup a recBinders of
                                    Nothing -> fail "Wrong ScrutineeArg"
                                    Just a' -> return (as, Just a')
                            (_, ScrutineeArg) -> fail "Weird ScrutineeArg"
                            (_, TypeArg) -> return (as, rst)
                            (_, OtherArg) -> do
                                arg' <- rewriteAltBody arg
                                return (as ++ [arg'], rst))
                        ([], Nothing)
                        (zip args argInfos)

                recSubTerm' <- case recSubTerm of
                    Just rst -> return rst
                    _        -> fail "No recSubTerm"

                tell $ AnyBool True  -- We have recursion!
                return $ MkCore.mkCoreApps (Var recSubTerm') args'

            | otherwise -> do
                args' <- mapM rewriteAltBody args
                return $ MkCore.mkCoreApps (Var v) args'

        _ -> do
            args' <- mapM rewriteAltBody args
            return $ MkCore.mkCoreApps app args'

rewriteAltBody (Lam x e) = Lam x <$> rewriteAltBody e

rewriteAltBody (Let bs ex) = do
    bs' <- withBinds bs $ \b e ->
        rewriteAltBody e >>= \e' -> return (b, e')
    ex' <- rewriteAltBody ex
    return (Let bs' ex')

rewriteAltBody (Case e b t alts) = do
    e'    <- rewriteAltBody e
    alts' <- forM alts $ \(ac, bs, ex) -> do
        ex' <- rewriteAltBody ex
        return (ac, bs, ex')
    return (Case e' b t alts')

rewriteAltBody (Cast e c) = Cast <$> rewriteAltBody e <*> pure c

rewriteAltBody (Tick t e) = Tick t <$> rewriteAltBody e

rewriteAltBody e@(Type _) = return e

rewriteAltBody e@(Coercion _) = return e


--------------------------------------------------------------------------------
{-
toFold :: Var -> Expr Var -> RewriteM (Expr Var)
toFold f body = do
    message $ "Starting with: " ++ dump body
    toFold' f (Var f) id body
-}


--------------------------------------------------------------------------------
{-
toFold' :: Var
        -> Expr Var
        -> (Expr Var -> Expr Var)
        -> Expr Var
        -> RewriteM (Expr Var)
toFold' f ef mkF (Lam x body) =
    toFoldOver f (\t -> App ef (Var t)) (\e -> mkF (Lam x e)) x body <|>
    toFold' f (App ef (Var x)) (\e -> mkF (Lam x e)) body
toFold' _ _  _   _            = fail "No top-level Lam"
-}


--------------------------------------------------------------------------------
{-
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
-}


--------------------------------------------------------------------------------
altLhs :: Var -> Alt Var -> Fold (Expr Var)
altLhs x (ac, bs, _) = case ac of
    LitAlt l   -> return (Lit l)
    DEFAULT    -> return (Var x)
    DataAlt dc -> do
        xTyArgs <- liftRewriteM $ liftMaybe "Destructed Var is no TyCon..." $ do
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
{-
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
-}


--------------------------------------------------------------------------------
-- | We don't actually do any scoping, we just have a list of vars which can't
-- appear anymore.
assertWellScoped :: [Var] -> Expr Var -> Fold ()
assertWellScoped vars body = case find (`VarSet.elemVarSet` varSet) vars of
    Just v -> fail $ "Not well-scoped: " ++ dump v ++ " still appears"
    _      -> return ()
  where
    varSet = CoreFVs.exprFreeVars body
