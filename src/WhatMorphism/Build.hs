--------------------------------------------------------------------------------
-- | Try to detect build-like expressions. As a reminder,
--
-- > build :: (forall b. (a -> b -> b) -> b -> b) -> a
-- > build g = g (:) []
--
-- The forall parameter is of huge importance, since otherwise we can just
-- construct lists any way we like and ignore the passed-in constructors.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WhatMorphism.Build
    ( buildPass
    , toBuild
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   (pure, (<$>), (<*>))
import           Control.Monad         (forM, unless, when)
import           Control.Monad.Error   (catchError)
import           Control.Monad.Reader  (ask)
import           Control.Monad.RWS     (RWST, runRWST)
import           Control.Monad.State   (modify)
import           Control.Monad.Trans   (lift)
import           Control.Monad.Writer  (tell)
import           CoreSyn               (Bind (..), CoreBind, Expr (..))
import qualified CoreSyn               as CoreSyn
import           Data.Maybe            (isJust)
import           Data.Monoid           (Monoid (..))
import           DataCon               (DataCon)
import qualified DataCon               as DataCon
import qualified MkCore                as MkCore
import qualified Outputable            as Outputable
import           Type                  (Type)
import qualified Type                  as Type
import           Var                   (Var)
import qualified Var                   as Var


--------------------------------------------------------------------------------
import           WhatMorphism.AnyBool
import           WhatMorphism.Dump
import           WhatMorphism.Expr
import           WhatMorphism.RewriteM
import           WhatMorphism.SynEq


--------------------------------------------------------------------------------
buildPass :: [CoreBind] -> RewriteM [CoreBind]
buildPass = mapM buildPass'
  where
    buildPass' = withBindsEverywhere $ \cb -> withBinds cb $ \f e -> do
        reg <- isRegisteredFoldOrBuild f
        if reg
            then return (f, e)
            else do
                important $ "====== toBuild: " ++ dump f
                flip catchError (report f e) $ do
                    e' <- toBuild f e
                    registerForInlining f e'
                    return (setInlineInfo f, e')
    report f e err = do
        message $ "====== Error: " ++ err
        return (f, e)


--------------------------------------------------------------------------------
data Feature = RecBuild | NestedBuild deriving (Show)


--------------------------------------------------------------------------------
toBuild :: Var -> Expr Var -> RewriteM (Expr Var)
toBuild f body = do
    -- We need some info on our type, e.g. 'List'. We find this type by guessing
    -- the return type of our function (real professionalism here).
    let fTy = Var.varType f
        rTy = guessFunctionReturnType fTy
        (fTyBinders, fValBinders, body') = CoreSyn.collectTyAndValBinders body
    (rTyCon, _rTyArgs) <- liftMaybe "Build has no TyCon" $
        Type.splitTyConApp_maybe rTy
    liftCoreM $ Outputable.pprTrace "rTy" (Type.pprType rTy) $ return ()
    -- liftCoreM $ Outputable.pprTrace "rTy" (Outputable.ppr rTy) $ return ()
    conses <- liftEither $ getDataCons rTy

    -- If it's not a recursive type, e.g. a tuple, a Maybe... there's not much
    -- to do here
    unless (or $ conses >>= recursiveIndices) $
        fail $ "Build: Not a recursive type " ++ dump rTyCon

    -- TODO: This run is not needed! But useful for now... in some way or
    -- another.
    maybeBuild <- catchError (Just <$> registeredBuild rTy)
        (\_ -> return Nothing)
    (_, state, write) <- runRWST (replace [] body')
        (BuildRead maybeBuild f f rTy []) emptyBuildState
    dataConTyArgs <- liftMaybe "Build: No DataCon TyArgs!" $
        buildDataConTyArgs state

    -- At this point we can do our reporting.
    module' <- rewriteModule
    let features =
            [RecBuild    | unAnyBool (buildRec write)] ++
            [NestedBuild | unAnyBool (buildNested write)]
    important $ "WhatMorphismResult: Build: " ++
        dump module' ++ "." ++ dump f ++ " " ++ dump rTyCon ++ " " ++
        show features
    detect <- isDetectMode
    when detect $ fail "Not changing build"

    -- Get the build function, if available
    build <- registeredBuild rTy
    (lamTy, consForAllTys) <- liftMaybe "Build has no Fun Forall type" $ do
        let (consForAllTys, buildTy) = Type.splitForAllTys (Var.varType build)
        (argTy, _) <- Type.splitFunTy_maybe buildTy
        (_, lamTy) <- Type.splitForAllTy_maybe argTy
        return (lamTy, consForAllTys)
    let (_, lamReTy) = Type.splitFunTys lamTy
    lamReTyVar <- liftMaybe "lamRe is no TyVar" $ Type.getTyVar_maybe lamReTy

    -- Create a worker function 'g'. The type of 'g' is like the fixed type of
    -- 'f', but with a more general return type.
    --
    -- TODO: This return type is just our new 'b', right? Right? Guys?
    let (_, fArgs, _) = CoreSyn.collectTyAndValBinders body
        gTy           = Type.mkFunTys (map Var.varType fArgs) lamReTy
        -- gTyArgs = fst $ Type.splitFunTys $ snd $ Type.splitForAllTys fTy
        -- gTy     = Type.mkFunTys gTyArgs lamReTy
    g <- liftCoreM $ freshVar "g" gTy
    newArgs <- liftCoreM $
        forM fValBinders $ \arg -> freshVar "fArg" (Var.varType arg)

    -- The types for the arguments of the lambda MUST EXACTLY MATCH the
    -- different constructors of the datatype. This is EXTREMELY IMPORTANT.
    -- However, we BLATANTLY DISREGARD checking this. #yolo
    let (consTys, _) = Type.splitFunTys lamTy
    let env = zip consForAllTys dataConTyArgs
    lamArgs <- liftCoreM $ forM consTys (freshVar "cons" . substTy env)
    let replacements' = zip conses lamArgs

    -- Second run go go go. More precise types are now available.
    (body'', _, _) <- runRWST (replace [] body')
        (BuildRead (Just build) f g lamReTy replacements') emptyBuildState

    important $ "WhatMorphismResult: Build using: " ++ dump build
    return $
        MkCore.mkCoreLams (fTyBinders ++ newArgs)
            (App
                (MkCore.mkCoreApps
                    (Var build)
                    (map Type dataConTyArgs))
                (MkCore.mkCoreLams (lamReTyVar : lamArgs)
                    (Let
                        (Rec [(g, MkCore.mkCoreLams fValBinders body'')])
                        (MkCore.mkCoreApps (Var g) (map Var newArgs)))))


--------------------------------------------------------------------------------
data BuildRead = BuildRead
    { buildBuild               :: Maybe Var  -- Can be unknown (in detect mode)
    , buildVar                 :: Var
    , buildVarReplacement      :: Var
    -- , buildResultTy            :: Type
    , buildReplacementResultTy :: Type  -- Type of 'b'
    , buildReplacements        :: [(DataCon, Var)]
    }


--------------------------------------------------------------------------------
data BuildWrite = BuildWrite
    { buildRec    :: AnyBool
    , buildNested :: AnyBool
    }


--------------------------------------------------------------------------------
instance Monoid BuildWrite where
    mempty                                      = BuildWrite mempty mempty
    BuildWrite r1 n1 `mappend` BuildWrite r2 n2 =
        BuildWrite (mappend r1 r2) (mappend n1 n2)


--------------------------------------------------------------------------------
newtype BuildState = BuildState
    { buildDataConTyArgs :: Maybe [Type]
    }


--------------------------------------------------------------------------------
emptyBuildState :: BuildState
emptyBuildState = BuildState Nothing


--------------------------------------------------------------------------------
type Build a = RWST BuildRead BuildWrite BuildState RewriteM a


--------------------------------------------------------------------------------
replacementForDataCon :: Var -> DataCon -> Build Var
replacementForDataCon original dataCon = do
    replacements <- buildReplacements <$> ask
    case lookup dataCon replacements of
        Just v  -> return v
        Nothing -> do
            -- TODO: Shouldn't we fail here?
            liftRewriteM $ message $
                "WhatMorphism.Build.replacementForDataCon: " ++
                "No replacement for " ++ dump dataCon ++ ", using original"
            return original


--------------------------------------------------------------------------------
liftRewriteM :: RewriteM a -> Build a
liftRewriteM = lift


--------------------------------------------------------------------------------
replace :: [(Var, Expr Var)] -> Expr Var -> Build (Expr Var)
replace env e@(Var _) = recursionOrReplaceDataCon env e

replace _ (Lit x) = return (Lit x)

-- Real work here. TODO: replacement?
replace env e@(App _ _) = recursionOrReplaceDataCon env e

replace env (Lam x y) = Lam x <$> replace env y

-- TODO: We might want search the let bindings for the DataCon occurences
replace env (Let (NonRec v b) e) = Let (NonRec v b) <$> replace ((v, b) : env) e
replace env (Let bs e) = Let bs <$> replace env e

replace env (Case e b _t alts) = do
    -- rt    <- buildResultTy            <$> ask  -- Maybe we can just always
    --                                            -- replace this...
    t'    <- buildReplacementResultTy <$> ask
    alts' <- mapM replace' alts
    -- return $ Case e b (if t `Type.eqType` rt then t' else t) alts'
    return $ Case e b t' alts'
  where
    replace' (ac, bs, ae) = do
        ae' <- replace env ae
        return (ac, bs, ae')

replace env (Cast e c) = Cast <$> replace env e <*> pure c

replace env (Tick t e) = Tick t <$> replace env e

replace _ (Type t) = return (Type t)

replace _ (Coercion c) = return (Coercion c)


--------------------------------------------------------------------------------
-- We generally want to search for a DataCon OR recursion to our function (needs
-- to be added in Reader).
recursionOrReplaceDataCon :: [(Var, Expr Var)] -> Expr Var -> Build (Expr Var)
recursionOrReplaceDataCon env expr = do
    build        <- buildBuild <$> ask
    recursionVar <- buildVar   <$> ask
    resultTy     <- buildReplacementResultTy <$> ask

    -- In case we replace a constructor, or a (polymorphic) recursive call, we
    -- don't need the type arguments anymore (I think).
    let (app, args) = CoreSyn.collectArgs expr
        numTyArgs   = length $ filter CoreSyn.isTypeArg args
        tyArgs      = [t | Type t <- take numTyArgs args]

    case app of
        -- It seems like GHC sometimes generates weird code like this. If
        -- needed, this can be made more general by remembering the number of
        -- 'Lam's we can skip.
        (Lam x e) -> do
            e' <- recursionOrReplaceDataCon env e
            return $ MkCore.mkCoreApps (Lam x e') args
        (Var var)
            | var .==. recursionVar  -> do
                replacement <- buildVarReplacement <$> ask
                liftRewriteM $ message $ "Recursion found, OK"
                tell mempty {buildRec = AnyBool True}
                return $ MkCore.mkCoreApps (Var replacement) $
                    drop numTyArgs args
            | Just var .==. build -> do
                tell mempty {buildNested = AnyBool True}
                liftRewriteM $ message $ "Nested build, OK"
                g <- case drop numTyArgs args of
                        [g] -> return g
                        _   -> fail "Nested build should have exactly 1 arg (g)"
                ourArgs <- map snd . buildReplacements <$> ask
                return $ MkCore.mkCoreApps g (Type resultTy : map Var ourArgs)
            -- Simple variable substitution
            | null args && inEnv var ->
                let Just expr' = lookup var env
                in recursionOrReplaceDataCon env expr'
            | Var.isId var           -> case idToDataCon var of
                Just dc -> do
                    replacement <- replacementForDataCon var dc
                    let ris = recursiveIndices dc
                    liftRewriteM $ message $ "Recursive indices for " ++
                        (dump dc) ++ ": " ++ show ris
                    -- TODO: Check if at least length of 'args' and 'ris' is
                    -- equal?
                    args' <- forM (zip args ris) $ \(arg, rec) ->
                        if rec
                            then recursionOrReplaceDataCon env arg
                            else return arg
                    modify $ \s -> s {buildDataConTyArgs = Just tyArgs}
                    return $ MkCore.mkCoreApps (Var replacement) $
                        drop numTyArgs args'
                _                       -> fail' $ "No DataCon Id: " ++ dump var
            | otherwise              -> fail' "Unexpected Var"

        -- Another quick hack
        (Let (NonRec v b) e) -> do
            e' <- recursionOrReplaceDataCon ((v, b) : env) e
            return $ (Let (NonRec v b) e')

        _ -> fail' "No App or Var found"
  where
    fail' err = fail $ "WhatMorphism.Build.recursionOrReplaceDataCon: " ++ err
    inEnv e   = isJust (lookup e env)


--------------------------------------------------------------------------------
recursiveIndices :: DataCon -> [Bool]
recursiveIndices dc =
    let (tyVars, _, args, ty) = DataCon.dataConSig dc
    in map (const False) tyVars ++ map (Type.eqType ty) args
