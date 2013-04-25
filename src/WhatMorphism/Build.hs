--------------------------------------------------------------------------------
-- | Try to detect build-like expressions. As a reminder,
--
-- > build :: (forall b. (a -> b -> b) -> b -> b) -> a
-- > build g = g (:) []
--
-- The forall parameter is of huge importance, since otherwise we can just
-- construct lists any way we like and ignore the passed-in constructors.
module WhatMorphism.Build
    ( toBuild
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   (pure, (<$>), (<*>))
import           Control.Monad         (forM)
import           Control.Monad.Reader  (ReaderT, ask, runReaderT)
import           Control.Monad.Trans   (lift)
import           CoreSyn               (Bind (..), Expr (..))
import qualified CoreSyn               as CoreSyn
import           DataCon               (DataCon)
import qualified DataCon               as DataCon
import qualified IdInfo                as IdInfo
import qualified MkCore                as MkCore
import qualified Outputable            as Outputable
import qualified Type                  as Type
import           Var                   (Var)
import qualified Var                   as Var


--------------------------------------------------------------------------------
import           WhatMorphism.Dump
import           WhatMorphism.Expr
import           WhatMorphism.RewriteM
import           WhatMorphism.SynEq


--------------------------------------------------------------------------------
toBuild :: Var -> Expr Var -> RewriteM (Expr Var)
toBuild f body = do
    -- We need some info on our type, e.g. 'List'. We find this type by guessing
    -- the return type of our function (real professionalism here).
    let fTy     = Var.varType f
        rTy     = guessFunctionReturnType fTy
        rTyArgs = case Type.splitTyConApp_maybe rTy of
                    Just (_, tyArgs) -> tyArgs
                    Nothing          -> []
    liftCoreM $ Outputable.pprTrace "rTy" (Type.pprType rTy) $ return ()
    conses <- liftEither $ getDataCons rTy

    -- Get the build function, if available
    build <- registeredBuild rTy
    lamTy <- liftMaybe "Build has no Fun Forall type" $ do
        let (_, buildTy) = Type.splitForAllTys (Var.varType build)
        (argTy, _) <- Type.splitFunTy_maybe buildTy
        (_, lamTy) <- Type.splitForAllTy_maybe argTy
        return lamTy
    bTy <- liftCoreM $ freshTyVar "bbb"

    -- Create a worker function 'g'. The type of 'g' is like the fixed type of
    -- 'f', but with a more general return type.
    --
    -- TODO: This return type is just our new 'b', right? Right? Guys?
    let gTyArgs = fst $ Type.splitFunTys $ snd $ Type.splitForAllTys fTy
        gTy     = Type.mkFunTys gTyArgs (Type.mkTyVarTy bTy)
    g <- liftCoreM $ freshVar "g" gTy
    let (fTyBinders, fValBinders, body') = CoreSyn.collectTyAndValBinders body
    newArgs <- liftCoreM $
        forM fValBinders $ \arg -> freshVar "fArg" (Var.varType arg)

    -- The types for the arguments of the lambda MUST EXACTLY MATCH the
    -- different constructors of the datatype. This is EXTREMELY IMPORTANT.
    -- However, we BLATANTLY DISREGARD checking this. #yolo
    let (consTys, _) = Type.splitFunTys lamTy
    lamArgs <- liftCoreM $ forM consTys (freshVar "cons")
    let replacements = zip conses lamArgs

    -- TODO: This run is not needed! But useful for now... in some way or
    -- another.
    body'' <- runReaderT (replace body') (BuildRead f g replacements)
    return $
        MkCore.mkCoreLams (fTyBinders ++ newArgs)
            (App
                (MkCore.mkCoreApps (Var build) (map Type rTyArgs))
                (MkCore.mkCoreLams (bTy : lamArgs)
                    (Let
                        (Rec [(g, MkCore.mkCoreLams fValBinders body'')])
                        (MkCore.mkCoreApps (Var g) (map Var newArgs)))))


--------------------------------------------------------------------------------
data BuildRead = BuildRead
    { buildVar            :: Var
    , buildVarReplacement :: Var
    , buildReplacements   :: [(DataCon, Var)]
    }


--------------------------------------------------------------------------------
type Build a = ReaderT BuildRead RewriteM a


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
replace :: Expr Var -> Build (Expr Var)
replace (Var x) = return (Var x)

replace (Lit x) = return (Lit x)

-- Real work here. TODO: replacement?
replace e@(App _ _) = recursionOrReplaceDataCon e

replace (Lam x y) = Lam x <$> replace y

-- TODO: We might want search the let bindings for the DataCon occurences
replace (Let bs e) = Let bs <$> replace e

replace (Case e b t alts) = Case e b t <$> mapM replace' alts
  where
    replace' (ac, bs, ae) = do
        ae' <- replace ae
        return (ac, bs, ae')

replace (Cast e c) = Cast <$> replace e <*> pure c

replace (Tick t e) = Tick t <$> replace e

replace (Type t) = return (Type t)

replace (Coercion c) = return (Coercion c)


--------------------------------------------------------------------------------
-- We generally want to search for a DataCon OR recursion to our function (needs
-- to be added in Reader).
recursionOrReplaceDataCon :: Expr Var -> Build (Expr Var)
recursionOrReplaceDataCon expr = do
    recursionVar <- buildVar <$> ask

    -- In case we replace a constructor, or a (polymorphic) recursive call, we
    -- don't need the type arguments anymore (I think).
    let (app, args) = CoreSyn.collectArgs expr
        numTyArgs   = length $ filter CoreSyn.isTypeArg args

    case app of
        -- It seems like GHC sometimes generates weird code like this. If
        -- needed, this can be made more general by remembering the number of
        -- 'Lam's we can skip.
        (Lam x e) -> do
            e' <- recursionOrReplaceDataCon e
            return $ MkCore.mkCoreApps (Lam x e') args
        (Var var)
            | var .==. recursionVar -> do
                replacement <- buildVarReplacement <$> ask
                liftRewriteM $ message $ "Recursion found, OK"
                return $ MkCore.mkCoreApps (Var replacement) $
                    drop numTyArgs args
            | Var.isId var          -> case Var.idDetails var of
                IdInfo.DataConWorkId dc -> do
                    replacement <- replacementForDataCon var dc
                    let ris = recursiveIndices dc
                    liftRewriteM $ message $ "Recursive indices for " ++
                        (dump dc) ++ ": " ++ show ris
                    -- TODO: Check if at least length of 'args' and 'ris' is
                    -- equal?
                    args' <- forM (zip args ris) $ \(arg, rec) ->
                        if rec
                            then recursionOrReplaceDataCon arg
                            else return arg
                    return $ MkCore.mkCoreApps (Var replacement) $
                        drop numTyArgs args'
                _                       -> fail' $ "No DataCon Id: " ++ dump var
            | otherwise             -> fail' "Unexpected Var"
        _ -> fail' "No App or Var found"
  where
    fail' err = fail $ "WhatMorphism.Build.recursionOrReplaceDataCon: " ++ err


--------------------------------------------------------------------------------
recursiveIndices :: DataCon -> [Bool]
recursiveIndices dc =
    let (tyVars, _, args, ty) = DataCon.dataConSig dc
    in map (const False) tyVars ++ map (Type.eqType ty) args
