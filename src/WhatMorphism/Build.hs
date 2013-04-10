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
    -- Run to detect data constructors
    let rTyp = guessFunctionReturnType (Var.varType f)
    liftCoreM $ Outputable.pprTrace "rTyp" (Type.pprType rTyp) $ return ()
    conses        <- getDataCons rTyp

    -- Create a worker function 'g'. The type of 'g' is like the fixed type of
    -- 'f', but with a more general return type (TODO).
    let gTyp = snd $ Type.splitForAllTys (Var.varType f)
    g <- freshVar "g" gTyp
    let (fTyBinders, fValBinders, body') = CoreSyn.collectTyAndValBinders body
    newArgs <- forM fValBinders $ \arg -> freshVar "fArg" (Var.varType arg)

    -- Get the build function, if available
    build <- registeredBuild rTyp
    liftCoreM $ Outputable.pprTrace "builType" (Type.pprType (Var.varType build)) $ return ()
    (buildTyArgs, bTy, lamTy) <- liftMaybe "Build has no Fun Forall type" $ do
        let (buildTyArgs, buildTy) = Type.splitForAllTys (Var.varType build)
        (argTy, _)             <- Type.splitFunTy_maybe buildTy
        (bTy, lamTy)           <- Type.splitForAllTy_maybe argTy
        return (buildTyArgs, bTy, lamTy)

    -- The types for the arguments of the lambda MUST EXACTLY MATCH the
    -- different constructors of the datatype. This is EXTREMELY IMPORTANT.
    -- However, we BLATANTLY DISREGARD checking this.
    let (consTys, _) = Type.splitFunTys lamTy
    lamArgs <- forM consTys (freshVar "cons")
    let replacements = zip conses lamArgs

    -- TODO: This run is not needed! But useful for now... in some way or
    -- another.
    _ <- runReaderT (replace body) (BuildRead f g replacements)

    let body'' =
            MkCore.mkCoreLams (fTyBinders ++ newArgs)
                (MkCore.mkCoreLet
                    (Rec [(g, MkCore.mkCoreLams fValBinders body')])
                    (MkCore.mkCoreApps (Var g) (map Var newArgs)))

    -- TODO: Figure out replacements
    -- TODO: Second run to replace them
    -- message $ "DataCons: " ++ dump dataCons
    message $ "Conses: " ++ dump conses
    return body''


--------------------------------------------------------------------------------
data BuildRead = BuildRead
    { buildVar            :: Var
    , buildVarReplacement :: Var
    , buildReplacements   :: [(DataCon, Var)]
    }


--------------------------------------------------------------------------------
-- | After we examine a function, we get two things:
--
-- - A list of used DataCon's
-- - A function which allows replacement of these DataCon's by other expressions
type Build a = ReaderT BuildRead RewriteM a


--------------------------------------------------------------------------------
replacementForDataCon :: Var -> DataCon -> Build Var
replacementForDataCon original dataCon = do
    replacements <- buildReplacements <$> ask
    case lookup dataCon replacements of
        Just v  -> return v
        Nothing -> do
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
replace e@(App _ _) = do
    (e', _) <- recursionOrReplaceDataCon e
    return e'

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
-- | TODO: We generally want to search for a DataCon OR recursion to our
-- function (needs to be added in Reader).
--
-- Also returns a list of recursiveIndices so we know what other places to
-- check...
recursionOrReplaceDataCon :: Expr Var -> Build (Expr Var, [Bool])
recursionOrReplaceDataCon e = do
    recursionVar <- buildVar <$> ask
    case e of
        -- It seems like GHC sometimes generates weird code like this. If
        -- needed, this can be made more general by remembering the number of
        -- 'Lam's we can skip.
        (App (Lam x e') a) -> do
            (e'', ris) <- recursionOrReplaceDataCon e'
            return (App (Lam x e'') a, ris)
        (App e' a)         -> do
            (e'', ris) <- recursionOrReplaceDataCon e'
            case ris of
                []           -> return (App e'' a, ris)
                (rec : ris') -> do
                    liftRewriteM $ message $ "Checking App: " ++ show ris
                    (a', _) <- if rec
                        then recursionOrReplaceDataCon a
                        else return (a, [])
                    return (App e'' a', ris')
        (Var var)
            | var .==. recursionVar -> do
                liftRewriteM $ message $ "Recursion found, OK"
                return (e, [])
            | Var.isId var          -> case Var.idDetails var of
                IdInfo.DataConWorkId dc -> do
                    replacement <- replacementForDataCon var dc
                    let ris = recursiveIndices dc
                    liftRewriteM $ message $ "Recursive indices for " ++
                        (dump dc) ++ ": " ++ show ris
                    return (Var replacement, recursiveIndices dc)
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
