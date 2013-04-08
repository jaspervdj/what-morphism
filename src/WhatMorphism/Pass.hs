--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module WhatMorphism.Pass
    ( whatMorphismPass
    ) where


--------------------------------------------------------------------------------
import           Control.Monad         (foldM, forM_)
import           Control.Monad.Error   (catchError, throwError)
import           CoreMonad             (CoreM)
import qualified CoreMonad             as CoreM
import           CoreSyn
import           Data.List             (findIndex)
import           Data.Maybe            (fromMaybe, listToMaybe)
import qualified Module                as Module
import           Name                  (Name)
import qualified Name                  as Name
import qualified OccName               as OccName
import           Outputable
import           Type                  (Type)
import qualified Type                  as Type
import           Var


--------------------------------------------------------------------------------
import           WhatMorphism.Build
import           WhatMorphism.Dump
import           WhatMorphism.Expr
import           WhatMorphism.Function
import           WhatMorphism.Fusion
import           WhatMorphism.Pattern
import           WhatMorphism.RewriteM


--------------------------------------------------------------------------------
whatMorphismPass :: [CoreBind] -> RewriteM [CoreBind]
whatMorphismPass = mapM whatMorphism


--------------------------------------------------------------------------------
whatMorphism :: CoreBind -> RewriteM CoreBind
whatMorphism coreBind = do
    coreBind' <- withBinds coreBind $ \f e -> do
        reg <- isRegisteredFoldOrBuild f
        if reg
            then return e
            else do
                message $ "====== toBuild: " ++ dump f
                catchError (toBuild f e) $ \err -> do
                    message $ "====== Error: " ++ err
                    return e

    coreBind'' <- withBinds coreBind' $ \f e -> do
        reg <- isRegisteredFoldOrBuild f
        if reg
            then return e
            else do
                message $ "====== toFold: " ++ dump f
                catchError (toFold f e) $ \err -> do
                    message $ "====== Error: " ++ err
                    return e

    return coreBind''


--------------------------------------------------------------------------------
data RewriteResult
    = NoFold
    | ListFold
    | DataFold
    | HeadFold
    deriving (Show)


--------------------------------------------------------------------------------
rewrite :: Function Var Var -> Expr Var -> RewriteM RewriteResult
rewrite func body = do
    let efunc = mapFunction Var func :: Function (Expr Var) (Expr Var)
    (destr, cTyp, alts) <- liftMaybe "topLevelCase" $ topLevelCase body
    dIdx                <- liftMaybe "findIndex" $
        findIndex (== destr) $ functionArgs func

    -- Recs is the number of recursive calls replaced...
    (alts', recs) <- foldM' alts ([], 0) $ \(as, recs) (ac, bnds, expr) -> do
        message $ "AltCon: " ++ dump ac
        let step :: (Expr Var, Int) -> Var -> RewriteM (Expr Var, Int)
            step (e, nr) b
                -- The same type should be destroyed in the same way for now
                | Var.varType b `Type.eqType` Var.varType destr = do
                    let needle = replaceArg dIdx (Var b) efunc
                        expr'  = toAppExpr needle
                    lam <- liftCoreM $ mkLambda cTyp expr' e
                    if count (Var b) lam > 0
                        then throwError $
                            (dump b) ++ " still appears in body: " ++ dump lam
                        else return (lam, nr + 1)

                -- Otherwise we can just create a lambda expression
                | otherwise = do
                    lam <- liftCoreM $ mkLambda (Var.varType b) (Var b) e
                    return (lam, nr)

        (expr', recs') <- foldM step (expr, 0) $ reverse bnds

        pretty expr' >>= \e -> message $ "Rewritten: " ++ e

        return ((ac, binds, expr') : as, recs + recs')

    return $ if recs <= 0
        then HeadFold
        else if (any isListConstructor [ac | (ac, _, _) <- alts'])
            then ListFold
            else DataFold
  where
    foldM' ls x f = foldM f x ls


--------------------------------------------------------------------------------
isListConstructor :: AltCon -> Bool
isListConstructor ac = fromMaybe False $ do
    dc <- dataAlt ac
    return $ Name.occNameString (Name.getOccName dc) `elem` [":", "[]"]
  where
    dataAlt (DataAlt c) = Just c
    dataAlt _           = Nothing


--------------------------------------------------------------------------------
-- | Destructs a top-level case. The case must be applied to a variable, not an
-- expression. We might want to extend this later.
--
-- TODO: The second parameter of the Case is a variable which we bind the expr
-- to, this variable should not be used in the case of a katamorphism, we need
-- to check that, etc...
topLevelCase :: Expr Var -> Maybe (Var, Type, [Alt Var])
topLevelCase (Case (Var b) _ t alts) = Just (b, t, alts)
topLevelCase _                       = Nothing


--------------------------------------------------------------------------------
pretty :: Outputable a => a -> RewriteM String
pretty x = do
    dflags <- liftCoreM CoreM.getDynFlags
    return $ showSDoc dflags $ ppr x


--------------------------------------------------------------------------------
findName :: String -> String -> RewriteM Name
findName moduleName name = do
    origNameCache <- liftCoreM $ CoreM.getOrigNameCache
    occEnv        <- liftMaybe ("Module not found: " ++ moduleName) $
        lookupByModuleName moduleName origNameCache
    liftMaybe ("Symbol not found: " ++ name) $
        OccName.lookupOccEnv occEnv (OccName.mkVarOcc name)


--------------------------------------------------------------------------------
lookupByModuleName :: String -> Module.ModuleEnv a -> Maybe a
lookupByModuleName name env = listToMaybe
    [ x
    | (m, x) <- Module.moduleEnvToList env
    , Module.moduleNameString (Module.moduleName m) == name
    ]
