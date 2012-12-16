--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module WhatMorphism.Pass
    ( whatMorphismPass
    ) where


--------------------------------------------------------------------------------
import           Control.Monad         (foldM, forM_)
import           Control.Monad.Error   (catchError, throwError)
import           CoreMonad
import           CoreSyn
import           Data.List             (findIndex)
import           Data.Maybe            (fromMaybe)
import qualified Name                  as Name
import           Outputable
import           Type                  (Type)
import qualified Type                  as Type
import           Var


--------------------------------------------------------------------------------
import           WhatMorphism.Dump
import           WhatMorphism.Expr
import           WhatMorphism.Function
import           WhatMorphism.RewriteM


--------------------------------------------------------------------------------
whatMorphismPass :: [CoreBind] -> CoreM [CoreBind]
whatMorphismPass binds' = do
    mapM_ whatMorphism binds'
    return binds'


--------------------------------------------------------------------------------
-- 1. Descend into the bind, which should be a lambda. We need to figure out it
-- arguments, one of these might be 'destructed'.
--
-- 2. If in the body of the lambda we find a case statement which destructs any
-- of these arguments, we have a good hint.
--
-- 3. In this destruction we get a number of binds. Recursive calls can only be
-- made on these binds.


--------------------------------------------------------------------------------
whatMorphism :: CoreBind -> CoreM ()
whatMorphism bs = do
    forM_ (fromBinds bs) $ \(f, e) -> do
        res <- runRewriteM $ catchError (rewrite f e) (const $ return NoFold)
        _   <- runRewriteM $ case res of
            Left err -> message $ "Error: " ++ err
            Right x  -> do
                name <- pretty (functionTerm f)
                message $ "RewriteResult: " ++ name ++ " " ++ show x
        return ()

    return ()


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
message :: String -> RewriteM ()
message = liftCoreM . putMsgS


--------------------------------------------------------------------------------
pretty :: Outputable a => a -> RewriteM String
pretty x = do
    dflags <- liftCoreM getDynFlags
    return $ showSDoc dflags $ ppr x
