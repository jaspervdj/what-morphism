--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module WhatMorphism.Pass
    ( whatMorphismPass
    ) where


--------------------------------------------------------------------------------
import           Control.Monad              (foldM, forM_, liftM2)
import           CoreMonad
import           CoreSyn
import           Data.List                  (findIndex)
import           Data.Maybe                 (maybeToList)
import           Data.Monoid                (mappend, mconcat, mempty)
import qualified Data.Set                   as S
import           Literal
import           Outputable
import           Type                       (Type)
import qualified Type                       as Type
import           Var
import qualified Var                        as Var


--------------------------------------------------------------------------------
import           WhatMorphism.DirectedGraph (DirectedGraph)
import qualified WhatMorphism.DirectedGraph as DG
import           WhatMorphism.Expr
import           WhatMorphism.Function
import           WhatMorphism.RewriteM


--------------------------------------------------------------------------------
whatMorphismPass :: [CoreBind] -> CoreM [CoreBind]
whatMorphismPass binds = do
    mapM_ whatMorphism binds
    return binds


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
        message $ "Function: " .++. pretty f
        message $ "Body: " .++. pretty e
        _ <- runRewriteM $ rewrite f e
        return ()

        message $ ""

        {-
        message $ "Destructed: " .++. pretty (destruction bs)

        message ""
        message "SUBEXPRESSIONS"
        forM_ (subExprs e) $ message . pretty
        message ""
        -}


--------------------------------------------------------------------------------
destruction :: CoreBind -> [Int]
destruction topBind =
    [ dIndex
    | (Function name args, body) <- fromBinds topBind
    , (destr, alts)              <- maybeToList $ topLevelCase body
    , dIndex                     <- maybeToList $ findIndex (== destr) args
    ]


--------------------------------------------------------------------------------
rewrite :: Function Var Var -> Expr Var -> RewriteM ()
rewrite func body = do
    let efunc = mapFunction Var func :: Function (Expr Var) (Expr Var)
    (destr, alts) <- liftMaybe $ topLevelCase body
    dIdx          <- liftMaybe $ findIndex (== destr) $ functionArgs func

    liftCoreM $ forM_ alts $ \(ac, bnds, expr) -> do
        message $ "AltCon: " .++. pretty ac
        let step :: Expr Var -> Var -> CoreM (Expr Var)
            step e b
                | Var.varType b `Type.eqType` Var.varType destr = do
                    message $ "Searching: " .++. pretty b
                    let needle = replaceArg dIdx (Var b) efunc
                    message $ "Replacing: " .++. pretty needle
                    mkLambda (Var.varType b) (toAppExpr needle) e
                    -- mkLambda (Var.varType b) (Var b) e
                | otherwise = return e

        expr' <- foldM step expr bnds

        message $ "Rewritten: " .++. pretty expr'

        message $ ""

        return expr'


--------------------------------------------------------------------------------
-- | Destructs a top-level case. The case must be applied to a variable, not an
-- expression. We might want to extend this later.
--
-- TODO: The second parameter of the Case is a variable which we bind the expr
-- to, this variable should not be used in the case of a katamorphism, we need
-- to check that, etc...
topLevelCase :: Expr Var -> Maybe (Var, [Alt Var])
topLevelCase (Case (Var b) _ _ alts) = Just (b, alts)
topLevelCase _                       = Nothing


--------------------------------------------------------------------------------
-- | Tries to float a recursive call.
--
--
floatRecursion :: Int -> Function Var Var -> Var -> Var -> Expr Var -> Expr Var
floatRecursion dindex def subterm fresh = everywhere $ \e ->
    e


{-
--------------------------------------------------------------------------------
whatMorphismRec :: CoreBndr -> Expr CoreBndr -> CoreM ()
whatMorphismRec name expr = do
    message $ "Analyzing " .++. pretty name
    message $ pretty $ Rec [(name, expr)]

    message $ "Graph:"
    messageGraph $ fromBindVar name expr

    message ""
  where
    (args, body) = arguments expr
-}


--------------------------------------------------------------------------------
-- | Collect all the simple calls to a certain function
calls :: Var -> Expr Var -> [[Expr Var]]
calls fname = undefined
  where
    {-
    go   (Var _)   = []
    go   (Lit _)   = []
    go a@(App f a) = maybeToList (args a) ++ go f ++ go a
    go   (Lam _ e) = go e
    go   (Let b e) =
    -}

    args (App (Var f) a)
        | f == fname = Just [a]
        | otherwise  = Nothing
    args (App f@(App _ _) a) = case args f of
        Nothing -> Nothing
        Just as -> Just $ a : as
    args _ = Nothing


--------------------------------------------------------------------------------
data Trace = Trace {unTrace :: CoreM String}


--------------------------------------------------------------------------------
class ToTrace t where
    toTrace :: t -> Trace


--------------------------------------------------------------------------------
instance ToTrace Trace where
    toTrace = id


--------------------------------------------------------------------------------
instance ToTrace String where
    toTrace = Trace . return


--------------------------------------------------------------------------------
instance ToTrace (CoreM String) where
    toTrace = Trace


--------------------------------------------------------------------------------
message :: ToTrace a => a -> CoreM ()
message x = unTrace (toTrace x) >>= putMsgS


--------------------------------------------------------------------------------
(.++.) :: (ToTrace a, ToTrace b) => a -> b -> Trace
x .++. y = Trace $ liftM2 (++) (unTrace $ toTrace x) (unTrace $ toTrace y)


--------------------------------------------------------------------------------
pretty :: Outputable a => a -> CoreM String
pretty x = do
    dflags <- getDynFlags
    return $ showSDoc dflags $ ppr x


--------------------------------------------------------------------------------
messageGraph :: (Ord a, Outputable a) => DirectedGraph a -> CoreM ()
messageGraph dg = mapM_ prettyPrint' $ S.toList (DG.nodes dg)
  where
    prettyPrint' x =
        let nb = DG.neighbours x dg
        in message $ pretty x .++. " -> " .++. pretty (S.toList nb)
