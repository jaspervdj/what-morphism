--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module WhatMorphism.Pass
    ( whatMorphismPass
    ) where


--------------------------------------------------------------------------------
import           Control.Monad              (liftM2)
import           CoreMonad
import           CoreSyn
import           Data.Monoid                (mappend, mconcat, mempty)
import qualified Data.Set                   as S
import           Outputable
import           Var


--------------------------------------------------------------------------------
import           WhatMorphism.DirectedGraph (DirectedGraph)
import qualified WhatMorphism.DirectedGraph as DG


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
whatMorphism (NonRec _ _) = return ()
whatMorphism (Rec recs)   = mapM_ (uncurry whatMorphismRec) recs


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


--------------------------------------------------------------------------------
fromExpr :: Expr Var -> DirectedGraph Var
fromExpr (Var x)         = DG.fromNode x
fromExpr (Lit _)         = mempty
fromExpr (App e a)       = fromExpr e `mappend` fromExpr a
fromExpr (Lam x e)       = DG.fromNode x `mappend` fromExpr e  -- Special node?
fromExpr (Let b e)       = fromBind b `mappend` fromExpr e
fromExpr (Case e b _ as) =
    let fromAlt (_, bs, e') =
            mconcat [DG.fromEdge b' b | b' <- bs] `mappend`
            fromExpr e'
    in fromBindVar b e `mappend` mconcat [fromAlt a | a <- as]
fromExpr (Cast e _)      = fromExpr e
fromExpr (Tick _ e)      = fromExpr e
fromExpr (Type _)        = mempty
fromExpr (Coercion _)    = mempty


--------------------------------------------------------------------------------
fromBind :: Bind Var -> DirectedGraph Var
fromBind b = case b of
    NonRec x e -> fromBindVar x e
    Rec bs     -> mconcat $ map (uncurry fromBindVar) bs


--------------------------------------------------------------------------------
fromBindVar :: Var -> Expr Var -> DirectedGraph Var
fromBindVar x e =
    let dg = fromExpr e
    in DG.fromEdges x (DG.nodes dg) `mappend` dg


--------------------------------------------------------------------------------
-- | Collects the arguments to a function
arguments :: Expr CoreBndr -> ([CoreBndr], Expr CoreBndr)
arguments = go []
  where
    go xs (Lam x e) = go (x : xs) e
    go xs e         = (reverse xs, e)


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
