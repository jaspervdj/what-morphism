--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module WhatMorphism.Pass
    ( whatMorphismPass
    ) where


--------------------------------------------------------------------------------
import           Control.Monad (liftM2)
import           CoreMonad
import           CoreSyn
import           Outputable


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
    message $ "Args: " .++. pretty args
    message $ "Body: " .++. pretty body
    return ()
  where
    (args, body) = arguments expr


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
