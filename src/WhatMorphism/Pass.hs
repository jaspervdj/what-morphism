--------------------------------------------------------------------------------
module WhatMorphism.Pass
    ( whatMorphismPass
    ) where


--------------------------------------------------------------------------------
import           GhcPlugins


--------------------------------------------------------------------------------
whatMorphismPass :: [CoreBind] -> CoreM [CoreBind]
whatMorphismPass binds = do
    tracePretty binds
    return binds


--------------------------------------------------------------------------------
tracePretty :: Outputable a => a -> CoreM ()
tracePretty x = do
    dflags <- getDynFlags
    putMsgS $ "WhatMorphism: " ++ showSDoc dflags (ppr x)


--------------------------------------------------------------------------------
-- 1. Descend into the bind, which should be a lambda. We need to figure out it
-- arguments, one of these might be 'destructed'.
--
-- 2. If in the body of the lambda we find a case statement which destructs any
-- of these arguments, we have a good hint.
--
-- 3. In this destruction we get a number of binds. Recursive calls can only be
-- made on these binds.
