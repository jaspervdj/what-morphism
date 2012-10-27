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
