--------------------------------------------------------------------------------
module WhatMorphism
    ( plugin
    ) where


--------------------------------------------------------------------------------
import           Data.List         (intersperse)
import           GhcPlugins


--------------------------------------------------------------------------------
import           WhatMorphism.Pass


--------------------------------------------------------------------------------
plugin :: Plugin
plugin = defaultPlugin
    { installCoreToDos = installWhatMorphism
    }


--------------------------------------------------------------------------------
installWhatMorphism :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installWhatMorphism _args todos = do
    reinitializeGlobals
    return $ intersperse passTodo todos
  where
    pass     = bindsOnlyPass whatMorphismPass
    passTodo = CoreDoPasses [CoreDoPluginPass "WhatMorphism" pass]
