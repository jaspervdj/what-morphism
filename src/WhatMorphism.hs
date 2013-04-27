--------------------------------------------------------------------------------
module WhatMorphism
    ( plugin
    ) where


--------------------------------------------------------------------------------
import qualified CoreMonad             as CoreMonad
import           Data.List             (intersperse)
import           GhcPlugins
import qualified Serialized            as Serialized


--------------------------------------------------------------------------------
import           WhatMorphism.Fusion
import           WhatMorphism.Inliner
import           WhatMorphism.Pass
import           WhatMorphism.RewriteM


--------------------------------------------------------------------------------
plugin :: Plugin
plugin = defaultPlugin
    { installCoreToDos = installWhatMorphism
    }


--------------------------------------------------------------------------------
installWhatMorphism :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installWhatMorphism _args todos = do
    reinitializeGlobals
    inliner <- CoreMonad.liftIO newInlinerState
    return $ intersperse
        (CoreDoPasses
            [ CoreDoPluginPass "WhatMorphism"
                (runRewritePass whatMorphismPass inliner)
            , CoreDoPluginPass "WhatMorphism.Inliner" (inline inliner)
            -- , CoreDoPluginPass "WhatMorphism.Fusion"
                -- (runRewritePass fusePass inliner)
            ] )
        todos
  where
    runRewritePass pass inliner mg = do
        register  <- CoreMonad.getFirstAnnotations
            Serialized.deserializeWithData mg
        mg_binds' <- runRewriteM (pass $ mg_binds mg)
            (mkRewriteRead mg register inliner)
        case mg_binds' of
            Left err     -> do
                CoreMonad.putMsgS $ "Pass failed: " ++ err
                return mg
            Right binds' -> do
                CoreMonad.putMsgS "Pass okay!"
                return mg {mg_binds = binds'}

    inline inliner = bindsOnlyPass (CoreMonad.liftIO . inlinerPass inliner)
