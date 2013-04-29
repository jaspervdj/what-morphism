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
import           WhatMorphism.Build
import           WhatMorphism.Fold
import           WhatMorphism.Fusion
import           WhatMorphism.Inliner
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
            [ CoreDoPluginPass "WhatMorphism.Build"
                (runRewritePass buildPass inliner)
            , CoreDoPluginPass "WhatMorphism.Fold"
                (runRewritePass foldPass inliner)
            , CoreDoPluginPass "WhatMorphism.Inliner" (inline inliner)
            , CoreDoPluginPass "WhatMorphism.Fusion"
                (runRewritePass fusePass inliner)
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
