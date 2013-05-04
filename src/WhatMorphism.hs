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
import           WhatMorphism.Types


--------------------------------------------------------------------------------
plugin :: Plugin
plugin = defaultPlugin
    { installCoreToDos = installWhatMorphism
    }


--------------------------------------------------------------------------------
-- | CHANGE ME
whatMorphismMode :: WhatMorphismMode
whatMorphismMode = WhatMorphismFull


--------------------------------------------------------------------------------
-- | CHANGE ME
whatMorphismVerbosity :: WhatMorphismVerbosity
whatMorphismVerbosity = WhatMorphismQuiet


--------------------------------------------------------------------------------
installWhatMorphism :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installWhatMorphism _args todos = do
    reinitializeGlobals
    inliner <- CoreMonad.liftIO newInlinerState

    let passes
            | whatMorphismMode == WhatMorphismQuick =
                [ CoreDoPluginPass "WhatMorphism.Fold"
                    (runRewritePass foldPass inliner)
                ]

            | otherwise                             =
                [ CoreDoPluginPass "WhatMorphism.Build"
                    (runRewritePass buildPass inliner)
                , CoreDoPluginPass "WhatMorphism.Fold"
                    (runRewritePass foldPass inliner)
                , CoreDoPluginPass "WhatMorphism.Inliner" (inline inliner)
                , CoreDoPluginPass "WhatMorphism.Fusion"
                    (runRewritePass fusePass inliner)
                ]

    return $ intersperse (CoreDoPasses passes) todos
  where
    runRewritePass pass inliner mg = do
        register  <- CoreMonad.getFirstAnnotations
            Serialized.deserializeWithData mg
        mg_binds' <- runRewriteM (pass $ mg_binds mg)
            (mkRewriteRead
                whatMorphismMode whatMorphismVerbosity mg register inliner)
        case mg_binds' of
            Left err     -> do
                CoreMonad.putMsgS $ "Pass failed: " ++ err
                return mg
            Right binds' -> do
                CoreMonad.putMsgS "Pass okay!"
                return mg {mg_binds = binds'}

    inline inliner = bindsOnlyPass (CoreMonad.liftIO . inlinerPass inliner)
