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
whatMorphismConfig :: WhatMorphismConfig
whatMorphismConfig = WhatMorphismConfig
    { whatMorphismScope     = WhatMorphismQuick
    , whatMorphismMode      = WhatMorphismDetect
    , whatMorphismVerbosity = WhatMorphismDebug
    }


--------------------------------------------------------------------------------
installWhatMorphism :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installWhatMorphism _args todos = do
    reinitializeGlobals
    inliner <- CoreMonad.liftIO newInlinerState

    let passes
            | detect     =
                [ CoreDoPluginPass "WhatMorphism.Build"
                    (runRewritePass buildPass inliner)
                , CoreDoPluginPass "WhatMorphism.Fold"
                    (runRewritePass foldPass inliner)
                ]

            | otherwise =
                [ CoreDoPluginPass "WhatMorphism.Build"
                    (runRewritePass buildPass inliner)
                , CoreDoPluginPass "WhatMorphism.Fold"
                    (runRewritePass foldPass inliner)

                , CoreDoSimplify 1 (SimplMode ["WhatMorphism.Simplifier"]
                    (Phase 2) True True False False)

                -- , CoreDoPluginPass "WhatMorphism.Inliner" (inline inliner)
                , CoreDoPluginPass "WhatMorphism.Fusion"
                    (runRewritePass fusePass inliner)
                ]

    return $
        (if whatMorphismScope whatMorphismConfig == WhatMorphismFull
            then insertSmart
            else afterSimplifier)
                (CoreDoPasses passes) todos
  where
    runRewritePass pass inliner mg = do
        register  <- CoreMonad.getFirstAnnotations
            Serialized.deserializeWithData mg
        mg_binds' <- runRewriteM (pass $ mg_binds mg)
            (mkRewriteRead whatMorphismConfig mg register inliner)
        case mg_binds' of
            Left err     -> do
                CoreMonad.putMsgS $ "Pass failed: " ++ err
                return mg
            Right binds' -> do
                CoreMonad.putMsgS "Pass okay!"
                return $ if detect then mg else mg {mg_binds = binds'}

    inline inliner = bindsOnlyPass (CoreMonad.liftIO . inlinerPass inliner)

    detect = whatMorphismMode whatMorphismConfig == WhatMorphismDetect


--------------------------------------------------------------------------------
insertSmart :: CoreToDo -> [CoreToDo] -> [CoreToDo]
insertSmart wm passes =
    case break (isPhase 0) passes of
        (beforePhase0, phase0) ->
            intersperse wm beforePhase0 ++ [wm] ++ phase0
  where
    isPhase n (CoreDoSimplify _ sm) = case sm_phase sm of
        Phase m -> n == m
        _       -> False
    isPhase _ _                     = False


--------------------------------------------------------------------------------
afterSimplifier :: CoreToDo -> [CoreToDo] -> [CoreToDo]
afterSimplifier as = go
  where
    go (s@(CoreDoSimplify _ _) : tds) = s : as : go tds
    go (td                     : tds) = td : go tds
    go []                             = []
