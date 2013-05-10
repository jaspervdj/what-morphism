--------------------------------------------------------------------------------
module WhatMorphism.Inliner
    ( InlinerState
    , newInlinerState
    , setNeedsInlining
    , getNeedsInlining
    , inlinerPass
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad       (guard)
import qualified CoreFVs             as CoreFVs
import qualified CoreSubst           as CoreSubst
import           CoreSyn             (CoreBind, Expr (..))
import qualified CoreSyn             as CoreSyn
import           Data.IORef          (IORef, modifyIORef, newIORef, readIORef)
import           Data.Map            (Map)
import qualified Data.Map            as M
import qualified MkCore              as MkCore
import qualified Outputable          as Outputable
import           Var                 (Var)
import qualified VarEnv              as VarEnv


--------------------------------------------------------------------------------
import           WhatMorphism.Expr


--------------------------------------------------------------------------------
data InlinerTemplate = InlinerTemplate
    { inlinerExpr  :: Expr Var
    , inlinerArgs  :: [Var]
    , inlinerArity :: Int
    }


--------------------------------------------------------------------------------
mkInlinerTemplate :: Expr Var -> InlinerTemplate
mkInlinerTemplate expr = InlinerTemplate
    { inlinerExpr   = expr'
    , inlinerArgs   = args
    , inlinerArity  = length (args)
    }
  where
    (args, expr') = CoreSyn.collectBinders expr


--------------------------------------------------------------------------------
newtype InlinerState = InlinerState (IORef (Map Var InlinerTemplate))


--------------------------------------------------------------------------------
newInlinerState :: IO InlinerState
newInlinerState = InlinerState <$> newIORef M.empty


--------------------------------------------------------------------------------
setNeedsInlining :: Var -> Expr Var -> InlinerState -> IO ()
setNeedsInlining v e (InlinerState ref) = modifyIORef ref $
    Outputable.pprTrace "var" (Outputable.ppr v) $
    Outputable.pprTrace "expr" (Outputable.ppr $ inlinerExpr tpl) $
    Outputable.pprTrace "args" (Outputable.ppr $ inlinerArgs tpl) $
    Outputable.pprTrace "arity" (Outputable.ppr $ inlinerArity tpl) $
    M.insert v tpl
  where
    tpl = mkInlinerTemplate e


--------------------------------------------------------------------------------
getNeedsInlining :: Var -> InlinerState -> IO Bool
getNeedsInlining v (InlinerState ref) = (v `M.member`) <$> readIORef ref


--------------------------------------------------------------------------------
inlinerPass :: InlinerState -> [CoreBind] -> IO [CoreBind]
inlinerPass (InlinerState ref) = mapM $ \b -> withBinds b $ \_ expr -> do
    needsInlining <- readIORef ref
    let (expr', _) = replaceExpr (inline needsInlining) expr
    return expr'


--------------------------------------------------------------------------------
inline :: Map Var InlinerTemplate -> Expr Var -> Maybe (Expr Var)
inline needsInlining expr = case CoreSyn.collectArgs expr of
    (Var v, args) -> do
        tpl <- M.lookup v needsInlining
        guard $ length args >= inlinerArity tpl

        let (args', excess) = splitAt (inlinerArity tpl) args

            inScopeSet      =
                buildInScopeSet (inlinerExpr tpl : args') (inlinerArgs tpl)
            subst           =
                CoreSubst.extendSubstList
                    (CoreSubst.setInScope CoreSubst.emptySubst inScopeSet)
                    (zip (inlinerArgs tpl) args')

        return $ MkCore.mkCoreApps
            (CoreSubst.substExpr (error "ignored SDoc") subst (inlinerExpr tpl))
            excess
    _ -> Nothing


--------------------------------------------------------------------------------
buildInScopeSet :: [Expr Var] -> [Var] -> VarEnv.InScopeSet
buildInScopeSet exprs args =
    foldr
        (\v s -> VarEnv.delInScopeSet s v)
        (foldr
            (\e s -> VarEnv.extendInScopeSetSet s (CoreFVs.exprFreeVars e))
            VarEnv.emptyInScopeSet exprs)
        args
