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
import           CoreSyn             (CoreBind, Expr (..))
import qualified CoreSyn             as CoreSyn
import qualified Data.Generics       as Data
import           Data.IORef          (IORef, modifyIORef, newIORef, readIORef)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Typeable       (cast)
import qualified Outputable          as Outputable
import           Type                (Type)
import qualified Type                as Type
import           Unsafe.Coerce       (unsafeCoerce)
import           Var                 (Var)


--------------------------------------------------------------------------------
import           WhatMorphism.Expr


--------------------------------------------------------------------------------
data InlinerTemplate = InlinerTemplate
    { inlinerExpr   :: Expr Var
    , inlinerTyArgs :: [Type.TyVar]
    , inlinerArgs   :: [Var]
    , inlinerArity  :: Int
    }


--------------------------------------------------------------------------------
mkInlinerTemplate :: Expr Var -> InlinerTemplate
mkInlinerTemplate expr = InlinerTemplate
    { inlinerExpr   = expr'
    , inlinerTyArgs = tyArgs
    , inlinerArgs   = args
    , inlinerArity  = length (tyArgs ++ args)
    }
  where
    (tyArgs, args, expr') = CoreSyn.collectTyAndValBinders expr


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
        let numTyArgs = length (inlinerTyArgs tpl)
            tvEnv     = zip (inlinerTyArgs tpl) [t | Type t <- args]
            env       = zip (inlinerArgs tpl) (drop numTyArgs args)
        return $ substVars env $ substTyVarsInExpr tvEnv $ inlinerExpr tpl
    _ -> Nothing

  where
    substTyVarsInExpr :: [(Type.TyVar, Type)] -> Expr Var -> Expr Var
    substTyVarsInExpr env = Data.everywhere $ \x -> case cast x of
        Just t  -> unsafeCoerce $ substTyVars env t
        Nothing -> x
