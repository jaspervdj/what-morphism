--------------------------------------------------------------------------------
module WhatMorphism.Inliner
    ( InlinerState
    , newInlinerState
    , setNeedsInlining
    , inlinerPass
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           CoreSyn             (CoreBind, Expr (..))
import           Data.IORef          (IORef, modifyIORef, newIORef, readIORef)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Var                 (Var)


--------------------------------------------------------------------------------
import           WhatMorphism.Expr


--------------------------------------------------------------------------------
newtype InlinerState = InlinerState (IORef (Map Var (Expr Var)))


--------------------------------------------------------------------------------
newInlinerState :: IO InlinerState
newInlinerState = InlinerState <$> newIORef M.empty


--------------------------------------------------------------------------------
setNeedsInlining :: Var -> Expr Var -> InlinerState -> IO ()
setNeedsInlining v e (InlinerState ref) = modifyIORef ref $ M.insert v e


--------------------------------------------------------------------------------
inlinerPass :: InlinerState -> [CoreBind] -> IO [CoreBind]
inlinerPass (InlinerState ref) = mapM $ \b -> withBinds b $ \_ expr -> do
    needsInlining <- readIORef ref
    let (expr', _)      = replaceExpr replace expr
        replace (Var v) = M.lookup v needsInlining
        replace _       = Nothing
    return expr'
