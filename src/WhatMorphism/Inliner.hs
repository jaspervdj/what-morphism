--------------------------------------------------------------------------------
module WhatMorphism.Inliner
    ( InlinerState
    , newInlinerState
    , setNeedsInlining
    , inlinerPass
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad       (guard)
import           CoreSyn             (CoreBind, Expr (..))
import qualified CoreSyn             as CoreSyn
import           Data.IORef          (IORef, modifyIORef, newIORef, readIORef)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Var                 (Var)
import Debug.Trace


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
    { inlinerExpr  = expr'
    , inlinerArgs  = binders
    , inlinerArity = length binders
    }
  where
    (binders, expr') = CoreSyn.collectBinders expr


--------------------------------------------------------------------------------
newtype InlinerState = InlinerState (IORef (Map Var InlinerTemplate))


--------------------------------------------------------------------------------
newInlinerState :: IO InlinerState
newInlinerState = InlinerState <$> newIORef M.empty


--------------------------------------------------------------------------------
setNeedsInlining :: Var -> Expr Var -> InlinerState -> IO ()
setNeedsInlining v e (InlinerState ref) = modifyIORef ref $
    M.insert v $ mkInlinerTemplate e


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
        template <- M.lookup v needsInlining
        guard $ length args >= inlinerArity template
        trace "INLINING SHIZZLE!" $ return $ foldl
            (\e (a, ta) -> let (e', _) = replaceExpr (replaceArg ta a) e in e')
            (inlinerExpr template)
            (zip args $ inlinerArgs template)
    _ -> Nothing
  where
    replaceArg v a (Var v')
        | v == v'     = Just a
        | otherwise   = Nothing
    replaceArg _ _ _  = Nothing
