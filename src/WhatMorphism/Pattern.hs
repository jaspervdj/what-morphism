--------------------------------------------------------------------------------
module WhatMorphism.Pattern
    ( toFold
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   ((<|>))
import           Control.Monad         (forM, forM_)
import           CoreSyn
import           Data.List             (find)
import           Data.Maybe            (fromMaybe)
import qualified MkCore                as MkCore
import qualified Name                  as Name
import           Type                  (Type)
import qualified Type                  as Type
import qualified TysWiredIn            as TysWiredIn
import           Var                   (Var)
import qualified Var                   as Var


--------------------------------------------------------------------------------
import           WhatMorphism.Dump
import           WhatMorphism.Expr
import           WhatMorphism.RewriteM


--------------------------------------------------------------------------------
toFold :: Var -> Expr Var -> RewriteM (Expr Var)
toFold f body = do
    message $ "Starting with: " ++ dump body
    toFold' (Var f) id body


--------------------------------------------------------------------------------
toFold' :: Expr Var
        -> (Expr Var -> Expr Var)
        -> Expr Var
        -> RewriteM (Expr Var)
toFold' f mkF (Lam x body) =
    toFoldOver (\t -> App f (Var t)) (\e -> mkF (Lam x e)) x body <|>
    toFold' (App f (Var x)) (\e -> Lam x (mkF e)) body
toFold' _ _   _            = fail "No top-level Lam"


--------------------------------------------------------------------------------
toFoldOver :: (Var -> Expr Var)
           -> (Expr Var -> Expr Var)
           -> Var
           -> Expr Var
           -> RewriteM (Expr Var)
toFoldOver f mkF d (Lam x body) =
    toFoldOver (\t -> App (f t) (Var x)) (\e -> mkF (Lam x e)) d body
toFoldOver f mkF d c@(Case (Var x) _ rTyp alts)
    | x == d                    = do
        alts' <- forM alts $ \(ac, bnds, expr) -> do
            message $ "Rewriting AltCon " ++ dump ac
            message $ "Was: " ++ dump expr
            expr' <- rewriteAlt f d bnds rTyp expr
            message $ "Now: " ++ dump expr'
            assertWellScoped (x : bnds) expr'
            return (ac, expr')
        fold <- mkListFold d rTyp alts'
        return $ mkF fold
    | otherwise                 = fail "Wrong argument destructed"
toFoldOver _ _ _ _              = fail "No top-level Case"


--------------------------------------------------------------------------------
mkListFold :: Var
           -> Type
           -> [(AltCon, Expr Var)]
           -> RewriteM (Expr Var)
mkListFold d rTyp alts = do
    elemTyp <- getElemTyp
    consF   <- getAlt TysWiredIn.consDataCon
    nilF    <- getAlt TysWiredIn.nilDataCon
    liftCoreM $ MkCore.mkFoldrExpr elemTyp rTyp consF nilF (Var d)
  where
    getElemTyp = case Type.splitTyConApp_maybe (Var.varType d) of
        Nothing                            -> fail "Not working on a TyConApp"
        Just (tcon, [elemTyp])
            | tcon == TysWiredIn.listTyCon -> return elemTyp
            | otherwise                    -> fail "Not working on listTyCon"
        Just _                             -> fail "Weird listTyCon?!"

    getAlt dataCon = liftMaybe ("No alt found for " ++ dump dataCon) $
        lookup (DataAlt dataCon) alts


--------------------------------------------------------------------------------
isListConstructor :: AltCon -> Bool
isListConstructor ac = fromMaybe False $ do
    dc <- dataAlt ac
    return $ Name.occNameString (Name.getOccName dc) `elem` [":", "[]"]
  where
    dataAlt (DataAlt c) = Just c
    dataAlt _           = Nothing


--------------------------------------------------------------------------------
rewriteAlt :: (Var -> Expr Var)
           -> Var
           -> [Var]
           -> Type
           -> Expr Var
           -> RewriteM (Expr Var)
rewriteAlt _ _ []       _    body = return body
rewriteAlt f d (t : ts) rTyp body = do
    expr <- rewriteAlt f d ts rTyp body
    if isRecursive
        then liftCoreM $ mkLambda rTyp            (f t)   expr
        else liftCoreM $ mkLambda (Var.varType t) (Var t) expr
  where
    isRecursive = Var.varType t `Type.eqType` Var.varType d


--------------------------------------------------------------------------------
-- | We don't actually do any scoping, we just have a list of vars which can't
-- appear anymore.
assertWellScoped :: [Var] -> Expr Var -> RewriteM ()
assertWellScoped vars body = case find (`inScope` body) vars of
    Nothing  -> message "Scope okay"
    Just var -> fail $
        "Not well-scoped: " ++ dump var ++ " still appears after rewriting"


--------------------------------------------------------------------------------
inScope :: Var -> Expr Var -> Bool
inScope x body = count (Var x) body > 0
