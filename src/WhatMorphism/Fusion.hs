--------------------------------------------------------------------------------
module WhatMorphism.Fusion
    ( foldFoldFusion
    , isListFold
    ) where


--------------------------------------------------------------------------------
import           CoreSyn
import qualified PrelNames             as PrelNames
import           Type                  (Type)
import           Var                   (Var)
import qualified Var                   as Var


--------------------------------------------------------------------------------
import           WhatMorphism.Dump
import           WhatMorphism.RewriteM
import           WhatMorphism.SynEq


--------------------------------------------------------------------------------
foldFoldFusion :: Expr Var -> RewriteM (Expr Var)
foldFoldFusion = undefined


--------------------------------------------------------------------------------
data FoldSpec = FoldSpec
    { foldFunction   :: Var
    , foldReturnType :: Type
    , foldAlgebra    :: [Expr Var]
    , foldDestroys   :: Expr Var
    }


--------------------------------------------------------------------------------
instance Dump FoldSpec where
    dump (FoldSpec f r a d) =
        "(FoldSpec " ++ unwords [dump f, dump r, dump a, dump d] ++ ")"


--------------------------------------------------------------------------------
isFusable :: FoldSpec -> FoldSpec -> Bool
isFusable fs1 fs2 =
    foldFunction fs1 .==. foldFunction fs2 &&
    foldDestroys fs1 .==. foldDestroys fs2


--------------------------------------------------------------------------------
isListFold :: Expr Var -> Maybe FoldSpec

isListFold (App (App (App (App (App (Var foldrVar) _) rTyp) cons) nilF) d)
    | Var.varName foldrVar /= PrelNames.foldrName = Nothing
    | otherwise                                   = do
        rTyp' <- fromType rTyp
        return FoldSpec
            { foldFunction   = foldrVar
            , foldReturnType = rTyp'
            , foldAlgebra    = [cons, nilF]
            , foldDestroys   = d
            }
  where
    fromType (Type t) = Just t
    fromType _        = Nothing

isListFold _                                      = Nothing
