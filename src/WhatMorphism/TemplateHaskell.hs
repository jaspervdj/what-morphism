--------------------------------------------------------------------------------
module WhatMorphism.TemplateHaskell
    ( deriveFold
    ) where


--------------------------------------------------------------------------------
import           Language.Haskell.TH


--------------------------------------------------------------------------------
deriveFold :: Name -> Q [Dec]
deriveFold typeName = do
    info <- reify typeName
    case info of
        TyConI (DataD _ctx name bndrs cs _derives) -> mkFold name bndrs cs
        _                                          -> fail $
            "WhatMorphism.TemplateHaskell.deriveFold: " ++
            "can only derive simple data declarations"


--------------------------------------------------------------------------------
mkFold :: Name -> [TyVarBndr] -> [Con] -> Q [Dec]
mkFold typeName typeBndrs cons = do
    a <- newName "a"  -- Result type of the fold

    return
        [ SigD foldName $ ForallT (typeBndrs ++ [PlainTV a]) [] $
            mkFunTy
                [mkAppTy typeName typeBndrs]
                (VarT a)

        , FunD foldName
            [ Clause [] (NormalB (VarE $ mkName "undefined")) []
            ]
        ]
  where
    foldName = mkName $ "fold" ++ nameBase typeName


--------------------------------------------------------------------------------
mkAppTy :: Name -> [TyVarBndr] -> Type
mkAppTy name []                  = ConT name
mkAppTy name (PlainTV x : xs)    = AppT (mkAppTy name xs) (VarT x)
mkAppTy name (KindedTV x _ : xs) = AppT (mkAppTy name xs) (VarT x)


--------------------------------------------------------------------------------
-- mkFunTy :: Type -> Type -> Type
-- mkFunTy x y = (AppT (AppT ArrowT x) y)


--------------------------------------------------------------------------------
mkFunTy :: [Type] -> Type -> Type
mkFunTy (x : xs) y = AppT (AppT ArrowT x) (mkFunTy xs y)
mkFunTy [] y       = y


--------------------------------------------------------------------------------
conTypes :: Con -> Q [Type]
conTypes (NormalC _ ts)   = return [t | (_, t) <- ts]
conTypes (RecC _ ts)      = return [t | (_, _, t) <- ts]
conTypes (InfixC t1 _ t2) = return $ map snd [t1, t2]
conTypes (ForallC _ _ _)  = fail $
    "WhatMorphism.TemplateHaskell.conTypes: " ++
    "cannot yet define folds for forall'd types"
