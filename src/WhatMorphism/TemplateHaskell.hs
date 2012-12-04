--------------------------------------------------------------------------------
module WhatMorphism.TemplateHaskell
    ( deriveFold
    ) where


--------------------------------------------------------------------------------
import           Control.Monad       (forM, replicateM)
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
    a      <- newName "a"  -- Result type of the fold
    consFs <- forM cons $ \c -> do
        f  <- newName "f"
        ys <- replicateM (length $ conTypes c) (newName "y")
        return $ (c, f, ys)
    x      <- newName "x"  -- Argument we're destroying

    return
        [ SigD foldName $ ForallT (typeBndrs ++ [PlainTV a]) [] $
            mkFunTy
                ([mkFunTy (conTypes con) (VarT a) | con <- cons] ++
                    [mkAppTy typeName typeBndrs])
                (VarT a)

        , FunD foldName
            [ Clause
                ([VarP f | (_, f, _) <- consFs] ++ [VarP x])
                (NormalB
                    (CaseE (VarE x)
                        [ Match
                            (ConP (conName c) (map VarP ys))
                            (NormalB $ mkAppE (VarE f) ys)
                            []
                        | (c, f, ys) <- consFs
                        ]))
                []
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
mkAppE :: Exp -> [Name] -> Exp
mkAppE f []       = f
mkAppE f (a : as) = mkAppE (AppE f (VarE a)) as


--------------------------------------------------------------------------------
-- mkFunTy :: Type -> Type -> Type
-- mkFunTy x y = (AppT (AppT ArrowT x) y)


--------------------------------------------------------------------------------
mkFunTy :: [Type] -> Type -> Type
mkFunTy (x : xs) y = AppT (AppT ArrowT x) (mkFunTy xs y)
mkFunTy [] y       = y


--------------------------------------------------------------------------------
conName :: Con -> Name
conName (NormalC n _)   = n
conName (RecC n _)      = n
conName (InfixC _ n _)  = n
conName (ForallC _ _ _) = error $
    "WhatMorphism.TemplateHaskell.conTypes: " ++
    "cannot yet define folds for forall'd types"


--------------------------------------------------------------------------------
conTypes :: Con -> [Type]
conTypes (NormalC _ ts)   = [t | (_, t) <- ts]
conTypes (RecC _ ts)      = [t | (_, _, t) <- ts]
conTypes (InfixC t1 _ t2) = map snd [t1, t2]
conTypes (ForallC _ _ _)  = error $
    "WhatMorphism.TemplateHaskell.conTypes: " ++
    "cannot yet define folds for forall'd types"
