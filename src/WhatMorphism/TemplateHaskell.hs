--------------------------------------------------------------------------------
module WhatMorphism.TemplateHaskell
    ( deriveFoldBuildFusion
    , deriveFold
    , deriveBuild
    , deriveFusion
    ) where


--------------------------------------------------------------------------------
import           Control.Monad       (forM, mapM)
import           Data.Char           (toLower)
import           Language.Haskell.TH


--------------------------------------------------------------------------------
deriveFoldBuildFusion :: Name -> String -> String -> Q [Dec]
deriveFoldBuildFusion typeName foldName buildName = do
    fold   <- deriveFold typeName foldName
    build  <- deriveBuild typeName buildName
    fusion <- deriveFusion typeName foldName buildName
    return $ concat [fold, build, fusion]


--------------------------------------------------------------------------------
deriveFold :: Name -> String -> Q [Dec]
deriveFold typeName foldName = do
    info <- reify typeName
    case info of
        TyConI (DataD _ctx name bndrs cs _derives) ->
            mkFold foldName name bndrs cs
        _                                          -> fail $
            "WhatMorphism.TemplateHaskell.deriveFold: " ++
            "can only derive simple data declarations"


--------------------------------------------------------------------------------
mkFold :: String -> Name -> [TyVarBndr] -> [Con] -> Q [Dec]
mkFold foldName typeName typeBndrs cons = do

    a      <- newName "a"  -- Result type of the fold
    consFs <- forM cons $ \c -> do
        f   <- newName "f"
        tys <- forM (conTypes c) $ \t -> do
            y  <- newName "y"
            return (y, t)
        return $ (c, f, tys)
    go     <- newName (foldName ++ "_go")  -- Worker

    return
        [ SigD foldName' $ ForallT (typeBndrs ++ [PlainTV a]) [] $
            mkFunTy
                ([mkFunTy
                    [ if isRecursive t then (VarT a) else t
                    | t <- conTypes con
                    ]
                    (VarT a)
                 | con <- cons
                 ] ++ [typ])
                (VarT a)

        , FunD foldName'
            [ Clause
                ([VarP f | (_, f, _) <- consFs])
                (NormalB (VarE go))
                [ FunD go
                    [ Clause
                        [ConP (conName c) (map VarP $ map fst ys)]
                        (NormalB $ mkAppE (VarE f)
                            [ if isRecursive t
                                then AppE (VarE go) (VarE y)
                                else VarE y
                            | (y, t) <- ys]
                            )
                        []
                    | (c, f, ys) <- consFs
                    ]
                ]
            ]

        , inlineFromPhase foldName' 0
        -- , PragmaD (InlineP foldName' NoInline FunLike AllPhases)
        ]
  where
    foldName'     = mkName foldName
    typ           = mkAppTy typeName typeBndrs
    isRecursive t = typ == t


--------------------------------------------------------------------------------
deriveBuild :: Name -> String -> Q [Dec]
deriveBuild typeName buildName = do
    info <- reify typeName
    case info of
        TyConI (DataD _ctx name bndrs cs _derives) ->
            mkBuild buildName name bndrs cs
        _                                          -> fail $
            "WhatMorphism.TemplateHaskell.deriveBuild: " ++
            "can only derive simple data declarations"


--------------------------------------------------------------------------------
mkBuild :: String -> Name -> [TyVarBndr] -> [Con] -> Q [Dec]
mkBuild buildName typeName typeBndrs cons = do
    b   <- newName "b"  -- Internal return type
    g   <- newName "g"  -- Function given by the user
    gTy <- mkGTy typeName typeBndrs cons
    return
        [ SigD buildName' $ ForallT (typeBndrs) [] $
            mkFunTy [gTy] typ

        , FunD buildName'
            [ Clause
                [VarP g]
                (NormalB (mkAppE
                    (VarE g)
                    [ConE (conName con) | con <- cons]))
                []
            ]

        , inlineFromPhase buildName' 0
        -- , PragmaD (InlineP buildName' NoInline FunLike AllPhases)
        ]
  where
    buildName'    = mkName buildName
    typ           = mkAppTy typeName typeBndrs
    isRecursive t = typ == t


--------------------------------------------------------------------------------
deriveFusion :: Name -> String -> String -> Q [Dec]
deriveFusion typeName foldName buildName = do
    info <- reify typeName
    case info of
        TyConI (DataD _ctx name bndrs cs _derives) ->
            mkFusion foldName buildName name bndrs cs
        _                                          -> fail $
            "WhatMorphism.TemplateHaskell.deriveBuild: " ++
            "can only derive simple data declarations"


--------------------------------------------------------------------------------
mkFusion :: String -> String -> Name -> [TyVarBndr] -> [Con] -> Q [Dec]
mkFusion foldName buildName typeName typeBndrs cons = do
    cvars <- mapM (newName . map toLower . nameBase . conName) cons
    g     <- newName "g"
    gTy   <- mkGTy typeName typeBndrs cons
    return
        [ PragmaD $
            RuleP
                (foldName ++ "/" ++ buildName ++ "-fusion")
                (map RuleVar cvars ++ [TypedRuleVar g gTy])
                (mkAppE (VarE foldName')
                    (map VarE cvars ++ [AppE (VarE buildName') (VarE g)]))
                (mkAppE (VarE g) (map VarE cvars))
                AllPhases
        ]
  where
    foldName'  = mkName foldName
    buildName' = mkName buildName


--------------------------------------------------------------------------------
mkGTy :: Name -> [TyVarBndr] -> [Con] -> Q Type
mkGTy typeName typeBndrs cons = do
    b <- newName "b"
    return $ ForallT [PlainTV b] [] $ mkFunTy
        [ mkFunTy
            [ if isRecursive t then (VarT b) else t
            | t <- conTypes con
            ]
            (VarT b)
        | con <- cons
        ]
        (VarT b)
  where
    typ           = mkAppTy typeName typeBndrs
    isRecursive t = typ == t


--------------------------------------------------------------------------------
mkAppTy :: Name -> [TyVarBndr] -> Type
mkAppTy name = foldl (\t tv -> AppT t (VarT (getTv tv))) (ConT name)
  where
    getTv (PlainTV x)    = x
    getTv (KindedTV x _) = x


--------------------------------------------------------------------------------
mkAppE :: Exp -> [Exp] -> Exp
mkAppE f []       = f
mkAppE f (a : as) = mkAppE (AppE f a) as


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


--------------------------------------------------------------------------------
inlineFromPhase :: Name -> Int -> Dec
inlineFromPhase name n = PragmaD (InlineP name Inline FunLike (FromPhase n))
