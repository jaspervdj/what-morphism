--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module WhatMorphism.Pass
    ( whatMorphismPass
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Error   (catchError)
import           CoreSyn


--------------------------------------------------------------------------------
import           WhatMorphism.Build
import           WhatMorphism.Dump
import           WhatMorphism.Expr
import           WhatMorphism.Pattern
import           WhatMorphism.RewriteM


--------------------------------------------------------------------------------
whatMorphismPass :: [CoreBind] -> RewriteM [CoreBind]
whatMorphismPass = mapM whatMorphism


--------------------------------------------------------------------------------
whatMorphism :: CoreBind -> RewriteM CoreBind
whatMorphism coreBind = do
    coreBind' <- withBinds coreBind $ \f e -> do
        reg <- isRegisteredFoldOrBuild f
        if reg
            then return e
            else do
                message $ "====== toBuild: " ++ dump f
                catchError (toBuild f e) $ \err -> do
                    message $ "====== Error: " ++ err
                    return e

    coreBind'' <- withBinds coreBind' $ \f e -> do
        reg <- isRegisteredFoldOrBuild f
        if reg
            then return e
            else do
                message $ "====== toFold: " ++ dump f
                catchError (toFold f e) $ \err -> do
                    message $ "====== Error: " ++ err
                    return e

    return coreBind''
