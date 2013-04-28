--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module WhatMorphism.Pass
    ( whatMorphismPass
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Error    (catchError)
import           CoreSyn
import qualified Data.Generics.Schemes  as Data
import           Data.Typeable          (cast)
import           Unsafe.Coerce          (unsafeCoerce)


--------------------------------------------------------------------------------
import           WhatMorphism.Build
import           WhatMorphism.Dump
import           WhatMorphism.Expr
import           WhatMorphism.Fold
import           WhatMorphism.RemoveRec
import           WhatMorphism.RewriteM


--------------------------------------------------------------------------------
whatMorphismPass :: [CoreBind] -> RewriteM [CoreBind]
whatMorphismPass = fmap removeRec . mapM whatMorphism


--------------------------------------------------------------------------------
whatMorphism :: CoreBind -> RewriteM CoreBind
whatMorphism = Data.everywhereM $ \b -> case cast b of
    Just cb -> whatMorphismBind cb >>= \cb' -> return (unsafeCoerce cb')
    Nothing -> return b


--------------------------------------------------------------------------------
whatMorphismBind :: CoreBind -> RewriteM CoreBind
whatMorphismBind coreBind = do
    coreBind'  <- pass toBuild "toBuild" coreBind
    coreBind'' <- pass toFold "toFold"   coreBind'
    return coreBind''
  where
    pass p name bind = withBinds bind $ \f e -> do
        reg <- isRegisteredFoldOrBuild f
        if reg
            then return e
            else do
                message $ "====== " ++ name ++ ": " ++ dump f
                flip catchError (report e) $ do
                    e' <- p f e
                    registerForInlining f e'
                    return e'
    report e err = do
        message $ "====== Error: " ++ err
        return e
