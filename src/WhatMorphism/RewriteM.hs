--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module WhatMorphism.RewriteM
    ( RewriteRead (..)
    , RewriteM
    , runRewriteM
    , liftCoreM
    , liftMaybe
    , liftMaybe'
    , message
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative      (Alternative (..), Applicative (..))
import           Control.Monad            (ap)
import           Control.Monad.Error      (MonadError (..))
import           CoreMonad                (CoreM)
import qualified CoreMonad                as CoreMonad
import           UniqFM                   (UniqFM)
import           UniqSupply               (MonadUnique (..))
import qualified UniqSupply               as Unique


--------------------------------------------------------------------------------
import           WhatMorphism.Annotations


--------------------------------------------------------------------------------
data RewriteRead = RewriteRead
    { rewriteRegister :: UniqFM RegisterFoldBuild
    }


--------------------------------------------------------------------------------
newtype RewriteM a = RewriteM
    { unRewriteM :: RewriteRead -> CoreM (Either String a)
    }


--------------------------------------------------------------------------------
instance Functor RewriteM where
    fmap f (RewriteM x) = RewriteM $ \r -> fmap (fmap f) (x r)
    {-# INLINE fmap #-}


--------------------------------------------------------------------------------
instance Monad RewriteM where
    return x            = RewriteM $ const $ return $ Right x
    {-# INLINE return #-}

    (RewriteM mx) >>= f = RewriteM $ \r -> do
        x <- mx r
        case x of
            Left err -> return $ Left err
            Right x' -> do
                y <- unRewriteM (f x') r
                case y of
                    Left err -> return $ Left err
                    Right y' -> return $ Right y'
    {-# INLINE (>>=) #-}

    fail err = RewriteM $ const $ return $ Left err


--------------------------------------------------------------------------------
instance Unique.MonadUnique RewriteM where
    getUniqueSupplyM = liftCoreM getUniqueSupplyM


--------------------------------------------------------------------------------
instance MonadError String RewriteM where
    throwError err             = fail err
    catchError (RewriteM mx) f = RewriteM $ \r -> do
        x <- mx r
        case x of
            Left e   -> unRewriteM (f e) r
            Right x' -> return $ Right x'


--------------------------------------------------------------------------------
-- | Implemented in terms of the monad interface since I'm lazy
instance Applicative RewriteM where
    pure  = return
    (<*>) = ap


--------------------------------------------------------------------------------
instance Alternative RewriteM where
    empty   = throwError "WhatMorphism.RewriteM.RewriteM: empty Alternative"
    x <|> y = catchError x $ \err -> do
        message $ "WhatMorphism.RewriteM.RewriteM: <|> branch failed: " ++ err
        y


--------------------------------------------------------------------------------
runRewriteM :: RewriteM a -> RewriteRead -> CoreM (Either String a)
runRewriteM = unRewriteM


--------------------------------------------------------------------------------
liftCoreM :: CoreM a -> RewriteM a
liftCoreM = RewriteM . const . fmap Right


--------------------------------------------------------------------------------
liftMaybe :: String -> Maybe a -> RewriteM a
liftMaybe err m = RewriteM $ const $ return $ maybe (Left err) Right m
{-# INLINE liftMaybe #-}


--------------------------------------------------------------------------------
liftMaybe' :: Maybe a -> RewriteM a
liftMaybe' = liftMaybe "WhatMorphism.RewriteM.liftMaybe'"


--------------------------------------------------------------------------------
message :: String -> RewriteM ()
message = liftCoreM . CoreMonad.putMsgS
