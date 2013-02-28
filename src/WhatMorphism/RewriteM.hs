--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module WhatMorphism.RewriteM
    ( RewriteM
    , runRewriteM
    , liftCoreM
    , liftMaybe
    , liftMaybe'
    , message
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative (Alternative (..), Applicative (..))
import           Control.Monad       (ap)
import           Control.Monad.Error (MonadError (..))
import           CoreMonad           (CoreM, putMsgS)
import           UniqSupply          (MonadUnique (..))
import qualified UniqSupply          as Unique


--------------------------------------------------------------------------------
newtype RewriteM a = RewriteM {unRewriteM :: CoreM (Either String a)}


--------------------------------------------------------------------------------
instance Functor RewriteM where
    fmap f (RewriteM r) = RewriteM $ fmap (fmap f) r
    {-# INLINE fmap #-}


--------------------------------------------------------------------------------
instance Monad RewriteM where
    return x            = RewriteM $ return $ Right x
    {-# INLINE return #-}

    (RewriteM mx) >>= f = RewriteM $ do
        x <- mx
        case x of
            Left err -> return $ Left err
            Right x' -> do
                y <- unRewriteM $ f x'
                case y of
                    Left err -> return $ Left err
                    Right y' -> return $ Right y'
    {-# INLINE (>>=) #-}

    fail err = RewriteM $ return $ Left err


--------------------------------------------------------------------------------
instance Unique.MonadUnique RewriteM where
    getUniqueSupplyM = liftCoreM getUniqueSupplyM


--------------------------------------------------------------------------------
instance MonadError String RewriteM where
    throwError err             = fail err
    catchError (RewriteM mx) f = RewriteM $ do
        x <- mx
        case x of
            Left e   -> unRewriteM $ f e
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
runRewriteM :: RewriteM a -> CoreM (Either String a)
runRewriteM = unRewriteM


--------------------------------------------------------------------------------
liftCoreM :: CoreM a -> RewriteM a
liftCoreM = RewriteM . fmap Right


--------------------------------------------------------------------------------
liftMaybe :: String -> Maybe a -> RewriteM a
liftMaybe err m = RewriteM $ return $ maybe (Left err) Right m
{-# INLINE liftMaybe #-}


--------------------------------------------------------------------------------
liftMaybe' :: Maybe a -> RewriteM a
liftMaybe' = liftMaybe "WhatMorphism.RewriteM.liftMaybe'"


--------------------------------------------------------------------------------
message :: String -> RewriteM ()
message = liftCoreM . putMsgS
