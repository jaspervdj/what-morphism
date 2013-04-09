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
    , registeredFold
    , isRegisteredFoldOrBuild
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative      (Alternative (..), Applicative (..),
                                           (<$>))
import           Control.Monad            (ap)
import           Control.Monad.Error      (MonadError (..))
import           CoreMonad                (CoreM)
import qualified CoreMonad                as CoreMonad
import           HscTypes                 (ModGuts)
import qualified HscTypes                 as HscTypes
import           Name                     (Name)
import qualified Name                     as Name
import           OccName                  (OccName)
import qualified OccName                  as OccName
import qualified RdrName                  as RdrName
import           Type                     (Type)
import qualified Type                     as Type
import           UniqFM                   (UniqFM)
import qualified UniqFM                   as UniqFM
import           UniqSupply               (MonadUnique (..))
import qualified UniqSupply               as Unique
import           Var                      (Id)
import qualified Var                      as Var


--------------------------------------------------------------------------------
import           WhatMorphism.Annotations
import           WhatMorphism.Dump


--------------------------------------------------------------------------------
data RewriteRead = RewriteRead
    { rewriteModGuts  :: ModGuts
    , rewriteRegister :: UniqFM RegisterFoldBuild
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


--------------------------------------------------------------------------------
rewriteAsk :: RewriteM RewriteRead
rewriteAsk = RewriteM $ \r -> return $ Right r


--------------------------------------------------------------------------------
registerFor :: Type -> RewriteM RegisterFoldBuild
registerFor typ = do
    case Type.splitTyConApp_maybe typ of
        Nothing      -> fail "No TyCon, nothing registered"
        Just (tc, _) -> do
            register <- rewriteRegister <$> rewriteAsk
            case UniqFM.lookupUFM register tc of
                Nothing -> fail $ "No register for " ++ dump typ
                Just r  -> return r


--------------------------------------------------------------------------------
registeredFold :: Type -> RewriteM Id
registeredFold typ = do
    register <- registerFor typ
    lookupImportedString (registerFold register)


--------------------------------------------------------------------------------
lookupImportedString :: String -> RewriteM Id
lookupImportedString name = do
    name' <- lookupImportedOccName (OccName.mkVarOcc name)
    liftCoreM $ HscTypes.lookupId name'


--------------------------------------------------------------------------------
lookupImportedOccName :: OccName -> RewriteM Name
lookupImportedOccName oname = do
    gre <- HscTypes.mg_rdr_env . rewriteModGuts <$> rewriteAsk
    let candidates = filter isImported $ lookupGlobalRdrEnv gre oname
    case candidates of
        []        -> fail $ "No Name found for " ++ dump oname ++ " in rdr_env"
        (elt : _) -> return (RdrName.gre_name elt)
  where
    isImported :: RdrName.GlobalRdrElt -> Bool
    isImported elt = case RdrName.gre_prov elt of
        RdrName.Imported _ -> True
        _                  -> False


--------------------------------------------------------------------------------
-- | This is a SLOW, UGLY replacement for 'RdrName.lookupGlobalRdrEnv'. Why do
-- we need it? Because the original seems to be broken in very peculiar ways. I
-- think I might be generating 'OccName's in a wrong way but there is no way to
-- tell with so little documentation...
lookupGlobalRdrEnv :: RdrName.GlobalRdrEnv -> OccName -> [RdrName.GlobalRdrElt]
lookupGlobalRdrEnv env oname =
    [ elt
    | elts <- OccName.occEnvElts env
    , elt  <- elts
    , OccName.occNameString (Name.nameOccName (RdrName.gre_name elt)) ==
        OccName.occNameString oname
    ]


--------------------------------------------------------------------------------
-- | Warning!!! TODO REMOVE ME
isRegisteredFoldOrBuild :: Id -> RewriteM Bool
isRegisteredFoldOrBuild id' = do
    message $ "Checking " ++ dump id'
    reg <- rewriteRegister <$> rewriteAsk
    return $ or
        [ n == OccName.occNameString (Name.nameOccName (Var.varName id'))
        | RegisterFoldBuild f b <- UniqFM.eltsUFM reg, n <- [f, b]
        ]
