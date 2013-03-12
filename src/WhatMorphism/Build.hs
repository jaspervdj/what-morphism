--------------------------------------------------------------------------------
-- | Try to detect build-like expressions. As a reminder,
--
-- > build :: (forall b. (a -> b -> b) -> b -> b) -> a
-- > build g = g (:) []
--
-- The forall parameter is of huge importance, since otherwise we can just
-- construct lists any way we like and ignore the passed-in constructors.
module WhatMorphism.Build
    ( toBuild
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   (pure, (<$>), (<*>))
import           Control.Monad.Reader  (ReaderT, runReaderT, ask)
import           Control.Monad.Trans   (lift)
import           Control.Monad.Writer  (WriterT, runWriterT, tell)
import           CoreSyn
import           Data.Maybe            (maybeToList)
import           DataCon               (DataCon)
import qualified IdInfo                as IdInfo
import           Var                   (Var)
import qualified Var                   as Var


--------------------------------------------------------------------------------
import           WhatMorphism.Dump
import           WhatMorphism.RewriteM
import           WhatMorphism.SynEq


--------------------------------------------------------------------------------
toBuild :: Var -> Expr Var -> RewriteM (Expr Var)
toBuild f body = do
    -- Run to detect data constructors
    (_, dataCons) <- runWriterT $ runReaderT (replace body) (BuildRead f [])

    -- TODO: Second run to replace them
    message $ "DataCons: " ++ dump dataCons
    return body


--------------------------------------------------------------------------------
data BuildRead = BuildRead
    { buildVar          :: Var
    , buildReplacements :: [(DataCon, Expr Var)]
    }


--------------------------------------------------------------------------------
-- | After we examine a function, we get two things:
--
-- - A list of used DataCon's
-- - A function which allows replacement of these DataCon's by other expressions
type Build a = ReaderT BuildRead (WriterT [DataCon] RewriteM) a


--------------------------------------------------------------------------------
liftRewriteM :: RewriteM a -> Build a
liftRewriteM = lift . lift


--------------------------------------------------------------------------------
replace :: Expr Var -> Build (Expr Var)
replace (Var x) = return (Var x)

replace (Lit x) = return (Lit x)

-- Real work here. TODO: replacement?
replace e@(App x y) = do
    dc <- recursionOrDataCon e
    tell $ maybeToList dc
    return (App x y)

replace (Lam x y) = Lam x <$> replace y

-- TODO: We might want search the let bindings for the DataCon occurences
replace (Let bs e) = Let bs <$> replace e

replace (Case e b t alts) = Case e b t <$> mapM replace' alts
  where
    replace' (ac, bs, ae) = do
        ae' <- replace ae
        return (ac, bs, ae')

replace (Cast e c) = Cast <$> replace e <*> pure c

replace (Tick t e) = Tick t <$> replace e

replace (Type t) = return (Type t)

replace (Coercion c) = return (Coercion c)


--------------------------------------------------------------------------------
-- | TODO: We generally want to search for a DataCon OR recursion to our
-- function (needs to be added in Reader).
--
-- * Returns 'Nothing' on recursion
--
-- * Returns the 'DataCon' when one is found
--
-- * 'fail's otherwise
recursionOrDataCon :: Expr Var -> Build (Maybe DataCon)
recursionOrDataCon e = do
    recursionVar <- buildVar <$> ask
    case e of
        (App e' _) -> recursionOrDataCon e'
        (Var var)
            | var .==. recursionVar -> return Nothing
            | Var.isId var          -> case Var.idDetails var of
                IdInfo.DataConWorkId dc -> return $ Just dc
                _                       -> fail' $ "No DataCon Id: " ++ dump var
            | otherwise             -> fail' "Unexpected Var"
        _ -> fail' "No App or Var found"
  where
    fail' err = fail $ "WhatMorphism.Build.recursionOrDataCon: " ++ err
