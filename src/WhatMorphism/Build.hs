--------------------------------------------------------------------------------
-- | Try to detect build-like expressions. As a reminder,
--
-- > build :: (forall b. (a -> b -> b) -> b -> b) -> a
-- > build g = g (:) []
--
-- The forall parameter is of huge importance, since otherwise we can just
-- construct lists any way we like and ignore the passed-in constructors.
module WhatMorphism.Build
    (
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative   (pure, (<$>), (<*>))
import           Control.Monad.Reader  (ReaderT, runReaderT)
import           Control.Monad.Trans   (lift)
import           Control.Monad.Writer  (WriterT, runWriterT, tell)
import           CoreSyn
import           DataCon               (DataCon)
import qualified IdInfo                as IdInfo
import           Var                   (Var)
import qualified Var                   as Var


--------------------------------------------------------------------------------
import           WhatMorphism.RewriteM


--------------------------------------------------------------------------------
-- | After we examine a function, we get two things:
--
-- - A list of used DataCon's
-- - A function which allows replacement of these DataCon's by other expressions
type Build a = ReaderT [(DataCon, Expr Var)] (WriterT [DataCon] RewriteM) a


--------------------------------------------------------------------------------
liftRewriteM :: RewriteM a -> Build a
liftRewriteM = lift . lift


--------------------------------------------------------------------------------
toBuild :: Expr Var -> Build (Expr Var)
toBuild (Var x) = return (Var x)

toBuild (Lit x) = return (Lit x)

-- Real work here. TODO: replacement?
toBuild e@(App x y) = do
    dc <- liftRewriteM $ liftMaybe "No DataCon found" $ appToDataCon e
    tell [dc]
    return (App x y)

toBuild (Lam x y) = Lam x <$> toBuild y

-- TODO: We might want search the let bindings for the DataCon occurences
toBuild (Let bs e) = Let bs <$> toBuild e

toBuild (Case e b t alts) = Case e b t <$> mapM toBuild' alts
  where
    toBuild' (ac, bs, ae) = do
        ae' <- toBuild ae
        return (ac, bs, ae')

toBuild (Cast e c) = Cast <$> toBuild e <*> pure c

toBuild (Tick t e) = Tick t <$> toBuild e

toBuild (Type t) = return (Type t)

toBuild (Coercion c) = return (Coercion c)


--------------------------------------------------------------------------------
-- | TODO: We generally want to search for a DataCon OR recursion to our
-- function (needs to be added in Reader).
appToDataCon :: Expr Var -> Maybe DataCon
appToDataCon (App e _) = appToDataCon e
appToDataCon (Var var)
    | Var.isId var     = case Var.idDetails var of
        IdInfo.DataConWorkId dc -> Just dc
        _                       -> Nothing
appToDataCon _         = Nothing
