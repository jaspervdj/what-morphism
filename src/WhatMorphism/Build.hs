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
import           Control.Monad.Reader  (ReaderT, ask, runReaderT)
import           Control.Monad.Trans   (lift)
import           Control.Monad.Writer  (WriterT, runWriterT, tell)
import           CoreSyn
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

    -- TODO: Figure out replacements
    -- TODO: Second run to replace them
    message $ "DataCons: " ++ dump dataCons
    return body


--------------------------------------------------------------------------------
data BuildRead = BuildRead
    { buildVar          :: Var
    , buildReplacements :: [(DataCon, Var)]
    }


--------------------------------------------------------------------------------
-- | After we examine a function, we get two things:
--
-- - A list of used DataCon's
-- - A function which allows replacement of these DataCon's by other expressions
type Build a = ReaderT BuildRead (WriterT [DataCon] RewriteM) a


--------------------------------------------------------------------------------
replacementForDataCon :: Var -> DataCon -> Build Var
replacementForDataCon original dataCon = do
    replacements <- buildReplacements <$> ask
    case lookup dataCon replacements of
        Just v  -> return v
        Nothing -> do
            liftRewriteM $ message $
                "WhatMorphism.Build.replacementForDataCon: " ++
                "No replacement for " ++ dump dataCon ++ ", using original"
            return original


--------------------------------------------------------------------------------
liftRewriteM :: RewriteM a -> Build a
liftRewriteM = lift . lift


--------------------------------------------------------------------------------
replace :: Expr Var -> Build (Expr Var)
replace (Var x) = return (Var x)

replace (Lit x) = return (Lit x)

-- Real work here. TODO: replacement?
replace e@(App _ _) = do
    e' <- recursionOrReplaceDataCon e
    return e'

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
recursionOrReplaceDataCon :: Expr Var -> Build (Expr Var)
recursionOrReplaceDataCon e = do
    recursionVar <- buildVar <$> ask
    case e of
        -- It seems like GHC sometimes generates weird code like this. If
        -- needed, this can be made more general by remembering the number of
        -- 'Lam's we can skip.
        (App (Lam x e') a) ->
            App <$> (Lam x <$> recursionOrReplaceDataCon e') <*> pure a
        (App e' a)         -> App <$> recursionOrReplaceDataCon e' <*> pure a
        (Var var)
            | var .==. recursionVar -> return e
            | Var.isId var          -> case Var.idDetails var of
                IdInfo.DataConWorkId dc -> do
                    tell [dc]
                    replacement <- replacementForDataCon var dc
                    return (Var replacement)
                _                       -> fail' $ "No DataCon Id: " ++ dump var
            | otherwise             -> fail' "Unexpected Var"
        _ -> fail' "No App or Var found"
  where
    fail' err = fail $ "WhatMorphism.Build.recursionOrReplaceDataCon: " ++ err
