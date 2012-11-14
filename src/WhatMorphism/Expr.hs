-------------------------------------------------------------------------------
-- | Utilities for manipulating expressions
module WhatMorphism.Expr
    ( subExprs
    , everywhere
    , replace
    , toVar
    , mkLambda
    , binds
    , foldExpr
    ) where


--------------------------------------------------------------------------------
import           Coercion                   (Coercion)
import           Control.Monad.State.Strict (State, modify, runState)
import           CoreMonad                  (CoreM)
import           CoreSyn
import qualified Data.Generics.Schemes      as Data
import           Data.Typeable              (cast)
import           Debug.Trace
import qualified IdInfo                     as IdInfo
import           Literal                    (Literal)
import qualified Name                       as Name
import qualified OccName                    as OccName
import qualified SrcLoc                     as SrcLoc
import           Type                       (Type)
import qualified UniqSupply                 as Unique
import qualified Unique                     as Unique
import           Unsafe.Coerce              (unsafeCoerce)
import           Var                        (Id, Var)
import qualified Var                        as Var


--------------------------------------------------------------------------------
import           WhatMorphism.Dump


--------------------------------------------------------------------------------
subExprs :: Expr b -> [Expr b]
subExprs x = x : go x
  where
    go (Var _)         = []
    go (Lit _)         = []
    go (App f a)       = subExprs f ++ subExprs a
    go (Lam _ e)       = subExprs e
    go (Let b e)       = concatMap subExprs (map snd $ binds b) ++ subExprs e
    go (Case e _ _ as) = subExprs e ++ concatMap subExprs [a | (_, _, a) <- as]
    go (Cast e _)      = subExprs e
    go (Tick _ e)      = subExprs e
    go (Type _)        = []
    go (Coercion _)    = []


--------------------------------------------------------------------------------
everywhere :: (Expr Var -> Expr Var) -> Expr Var -> Expr Var
everywhere f = Data.everywhere $ \x -> maybe x (unsafeCoerce . f) (cast x)


--------------------------------------------------------------------------------
-- | Count replacements
replace :: (Expr Var -> Maybe (Expr Var)) -> Expr Var -> (Expr Var, Int)
replace f = flip runState 0 . state
  where
    state :: Expr Var -> State Int (Expr Var)
    state = Data.everywhereM $ \x -> case cast x of
        Nothing -> return x
        Just y  -> case f y of
            Nothing -> return x
            Just y' -> modify (+ 1) >> return (unsafeCoerce y')


--------------------------------------------------------------------------------
binds :: Bind b -> [(b, Expr b)]
binds (NonRec b e) = [(b, e)]
binds (Rec bs)     = bs


--------------------------------------------------------------------------------
toVar :: Expr Var -> Maybe Var
toVar (Var v) = Just v
toVar _       = Nothing


--------------------------------------------------------------------------------
-- | Remove an expression by creating a lambda
--
-- If we float out @x@, this:
--
-- > foo x + x
--
-- Becomes something like:
--
-- > (\tmp -> foo tmp + tmp) x
mkLambda :: Type -> Expr Var -> Expr Var -> CoreM (Expr Var)
mkLambda typ needle haystack = do
    tmp <- freshVar typ

    let check n =
            let eq = n .==. needle
            in trace (dump needle ++ " =?= " ++ dump n ++ " -> " ++ show eq) $
                if eq then Just (Var tmp) else Nothing

        (haystack', repl) = replace check haystack

    return $ if repl > 0
        then Lam tmp haystack'
        else haystack


--------------------------------------------------------------------------------
-- | Generate a fresh variable
freshVar :: Type -> CoreM Var
freshVar typ = do
    unique <- Unique.getUniqueM
    let occn = OccName.mkVarOcc $ "wm_" ++ show (Unique.getKey unique)
        name = Name.mkInternalName unique occn SrcLoc.noSrcSpan
        -- var  = Var.mkLocalVar IdInfo.VanillaId name typ IdInfo.vanillaIdInfo
        var  = Var.mkCoVar name typ
    return var


--------------------------------------------------------------------------------
class SynEq a where
    (.==.) :: a -> a -> Bool
    infix 4 .==.


--------------------------------------------------------------------------------
instance SynEq Var where
    x .==. y =
        Name.getOccName (Var.varName x) == Name.getOccName (Var.varName y)


--------------------------------------------------------------------------------
instance SynEq AltCon where
    x .==. y = x == y


--------------------------------------------------------------------------------
instance SynEq Type where
    -- If an entire syntax tree is equal, the types must be the same as well?
    _ .==. _ = True


--------------------------------------------------------------------------------
instance SynEq b => SynEq (Expr b) where
    Var x1 .==. Var x2 = x1 .==. x2
    Lit x1 .==. Lit x2 = x1 == x2
    App f1 a1 .==. App f2 a2 = f1 .==. f2 && a1 .==. a2
    Lam b1 e1 .==. Lam b2 e2 = b1 .==. b2 && e1 .==. e2
    Let b1 e1 .==. Let b2 e2 = b1 .==. b2 && e1 .==. e2
    Case e1 b1 t1 as1 .==. Case e2 b2 t2 as2 =
        e1 .==. e2 && b1 .==. b2 && t1 .==. t2 && as1 .==. as2
    Type _ .==. Type _ = True
    Coercion _ .==. Coercion _ = True

    c1@(Cast e1 _) .==. c2@(Cast e2 _) = e1 .==. e2 || c1 .==. e2 || e1 .==. c2
    t1@(Tick _ e1) .==. t2@(Tick _ e2) = e1 .==. e2 || t1 .==. e2 || e1 .==. t2

    _ .==. _ = False


--------------------------------------------------------------------------------
instance SynEq b => SynEq (Bind b) where
    NonRec b1 e1 .==. NonRec b2 e2 = b1 .==. b2 && e1 .==. e2
    Rec es1      .==. Rec es2      = es1 .==. es2
    _            .==. _            = False


--------------------------------------------------------------------------------
instance (SynEq a, SynEq b) => SynEq (a, b) where
    (x1, y1) .==. (x2, y2) = x1 .==. x2 && y1 .==. y2


--------------------------------------------------------------------------------
instance (SynEq a, SynEq b, SynEq c) => SynEq (a, b, c) where
    (x1, y1, z1) .==. (x2, y2, z2) = x1 .==. x2 && y1 .==. y2 && z1 .==. z2


--------------------------------------------------------------------------------
instance SynEq a => SynEq [a] where
    []       .==. []       = True
    (x : xs) .==. (y : ys) = x .==. y && xs .==. ys
    _        .==. _        = False


--------------------------------------------------------------------------------
-- | Since we love folds...
-- TODO: Delete?
foldExpr
    :: (Id -> a)                                    -- ^ Var
    -> (Literal -> a)                               -- ^ Lit
    -> (a -> a -> a)                                -- ^ App
    -> (b -> a -> a)                                -- ^ Lam
    -> (b -> a -> a -> a)                           -- ^ Bind NonRec
    -> ([(b, a)] -> a -> a)                         -- ^ Bind Rec
    -> (a -> b -> Type -> [(AltCon, [b], a)] -> a)  -- ^ Case
    -> (a -> Coercion -> a)                         -- ^ Cast
    -> (Tickish Id -> a -> a)                       -- ^ Tick
    -> (Type -> a)                                  -- ^ Type
    -> (Coercion -> a)                              -- ^ Coercion
    -> Expr b                                       -- ^ Expr to fold over
    -> a                                            -- ^ Result
foldExpr var lit app lam bnrec brec cas cast' tick typ coer = go
  where
    go (Var x)         = var x
    go (Lit x)         = lit x
    go (App f x)       = app (go f) (go x)
    go (Lam a x)       = lam a (go x)
    go (Let b e)       = case b of
        NonRec b' e' -> bnrec b' (go e') (go e)
        Rec bs       -> brec [(b', go e') | (b', e') <- bs] (go e)
    go (Case e b t as) = cas (go e) b t [(ac, bs, go e') | (ac, bs, e') <- as]
    go (Cast e c)      = cast' (go e) c
    go (Tick t e)      = tick t (go e)
    go (Type t)        = typ t
    go (Coercion c)    = coer c
