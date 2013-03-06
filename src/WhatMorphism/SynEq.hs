--------------------------------------------------------------------------------
-- | Syntactic equality
module WhatMorphism.SynEq
    ( SynEq (..)
    ) where


--------------------------------------------------------------------------------
import           CoreSyn
import qualified Name    as Name
import           Type    (Type)
import qualified Type    as Type
import           Var     (Var)
import qualified Var     as Var


--------------------------------------------------------------------------------
class SynEq a where
    (.==.) :: a -> a -> Bool
    infix 4 .==.


--------------------------------------------------------------------------------
instance SynEq Var where
    x .==. y =
        Name.nameUnique (Var.varName x) == Name.nameUnique (Var.varName y)


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

    -- In some cases, a type can equal a variable
    Var v .==. Type t = case Type.getTyVar_maybe t of
        Just v' -> v .==. v'
        Nothing -> False
    t@(Type _) .==. v@(Var _) = v .==. t

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
