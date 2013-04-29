--------------------------------------------------------------------------------
module WhatMorphism.Dump
    ( Dump (..)
    ) where


--------------------------------------------------------------------------------
import           Coercion  (Coercion)
import           CoreSyn
import           Data.List (intercalate)
import           DataCon   (DataCon)
import           Literal   (Literal)
import           Name      (Name)
import qualified Name      as Name
import           OccName   (OccName)
import qualified OccName   as OccName
import           TyCon     (TyCon)
import qualified TyCon     as TyCon
import           TypeRep   (Type)
import           Var       (Var)
import qualified Var       as Var


--------------------------------------------------------------------------------
class Dump a where
    dump :: a -> String


--------------------------------------------------------------------------------
instance (Dump a, Dump b) => Dump (a, b) where
    dump (x, y) = "(" ++ dump x ++ ", " ++ dump y ++ ")"


--------------------------------------------------------------------------------
instance (Dump a, Dump b, Dump c) => Dump (a, b, c) where
    dump (x, y, z) = "(" ++ dump x ++ ", " ++ dump y ++ ", " ++ dump z ++ ")"


--------------------------------------------------------------------------------
instance Dump a => Dump [a] where
    dump xs = "[" ++ intercalate ", " (map dump xs) ++ "]"


--------------------------------------------------------------------------------
instance Dump a => Dump (Maybe a) where
    dump Nothing  = "Nothing"
    dump (Just x) = "(Just " ++ dump x ++ ")"


--------------------------------------------------------------------------------
instance Dump Name where
    dump name = OccName.occNameString (Name.getOccName name) ++ "_" ++
        show (Name.nameUnique name)


--------------------------------------------------------------------------------
instance Dump OccName where
    dump = OccName.occNameString


--------------------------------------------------------------------------------
instance Dump Var where
    dump = dump . Var.varName


--------------------------------------------------------------------------------
instance Dump DataCon where
    dump = OccName.occNameString . Name.getOccName


--------------------------------------------------------------------------------
instance Dump TyCon where
    dump tc = "@" ++ dump (TyCon.tyConName tc)


--------------------------------------------------------------------------------
instance Dump AltCon where
    dump (DataAlt x) = dc "DataAlt" [dump x]
    dump (LitAlt x)  = dc "Literal" [dump x]
    dump DEFAULT     = dc "DEFAULT" []


--------------------------------------------------------------------------------
instance Dump Literal where
    dump _ = "<Literal>"


--------------------------------------------------------------------------------
instance Dump Coercion where
    dump _ = "<Coercion>"


--------------------------------------------------------------------------------
instance Dump Type where
    dump _ = "<Type>"


--------------------------------------------------------------------------------
instance Dump (Tickish a) where
    dump _ = "<Tickish>"


--------------------------------------------------------------------------------
instance Dump b => Dump (Bind b) where
    dump (NonRec x y) = dc "NonRec" [dump x, dump y]
    dump (Rec x)      = dc "Rec" [dump x]


--------------------------------------------------------------------------------
instance Dump b => Dump (Expr b) where
    dump (Var x)        = dc "Var"      [dump x]
    dump (Lit x)        = dc "Lit"      [dump x]
    dump (App x y)      = dc "App"      [dump x, dump y]
    dump (Lam x y)      = dc "Lam"      [dump x, dump y]
    dump (Let x y)      = dc "Let"      [dump x, dump y]
    dump (Case x y z w) = dc "Case"     [dump x, dump y, dump z, dump w]
    dump (Cast x y)     = dc "Cast"     [dump x, dump y]
    dump (Tick x y)     = dc "Tick"     [dump x, dump y]
    dump (Type x)       = dc "Type"     [dump x]
    dump (Coercion x)   = dc "Coercion" [dump x]


--------------------------------------------------------------------------------
dc :: String -> [String] -> String
dc constr []   = constr
dc constr args = "(" ++ constr ++ " " ++ intercalate " " args ++ ")"
