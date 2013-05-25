--------------------------------------------------------------------------------
module WhatMorphism.AnyBool
    ( AnyBool (..)
    ) where


--------------------------------------------------------------------------------
import           Data.Monoid (Monoid (..))


--------------------------------------------------------------------------------
newtype AnyBool = AnyBool {unAnyBool :: Bool} deriving (Eq, Show)


--------------------------------------------------------------------------------
instance Monoid AnyBool where
    mempty                          = AnyBool False
    mappend (AnyBool x) (AnyBool y) = AnyBool (x || y)
