--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module WhatMorphism.Annotations
    ( RegisterFoldBuild (..)
    ) where


--------------------------------------------------------------------------------
import           Data.Data           (Data)
import           Data.Typeable       (Typeable)
import qualified Language.Haskell.TH as TH


--------------------------------------------------------------------------------
data RegisterFoldBuild = RegisterFoldBuild
    { registerFold  :: TH.Name
    , registerBuild :: TH.Name
    } deriving (Data, Typeable, Show)
