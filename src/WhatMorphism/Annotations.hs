--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module WhatMorphism.Annotations
    ( RegisterFoldBuild (..)
    ) where


--------------------------------------------------------------------------------
import           Data.Data     (Data)
import           Data.Typeable (Typeable)


--------------------------------------------------------------------------------
data RegisterFoldBuild = RegisterFoldBuild
    { registerFold  :: String
    , registerBuild :: String
    } deriving (Data, Typeable, Show)
