--------------------------------------------------------------------------------
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}
module WhatMorphism.HaskellList
    ( foldHaskellList
    , buildHaskellList
    , haskellListRegister
    ) where


--------------------------------------------------------------------------------
import qualified TysWiredIn                   as TysWiredIn
import           UniqFM                       (UniqFM)
import qualified UniqFM                       as UniqFM


--------------------------------------------------------------------------------
import           WhatMorphism.Annotations
import           WhatMorphism.TemplateHaskell


--------------------------------------------------------------------------------
$(deriveFold ''[] "foldHaskellList")
$(deriveBuild ''[] "buildHaskellList")


--------------------------------------------------------------------------------
haskellListRegister :: UniqFM RegisterFoldBuild
haskellListRegister = UniqFM.unitUFM TysWiredIn.listTyCon
    (RegisterFoldBuild "foldHaskellList" "buildHaskellList")
