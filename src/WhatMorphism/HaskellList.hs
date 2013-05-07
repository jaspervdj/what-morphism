--------------------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}
module WhatMorphism.HaskellList
    ( foldHaskellList
    , buildHaskellList
    , haskellListRegister
    ) where


--------------------------------------------------------------------------------
import qualified TysWiredIn               as TysWiredIn
import           UniqFM                   (UniqFM)
import qualified UniqFM                   as UniqFM


--------------------------------------------------------------------------------
import           WhatMorphism.Annotations


--------------------------------------------------------------------------------
-- NOTE: In theory, we could use something like
--
-- > $(deriveFold ''[] "foldHaskellList")
-- > $(deriveBuild ''[] "buildHaskellList")
--
-- here. However, the Template Haskell code can't deal very well with the
-- wired-in list type. In particular, we see two types:
--
-- > ListT
-- > ConT GHC.Types.[]
--
-- which is why can't figure out the recursive subterm correctly.


--------------------------------------------------------------------------------
foldHaskellList :: forall a b. b -> (a -> b -> b) -> [a] -> b
foldHaskellList nil _    []       = nil
foldHaskellList nil cons (x : xs) = cons x (foldHaskellList nil cons xs)


--------------------------------------------------------------------------------
buildHaskellList :: forall a. (forall b. b -> (a -> b -> b) -> b) -> [a]
buildHaskellList g = g [] (:)


--------------------------------------------------------------------------------
haskellListRegister :: UniqFM RegisterFoldBuild
haskellListRegister = UniqFM.unitUFM TysWiredIn.listTyCon
    (RegisterFoldBuild "foldHaskellList" "buildHaskellList")
