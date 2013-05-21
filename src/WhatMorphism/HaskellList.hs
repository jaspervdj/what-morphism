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
foldHaskellList nil cons = foldHaskellList_go
  where
    foldHaskellList_go []       = nil
    foldHaskellList_go (x : xs) = cons x (foldHaskellList_go xs)
{-# INLINE [0] foldHaskellList #-}


--------------------------------------------------------------------------------
buildHaskellList :: forall a. (forall b. b -> (a -> b -> b) -> b) -> [a]
buildHaskellList g = g [] (:)
{-# INLINE [0] buildHaskellList #-}


--------------------------------------------------------------------------------
haskellListRegister :: UniqFM RegisterFoldBuild
haskellListRegister = UniqFM.unitUFM TysWiredIn.listTyCon
    (RegisterFoldBuild "foldHaskellList" "buildHaskellList")
