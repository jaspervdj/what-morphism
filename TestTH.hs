--------------------------------------------------------------------------------
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}
import           WhatMorphism.TemplateHaskell


--------------------------------------------------------------------------------
data List a
    = Cons a (List a)
    | Empty
    deriving (Show)


--------------------------------------------------------------------------------
$(deriveFold ''List "foldList")
$(deriveBuild ''List "buildList")


--------------------------------------------------------------------------------
buildListTest :: List Int
buildListTest = buildList $ \cons nil ->
    cons 4 $ cons 3 $ cons 2 $ cons 1 nil


--------------------------------------------------------------------------------
data Tree a
    = Leaf a
    | Node (Tree a) (Tree a)
    deriving (Show)


--------------------------------------------------------------------------------
$(deriveFold ''Tree "foldTreeHerp")
$(deriveBuild ''Tree "buildTree")


--------------------------------------------------------------------------------
foldTreeTest :: Int
foldTreeTest = foldTreeHerp id (+) buildTreeTest


--------------------------------------------------------------------------------
buildTreeTest :: Tree Int
buildTreeTest = buildTree $ \leaf node ->
    node
        (leaf 2)
        (node
            (leaf 7)
            (leaf 8))
