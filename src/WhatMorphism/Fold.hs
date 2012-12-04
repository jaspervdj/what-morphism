--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module WhatMorphism.Fold
    ( Fold (..)
    , Algebra
    , fold
    ) where


--------------------------------------------------------------------------------
class Functor (RecF t) => Fold t where
    type RecF t :: * -> *

    toRecF   :: t -> RecF t t
    fromRecF :: RecF t t -> t


--------------------------------------------------------------------------------
type Algebra t a = RecF t a -> a


--------------------------------------------------------------------------------
fold :: Fold t => Algebra t a -> t -> a
fold alg = alg . fmap (fold alg) . toRecF


--------------------------------------------------------------------------------
data ListF a t
    = NilF
    | ConsF a t
    deriving (Show)


--------------------------------------------------------------------------------
instance Functor (ListF a) where
    fmap _ NilF         = NilF
    fmap f (ConsF x xs) = ConsF x (f xs)


--------------------------------------------------------------------------------
instance Fold [a] where
    type RecF [a] = ListF a

    toRecF []       = NilF
    toRecF (x : xs) = ConsF x xs

    fromRecF NilF         = []
    fromRecF (ConsF x xs) = x : xs


--------------------------------------------------------------------------------
sumListAlg :: Num a => Algebra [a] a
sumListAlg NilF        = 0
sumListAlg (ConsF x y) = x + y


--------------------------------------------------------------------------------
testList :: Int
testList = fold sumListAlg [1 .. 10]


--------------------------------------------------------------------------------
data Tree a
    = Empty
    | Branch a (Tree a) (Tree a)
    deriving (Show)


--------------------------------------------------------------------------------
data TreeF a t
    = EmptyF
    | BranchF a t t
    deriving (Show)


--------------------------------------------------------------------------------
instance Functor (TreeF a) where
    fmap _ EmptyF          = EmptyF
    fmap f (BranchF x l r) = BranchF x (f l) (f r)


--------------------------------------------------------------------------------
instance Fold (Tree a) where
    type RecF (Tree a) = TreeF a

    toRecF Empty          = EmptyF
    toRecF (Branch x l r) = BranchF x l r

    fromRecF EmptyF          = Empty
    fromRecF (BranchF x l r) = Branch x l r


--------------------------------------------------------------------------------
sumTreeAlg :: Num a => Algebra (Tree a) a
sumTreeAlg EmptyF          = 0
sumTreeAlg (BranchF x y z) = x + y + z


--------------------------------------------------------------------------------
testTree :: Int
testTree = fold sumTreeAlg $
    Branch 1
        (Branch 2
            Empty
            (Branch 4 Empty Empty))
        (Branch 1
            (Branch 5
                (Branch 2 Empty Empty)
                Empty)
            (Branch 0 Empty Empty))
