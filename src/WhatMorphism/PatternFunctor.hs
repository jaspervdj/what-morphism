--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module WhatMorphism.PatternFunctor
    ( Fold (..)
    , Algebra
    , fold
    ) where


--------------------------------------------------------------------------------
class Functor (PF t) => Fold t where
    type PF t :: * -> *

    toPF   :: t -> PF t t
    fromPF :: PF t t -> t


--------------------------------------------------------------------------------
type Algebra t a = PF t a -> a


--------------------------------------------------------------------------------
fold :: Fold t => Algebra t a -> t -> a
fold alg = alg . fmap (fold alg) . toPF


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
    type PF [a] = ListF a

    toPF []       = NilF
    toPF (x : xs) = ConsF x xs

    fromPF NilF         = []
    fromPF (ConsF x xs) = x : xs


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
    type PF (Tree a) = TreeF a

    toPF Empty          = EmptyF
    toPF (Branch x l r) = BranchF x l r

    fromPF EmptyF          = Empty
    fromPF (BranchF x l r) = Branch x l r


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
