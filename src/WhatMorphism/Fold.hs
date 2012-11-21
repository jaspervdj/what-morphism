--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeFamilies     #-}
module WhatMorphism.Fold
    ( Fold (..)
    , Algebra
    , fold
    ) where


--------------------------------------------------------------------------------
class Functor (Recursive t) => Fold t where
    type Recursive t :: * -> *

    out :: t -> Recursive t t


--------------------------------------------------------------------------------
type Algebra t a = Fold t => Recursive t a -> a


--------------------------------------------------------------------------------
fold :: Fold t => Algebra t a -> t -> a
fold alg = alg . fmap (fold alg) . out


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
    type Recursive [a] = ListF a

    out []       = NilF
    out (x : xs) = ConsF x xs


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
    type Recursive (Tree a) = TreeF a

    out Empty          = EmptyF
    out (Branch x l r) = BranchF x l r


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
