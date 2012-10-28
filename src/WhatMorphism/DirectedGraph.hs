--------------------------------------------------------------------------------
module WhatMorphism.DirectedGraph
    ( DirectedGraph

    , fromNode
    , fromEdge
    , fromEdges

    , nodes
    , neighbours

    , delete
    ) where


--------------------------------------------------------------------------------
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Maybe  (fromMaybe)
import           Data.Monoid (Monoid (..))
import           Data.Set    (Set)
import qualified Data.Set    as S


--------------------------------------------------------------------------------
newtype DirectedGraph a = DirectedGraph
    { unDirectedGraph :: Map a (Set a)
    } deriving (Show)


--------------------------------------------------------------------------------
instance Ord a => Monoid (DirectedGraph a) where
    mempty      = DirectedGraph M.empty
    mappend x y = DirectedGraph $
        M.unionWith S.union (unDirectedGraph x) (unDirectedGraph y)


--------------------------------------------------------------------------------
fromNode :: a -> DirectedGraph a
fromNode x = DirectedGraph $ M.singleton x S.empty


--------------------------------------------------------------------------------
fromEdge :: a -> a -> DirectedGraph a
fromEdge x y = fromEdges x (S.singleton y)


--------------------------------------------------------------------------------
fromEdges :: a -> Set a -> DirectedGraph a
fromEdges x ys = DirectedGraph $ M.singleton x ys


--------------------------------------------------------------------------------
nodes :: DirectedGraph a -> Set a
nodes = M.keysSet . unDirectedGraph


--------------------------------------------------------------------------------
neighbours :: Ord a => a -> DirectedGraph a -> Set a
neighbours x (DirectedGraph m) = fromMaybe S.empty $ M.lookup x m


--------------------------------------------------------------------------------
-- | Expensive...
delete :: Ord a => a -> DirectedGraph a -> DirectedGraph a
delete x (DirectedGraph m) = DirectedGraph $ M.map (S.delete x) $ M.delete x m
