{-# LANGUAGE BangPatterns #-}

module Data.Set.BKTree.Strict (
	insert'
) where

import qualified Data.IntMap as M
import Data.Set.BKTree
import Data.Set.BKTree.Internal

-- | Inserts an element into the tree. If an element is inserted several times
--   it will be stored several times.
insert' :: Metric a => a -> BKTree a -> BKTree a
insert' a Empty = Node a 1 M.empty
insert' a (Node b n dtrs) = Node b (n + 1) dtrs'
  where !dtrs' = M.insertWith' recurse d (Node a 1 M.empty) dtrs
        d    = distance a b
        recurse _ tree = insert' a tree