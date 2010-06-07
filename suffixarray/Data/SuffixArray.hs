-- |
-- Module      : Data.SuffixArray
-- Copyright   : (c) 2010 Daniël de Kok
-- License     : BSD3
--
-- Maintainer  : Daniël de Kok <me@danieldk.eu>
-- Stability   : experimental
--
-- Construction of suffix arrays (arrays ordered by suffix). Given an
-- array /d/ elements, the suffix array is a sorted array of the sub-arrays
-- in /d/. For instance, the suffix array of /banana apple pear apple/ is:
--
-- * apple
-- 
-- * apple pear apple
-- 
-- * banana apple pear apple
-- 
-- * pear apple

module Data.SuffixArray (SuffixArray(..),
                         fromList,
                         suffixArray,
                         suffixArrayBy,
                         toList) where

import qualified Data.Vector as V
import Data.List (sortBy)

data SuffixArray a = SuffixArray (V.Vector a) (V.Vector Int)
                 deriving Show

-- |
-- 'elems' provides a vector of each element in the suffix array. One element
-- of the suffix array contains the full data array.
elems :: SuffixArray a -> V.Vector (V.Vector a)
elems (SuffixArray d i) = V.map vecAt i
    where vecAt idx = V.drop idx d

-- |
-- 'fromList' constructs a suffix array from a list of elements.
fromList :: Ord a => [a] -> SuffixArray a
fromList = suffixArray . V.fromList

-- |
-- 'suffixArray' is a specialization of 'suffixArrayBy' that uses the
-- default 'Prelude.compare' function.
suffixArray :: Ord a => V.Vector a -> SuffixArray a
suffixArray = suffixArrayBy compare

-- |
-- 'suffixArrayBy' constructs a suffix array. The sorting order is determined
-- by the supplied compare function.
suffixArrayBy :: Ord a => (V.Vector a -> V.Vector a -> Ordering) ->
                 V.Vector a -> SuffixArray a
suffixArrayBy cmp d = SuffixArray d (V.fromList srtIndex)
    where uppBound = V.length d - 1
          usrtIndex = [0..uppBound]
          srtIndex = sortBy (saCompare cmp d) usrtIndex

saCompare :: Ord a => (V.Vector a -> V.Vector a -> Ordering) ->
             V.Vector a -> Int -> Int -> Ordering
saCompare cmp d a b = cmp (V.drop a d) (V.drop b d)

-- |
-- 'toList' constructs a list from a suffix array.
toList :: SuffixArray a -> [[a]]
toList (SuffixArray d i) = V.foldr vecAt [] i
    where vecAt idx l = V.toList (V.drop idx d) : l 

-- sample1 = V.fromList [9,8,7,6,5,4,3,2,1]
-- sample2 = V.fromList "abaa"