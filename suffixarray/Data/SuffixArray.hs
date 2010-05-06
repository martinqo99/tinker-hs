-- |
-- Module      : Data.SuffixArray
-- Copyright   : (c) 2010 Daniël de Kok
-- License     : Apache 2
--
-- Maintainer  : Daniël de Kok <me@danieldk.eu>
-- Stability   : experimental
--
-- Construction of suffix arrays (arrays ordered by suffix).
--

module Data.SuffixArray (SuffixArray(..),
                         fromList,
                         suffixArray,
                         toList) where

import qualified Data.Vector as V
import Data.List (sortBy)

data SuffixArray a = SuffixArray (V.Vector a) (V.Vector Int)
                 deriving Show

elems :: SuffixArray a -> V.Vector (V.Vector a)
elems (SuffixArray d i) = V.map vecAt i
    where vecAt idx = V.drop idx d

fromList :: Ord a => [a] -> SuffixArray a
fromList = suffixArray . V.fromList

suffixArray :: Ord a => V.Vector a -> SuffixArray a
suffixArray d = SuffixArray d (V.fromList srtIndex)
    where uppBound = V.length d - 1
          usrtIndex = [0..uppBound]
          srtIndex = sortBy (saCompare d) usrtIndex

saCompare :: Ord a => V.Vector a -> Int -> Int -> Ordering
saCompare d a b = compare (V.drop a d) (V.drop b d)

toList :: SuffixArray a -> [[a]]
toList (SuffixArray d i) = V.foldr vecAt [] i
    where vecAt idx l = V.toList (V.drop idx d) : l 

-- sample1 = V.fromList [9,8,7,6,5,4,3,2,1]
-- sample2 = V.fromList "abaa"