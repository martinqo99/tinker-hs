module FST.OperatorsT ((<$>), (<->>), (<=>>), Marker(..)) where

import FST.TransducerInterface
import FST.Operators

-- |
-- Instances of TMarker can return markers for special use for implemented
-- operators.
class Marker t where
    giveMarker :: Int -> Reg t

-- |
-- Implementation of markers for 'Char': '<' followed by 'n' percent
-- signs.
instance Marker Char where
    giveMarker n = s '<' |> foldl (|>) eps (take n $ repeat $ s '%')

-- |
-- Replacement: build a transducer that replaces 'upper' by 'lower'.
(<->>) :: Eq a => Reg a -> Reg a -> RReg a
(<->>) upper lower = star (idR (complement $ (<$>) $ upper <-> eps)
                               |> (upper <*> lower))
                   |> idR (complement $ (<$>) $ upper <-> eps)
