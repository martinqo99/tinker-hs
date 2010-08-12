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
-- Inverse the upper and lower language of a transducer.
inverse :: Eq a => RReg a -> RReg a
inverse (Cross r1 r2) = Cross r2 r1
inverse (Comp r1 r2) = Comp (inverse r1) (inverse r2)
inverse (ProductR r1 r2) = Comp (inverse r1) (inverse r2)
inverse (UnionR r1 r2) = Comp (inverse r1) (inverse r2)
inverse (StarR r) = StarR $ inverse r
inverse (Identity r) = Identity r
inverse (Relation s1 s2) = Relation s2 s1
inverse EmptyR = EmptyR

-- |
-- Replacement: build a transducer that replaces 'upper' by 'lower'.
(<->>) :: Eq a => Reg a -> Reg a -> RReg a
(<->>) upper lower = star (idR (complement $ (<$>) $ upper <-> eps)
                               |> (upper <*> lower))
                   |> idR (complement $ (<$>) $ upper <-> eps)
