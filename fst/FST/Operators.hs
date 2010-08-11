module FST.Operators ((<$>), (<->>), (<=>>)) where

import FST.TransducerInterface

-- |
-- Containment: recognize strings that contains the expression 'r'
-- at least once.
(<$>) :: Eq a => Reg a -> Reg a
(<$>) r = star allS |> r |> star allS


-- |
-- Replacement: build a transducer that replaces 'upper' by 'lower'.
(<->>) :: Eq a => Reg a -> Reg a -> RReg a
(<->>) upper lower = star (idR (complement ((<$>) (upper <-> eps)))
                               |> (upper <*> lower))
                   |> idR (complement ((<$>) (upper <-> eps)))

-- |
-- Restriction: recognize a string only if every instance of 'a' is
-- preceded by 'l' and succeeded by 'r'.
(<=>>) :: (Eq a) => Reg a -> (Reg a, Reg a) -> Reg a
a <=>> (l, r) = complement
                ((complement ((star allS) |> l) |> a |> (star allS)) <|>
                 ((star allS) |> a |> complement (r |> star allS)))
