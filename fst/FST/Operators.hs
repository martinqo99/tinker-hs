module FST.Operators ((<$>), (<->>)) where

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

