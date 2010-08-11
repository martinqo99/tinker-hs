module FST.OperatorsT ((<$>), (<->>), (<=>>)) where

import FST.TransducerInterface
import FST.Operators

-- |
-- Replacement: build a transducer that replaces 'upper' by 'lower'.
(<->>) :: Eq a => Reg a -> Reg a -> RReg a
(<->>) upper lower = star (idR (complement ((<$>) (upper <-> eps)))
                               |> (upper <*> lower))
                   |> idR (complement ((<$>) (upper <-> eps)))

