module FST.Operators ((<$>), (<=>>)) where

import FST.TransducerInterface

-- |
-- Containment: recognize strings that contains the expression 'r'
-- at least once.
(<$>) :: Eq a => Reg a -> Reg a
(<$>) r = anyS |> r |> anyS

-- |
-- Restriction: recognize a string only if every instance of 'a' is
-- preceded by 'l' and succeeded by 'r'.
(<=>>) :: (Eq a) => Reg a -> (Reg a, Reg a) -> Reg a
a <=>> (l, r) = complement $
                (complement (anyS |> l) |> a |> anyS) <|>
                (anyS |> a |> complement (r |> anyS))

anyS :: Eq a => Reg a
anyS = star allS