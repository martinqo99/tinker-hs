-- An interpreter for the esoteric brainf*ck programming language.
--
-- Copyright (c) 2010 Daniel de Kok <me@danieldk.eu>
-- Copyright (c) 2010 Harm Brouwer <harm.brouwer@rug.nl>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

make_data :: Int -> [Int]
make_data len = [0|_ <- [1..len]]

data BFInstr = PtrLeft | PtrRight | Incr | Decr | JumpFwd | JumpBwd deriving Show

interpret :: [BFInstr] -> [BFInstr] -> [Int] -> [Int] -> [Int]
interpret [] _ rData lData = (reverse lData) ++ rData
interpret (PtrLeft:ris) lil rdl (ld:lds) = interpret ris (PtrLeft:lil) (ld:rdl) lds
interpret ril@(ri:ris) lil rdl@(rd:rds) ldl =
    case ri of
      Incr     -> interpret ris (ri:lil) (succ rd:rds) ldl
      Decr     -> interpret ris (ri:lil) (pred rd:rds) ldl
      PtrRight -> interpret ris (ri:lil) rds (rd:ldl)
      JumpFwd  -> if rd == 0 then
                      interpret newRil newLil rdl ldl
                  else
                      interpret ris (ri:lil) rdl ldl
                  where (newRil, newLil) = jump_fwd ris (ri:lil) 0
      JumpBwd  -> if rd == 0 then
                      interpret ris (ri:lil) rdl ldl
                  else
                      interpret newRil newLil rdl ldl
                  where (newLil, newRil) = jump_bwd lil ril 0
interpret _ _ _ _ = error "no"

jump_fwd :: [BFInstr] -> [BFInstr] -> Int -> ([BFInstr], [BFInstr])
jump_fwd (JumpBwd:ri) li n
    | n == 0 = (ri, JumpBwd:li)
    | otherwise = jump_fwd ri (JumpBwd:li) (pred n)
jump_fwd (JumpFwd:ri) li n = jump_fwd ri (JumpFwd:li) (succ n)
jump_fwd (i:ri) li n = jump_fwd ri (i:li) n

jump_bwd :: [BFInstr] -> [BFInstr] -> Int ->([BFInstr], [BFInstr])
jump_bwd li@(JumpFwd:xs) ri n
    | n == 0 = (li, ri)
    | otherwise = jump_bwd xs (JumpFwd:ri) (pred n)
jump_bwd (JumpBwd:li) ri n = jump_bwd li (JumpBwd:ri) (succ n)
jump_bwd (i:li) ri n = jump_bwd li (i:ri) n
