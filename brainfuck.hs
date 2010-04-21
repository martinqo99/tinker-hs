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

import Data.List.Zipper as ListZipper

make_data :: Int -> ListZipper.Zipper Int
make_data len = ListZipper.fromList [0|_ <- [1..len]]

data BFInstr = PtrLeft | PtrRight | Incr | Decr | Input | Output | JumpFwd | JumpBwd deriving Show
data BFState = BFState [Int] [Int] | Fail deriving Show

interpret :: ListZipper.Zipper BFInstr -> ListZipper.Zipper Int -> [Int] -> [Int] -> BFState
interpret instr dat inp outp
    | ListZipper.endp instr = BFState (ListZipper.toList dat) (reverse outp)
    | otherwise = case curInstr of
                    Incr     -> interpret nextInstr (ListZipper.replace (succ curData) dat) inp outp
                    Decr     -> interpret nextInstr (ListZipper.replace (pred curData) dat) inp outp
                    PtrLeft  -> interpret nextInstr (ListZipper.left dat) inp outp
                    PtrRight -> interpret nextInstr (ListZipper.right dat) inp outp
                    Input    -> interpret nextInstr (ListZipper.replace (head inp) dat) (tail inp) outp
                    Output   -> interpret nextInstr dat inp (curData:outp)
                    JumpFwd  -> if curData == 0 then
                                    interpret newInstr dat inp outp
                                else
                                    interpret nextInstr dat inp outp
                                where newInstr = jump_fwd nextInstr 0
                    JumpBwd  -> if curData == 0 then
                                    interpret nextInstr dat inp outp
                                else
                                    interpret newInstr dat inp outp
                                where newInstr = jump_bwd instr 0
    where
      curInstr = ListZipper.cursor instr
      nextInstr = ListZipper.right instr
      prevInstr = ListZipper.left instr
      curData = ListZipper.cursor dat

jump_fwd :: ListZipper.Zipper BFInstr -> Int -> ListZipper.Zipper BFInstr
jump_fwd instr n =
    case curInstr of
      JumpBwd -> if n == 0 then
                     nextInstr
                 else
                     jump_fwd nextInstr (pred n)
      JumpFwd -> jump_fwd nextInstr (succ n)
      _ -> jump_fwd nextInstr n
    where curInstr = ListZipper.cursor instr
          nextInstr = ListZipper.right instr

jump_bwd :: ListZipper.Zipper BFInstr -> Int -> ListZipper.Zipper BFInstr
jump_bwd instr n =
    case curInstr of
      JumpFwd -> if n == 0 then
                     nextInstr
                 else
                     jump_bwd prevInstr (pred n)
      JumpBwd -> jump_bwd prevInstr (succ n)
      _ -> jump_bwd prevInstr n
    where curInstr = ListZipper.cursor instr
          nextInstr = ListZipper.right instr
          prevInstr = ListZipper.left instr

