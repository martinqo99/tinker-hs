-- |
-- Module      : Language.Brainfuck 
-- Copyright   : (c) 2010 Daniël de Kok and Harm Brouwer
-- License     : BSD3
--
-- An interpreter for the esoteric brainf*ck programming language.

module Language.Brainfuck (bfFromString, interpret) where

import Data.List.Zipper as ListZipper

make_data :: Int -> ListZipper.Zipper Int
make_data len = ListZipper.fromList [0|_ <- [1..len]]

data BFInstr = PtrLeft | PtrRight | Incr | Decr | Input | Output | JumpFwd | JumpBwd deriving Show

instance Read BFInstr where
    readsPrec _ value = 
        tryParse [("<", PtrLeft), (">", PtrRight), ("+", Incr), ("-", Decr), (",", Input),
                                    (".", Output), ("[", JumpFwd), ("]", JumpBwd)]
        where tryParse [] = []
              tryParse ((attempt, result):xs) =
                      if (take (length attempt) value) == attempt
                         then [(result, drop (length attempt) value)]
                         else tryParse xs

data BFState = BFState [Int] [Int] | Fail deriving Show

bfFromString :: String -> [BFInstr]
bfFromString = map (\c -> (read [c])::BFInstr)

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

