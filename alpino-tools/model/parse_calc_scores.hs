module Main where

import Prelude hiding (lines)
import Data.Maybe (fromJust)
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lex.Double (readDouble)
import qualified Data.ByteString.UTF8 as BU
import System.IO (isEOF)

countsToScore :: (Double, Double, Double) -> Double
countsToScore (ovl, cor, sys) =
    if n > 0 then
        1.0 - pen / n
    else
        1.0
    where n = max cor sys
          pen = n - ovl

splitLine :: B.ByteString -> [B.ByteString]
splitLine l = B.split (c2w '#') l

getCounts :: B.ByteString -> (Double, Double, Double)
getCounts =  toTuple . bs2double . B.split (c2w '|')
    where bs2double = map (fst . fromJust . readDouble)
          toTuple [ovl, cor, sys] = (ovl, cor, sys)

scoreLine :: B.ByteString -> B.ByteString
scoreLine line = B.append  key . B.append sep . B.append n . B.append sep .
                 B.append score $ B.append sep features
    where lineParts = splitLine line
          score = BU.fromString . show . countsToScore . getCounts $ lineParts !! 2
          key = lineParts !! 0
          n = lineParts !! 1
          features = lineParts !! 3
          sep = BU.fromString "#"

lines :: B.ByteString -> [B.ByteString]
lines ps
    | B.null ps = []
    | otherwise = case search ps of
                    Nothing -> [ps]
                    Just n  -> B.take n ps : lines (B.drop (n+1) ps)
    where search = B.elemIndex $ c2w '\n'

processLines :: IO ()
processLines = do
  eof <- isEOF
  if eof
     then return()
     else do
       B.getLine >>= putStrLn . BU.toString . scoreLine
       processLines

main = do
  processLines