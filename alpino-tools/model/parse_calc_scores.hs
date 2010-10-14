module Main where

import Prelude hiding (lines)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lex.Lazy.Double (readDouble)
import qualified Data.ByteString.Lazy.UTF8 as LU
import System.Environment (getArgs)
import System.Console.GetOpt

countsToScore :: (Double, Double, Double) -> Double
countsToScore (ovl, cor, sys) =
    if n > 0 then
        1.0 - pen / n
    else
        1.0
    where n = max cor sys
          pen = n - ovl

splitLine :: L.ByteString -> [L.ByteString]
splitLine l = L.split (c2w '#') l

getCounts :: L.ByteString -> (Double, Double, Double)
getCounts =  toTuple . bs2double . L.split (c2w '|')
    where bs2double = map (fst . fromJust . readDouble)
          toTuple [ovl, cor, sys] = (ovl, cor, sys)

scoreLine :: L.ByteString -> L.ByteString
scoreLine line = L.append  key . L.append sep . L.append n . L.append sep .
                 L.append score $ L.append sep features
    where lineParts = splitLine line
          score = LU.fromString . show . countsToScore . getCounts $ lineParts !! 2
          key = lineParts !! 0
          n = lineParts !! 1
          features = lineParts !! 3
          sep = LU.fromString "#"

lines :: L.ByteString -> [L.ByteString]
lines ps
    | L.null ps = []
    | otherwise = case search ps of
                    Nothing -> [ps]
                    Just n  -> L.take n ps : lines (L.drop (n+1) ps)
    where search = L.elemIndex $ c2w '\n'

main = do
  lines <- fmap lines L.getContents
  let scoredLines = map scoreLine lines
  mapM (putStrLn . LU.toString) scoredLines