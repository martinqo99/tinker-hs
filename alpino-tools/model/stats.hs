import Prelude hiding (lines)

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lex.Lazy.Double (readDouble)
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as V
import Math.Statistics.Fusion

lines :: L.ByteString -> [L.ByteString]
lines ps
    | L.null ps = []
    | otherwise = case search ps of
                    Nothing -> [ps]
                    Just n  -> L.take n ps : lines (L.drop (n+1) ps)
    where search = L.elemIndex $ c2w '\n'

readScore = fst . fromJust . readDouble

main = do
 contents <- L.getContents
 let scores = V.fromList . map readScore . lines $ contents
 putStrLn $ (++) "Mean: " $ show $ mean scores
 putStrLn $ (++) "Variance: " $ show $ var scores
 putStrLn $ (++) "Std. dev: " $ show $ stddev scores
