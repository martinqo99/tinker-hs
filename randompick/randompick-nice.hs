import Prelude hiding (lines)

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w)
import Data.List (sort)
import qualified Data.Set as Set
import Monad (liftM)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Random
import qualified IO as IO


lines :: L.ByteString -> [L.ByteString]
lines ps
    | L.null ps = []
    | otherwise = case search ps of
                    Nothing -> [ps]
                    Just n  -> L.take n ps : lines (L.drop (n+1) ps)
    where search = L.elemIndex $ c2w '\n'

filterAccumL :: Ord x => (acc -> x -> (acc, Bool))
             -> acc
             -> [x]
             -> (acc, [x])
filterAccumL _ a []     = (a, [])
filterAccumL f a (x:xs) = (a'', [x|c] ++ ys)
    where (a', c)   = f a x
          (a'', ys) = filterAccumL f a' xs


fnub :: Ord a => [a] -> [a]
fnub = snd . filterAccumL inSet Set.empty
    where inSet acc x
              | Set.member x acc = (acc, False)
              | otherwise = (Set.insert x acc, True)

selectLines :: Eq a => [a] -> [(a,b)] -> [b]
selectLines [] _ = []
selectLines _ [] = []
selectLines xl@(c:xs) ((c',v):ys)
    | c == c' = v : selectLines xs ys
    | otherwise = selectLines xl ys

usage = putStrLn "Usage: shuffle N file"

parse [n, file] = return (n, file)
parse _ = usage >> exitFailure

linesError = putStrLn "Number of requested lines larger than file lines!"

linesCheck nLines reqNLines
    | reqNLines > nLines = linesError >> exitFailure
    | otherwise = return ()

main = do
  args <- getArgs
  (n, file) <- parse args
  let reqNLines = (read n)::Int
  h <- IO.openFile file IO.ReadMode
  fLines <- liftM lines $ L.hGetContents h
  let nLines = length fLines
  linesCheck nLines reqNLines
  g <- getStdGen
  let lineNums = sort . take reqNLines . fnub $ randomRs (1, nLines) g
  h <- IO.openFile file IO.ReadMode
  fLines <- liftM lines $ L.hGetContents h
  let sLines = selectLines lineNums . zip [1..nLines] $ fLines
  mapM_ L.putStrLn sLines

  