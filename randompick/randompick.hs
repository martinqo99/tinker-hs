import Prelude hiding (lines)

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w)
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

takeSet :: Ord a => Int -> [a] -> Set.Set a
takeSet n l = takeSet_ n l Set.empty
    where takeSet_ n (x:xs) acc
            | Set.size acc == n = acc
            | otherwise = takeSet_ n xs (Set.insert x acc)

selectLines :: Ord a => Set.Set a -> [(a,b)] -> [b]
selectLines _ [] = []
selectLines sLines ((line,v):ys)
    | Set.member line sLines = v : selectLines sLines ys
    | otherwise = selectLines sLines ys

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
  let lineNums = takeSet reqNLines $ randomRs (1, nLines) g
  h <- IO.openFile file IO.ReadMode
  fLines <- liftM lines $ L.hGetContents h
  let sLines = selectLines lineNums . zip [1..nLines] $ fLines
  mapM L.putStrLn sLines
  return ()
  