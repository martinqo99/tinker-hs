import Prelude hiding (lines)

import Data.Enumerator (Enumeratee, Enumerator, ($$), ($=), joinI, run_)
import qualified Data.Enumerator.Text as ET
import qualified Data.Enumerator.List as EL
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.Random

takeSet :: Ord a => Int -> [a] -> Set.Set a
takeSet n l = takeSet_ l Set.empty
    where takeSet_ (x:xs) acc
            | Set.size acc < n = takeSet_ xs (Set.insert x acc)
            | otherwise        = acc
          takeSet_ []     acc  = acc -- XXX - do we want this?

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " N file"

parse :: [a] -> IO (a, a)
parse [n, file] = return (n, file)
parse _ = usage >> exitFailure

linesError :: IO ()
linesError = putStrLn "Number of requested lines larger than file lines!"

linesCheck :: Ord a => a -> a -> IO ()
linesCheck nLines reqNLines
    | reqNLines > nLines = linesError >> exitFailure
    | otherwise = return ()

enumFileLines :: Enum a => a -> FilePath -> Enumerator (a, T.Text) IO b
enumFileLines n file =
  ET.enumFile file $= EL.mapAccum (\acc line -> (succ acc, (acc, line))) n

filterLines :: (Monad m, Ord o) => Set.Set o -> Enumeratee (o, a) (o, a) m b
filterLines lineNums =
  EL.filter (\(line, _) -> Set.member line lineNums) 

main :: IO ()
main = do
  args <- getArgs
  (n, file) <- parse args
  let reqNLines = (read n)::Int
  nLines <- run_ (ET.enumFile file $$ EL.fold (\acc _ -> succ acc) 0)
  linesCheck nLines reqNLines
  g <- getStdGen
  let lineNums = takeSet reqNLines $ randomRs (1, nLines) g
  run_ (enumFileLines 1 file $$ joinI $ filterLines lineNums $$
    joinI $ EL.map snd $$ EL.mapM_ (putStrLn . T.unpack))

