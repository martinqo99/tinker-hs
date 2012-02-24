{-# LANGUAGE DeriveDataTypeable #-}

import           Control.Exception.Base (Exception)
import           Control.Monad (replicateM)
import           Control.Monad.Random (evalRandIO)
import           Control.Monad.Random.Class (MonadRandom(..))
import           Control.Monad.Trans.Resource (ResourceThrow (..))
import           Data.Conduit (($$), ($=)) 
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Data.Typeable (Typeable)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           Text.Printf (printf)

data ReadException =
  DoubleConversionException String
  deriving (Show, Typeable)

instance Exception ReadException

-- | Subtract two vectors.
subVector :: Num n => [n] -> [n] -> [n]
subVector = zipWith (-)

t :: Num n => [n] -> [n] -> n
t v1 v2 = abs $ sum $ subVector v1 v2

permuteVectors :: MonadRandom r => [a] -> [a] ->
  r ([a], [a])
permuteVectors vec1 vec2 = do
  rands <- getRandoms
  return $ unzip $ zipWith3 permute vec1 vec2 rands
  where
    permute v1 v2 r = if r then (v1, v2) else (v2, v1)

readFileCol :: C.ResourceIO m => String -> Int -> m [Double]
readFileCol fn col =
  reverse `fmap` C.runResourceT (
    CB.sourceFile fn $=
    CB.lines $=
    CT.decode CT.utf8 $=
    CL.map (T.split (== ' ')) $=
    CL.map (!! col) $=
    toDouble $$
    CL.consume )

toDouble :: ResourceThrow m => C.Conduit T.Text m Double
toDouble = CL.mapM $ \v ->
  case TR.double v of
    Left err     -> resourceThrow $ DoubleConversionException err
    Right (d, _) -> return $ d

randApprox :: (Num a, Ord a, MonadRandom r) => Int -> a ->
  [a] -> [a] -> r [Bool]
randApprox n tOrig v1 v2 =
  replicateM n $ do
    (p1, p2) <- permuteVectors v1 v2
    return $ (t p1 p2) > tOrig

main :: IO ()
main = do
  args <- handleArgs
  v1 <- readFileCol (args !! 0) 0
  v2 <- readFileCol (args !! 1) 0
  let tOrig = t v1 v2
  putStrLn $ printf "t_orig: %f" tOrig
  r <- length `fmap` filter (== True)  `fmap`
    evalRandIO (randApprox 10000 tOrig v1 v2)
  let p = ((1.0) + fromIntegral r) / 10001.0 :: Double
  putStrLn $ printf "r: %d\np: %f" r p

handleArgs :: IO [String]
handleArgs = do
  args <- getArgs
  if length args /= 2 then do
    putStrLn "Usage: approx-rand scores scores2"
    exitFailure
  else
    return args
