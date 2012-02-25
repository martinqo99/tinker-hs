-- |
-- Copyright  : (c) 2012 Daniël de Kok
-- License    : Apache 2
--
-- Maintainer : Daniël de Kok <me@danieldk.eu>
-- Stability  : experimental
--
-- Approximate randomization test (Noreen, 1989)

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DoAndIfThenElse #-}

import           Control.Exception.Base (Exception)
import           Control.Monad (liftM, replicateM)
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
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic as VG
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.Random (Random)
import           Text.Printf (printf)

data ReadException =
  DoubleConversionException String
  deriving (Show, Typeable)

instance Exception ReadException

-- | Subtract two vectors.
subVector :: (VG.Vector v n, Num n) => v n -> v n -> v n
subVector = VG.zipWith (-)

-- | Get a Vector with random values.
randomVector :: (MonadRandom r, Random a, VG.Vector v a) => Int -> r (v a)
randomVector len = do
  VG.fromList `liftM` take len `liftM` getRandoms

-- | Permute two vectors.
permuteVectors :: (MonadRandom r, VG.Vector v a, VG.Vector v Bool) =>
  v a -> v a -> r (v a, v a)
permuteVectors vec1 vec2 = do
  randomVec <- randomVector (VG.length vec1)
  let pv1 = VG.zipWith3 permute vec1 vec2 randomVec
  let pv2 = VG.zipWith3 permute vec2 vec1 randomVec
  return (pv1, pv2)
  where
    permute val1 val2 coin =
      if coin then val1 else val2

-- | Simple test statistic.
t :: (VG.Vector v n, Num n) => v n -> v n -> n
t v1 v2 = abs $ VG.sum $ subVector v1 v2

readFileCol :: C.ResourceIO m => String -> Int -> m [Double]
readFileCol fn col =
  liftM reverse $ C.runResourceT (
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

randApprox :: (Num a, Ord a, MonadRandom r, VG.Vector v a, VG.Vector v Bool) => Int -> a ->
  v a -> v a -> r [Bool]
randApprox n tOrig v1 v2 =
  replicateM n $ do
    (p1, p2) <- permuteVectors v1 v2
    return $ (t p1 p2) > tOrig

main :: IO ()
main = do
  args <- handleArgs
  v1 <- liftM V.fromList $ readFileCol (args !! 0) 0
  v2 <- liftM V.fromList $ readFileCol (args !! 1) 0
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
