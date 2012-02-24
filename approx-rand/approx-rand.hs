{-# LANGUAGE DeriveDataTypeable #-}

import           Control.Exception.Base (Exception)
import           Control.Monad (forM_, liftM)
import           Control.Monad.Random (Rand, runRand)
import           Control.Monad.Random.Class (MonadRandom(..))
import           Control.Monad.ST (runST)
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
import           Data.Vector.Generic ((!))
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VM
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.Random (RandomGen, newStdGen)
import           Text.Printf (printf)

data ReadException =
  DoubleConversionException String
  deriving (Show, Typeable)

instance Exception ReadException

-- | Subtract two vectors.
subVector :: (VG.Vector v n, Num n) => v n -> v n -> v n
subVector = VG.zipWith (-)

permuteVectors' :: (RandomGen g, VG.Vector v a) => v a -> v a ->
  Rand g (v a, v a)
permuteVectors' vec1 vec2 = do
  let vLen = VG.length vec1
  rands <- getRandoms
  return $ runST $ do
    -- Initialize new vectors.
    nvec1 <- VM.new vLen 
    nvec2 <- VM.new vLen 

    -- Shuffle vectors.
    forM_ (zip [0..vLen - 1] rands) $ \(idx, coin) -> do
      if coin == True then do
        VM.write nvec1 idx $ vec1 ! idx
        VM.write nvec2 idx $ vec2 ! idx
      else do
        VM.write nvec1 idx $ vec2 ! idx
        VM.write nvec2 idx $ vec1 ! idx

    -- Freeze and return the permuted vectors.
    f1 <- VG.freeze nvec1
    f2 <- VG.freeze nvec2
    return (f1, f2)

-- | Simple test statistic.
t :: (VG.Vector v n, Num n) => v n -> v n -> n
t v1 v2 = abs $ VG.sum $ subVector v1 v2

readFileCol :: C.ResourceIO m => String -> Int -> m [Double]
readFileCol fn col =
  liftM reverse $ C.runResourceT (
    CB.sourceFile fn $=
    CB.lines $=
    CT.decode CT.utf8 $=
    CL.map (T.split (==' ')) $=
    CL.map (!! col) $=
    toDouble $$
    CL.consume )

toDouble :: ResourceThrow m => C.Conduit T.Text m Double
toDouble = CL.mapM $ \v ->
  case TR.double v of
    Left err     -> resourceThrow $ DoubleConversionException err
    Right (d, _) -> return $ d

randApprox :: (Num a, Ord a, RandomGen g, VG.Vector v a) => g -> a -> v a ->
  v a -> [Int]
randApprox gen tOrig v1 v2 =
  let ((p1, p2), newGen) = runRand (permuteVectors' v1 v2) gen in
    if (t p1 p2) > tOrig then
      1 : randApprox newGen tOrig v1 v2
    else
      0 : randApprox newGen tOrig v1 v2

main :: IO ()
main = do
  args <- handleArgs
  v1 <- liftM V.fromList $ readFileCol (args !! 0) 0
  v2 <- liftM V.fromList $ readFileCol (args !! 1) 0
  let tOrig = t v1 v2
  putStrLn $ printf "t_orig: %f" tOrig
  mt <- newStdGen
  let r = sum $ take 10000 $ randApprox mt tOrig v1 v2 :: Int
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
