--
-- Introduce confusables in a sentence
--
-- TODO:
--
--   - More than 1 edit operation
--   - Introduce spaces
--

module Main where

import Control.Monad (forM_, guard, liftM)
import Control.Monad.Trans.Resource (ResourceIO)
import Data.ByteString.Char8 (unpack)
import Data.Conduit (($$), ($=), Resource, Sink, SinkStateResult(..), runResourceT, sinkState)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.List (intercalate)
import qualified Data.List.Zipper as LZ
import qualified Data.Set.BKTree as BKT
import qualified Data.Set.BKTree.Strict as BKS
import System.Environment (getArgs)

main :: IO ()
main = do
  fn <- handleArgs
  bkTree <- readWords fn
  l <- lines `liftM` getContents
  forM_ l (mapM_ putStrLn . map (intercalate " ") .
    confuse (flip (BKT.elemsDistance 1) bkTree) . words)

handleArgs :: IO String
handleArgs = do
  args <- getArgs
  case args of
    [fn] -> return fn
    _    -> error "Please specify a dictionary file"

-- |
-- Introduce confusion in a list of elements using a function.
confuse :: Eq a => (a -> [a]) -> [a] -> [[a]]
confuse f sent = do
  zipper <- zippers sent
  let current = LZ.cursor zipper
  new <- f current
  -- Some confusion functions include the original element.
  guard $ new /= current
  return $ LZ.toList $ LZ.replace new zipper

-- |
-- Return a list of zippers pointing at each list element. The last
-- zipper that does not point to a list element is not included.
zippers :: [a] -> [LZ.Zipper a]
zippers =
  go . LZ.fromList
  where
    go z
      | LZ.endp z = []
      | otherwise = z : go (LZ.right z)

-- |
-- Read a list of words into a 'BKTree'.
readWords :: ResourceIO m => FilePath -> m (BKT.BKTree [Char])
readWords fn = 
  runResourceT (CB.sourceFile fn $= CB.lines $= CL.map unpack $$ bkTreeSink)

-- |
-- Sink that strictly constructs a BKTree
bkTreeSink :: (BKT.Metric a, Resource m) => Sink a m (BKT.BKTree a)
bkTreeSink =
  sinkState
    BKT.empty
    (\s i -> return $ StateProcessing $ BKS.insert' i s)
    return

