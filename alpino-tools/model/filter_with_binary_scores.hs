module Main where

import Control.Concurrent.MonadIO
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lex.Double (readDouble)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Enumerator as E
import Data.Enumerator (($$))
import Data.Enumerator.IO
import Data.Maybe (fromJust)
import qualified Control.Monad.Trans.Class as MT
import System.IO (isEOF, stdin)

data TrainingInstanceType =
    ParsingInstance | GenerationInstance
    deriving (Show, Eq)

data TrainingInstance = TrainingInstance {
      instanceType :: TrainingInstanceType,
      key          :: B.ByteString,
      n            :: B.ByteString,
      score        :: Double,
      features     :: B.ByteString
} deriving (Show, Eq)

bsToTrainingInstance :: B.ByteString -> TrainingInstance
bsToTrainingInstance l =
    TrainingInstance instType key n score features
    where lineParts = B.split instanceFieldSep l
          instType = bsToType $ lineParts !! 0
          key = lineParts !! 1
          n = lineParts !! 2
          score = fst . fromJust . readDouble $ lineParts !! 3
          features = lineParts !! 4


instanceFieldSep = c2w '#'

bsToType :: B.ByteString -> TrainingInstanceType
bsToType bs
    | bs == parseMarker = ParsingInstance
    | bs == generationMarker = GenerationInstance
    where parseMarker = BU.fromString "P"
          generationMarker = BU.fromString "G"

instanceParser :: (Monad m) =>
                  E.Enumeratee BU.ByteString TrainingInstance m b
instanceParser (E.Continue k) = do
  e <- E.head
  case e of
    Nothing -> return $ E.Continue k
    Just y -> do
           newStep <- MT.lift $ E.runIteratee $ k $ E.Chunks [bsToTrainingInstance y]
           instanceParser newStep
instanceParser step = return step

groupInstance :: (Monad m) =>
                   E.Enumeratee TrainingInstance [TrainingInstance] m b
groupInstance = loop [] (ParsingInstance, BU.fromString "")
    where loop acc cur@(curType, curKey) (E.Continue k) = do
            e <- E.head
            case e of
              Nothing -> return $ E.Continue k
              Just i ->
                  if instanceType i == curType && key i == curKey then do
                      loop (i:acc) cur (E.Continue k)
                  else do
                    case acc of
                      [] -> loop (i:acc) (instanceType i, key i) (E.Continue k)
                      otherwise -> do
                          let newAcc = [i]
                          newStep <- MT.lift $ E.runIteratee $ k $ E.Chunks [acc]
                          loop newAcc (instanceType i, key i) newStep
          loop _ _ step = return step

lineEnum :: MonadIO m => E.Enumerator B.ByteString m b
lineEnum = E.Iteratee . loop
    where loop (E.Continue k) = do
            eof <- liftIO isEOF
            case eof of
              True -> return $ E.Continue k
              False -> do
                       line <- liftIO B.getLine
                       E.runIteratee (k (E.Chunks [line])) >>= loop
          loop step = return step

main = E.run_ $ lineEnum $$ instanceParser $$ groupInstance $$ E.printChunks True