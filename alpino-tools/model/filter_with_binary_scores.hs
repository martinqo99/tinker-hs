module Main where

import Control.Monad.IO.Class (MonadIO(..), liftIO)
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lex.Double (readDouble)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Enumerator as E
import Data.Enumerator hiding (isEOF, map)
import Data.Enumerator.IO
import Data.List (groupBy)
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

trainingInstanceToBs :: TrainingInstance -> B.ByteString
trainingInstanceToBs i = B.append typeBS $ B.append fieldSep $
                         B.append keyBS $ B.append fieldSep $
                         B.append nBS $ B.append fieldSep $
                         B.append scoreBS $ B.append fieldSep featuresBS
    where typeBS = typeToBS $ instanceType i
          keyBS = key i
          nBS = n i
          scoreBS = BU.fromString . show $ score i
          featuresBS = features i
          fieldSep = BU.fromString "#"

instanceFieldSep = c2w '#'

bsToType :: B.ByteString -> TrainingInstanceType
bsToType bs
    | bs == parseMarker = ParsingInstance
    | bs == generationMarker = GenerationInstance

parseMarker = BU.fromString "P"
generationMarker = BU.fromString "G"

typeToBS :: TrainingInstanceType -> B.ByteString
typeToBS instanceType
    | instanceType == ParsingInstance = parseMarker
    | instanceType == GenerationInstance = generationMarker

instanceParser :: (Monad m) =>
                  Enumeratee BU.ByteString TrainingInstance m b
instanceParser = E.map bsToTrainingInstance

instanceGenerator :: (Monad m) =>
                     Enumeratee TrainingInstance B.ByteString m b
instanceGenerator = E.map trainingInstanceToBs

groupInstances = groupBy keyEq
    where keyEq i1 i2 = instanceType i1 == instanceType i2 &&
                        key i1 == key i2

lineEnum :: MonadIO m => Enumerator B.ByteString m b
lineEnum = Iteratee . loop
    where loop (Continue k) = do
            eof <- liftIO isEOF
            case eof of
              True -> return $ Continue k
              False -> do
                       line <- liftIO B.getLine
                       runIteratee (k (Chunks [line])) >>= loop
          loop step = return step

groupByEnum :: (Monad m) =>
                  Enumeratee TrainingInstance [TrainingInstance] m b
groupByEnum = loop
    where loop (Continue k) = do
            h <- peek
            case h of
              Nothing -> return $ Continue k
              Just h -> do
                     xs <- E.span $ keyEq h
                     newStep <- MT.lift $ runIteratee $ k $ Chunks [xs]
                     loop newStep
          loop step = return step
          keyEq i1 i2 = instanceType i1 == instanceType i2 &&
                              key i1 == key i2

concatEnum :: (Monad m) =>
              Enumeratee [a] a m b
concatEnum = loop
    where loop (Continue k) = do
            h <- E.head
            case h of
              Nothing -> return $ Continue k
              Just h -> do
                     newStep <- MT.lift $ runIteratee $ k $ Chunks h
                     loop newStep
          loop step = return step

printByteString :: MonadIO m => Iteratee B.ByteString m ()
printByteString = continue step
    where step (Chunks []) = continue step
          step (Chunks xs) = liftIO (mapM_ B.putStrLn xs) >> continue step
          step EOF = yield () EOF

rescore :: [TrainingInstance] -> [TrainingInstance]
rescore ctx = map (rescoreEvt maxScore) ctx
    where maxScore = foldl (\acc e -> max acc $ score e) 0.0 ctx
          rescoreEvt maxScore evt
            | score evt == maxScore = evt { score = 1.0 }
            | otherwise = evt { score = 0.0 }

rescoreEnum = E.map rescore

main = do
  run_ $ lineEnum $$ joinI $ instanceParser $$ joinI $ groupByEnum $$
       joinI $ rescoreEnum $$ joinI $ concatEnum $$
       joinI $ instanceGenerator $$ printByteString
--  let rescoredInstances = concat . (map rescore) . groupInstances $ instances
--  run_ $ enumList 1 rescoredInstances $$ joinI $ instanceGenerator $$
--       printByteString