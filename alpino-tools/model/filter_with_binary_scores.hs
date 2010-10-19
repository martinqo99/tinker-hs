module Main where

import Control.Monad.IO.Class (MonadIO(..), liftIO)
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lex.Double (readDouble)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Enumerator as E
import Data.Enumerator (($$))
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
                  E.Enumeratee BU.ByteString TrainingInstance m b
instanceParser (E.Continue k) = do
  e <- E.head
  case e of
    Nothing -> return $ E.Continue k
    Just y -> do
           newStep <- MT.lift $ E.runIteratee $ k $ E.Chunks [bsToTrainingInstance y]
           instanceParser newStep
instanceParser step = return step

instanceGenerator :: (Monad m) =>
                     E.Enumeratee TrainingInstance B.ByteString m b
instanceGenerator (E.Continue k) = do
  e <- E.head
  case e of
    Nothing -> return $ E.Continue k
    Just y -> do
           newStep <- MT.lift $ E.runIteratee $ k $ E.Chunks [trainingInstanceToBs y]      
           instanceGenerator newStep
instanceGenerator step = return step

groupInstances = groupBy keyEq
    where keyEq i1 i2 = instanceType i1 == instanceType i2 &&
                        key i1 == key i2

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

printByteString :: MonadIO m => E.Iteratee B.ByteString m ()
printByteString = E.continue step
    where step (E.Chunks []) = E.continue step
          step (E.Chunks xs) = liftIO (mapM_ B.putStrLn xs) >> E.continue step
          step E.EOF = E.yield () E.EOF

rescore :: [TrainingInstance] -> [TrainingInstance]
rescore ctx = map (rescoreEvt maxScore) ctx
    where maxScore = foldl (\acc e -> max acc $ score e) 0.0 ctx
          rescoreEvt maxScore evt
            | score evt == maxScore = evt { score = 1.0 }
            | otherwise = evt { score = 0.0 }

main = do
  instances <- E.run_ $ lineEnum $$ E.joinI $ instanceParser $$ E.consume
  let rescoredInstances = concat . (map rescore) . groupInstances $ instances
  E.run_ $ E.enumList 1 rescoredInstances $$ E.joinI $ instanceGenerator $$
       printByteString