import Prelude hiding (lines)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lex.Lazy.Double (readDouble)
import System (getArgs)
import System.Console.GetOpt

scoreFromLine :: L.ByteString -> Double
scoreFromLine = score . bs2double . L.split (c2w '|') . (!! 2) . L.split (c2w '#')
    where bs2double = map (fst . fromJust . readDouble)
          score [ov, corr, sys] = if n > 0
                                  then (1.0 - pen / n)
                                  else 1.0
              where n = max corr sys 
                    pen = n - ov

scoreFromLineFluency :: L.ByteString -> Double
scoreFromLineFluency = fst . fromJust . readDouble . (!! 3) . L.split (c2w '#')

lines :: L.ByteString -> [L.ByteString]
lines ps
    | L.null ps = []
    | otherwise = case search ps of
                    Nothing -> [ps]
                    Just n  -> L.take n ps : lines (L.drop (n+1) ps)
    where search = L.elemIndex $ c2w '\n'

main = do
  args <- getArgs
  let (flags, nonOpts, msgs) = getOpt RequireOrder options args
  let scoreFun = if elem Fluency flags
                 then scoreFromLineFluency
                 else scoreFromLine
  contents <- L.getContents
  let scores = map scoreFun $ lines contents
  mapM print scores

data Flag = Fluency
            deriving (Eq, Show)

options :: [OptDescr Flag]
options = [Option ['y'] ["fluency"] (NoArg Fluency) "fluency ranking"]