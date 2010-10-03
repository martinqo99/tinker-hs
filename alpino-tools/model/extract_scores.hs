import Prelude hiding (lines)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lex.Lazy.Double (readDouble)

scoreFromLine :: L.ByteString -> Double
scoreFromLine = fst . fromJust . readDouble . (!! 3) . L.split (c2w '#')

lines :: L.ByteString -> [L.ByteString]
lines ps
    | L.null ps = []
    | otherwise = case search ps of
                    Nothing -> [ps]
                    Just n  -> L.take n ps : lines (L.drop (n+1) ps)
    where search = L.elemIndex $ c2w '\n'

main = do
  contents <- L.getContents
  let scores = map scoreFromLine $ lines contents
  mapM print scores