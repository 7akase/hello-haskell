
-- define my splitOn
{- 
split :: Char -> String -> [String]
split p s = cons (case break (== p) s of
                   (l,s') -> (l, case s' of
                                   []  -> []
                                   _:s'' -> split p s''))
            where
              cons ~(h,t) = h:t
-}
-- import Data.Text
-- sample = readFile "csv.hs" >>= (\x -> return $ fmap unpack $ split (=='\n')  (pack x))

import Control.Applicative
import Data.List
import qualified Data.Text as T
makeCsv :: [[String]] -> String
makeCsv = intercalate "\n" . fmap (intercalate ",")

splitCsv :: String -> [[String]]
-- splitCsv = fmap (fmap T.unpack) . fmap (T.split (==',')) . T.split (=='\n') . T.pack
splitCsv = fmap (fmap T.unpack) . fmap (T.split (==',')) . T.split (=='\n') . T.pack

-- makeCsv <$> splitCsv <$> readfile "sample.csv" >>= writeFile "sample2.csv" 
