module Lib where

import Data.Function (on)
import Data.List (sortBy)
import Data.Text (Text)
import Text.CSV (parseCSVFromFile)
import Text.EditDistance (levenshteinDistance, defaultEditCosts)
import qualified Data.Text as T

type Scorer = Text -> Text -> Float
type Row = [Text]

-- Trivial scorer to always return 0
ignore :: Scorer
ignore _ _ = 0

-- Return 0 if strings are the same and 1 if they're different
exactMatch :: Scorer
exactMatch a b
  | a == b = 0
  | otherwise = 1

-- Return between 0 and 1, 0 if strings are same and 1 if completely different
normalizedLevenshtein :: Scorer
normalizedLevenshtein a b =
  if lev == 0 then 0 else lev / maxLength
  where lev = fromIntegral $ levenshtein a b
        maxLength = fromIntegral . maximum $ map T.length [a, b]

levenshtein :: Text -> Text -> Int
levenshtein a b = levenshteinDistance defaultEditCosts (T.unpack a) (T.unpack b)

-- Apply each of a list of functions to each of two lists of arguments
applyToEach :: [a -> b -> c] -> [a] -> [b] -> [c]
applyToEach [] _ _ = []
applyToEach _ [] _ = []
applyToEach _ _ [] = []
applyToEach (f:fs) (x:xs) (y:ys) = f x y : applyToEach fs xs ys

-- Compare two rows by applying scorers
scoreOnePair :: [Scorer] -> Row -> Row -> Float
scoreOnePair scorers row1 row2 =
  sum $ applyToEach scorers row1 row2

-- Get unique pairs
pairs :: Ord a => [a] -> [(a, a)]
pairs l = [(x,y) | x <- l, y <- l, x < y]

-- Calculate scores for all pairs of rows
score :: [Scorer] -> [Row] -> [(Float,Row,Row)]
score scorers csv =
  [(scoreOnePair scorers t1 t2, t1, t2) | (t1, t2) <- pairs csv]

-- Sort scored pairs by their scores
sortByScore :: [(Float,Row,Row)] -> [(Float,Row,Row)]
sortByScore = sortBy (compare `on` first)
  where first (x,_,_) = x

defaultMain :: IO ()
defaultMain = do
  let scorers = [ignore, normalizedLevenshtein, exactMatch]
  parsed <- parseCSVFromFile "data/baby_names.csv"
  case parsed of
    Left err -> print err
    Right csv -> 
      mapM_ print . take 10 . sortByScore . score scorers . 
      map (map T.pack) . init $ csv

