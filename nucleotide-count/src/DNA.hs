module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import Data.Map.Lazy (fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

counts :: String -> Map Nucleotide Int
counts xs = fromList
  [ (A, count 'A')
  , (C, count 'C')
  , (G, count 'G')
  , (T, count 'T')
  ]
    where 
      count x = length $ filter (== x) xs

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
  | all (`elem` "ACGT") xs  
    = Right $ counts xs
  | otherwise
    = Left xs

