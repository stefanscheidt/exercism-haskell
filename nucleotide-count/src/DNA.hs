module DNA (nucleotideCounts, Nucleotide(..)) where

import Control.Arrow ((&&&))
import Data.Map (Map)
import Data.Map.Lazy (fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs =
  let
    nucleotides = "ACGT"
    isValid = all (`elem` nucleotides) xs
    cnt x = length $ filter (== x) xs 
    nuc 'A' = A
    nuc 'C' = C
    nuc 'G' = G
    nuc 'T' = T
    nuc _ = error "invalid char"
  in
    if isValid
      then Right $ fromList $ map (nuc &&& cnt) nucleotides  
      else Left xs
