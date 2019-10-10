module Bob (responseFor) where
import Data.Char
import Safe

responseFor :: String -> String
responseFor xs
  | all isSpace xs          = "Fine. Be that way!"
  | isShouted && isQuestion = "Calm down, I know what I'm doing!"
  | isShouted               = "Whoa, chill out!"
  | isQuestion              = "Sure."
  | otherwise               = "Whatever."
  where
    alphas = filter isAlpha xs
    noSpaces = filter (not . isSpace) xs
    isShouted = (not $ null alphas) && (all isUpper alphas)
    isQuestion = lastMay noSpaces == Just '?'