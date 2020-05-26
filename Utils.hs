module Utils
  ( splitAtCommas
  , replaceNth
  , splitAtChar
  , splitEveryChar
  ) where

import Data.List

splitAtCommas :: String -> [String]
splitAtCommas = splitEveryChar ','

splitAtChar :: Char -> String -> (String,String)
splitAtChar _ "" = ("","")
splitAtChar c str = (before,after)
  -- Discard character in question
  where (before,_:after) = case elemIndex c str of
                             Just _ -> break (==c) str
                             Nothing -> (str," ")

splitEveryChar :: Char -> String -> [String]
splitEveryChar _ "" = []
splitEveryChar c str = first : splitEveryChar c rest
  where (first,rest) = splitAtChar c str

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth idx item list =
  before ++ (item : after)
  where (before,_:after) = splitAt idx list
