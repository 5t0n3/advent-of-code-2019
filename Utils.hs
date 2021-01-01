module Utils
  ( splitAtCommas,
    replaceNth,
    splitAtChar,
    splitEveryChar,
    itemPairs,
  )
where

import Data.List

splitAtCommas :: String -> [String]
splitAtCommas = splitEveryChar ','

splitAtChar :: Char -> String -> (String, String)
splitAtChar _ "" = ("", "")
splitAtChar c str = (before, after)
  where
    -- Discard character in question
    (before, _ : after) = case elemIndex c str of
      Just _ -> break (== c) str
      Nothing -> (str, " ")

splitEveryChar :: Char -> String -> [String]
splitEveryChar _ "" = []
splitEveryChar c str = first : splitEveryChar c rest
  where
    (first, rest) = splitAtChar c str

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth idx item list =
  before ++ (item : after)
  where
    (before, rest) = splitAt idx list
    after = if null rest then [] else tail rest

itemPairs :: [a] -> [(a, a)]
itemPairs [] = []
itemPairs [_] = []
itemPairs (item1 : item2 : rest) = (item1, item2) : itemPairs (item2 : rest)