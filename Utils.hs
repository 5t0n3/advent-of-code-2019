module Utils
  ( commaIndex
  , splitAtCommas
  , replaceNth
  ) where

import Data.List

commaIndex :: String -> Maybe Int
commaIndex = elemIndex ','

splitAtCommas :: String -> [String]
splitAtCommas "" = []
splitAtCommas rawString = case commaIndex rawString of
                            Just idx ->
                              -- Omit comma
                              let (headInput,_:restInput) = splitAt idx rawString
                              in headInput : splitAtCommas restInput
                            Nothing -> return rawString

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth idx item list =
  before ++ (item : after)
  where (before,_:after) = splitAt idx list
