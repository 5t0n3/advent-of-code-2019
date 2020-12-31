module Day08Image where

import Data.Char (digitToInt)
import Data.List (elemIndices)

type Layer = [Int]

type Dimensions = (Int, Int)

imageDimensions :: Dimensions
imageDimensions = (25, 6) -- Columns, rows

totalPixels :: Int
totalPixels = uncurry (*) imageDimensions

parseLayers :: String -> [Layer]
parseLayers [] = []
parseLayers raw =
  map digitToInt fullLayer : parseLayers otherLayers
  where
    (cols, rows) = imageDimensions
    (fullLayer, otherLayers) = splitAt totalPixels raw

part1 :: IO Int
part1 = do
  rawInput <- readFile "input/day08.txt"
  let parsedInput = parseLayers rawInput
      leastZeroesLayer = foldl1 (\acc layer -> if zeroCount layer < zeroCount acc then layer else acc) parsedInput
  return $ elemCount 1 leastZeroesLayer * elemCount 2 leastZeroesLayer
  where
    elemCount elem = length . elemIndices elem
    zeroCount = elemCount 0