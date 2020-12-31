module Day08Image where

import Control.Monad (join)
import Data.Char (digitToInt)
import Data.List (elemIndices)

type Layer = [Int]

type Image = [Layer]

type Dimensions = (Int, Int)

imageDimensions :: Dimensions
imageDimensions = (25, 6) -- Columns, rows

totalPixels :: Int
totalPixels = uncurry (*) imageDimensions

parseImage :: String -> Image
parseImage [] = []
parseImage raw =
  map digitToInt fullLayer : parseImage otherLayers
  where
    (cols, rows) = imageDimensions
    (fullLayer, otherLayers) = splitAt totalPixels raw

part1 :: IO Int
part1 = do
  rawInput <- readFile "input/day08.txt"
  let parsedInput = parseImage rawInput
      leastZeroesLayer = foldl1 (\acc layer -> if zeroCount layer < zeroCount acc then layer else acc) parsedInput
  return $ elemCount 1 leastZeroesLayer * elemCount 2 leastZeroesLayer
  where
    elemCount elem = length . elemIndices elem
    zeroCount = elemCount 0

part2 :: IO Layer
part2 = do
  rawInput <- readFile "input/day08.txt"
  let parsedInput = parseImage rawInput
      shownImage = map topOpaquePixel $ groupOverlappingPixels parsedInput
  putStrLn $ formatImage shownImage
  return shownImage

groupOverlappingPixels :: Image -> [[Int]]
groupOverlappingPixels img
  | null $ join img = []
  | otherwise = map head img : groupOverlappingPixels (map tail img)

topOpaquePixel :: [Int] -> Int
topOpaquePixel = head . dropWhile (== 2)

formatImage :: Layer -> String
formatImage [] = []
formatImage img = map displayDigit current ++ '\n' : formatImage rest
  where
    cols = fst imageDimensions
    (current, rest) = splitAt cols img
    displayDigit 0 = '-'
    displayDigit 1 = 'O'
    displayDigit 2 = ' '