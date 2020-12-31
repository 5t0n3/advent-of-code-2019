module Day08Image where

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
      shownImage = map (topOpaquePixel parsedInput) [0 .. totalPixels - 1]
  putStrLn $ formatImage shownImage
  return shownImage

topOpaquePixel :: Image -> Int -> Int
topOpaquePixel [] _ = 2
topOpaquePixel (top : rest) idx
  | topPixel == 2 = topOpaquePixel rest idx
  | otherwise = topPixel
  where
    topPixel = top !! idx

formatImage :: Layer -> String
formatImage [] = []
formatImage img = map displayDigit current ++ '\n' : formatImage rest
  where
    (cols, rows) = imageDimensions
    (current, rest) = splitAt cols img
    displayDigit 0 = '-'
    displayDigit 1 = 'O'
    displayDigit 2 = ' '