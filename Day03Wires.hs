module Day03Wires where

import Data.Functor ((<&>))
import Data.List
import Data.Maybe (mapMaybe)
import qualified Utils

part1 :: IO Int
part1 = do
  input <- readFile "input/day03.txt"
  let [wire1, wire2] = map (parseWireSegments (0, 0) . Utils.splitAtCommas) $ lines input
      manhattanDistances = map manhattanDistanceFromOrigin $ findIntersections wire1 wire2
      shortestDistance = minimum manhattanDistances
  return shortestDistance

part2 :: IO Int
part2 = do
  input <- readFile "input/day03.txt"
  let [wire1, wire2] = map (parseWireSegments (0, 0) . Utils.splitAtCommas) $ lines input
      pathLengths = mapMaybe (totalLengthToIntersection wire1 wire2) $ findIntersections wire1 wire2
      shortestLength = minimum pathLengths
  return shortestLength

test :: IO ()
test = do
  part1res <- part1
  if part1res /= 225
    then putStrLn ("Part 1 gave unexpected result: " ++ show part1res)
    else putStrLn "Part 1 succeeded."
  part2res <- part2
  if part2res /= 35194
    then putStrLn ("Part 2 gave unexpected result: " ++ show part2res)
    else putStrLn "Part 2 succeeded."

-- First Int -> location, 2nd/third -> start/end
data WireSegment = Vertical Int Int Int | Horizontal Int Int Int deriving (Show, Eq)

type Point = (Int, Int)

parseWireSegments :: Point -> [String] -> [WireSegment]
parseWireSegments _ [] = []
parseWireSegments (pointX, pointY) (first : rest) =
  case first of
    ('U' : units) ->
      Vertical pointX pointY (pointY + read units) : parseWireSegments (pointX, pointY + read units) rest
    ('D' : units) ->
      Vertical pointX pointY (pointY - read units) : parseWireSegments (pointX, pointY - read units) rest
    ('R' : units) ->
      Horizontal pointY pointX (pointX + read units) : parseWireSegments (pointX + read units, pointY) rest
    ('L' : units) ->
      Horizontal pointY pointX (pointX - read units) : parseWireSegments (pointX - read units, pointY) rest
    _ -> error "Invalid wire segment."

findIntersections :: [WireSegment] -> [WireSegment] -> [(Point, WireSegment, WireSegment)]
findIntersections wire1 wire2 =
  [ (intPoint, seg1, seg2)
    | seg1 <- wire1,
      seg2 <- wire2,
      segmentsIntersect seg1 seg2,
      let (loc2, _, _) = wireSegmentTuple seg2
          intPoint = case seg1 of
            Vertical x _ _ -> (x, loc2)
            Horizontal y _ _ -> (loc2, y)
  ]

segmentsIntersect :: WireSegment -> WireSegment -> Bool
segmentsIntersect (Vertical x startv endv) (Horizontal y starth endh) =
  isInRange y startv endv && isInRange x starth endh
segmentsIntersect (Horizontal y starth endh) (Vertical x startv endv) =
  isInRange y startv endv && isInRange x starth endh
segmentsIntersect _ _ = False

manhattanDistanceFromOrigin :: (Point, WireSegment, WireSegment) -> Int
manhattanDistanceFromOrigin ((intX, intY), _, _) = abs intX + abs intY

fullPathLength :: [WireSegment] -> WireSegment -> Point -> Maybe Int
fullPathLength wire endSegment endPoint =
  sum . map wireSegmentLength <$> leadingSegments <&> (+ finalPartialLength)
  where
    leadingSegments = elemIndex endSegment wire <&> \len -> take len wire
    finalPartialLength = partialSegmentLength endSegment endPoint

totalLengthToIntersection :: [WireSegment] -> [WireSegment] -> (Point, WireSegment, WireSegment) -> Maybe Int
totalLengthToIntersection wire1 wire2 (intPoint, finalSeg1, finalSeg2) =
  (+) <$> fullPathLength wire1 finalSeg1 intPoint <*> fullPathLength wire2 finalSeg2 intPoint

wireSegmentLength :: WireSegment -> Int
wireSegmentLength (Vertical _ start end) = abs $ start - end
wireSegmentLength (Horizontal _ start end) = abs $ start - end

partialSegmentLength :: WireSegment -> Point -> Int
partialSegmentLength (Vertical loc start _) (_, endY) =
  wireSegmentLength (Vertical loc start endY)
partialSegmentLength (Horizontal loc start _) (endX, _) =
  wireSegmentLength (Horizontal loc start endX)

isInRange :: Int -> Int -> Int -> Bool
isInRange loc start end =
  (start < loc && loc < end) || (end < loc && loc < start)

wireSegmentTuple :: WireSegment -> (Int, Int, Int)
wireSegmentTuple seg = case seg of
  Vertical loc start end ->
    (loc, start, end)
  Horizontal loc start end ->
    (loc, start, end)
