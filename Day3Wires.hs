module Day3Wires where

import Data.List
import Data.Maybe (fromMaybe)
import qualified Utils

part1 :: IO Int
part1 = do
  input <- readFile "input/day3input.txt"
  let [wire1,wire2] = map (parseWireSegments (0,0) . Utils.splitAtCommas) $ lines input
      (shortestManhattanDistance,_,_,_) = shortestIntersectDistance $ findIntersections wire1 wire2
  return shortestManhattanDistance

part2 :: IO Int
part2 = do
  input <- readFile "input/day3input.txt"
  let [wire1,wire2] = map (parseWireSegments (0,0) . Utils.splitAtCommas) $ lines input
      pathLengths = pathLengthsToIntersections wire1 wire2 (findIntersections wire1 wire2)
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
parseWireSegments (pointX, pointY) (first:rest) =
  case first of
    ('U':units) ->
      Vertical pointX pointY (pointY + read units) : parseWireSegments (pointX,pointY + read units) rest
    ('D':units) ->
      Vertical pointX pointY (pointY - read units) : parseWireSegments (pointX,pointY - read units) rest
    ('R':units) ->
      Horizontal pointY pointX (pointX + read units) : parseWireSegments (pointX + read units,pointY) rest
    ('L':units) ->
      Horizontal pointY pointX (pointX - read units) : parseWireSegments (pointX - read units,pointY) rest
    _ -> error "Invalid wire segment."

findIntersections :: [WireSegment] -> [WireSegment] -> [(Point, WireSegment, WireSegment)]
findIntersections wire1 wire2 =
  [ (intPoint, seg1, seg2) |
  seg1 <- wire1,
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

shortestIntersectDistance :: [(Point, WireSegment, WireSegment)] -> (Int, Point, WireSegment, WireSegment)
shortestIntersectDistance intersections =
  (foldl (\acc ((x,y), _, _) -> min (abs x + abs y) acc) firstDistance intersections, (firstX, firstY), seg1, seg2)
  where ((firstX, firstY), seg1, seg2) = head intersections
        firstDistance = abs firstX + abs firstY

pathLength :: [WireSegment] -> WireSegment -> Point -> Maybe Int
pathLength wire endSegment endPoint =
  let endIndex = elemIndex endSegment wire
      leadingSegments = case endIndex of
                          Just len ->
                            Just $ take len wire
                          Nothing -> Nothing
      lastLength = partialSegmentLength endSegment endPoint
  in case leadingSegments of
       Just segments ->
         Just ((sum . fmap (abs . wireSegmentLength) $ segments) + abs lastLength)
       Nothing -> Nothing

pathLengthsToIntersections :: [WireSegment] -> [WireSegment] -> [(Point, WireSegment, WireSegment)] -> [Int]
pathLengthsToIntersections _ _ [] = []
pathLengthsToIntersections wire1 wire2 ((intPoint, finSeg1, finSeg2):iscts) =
  let firstIntersectionTotal = pathLength wire1 finSeg1 intPoint
      firstUnwrapped = fromMaybe (-1) firstIntersectionTotal
      secondIntersectionTotal = pathLength wire2 finSeg2 intPoint
      secondUnwrapped = fromMaybe (-1) secondIntersectionTotal
      totalLength = firstUnwrapped + secondUnwrapped
  in if firstUnwrapped == -1 || secondUnwrapped == -1 then
    pathLengthsToIntersections wire1 wire2 iscts
  else
    totalLength : pathLengthsToIntersections wire1 wire2 iscts

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
  (loc > start && loc < end) || ( loc > end && loc < start )

wireSegmentTuple :: WireSegment -> (Int, Int, Int)
wireSegmentTuple seg = case seg of
                         Vertical loc start end ->
                           (loc, start, end)
                         Horizontal loc start end ->
                           (loc, start, end)
