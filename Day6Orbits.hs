module Day6Orbits where

import Data.Maybe
import Data.List
import Data.Map.Strict (Map, alter, empty, toList)
import qualified Utils

type OrbitSet = (String, [String])

part1 :: IO (Int, Int)
part1 = do
  rawInput <- readFile "input/day6input.txt"
  let parsedOrbits = parseOrbits rawInput
  let centerIdx = fromJust (findOrbitWithName parsedOrbits "COM")
  let (direct, indirect) = countOrbitsNew parsedOrbits centerIdx
  putStrLn ("Direct: " ++ show direct ++
            "\nIndirect: " ++ show indirect ++
            "\nTotal: " ++ show (direct + indirect))
  return (direct, indirect)
  where parseOrbits = toList . splitOrbitsToMap empty . splitOrbits

countOrbitsNew :: [OrbitSet] -> Int -> (Int, Int)
countOrbitsNew orbits orbitIdx = (length directOrbits + childDirects, childDirects + childIndirects)
  where (_, directOrbits) = orbits !! orbitIdx
        childIndices = map fromJust . filter isJust . map (findOrbitWithName orbits) $ directOrbits
        (childDirects, childIndirects) = foldl (\acc idx -> countOrbitsNew orbits idx <+> acc)
                                         (0,0) childIndices

findOrbitWithName :: [OrbitSet] -> String -> Maybe Int
findOrbitWithName orbits name = findIndex ((==name) . fst) $ orbits

splitOrbits :: String -> [[String]]
splitOrbits = map (Utils.splitEveryChar ')') . lines

splitOrbitsToMap :: Map String [String] -> [[String]] -> Map String [String]
splitOrbitsToMap orbitMap [] = orbitMap
splitOrbitsToMap orbitMap (o:os) = splitOrbitsToMap updatedMap os
  where (main:orbits) = o
        updatedMap = alter (appendIfExists orbits) main orbitMap

appendIfExists :: [a] -> Maybe [a] -> Maybe [a]
appendIfExists newList oldList =
  case oldList of
    Just old -> return $ old ++ newList
    Nothing -> return newList

(<+>) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(a1, b1) <+> (a2, b2) = (a1 + a2, b1 + b2)
