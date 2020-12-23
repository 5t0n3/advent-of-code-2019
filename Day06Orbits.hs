module Day06Orbits where

import Data.List (intersect, (\\))
import Data.Map.Strict (Map, alter, empty, foldlWithKey, (!?))
import qualified Utils

type OrbitMap = Map String [String]

part1 :: IO ()
part1 = do
  rawInput <- readFile "input/day06.txt"
  let (direct, indirect) = countOrbits (parseOrbits rawInput) "COM"
  putStrLn
    ( "Direct: " ++ show direct
        ++ "\nIndirect: "
        ++ show indirect
        ++ "\nTotal: "
        ++ show (direct + indirect)
    )
  where
    parseOrbits = makeOrbitMap empty . splitOrbits

part2 :: IO Int
part2 = do
  rawInput <- readFile "input/day06.txt"
  let youAncestors = findAncestors (parseOrbits rawInput) (Just "YOU")
      santaAncestors = findAncestors (parseOrbits rawInput) (Just "SAN")
      commonAncestors = intersect youAncestors santaAncestors
      orbitTransfers =
        (tail $ youAncestors \\ commonAncestors)
          ++ (reverse . tail $ santaAncestors \\ commonAncestors)
  print orbitTransfers
  return $ length orbitTransfers
  where
    parseOrbits = makeOrbitMap empty . splitOrbits

countOrbits :: OrbitMap -> String -> (Int, Int)
countOrbits orbitMap name = case orbitMap !? name of
  Just dirOrbits ->
    let (cDirects, cIndirects) = foldl (\acc oName -> countOrbits orbitMap oName <+> acc) (0, 0) dirOrbits
     in (length dirOrbits + cDirects, cDirects + cIndirects)
  Nothing -> (0, 0)

findAncestors :: OrbitMap -> Maybe String -> [String]
findAncestors _ Nothing = []
findAncestors orbitMap (Just name) = name : findAncestors orbitMap parentName
  where
    parentName = foldlWithKey (\acc k os -> if elem name os then Just k else acc) Nothing orbitMap

splitOrbits :: String -> [[String]]
splitOrbits = map (Utils.splitEveryChar ')') . lines

makeOrbitMap :: OrbitMap -> [[String]] -> OrbitMap
makeOrbitMap orbitMap [] = orbitMap
makeOrbitMap orbitMap (o : os) = makeOrbitMap updatedMap os
  where
    (main : orbits) = o
    updatedMap = alter (appendIfExists orbits) main orbitMap

appendIfExists :: [a] -> Maybe [a] -> Maybe [a]
appendIfExists newList oldList =
  case oldList of
    Just old -> return $ old ++ newList
    Nothing -> return newList

(<+>) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(a1, b1) <+> (a2, b2) = (a1 + a2, b1 + b2)
