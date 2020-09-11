module Day6Orbits where

import Data.Map.Strict (Map, alter, empty, (!?))
import qualified Utils

type OrbitMap = Map String [String]

part1 :: IO ()
part1 = do
  rawInput <- readFile "input/day6input.txt"
  let (direct, indirect) = countOrbits (parseOrbits rawInput) "COM"
  putStrLn ("Direct: " ++ show direct ++
            "\nIndirect: " ++ show indirect ++
            "\nTotal: " ++ show (direct + indirect))
    where parseOrbits = makeOrbitMap empty . splitOrbits

countOrbits :: OrbitMap -> String -> (Int, Int)
countOrbits orbitMap name = case orbitMap !? name of
  Just dirOrbits ->
    let (cDirects, cIndirects) = foldl (\acc oName -> countOrbits orbitMap oName <+> acc) (0,0) dirOrbits
    in (length dirOrbits + cDirects, cDirects + cIndirects)
  Nothing -> (0,0)

splitOrbits :: String -> [[String]]
splitOrbits = map (Utils.splitEveryChar ')') . lines

makeOrbitMap :: OrbitMap -> [[String]] -> OrbitMap
makeOrbitMap orbitMap [] = orbitMap
makeOrbitMap orbitMap (o:os) = makeOrbitMap updatedMap os
  where (main:orbits) = o
        updatedMap = alter (appendIfExists orbits) main orbitMap

appendIfExists :: [a] -> Maybe [a] -> Maybe [a]
appendIfExists newList oldList =
  case oldList of
    Just old -> return $ old ++ newList
    Nothing -> return newList

(<+>) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(a1, b1) <+> (a2, b2) = (a1 + a2, b1 + b2)
