module Day07Amplifier where

import Data.List (nub)
import qualified Intcode

part1 :: IO Int
part1 = do
  rawInput <- readFile "input/day07.txt"
  let parsedIntcode = Intcode.parseNumericInput rawInput
      phaseSequences = filter ((== 5) . length . nub) . mapM (const [0 .. 4]) $ [1 .. 5]
      dummyInput = Intcode.Finished [0] []
      allAmpResults = [head output | phaseSeq <- phaseSequences, let (Intcode.Finished output _) = feedOutputCycle parsedIntcode dummyInput phaseSeq]
  return $ maximum allAmpResults

feedOutputCycle :: [Int] -> Intcode.Program -> [Int] -> Intcode.Program
feedOutputCycle _ input [] = input
feedOutputCycle _ err@Intcode.Error {} _ = err
feedOutputCycle baseProgram (Intcode.Finished input _) (currentPhase : rest) =
  case programOutput of
    finished@(Intcode.Finished output _) -> feedOutputCycle baseProgram finished rest
    err@Intcode.Error {} -> err
  where
    programOutput = Intcode.executeIntcode 0 $ Intcode.Executing baseProgram (currentPhase : input) []
