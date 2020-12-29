module Day07Amplifier where

import Data.List (nub)
import qualified Intcode
import qualified Utils

part1 :: IO Int
part1 = do
  rawInput <- readFile "input/day07.txt"
  let parsedIntcode = Intcode.parseNumericInput rawInput
      allAmpResults = map (ampOutput parsedIntcode) $ generatePhaseSequences [0 .. 4]
  return $ maximum allAmpResults

part2 :: IO Int
part2 = do
  rawInput <- readFile "input/day07.txt"
  let parsedIntcode = Intcode.parseNumericInput rawInput
      feedbackAmpResults = map (ampOutput parsedIntcode) $ generatePhaseSequences [5 .. 9]
  return $ maximum feedbackAmpResults

generatePhaseSequences :: [Int] -> [[Int]]
generatePhaseSequences range = filter ((== 5) . length . nub) . mapM (const range) $ [1 .. 5]

ampOutput :: [Int] -> [Int] -> Int
ampOutput intcode = phaseFeedbackLoop . initializePhaseVM intcode

newtype FeedbackVM = FeedbackVM [Intcode.Program] deriving (Show)

initializePhaseVM :: [Int] -> [Int] -> FeedbackVM
initializePhaseVM program phaseSeq =
  FeedbackVM $ initWithInput [head phaseSeq, 0] : map (\phase -> initWithInput [phase]) (tail phaseSeq)
  where
    initWithInput input = Intcode.Executing 0 program input []

phaseFeedbackLoop :: FeedbackVM -> Int
phaseFeedbackLoop (FeedbackVM amps) =
  if all ampHalted ampResults
    then let (Intcode.Finished output _) = last ampResults in head output
    else phaseFeedbackLoop $ FeedbackVM continuedPrograms
  where
    ampResults = map Intcode.executeIntcode amps
    feedPairs = Utils.itemPairs $ last ampResults : ampResults
    continuedPrograms = map (\(prev, current) -> continueWithInput current $ extractOutput prev) feedPairs

extractOutput :: Intcode.Program -> [Int]
extractOutput (Intcode.Finished output _) = output
extractOutput (Intcode.Error _ _ _ output) = output

continueWithInput :: Intcode.Program -> [Int] -> Intcode.Program
continueWithInput (Intcode.Error cursor codes _ _) newInputs =
  Intcode.executeIntcode newProgram
  where
    newProgram = Intcode.Executing cursor codes newInputs []
continueWithInput other _ = other

ampHalted :: Intcode.Program -> Bool
ampHalted Intcode.Finished {} = True
ampHalted _ = False