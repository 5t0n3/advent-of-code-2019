module Day07Amplifier where

import Data.List (nub)
import qualified Intcode
import qualified Utils

part1 :: IO Int
part1 = do
  rawInput <- readFile "input/day07.txt"
  let parsedIntcode = Intcode.parseNumericInput rawInput
      phaseSequences = filter ((== 5) . length . nub) . mapM (const [0 .. 4]) $ [1 .. 5]
      dummyInput = Intcode.Finished [0] []
      allAmpResults = [head output | phaseSeq <- phaseSequences, let (Intcode.Finished output _) = feedOutputCycle parsedIntcode dummyInput phaseSeq]
  return $ maximum allAmpResults

part2 :: IO Int
part2 = do
  rawInput <- readFile "input/day07.txt"
  let parsedIntcode = Intcode.parseNumericInput rawInput
      phaseSequences = filter ((== 5) . length . nub) . mapM (const [5 .. 9]) $ [1 .. 5]
      feedbackAmpResults = [phaseFeedbackLoop phaseVM | phaseSeq <- phaseSequences, let phaseVM = initializePhaseVM parsedIntcode phaseSeq]
  return $ maximum feedbackAmpResults

feedOutputCycle :: [Int] -> Intcode.Program -> [Int] -> Intcode.Program
feedOutputCycle _ input [] = input
feedOutputCycle _ err@Intcode.Error {} _ = err
feedOutputCycle baseProgram (Intcode.Finished input _) (currentPhase : rest) =
  case programOutput of
    finished@(Intcode.Finished output _) -> feedOutputCycle baseProgram finished rest
    err@Intcode.Error {} -> err
  where
    programOutput = Intcode.executeIntcode $ Intcode.Executing 0 baseProgram (currentPhase : input) []

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