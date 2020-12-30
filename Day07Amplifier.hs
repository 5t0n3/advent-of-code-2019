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

-- | Generates all amplifier phase sequences within the given range.
generatePhaseSequences :: [Int] -> [[Int]]
generatePhaseSequences range = filter ((== 5) . length . nub) . mapM (const range) $ [1 .. 5]

-- | Returns the output of (amplifier E) after all | amplifiers halt.
ampOutput :: [Int] -> [Int] -> Int
ampOutput intcode = phaseFeedbackLoop . initializePhaseVM intcode

-- | Represents a set of amplifiers, each of which running an Intcode program.
newtype AmplifierVM = AmplifierVM [Intcode.Program] deriving (Show)

-- | Initializes an Intcode VM with 5 amplifiers with the given phase sequence.
initializePhaseVM :: [Int] -> [Int] -> AmplifierVM
initializePhaseVM program phaseSeq =
  AmplifierVM $ initWithInput [head phaseSeq, 0] : map (\phase -> initWithInput [phase]) (tail phaseSeq)
  where
    initWithInput input = Intcode.Executing 0 program input []

-- | Runs the amplifiers in the given VM until they all halt.
-- Returns the output of the final amplifier (amplifier E).
phaseFeedbackLoop :: AmplifierVM -> Int
phaseFeedbackLoop (AmplifierVM amps) =
  if all ampHalted ampResults
    then let (Intcode.Finished output _) = last ampResults in head output
    else phaseFeedbackLoop $ AmplifierVM continuedPrograms
  where
    ampResults = map Intcode.executeIntcode amps
    feedPairs = Utils.itemPairs $ last ampResults : ampResults
    continuedPrograms = map (\(prev, current) -> continueWithInput current $ extractOutput prev) feedPairs

-- | Extracts the output from the supplied Intcode program.
-- Only works if it exited normally or is awaiting input.
extractOutput :: Intcode.Program -> [Int]
extractOutput (Intcode.Finished output _) = output
extractOutput (Intcode.NeedsInput _ _ _ output) = output

-- | Continues the execution of an Intcode program with the supplied input.
-- If it doesn't need input, the original program is returned instead.
continueWithInput (Intcode.NeedsInput cursor codes _ _) newInputs =
  Intcode.executeIntcode newProgram
  where
    -- Outputs are cleared as they cause issues otherwise
    newProgram = Intcode.Executing cursor codes newInputs []
continueWithInput other _ = other

-- | Returns `True` if the given Intcode program exited normally; otherwise
-- returns `False`.
ampHalted :: Intcode.Program -> Bool
ampHalted Intcode.Finished {} = True
ampHalted _ = False