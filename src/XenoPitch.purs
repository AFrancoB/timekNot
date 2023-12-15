module XenoPitch (xenoPitchAsAuralPattern, xenoPitchToMIDIInterval, testXN) where

import Prelude

import Partial.Unsafe

import Data.Array ((:), elem, filter,unsafeIndex, length, zip, (!!), fromFoldable)
import Data.Tuple
import Data.Int (toNumber, floor)
import Data.Maybe 

import Data.Set (Set(..))
import Data.Set as Set

import Erv (makeCPSScale,ratioToCents)
import AST

---- this top is the list processed in AuralSpecs along with span and Value
-- top:: XenoPitch -> Array Number

-- lista: [0,1,2,3]
-- xnAsMIDI: [0, 2.5, 3.3, 5.5, 7.7, 8.8, 10.1]

-- data XenoPitch = CPSet Int (Array Int) (Array Int) | MOS Int Int | EDO Number Int
testXP = CPSet 2 [1,3,5,7] Nothing
testXN = xenoPitchAsAuralPattern (Tuple testXP Nothing) [0,1,2,3]

--- new fucntion that receives tuning system / note and index and can produce the midiInterval required



xenoPitchAsAuralPattern:: Tuple XenoPitch (Maybe Int) -> Array Int -> Array Number
xenoPitchAsAuralPattern (Tuple xn (Just i)) lista = toMIDI
    where   scaleAsMIDISubsets = xenoPitchToMIDIInterval xn -- Array Array Num
            subset = fromMaybe [0.0] $ scaleAsMIDISubsets !! i
            lengthOfSet = length subset
            cyclesAndOctave = cycleAndOctavesOfPatternInSet lista lengthOfSet
            toMIDI = map (\(Tuple index octave) -> (fromMaybe (0.0) $ subset !! index) + octave) cyclesAndOctave
xenoPitchAsAuralPattern (Tuple xn Nothing) lista = toMIDI
    where   scaleAsMIDISubsets = xenoPitchToMIDIInterval xn -- Array Array Num
            subset = fromMaybe [2.666] $ scaleAsMIDISubsets !! 0
            lengthOfSet = length subset
            cyclesAndOctave = cycleAndOctavesOfPatternInSet lista lengthOfSet
            toMIDI = map (\(Tuple index octave) -> (fromMaybe (0.0) $ subset !! index) + octave) cyclesAndOctave
            

cycleAndOctavesOfPatternInSet:: Array Int -> Int -> Array (Tuple Int Number)
cycleAndOctavesOfPatternInSet ns setLen = zip cycledList isOctave
    where cycledList = map (\n -> n `mod` setLen) ns
          isOctave = map (\n -> toNumber $ (floor $ (toNumber n) / (toNumber setLen))*12 ) ns



xenoPitchToMIDIInterval:: XenoPitch -> Array (Array Number)
xenoPitchToMIDIInterval (CPSet size factors Nothing) = map (addSampleRoot <<< toMIDIInterval) [scale]
    where scale = makeCPSScale size factors -- Array XenoNote


xenoPitchToMIDIInterval (CPSet size factors (Just subsets)) = map (addSampleRoot <<< toMIDIInterval) (scale : subs)
    where scale = makeCPSScale size factors -- Array XenoNote
          subs = map (getSubSet scale) subsets
xenoPitchToMIDIInterval _ = []

getSubSet:: Array XenoNote -> Subset -> Array XenoNote
getSubSet xn subset = fromFoldable $ getSubset' xn subset 

getSubset':: Array XenoNote -> Subset -> Set XenoNote
getSubset' xn (Subset isInSet) = Set.fromFoldable $ filter (\x -> elem isInSet x.set ) xn
getSubset' xn (Unions ns) = Set.unions $ map f ns
    where f isInSet = Set.fromFoldable $ filter (\x -> elem isInSet x.set ) xn
getSubset' xn (Intersection a b) = Set.intersection a' b'
    where a' = Set.fromFoldable $ filter (\x -> elem a x.set ) xn
          b' = Set.fromFoldable $ filter (\x -> elem b x.set ) xn
getSubset' xn (Difference a b) = Set.difference a' b'
    where a' = Set.fromFoldable $ filter (\x -> elem a x.set ) xn
          b' = Set.fromFoldable $ filter (\x -> elem b x.set ) xn
getSubset' _ _ = Set.fromFoldable []

toMIDIInterval:: Array XenoNote -> Array Number
toMIDIInterval xns = map toMIDIInterval' xns

toMIDIInterval':: XenoNote -> Number
toMIDIInterval' xn = (ratioToCents xn."bounded-ratio") / 100.0

addSampleRoot:: Array Number -> Array Number
addSampleRoot xs = 0.0 : xs


-- parseo:
-- v0.myCPS[0] = _ 0 1 2 3 4 5 6;

-- {
-- myCPS <- cps 2 (1,3,5,7) | setsWith 3, setsWith 5, setsWith 7;

-- myMOS <- mos 12 7 0;

-- }

-- where myCPS is an array and index 0 is the set, index 1 is setsWith 3, index 2 is setsWith 5, etc...
-- third arg of mos is rotation 