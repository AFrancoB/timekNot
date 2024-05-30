module XenoPitch (xenoPitchAsAuralPattern, xenoPitchToMIDIInterval, testXN, xenoPitchAsMIDINum) where

import Prelude

import Partial.Unsafe

import Data.Array ((:), elem, filter,unsafeIndex, length, sortWith, zip, (!!), fromFoldable, cons, uncons, snoc, init, tail, last,head,reverse, replicate, concat)

import Data.List (scanl)

import Data.Tuple
import Data.Int (toNumber, floor)
import Data.Number (floor) as N
import Data.Maybe 

import Data.Set (Set(..))
import Data.Set as Set

import Erv (makeCPSScale,ratioToCents)
import AST
import DurationAndIndex

---- this top is the list processed in AuralSpecs along with span and Value
-- top:: XenoPitch -> Array Number

-- lista: [0,1,2,3]
-- xnAsMIDI: [0, 2.5, 3.3, 5.5, 7.7, 8.8, 10.1]

-- data XenoPitch = CPSet Int (Array Int) (Array Int) | MOS Int Int | EDO Number Int
testXP = CPSet 2 [1,3,5,7] (Just $ [Unions [1,3]])
testXN = xenoPitchAsAuralPattern (Tuple testXP $ Just 1) [0,1,2,3]

--- new fucntion that receives tuning system / note and index and can produce the midiInterval required

xenoPitchAsMIDINum:: Tuple XenoPitch (Maybe Int) -> Int -> Number
xenoPitchAsMIDINum (Tuple xn (Just i)) nota = asMIDI
    where   scaleAsMIDISubsets = xenoPitchToMIDIInterval xn -- Array Array Num
            subset = fromMaybe [0.0] $ scaleAsMIDISubsets !! i
            lengthOfSet = length subset
            (Tuple index octave) = cycleAndOctavesOfPatternInSet' nota lengthOfSet
            asMIDI = (fromMaybe (0.0) $ subset !! index) + octave
xenoPitchAsMIDINum (Tuple xn Nothing) nota = asMIDI
    where   scaleAsMIDISubsets = xenoPitchToMIDIInterval xn -- Array Array Num
            subset = fromMaybe [2.666] $ scaleAsMIDISubsets !! 0
            lengthOfSet = length subset
            (Tuple index octave) = cycleAndOctavesOfPatternInSet' nota lengthOfSet
            asMIDI = (fromMaybe (0.0) $ subset !! index) + octave

cycleAndOctavesOfPatternInSet':: Int -> Int -> Tuple Int Number
cycleAndOctavesOfPatternInSet' n setLen = Tuple cycledNote isOctave
    where cycledNote = n `mod` setLen
          isOctave = toNumber $ (floor $ (toNumber n) / (toNumber setLen))*12

xenoPitchAsAuralPattern:: Tuple XenoPitch (Maybe Int) -> Array Int -> Span -> Rhythmic -> Array Number
xenoPitchAsAuralPattern (Tuple ShurNot Nothing) lista sp r = map (\n-> n.midiInterval) shurNot
    where shurNot = analysisShurNotPattern sp r lista
xenoPitchAsAuralPattern (Tuple Centaura Nothing) lista _ _ = midiNumber
    where midiNumber = map (\n -> (centaura n) + (addOctave n)) lista
xenoPitchAsAuralPattern (Tuple xn (Just i)) lista _ _ = asMIDI
    where   scaleAsMIDISubsets = xenoPitchToMIDIInterval xn -- Array Array Num
            subset = fromMaybe [0.0] $ scaleAsMIDISubsets !! i
            lengthOfSet = length subset
            cyclesAndOctave = cycleAndOctavesOfPatternInSet lista lengthOfSet
            asMIDI = map (\(Tuple index octave) -> (fromMaybe (0.0) $ subset !! index) + octave) cyclesAndOctave
xenoPitchAsAuralPattern (Tuple xn Nothing) lista _ _ = asMIDI
    where   scaleAsMIDISubsets = xenoPitchToMIDIInterval xn -- Array Array Num
            subset = fromMaybe [2.666] $ scaleAsMIDISubsets !! 0
            lengthOfSet = length subset
            cyclesAndOctave = cycleAndOctavesOfPatternInSet lista lengthOfSet
            asMIDI = map (\(Tuple index octave) -> (fromMaybe (0.0) $ subset !! index) + octave) cyclesAndOctave

cycleAndOctavesOfPatternInSet:: Array Int -> Int -> Array (Tuple Int Number)
cycleAndOctavesOfPatternInSet ns setLen = zip cycledList isOctave
    where cycledList = map (\n -> n `mod` setLen) ns
          isOctave = map (\n -> toNumber $ (floor $ (toNumber n) / (toNumber setLen))*12 ) ns

addOctave:: Int -> Number
addOctave n = 12.0 * (N.floor $ (toNumber n) / 12.0)

centaura:: Int -> Number
centaura n = case n`mod`12 of
                0 -> 0.0
                1 -> 53.27294323014412*0.01
                2 -> 203.91000173077484*0.01
                3 -> 266.8709056037379*0.01
                4 -> 386.3137138648348*0.01
                5 -> 498.04499913461217*0.01
                6 -> 551.3179423647567*0.01
                7 -> 701.9550008653874*0.01
                8 -> 764.9159047383506*0.01
                9 -> 884.3587129994477*0.01
                10 -> 968.8259064691249*0.01
                11 -> 1088.2687147302222*0.01
                _ -> 0.0


analysisShurNotPattern:: Span -> Rhythmic -> Array Int -> Array ShurNot
analysisShurNotPattern CycleEvent _ ns = map assambleShurNot zipped
  where first = ns
        s = fromMaybe {head: 0, tail: []} $ uncons ns
        second = snoc s.tail s.head
        zipped = zip first second
analysisShurNotPattern CycleBlock _ ns = map assambleShurNot zipped
  where first = ns
        s = fromMaybe {head: 0, tail: []} $ uncons ns
        second = snoc s.tail s.head
        zipped = zip first second
analysisShurNotPattern CycleInBlock r ns = map assambleShurNot zipped
  where structure = map (\x -> (x `mod` (length ns))) $ map (\x -> fromMaybe 0 $ head x) $ rhythmicStructIndex r [0]  
        seque = map (\x -> fromMaybe 0 (ns !! x)) structure
        first = seque
        s = fromMaybe {head: 0, tail: []} $ uncons seque
        second = snoc s.tail s.head
        zipped = zip first second
analysisShurNotPattern SpreadBlock r ns = map assambleShurNot zipped
  where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets r -- xxx[xx] : 0 0.25 0.5 0.75 0.875
        segment = 1.0 / toNumber (length ns)  -- 1 3 5 : 0.333333
        limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length ns) - 1) segment) -- [0, 0.333, 0.666]
        limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length ns) - 1) segment) 1.0 -- [0.333, 0.666, 1]
        limits = zip limitsFst limitsSnd  -- [(0,0.333), (0.333,0.666), (0.666,1)]
        noteLimits = zip ns limits -- [(1,(0,0.333)),(3,(0.333,0.666)),(5,(0.666,1))]
        funka:: Array (Tuple Int (Tuple Number Number)) -> Number -> Array Int
        funka noteLimits percenPos = map fst $ filter (\(Tuple _ limit) -> (percenPos >= (fst limit)) && (percenPos < (snd limit))) noteLimits
        realNS = concat $ map (\percenPos -> funka noteLimits percenPos) $ fromFoldable percenPositions 
        first = realNS
        s = fromMaybe {head: 0, tail: []} $ uncons realNS
        second = snoc s.tail s.head
        zipped = zip first second

 --- this tuple is the interval to analyse
assambleShurNot:: Tuple Int Int -> ShurNot
assambleShurNot (Tuple x y) = {movement: mov, midiInterval: checkedMidiInt}
  where midiInter = shurIntToMIDIInt x
        mov = getMovement x y
        secondChecked = if x == 8 then checkSec mov midiInter else midiInter
        sixthChecked = if x == 12 then checkSixth mov midiInter else secondChecked
        checkedMidiInt = sixthChecked

checkSec:: Interval -> Number -> Number
checkSec DownJump midiInter = (165.00422849992202 * 0.01) - (111.73128526977847 * 0.01) - midiInter
checkSec DownNext midiInter = (165.00422849992202 * 0.01) - (111.73128526977847 * 0.01) - midiInter
checkSec _ midiInter = midiInter

checkSixth:: Interval -> Number -> Number
checkSixth UpJump midiInter = midiInter + (866.9592293653092 * 0.01) - (813.6862861351652 * 0.01)
checkSixth UpNext midiInter = midiInter + (866.9592293653092 * 0.01) - (813.6862861351652 * 0.01)
checkSixth _ midiInter = midiInter




getMovement:: Int -> Int -> Interval
getMovement note target 
  | (note < target) && (target == (note+1)) = UpNext
  | (note < target) && (target /= (note+1)) = UpJump
  | (note > target) && (target == (note-1)) = DownNext
  | (note > target) && (target /= (note-1)) = DownJump
  | note == target = Unison
  | otherwise = Unison


shurIntToMIDIInt:: Int -> Number
shurIntToMIDIInt n = case n`mod`26 of
                          0 -> 0.0 - 24.0
                          1 -> (701.9550008653874 * 0.01) - 24.0
                          2 -> 0.0 - 12.0
                          3 -> (165.00422849992202*0.01) - 12.0
                          4 -> (315.64128700055255*0.01) - 12.0
                          5 -> (582.51219260429*0.01) - 12.0
                          6 -> (813.6862861351652*0.01) - 12.0
                          7 -> 0.0
                          8 -> 165.00422849992202 * 0.01 -- desciende con 111.73128526977847 
                          9 -> 315.64128700055255 * 0.01
                          10 -> 498.04499913461217 * 0.01
                          11 -> 701.9550008653874 * 0.01
                          12 -> 813.6862861351652 * 0.01  -- asciende con 866.9592293653092
                          13 -> 1017.5962878659401 * 0.01
                          14 -> 12.0 
                          15 -> 12.0 + (111.73128526977847 * 0.01)
                          16 -> 12.0 + (165.00422849992202 * 0.01)
                          17 -> 12.0 + (315.64128700055255 * 0.01)
                          18 -> 12.0 + (378.6021908735147 * 0.01)
                          19 -> 12.0 + (498.04499913461217 * 0.01)
                          20 -> 12.0 + (582.51219260429 * 0.01)
                          21 -> 12.0 + (701.9550008653874 * 0.01)
                          22 -> 12.0 + (813.6862861351652 * 0.01)
                          23 -> 12.0 + (866.9592293653092 * 0.01)
                          24 -> 12.0 + (1017.5962878659401 * 0.01)
                          25 -> 12.0 + (1080.557191738903 * 0.01)
                          _ -> 0.0

type ShurNot = {
  movement:: Interval,
  midiInterval:: Number
}

--  [4
--   (0.0  *  e
--    111.73128526977847
--    165.00422849992202  * ft
--    315.64128700055255  *  g
--    378.6021908735147
--    498.04499913461217  *
--    582.51219260429       bb
--    701.9550008653874   *
--    813.6862861351652  *  c   descendente
--    866.9592293653092  *   ascendente
--    1017.5962878659401  *
--    1080.557191738903)]

---- the ordering of subsets is still buggy, figure it out!! Jan 2024
xenoPitchToMIDIInterval:: XenoPitch -> Array (Array Number)
xenoPitchToMIDIInterval (CPSet size factors Nothing) = map (addSampleRoot <<< toMIDIInterval) [scale]
    where scale = makeCPSScale size factors -- Array XenoNote
xenoPitchToMIDIInterval (CPSet size factors (Just subsets)) = map (addSampleRoot <<< toMIDIInterval) (scale : subs)
    where scale = makeCPSScale size factors -- Array XenoNote
          subs = map (orderSetofXNotes <<< getSubSet scale) subsets
xenoPitchToMIDIInterval _ = []

getSubSet:: Array XenoNote -> Subset -> Array XenoNote
getSubSet xn subset = fromFoldable $ getSubset' xn subset 

------ this function might not be working properly.... also check the ordering func above
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

orderSetofXNotes:: Array XenoNote -> Array XenoNote
orderSetofXNotes s = sortWith (_."bounded-ratio") s 

-- sortWith (_.age) [{name: "Alice", age: 42}, {name: "Bob", age: 21}]
--    = [{name: "Bob", age: 21}, {name: "Alice", age: 42}]


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