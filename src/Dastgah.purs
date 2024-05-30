module Dastgah (analysisDastgahPattern, getMIDIInterval) where

import Prelude
import Data.Int (toNumber)
import Data.Array (filter,fromFoldable,(!!), zipWith, replicate, concat, (..), (:), init, tail, last,head,reverse,zip, cons, uncons, snoc, length, singleton) 
import Data.List
import Data.List (fromFoldable,concat,zip,zipWith,length,init,uncons) as L
import Data.Maybe 
import Data.Tuple


import AST
import DurationAndIndex

getMIDIInterval:: Array DastgahNote -> Array Number
getMIDIInterval xs = map (\x -> x.midiInterval) xs

analysisDastgahPattern::Span -> Rhythmic -> Array Int -> Array DastgahNote
analysisDastgahPattern CycleEvent _ ns = map assambleDastgahNote zipped
  where first = ns
        s = fromMaybe {head: 0, tail: []} $ uncons ns
        second = snoc s.tail s.head
        zipped = zip first second
analysisDastgahPattern CycleBlock _ ns = map assambleDastgahNote zipped
  where first = ns
        s = fromMaybe {head: 0, tail: []} $ uncons ns
        second = snoc s.tail s.head
        zipped = zip first second
analysisDastgahPattern CycleInBlock r ns = map assambleDastgahNote zipped
  where structure = map (\x -> (x `mod` (length ns))) $ map (\x -> fromMaybe 0 $ head x) $ rhythmicStructIndex r [0]  
        seque = map (\x -> fromMaybe 0 (ns !! x)) structure
        first = seque
        s = fromMaybe {head: 0, tail: []} $ uncons seque
        second = snoc s.tail s.head
        zipped = zip first second
analysisDastgahPattern SpreadBlock r ns = map assambleDastgahNote zipped
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

assambleDastgahNote:: Tuple Int Int -> DastgahNote
assambleDastgahNote (Tuple x y) = {function: fu, movement: mov, midiInterval: checkedMidiInt}
  where (Tuple midiInter fu) = shurIntToFuncAndMIDIInt x
        mov = getMovement x y
        checkedMidiInt = if x == 6 then checkSixth mov midiInter else midiInter

checkSixth:: Interval -> Number -> Number
checkSixth UpJump midiInter = midiInter + 0.92
checkSixth UpNext midiInter = midiInter + 0.92
checkSixth _ midiInter = midiInter

getMovement:: Int -> Int -> Interval
getMovement note target 
  | (note < target) && (target == (note+1)) = UpNext
  | (note < target) && (target /= (note+1)) = UpJump
  | (note > target) && (target == (note-1)) = DownNext
  | (note > target) && (target /= (note-1)) = DownJump
  | note == target = Unison
  | otherwise = Unison


---- need octaves!!! -- start from 0 to 6
shurIntToFuncAndMIDIInt:: Int -> Tuple Number String
shurIntToFuncAndMIDIInt n = case (n-1)`mod`8 of
                          0 -> Tuple 0.0 "unknown" 
                          1 -> Tuple 1.82 "unknown"
                          2 -> Tuple 2.96 "unknown"
                          3 -> Tuple 5.0 "unknown"
                          4 -> Tuple 7.04 "unknown"
                          5 -> Tuple 7.94 "unknown" -- upwards move 8.86
                          6 -> Tuple 9.98 "unknown"
                          7 -> Tuple 12.0 "unknown"
                          _ -> Tuple 0.0 "unknown"


-- 0 1.82 2.96 5.0 7.04 7.94 9.98 12.0
-- data Dastgah = Shur (List Int)
dToList:: Dastgah -> List Number
dToList (Shur ns) = map f ns
  where f n = case (n-1)`mod`8 of
                0 -> 0.0 
                1 -> 1.82
                2 -> 2.96
                3 -> 5.0
                4 -> 7.04 
                5 -> 7.94
                6 -> 9.98
                7 -> 12.0
                _ -> 0.0
dToList _ = Nil