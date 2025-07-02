module Dastgah (analysisDastgahPattern, getMIDIInterval) where

import Prelude
import Data.Int (toNumber)
import Data.Array (filter,fromFoldable,(!!), zipWith, replicate, concat, (..), (:), init, tail, last,head,reverse,zip, cons, uncons, snoc, length, singleton) 
import Data.List
import Data.List (fromFoldable,concat,zip,zipWith,length,init,uncons) as L
import Data.Maybe 
import Data.Tuple
import Data.Number (floor)


import AST
import DurationAndIndex

getMIDIInterval:: Array DastgahNote -> Array Number
getMIDIInterval xs = map (\x -> x.midiInterval) xs

analysisDastgahPattern::Span -> Rhythmic -> Dastgah -> Array DastgahNote
analysisDastgahPattern CycleEvent _ d = map (assambleDastgahNote d) zipped
  where ns = fromFoldable (getDastgahList d)
        first = ns
        s = fromMaybe {head: 0, tail: []} $ uncons ns
        second = snoc s.tail s.head
        zipped = zip first second
analysisDastgahPattern CycleBlock _ d = map (assambleDastgahNote d) zipped
  where ns = fromFoldable (getDastgahList d)
        first = ns
        s = fromMaybe {head: 0, tail: []} $ uncons ns
        second = snoc s.tail s.head
        zipped = zip first second
analysisDastgahPattern CycleInBlock r d = map (assambleDastgahNote d) zipped
  where ns = fromFoldable (getDastgahList d)
        structure = map (\x -> (x `mod` (length ns))) $ map (\x -> fromMaybe 0 $ head x) $ rhythmicStructIndex r [0]  
        seque = map (\x -> fromMaybe 0 (ns !! x)) structure
        first = seque
        s = fromMaybe {head: 0, tail: []} $ uncons seque
        second = snoc s.tail s.head
        zipped = zip first second
analysisDastgahPattern SpreadBlock r d = map (assambleDastgahNote d) zipped
  where ns = fromFoldable (getDastgahList d)
        percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets r -- xxx[xx] : 0 0.25 0.5 0.75 0.875
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

-- need to corroborate that octave works
assambleDastgahNote:: Dastgah -> Tuple Int Int -> DastgahNote
assambleDastgahNote d (Tuple x y) = {function: funcion, movement: mov, midiInterval: midiInter + octave}
  where mov = getMovement x y
        octave = getOctave x 
        (Tuple midiInter funcion) = dastgahToNote d mov x -- shurIntToFuncAndMIDIInt x

getOctave:: Int -> Number
getOctave n = (floor ((toNumber n)/7.0))*12.0

checkMoteghayyer:: Interval -> Number -> Number
checkMoteghayyer UpJump midiInter = midiInter + 0.5
checkMoteghayyer UpNext midiInter = midiInter + 0.5
checkMoteghayyer _ midiInter = midiInter

getMovement:: Int -> Int -> Interval
getMovement note target 
  | (note < target) && (target == (note+1)) = UpNext
  | (note < target) && (target /= (note+1)) = UpJump
  | (note > target) && (target == (note-1)) = DownNext
  | (note > target) && (target /= (note-1)) = DownJump
  | note == target = Unison
  | otherwise = Unison

dastgahToNote:: Dastgah -> Interval -> Int -> Tuple Number String
dastgahToNote (Shur _) mov n = Tuple checkedMidiInt funcion
  where (Tuple midiInter funcion) = shurIntToFuncAndMIDIInt n
        checkedMidiInt = if funcion == "Moteghayyer" then checkMoteghayyer mov midiInter else midiInter
dastgahToNote (Segah _) mov n = Tuple checkedMidiInt funcion
  where (Tuple midiInter funcion) = segahIntToFuncAndMIDIInt n
        checkedMidiInt = if funcion == "Moteghayyer" then checkMoteghayyer mov midiInter else midiInter
dastgahToNote (Nava _) mov n = Tuple checkedMidiInt funcion
  where (Tuple midiInter funcion) = navaIntToFuncAndMIDIInt n
        checkedMidiInt = if funcion == "Moteghayyer" then checkMoteghayyer mov midiInter else midiInter
dastgahToNote (Homayun _) mov n = Tuple checkedMidiInt funcion
  where (Tuple midiInter funcion) = homayunIntToFuncAndMIDIInt n
        checkedMidiInt = if funcion == "Moteghayyer" then checkMoteghayyer mov midiInter else midiInter
dastgahToNote (Chahargah _) mov n = Tuple checkedMidiInt funcion
  where (Tuple midiInter funcion) = chahargahIntToFuncAndMIDIInt n
        checkedMidiInt = if funcion == "Moteghayyer" then checkMoteghayyer mov midiInter else midiInter
dastgahToNote (Mahur _) mov n = Tuple checkedMidiInt funcion
  where (Tuple midiInter funcion) = chahargahIntToFuncAndMIDIInt n
        checkedMidiInt = if funcion == "Moteghayyer" then checkMoteghayyer mov midiInter else midiInter
dastgahToNote (RastPanjgah _) mov n = Tuple checkedMidiInt funcion
  where (Tuple midiInter funcion) = chahargahIntToFuncAndMIDIInt n
        checkedMidiInt = if funcion == "Moteghayyer" then checkMoteghayyer mov midiInter else midiInter

rastPanjgahIntToFuncAndMIDIInt:: Int -> Tuple Number String
rastPanjgahIntToFuncAndMIDIInt n = case (n`mod`7) of
                          0 -> Tuple 0.0 "Āghāz Finalis" 
                          1 -> Tuple 2.0 "Shāhed"
                          2 -> Tuple 4.0 "note"
                          3 -> Tuple 5.0 "Āghāz Finalis"
                          4 -> Tuple 7.0 "note"
                          5 -> Tuple 9.0 "note" 
                          6 -> Tuple 10.0 "note"
                          _ -> Tuple 0.0 "unknown"

mahurIntToFuncAndMIDIInt:: Int -> Tuple Number String
mahurIntToFuncAndMIDIInt n = case (n`mod`7) of
                          0 -> Tuple 0.0 "Āghāz Finalis" 
                          1 -> Tuple 2.0 "Shāhed"
                          2 -> Tuple 4.0 "note"
                          3 -> Tuple 5.0 "note"
                          4 -> Tuple 7.0 "note"
                          5 -> Tuple 9.0 "note" 
                          6 -> Tuple 11.0 "note"
                          _ -> Tuple 0.0 "unknown"

chahargahIntToFuncAndMIDIInt:: Int -> Tuple Number String
chahargahIntToFuncAndMIDIInt n = case (n`mod`7) of
                          0 -> Tuple 0.0 "Finalis" 
                          1 -> Tuple 1.5 "note"
                          2 -> Tuple 4.0 "note"
                          3 -> Tuple 5.0 "note"
                          4 -> Tuple 7.0 "note"
                          5 -> Tuple 8.5 "Āghāz" 
                          6 -> Tuple 11.0 "note"
                          _ -> Tuple 0.0 "unknown"

homayunIntToFuncAndMIDIInt:: Int -> Tuple Number String
homayunIntToFuncAndMIDIInt n = case (n`mod`7) of
                          0 -> Tuple 0.0 "note" 
                          1 -> Tuple 2.0 "note"
                          2 -> Tuple 3.0 "Moteghayyer" -- Moteghayyer
                          3 -> Tuple 5.0 "Ist"
                          4 -> Tuple 7.0 "Finalis"
                          5 -> Tuple 8.5 "Shāhed" 
                          6 -> Tuple 11.0 "note"
                          _ -> Tuple 0.0 "unknown"

navaIntToFuncAndMIDIInt:: Int -> Tuple Number String
navaIntToFuncAndMIDIInt n = case (n`mod`7) of
                          0 -> Tuple 0.0 "note" 
                          1 -> Tuple 2.0 "note"
                          2 -> Tuple 3.5 "Ist"
                          3 -> Tuple 5.0 "Āghāz"
                          4 -> Tuple 7.0 "Finalis"
                          5 -> Tuple 9.0 "note" 
                          6 -> Tuple 10.0 "note"
                          _ -> Tuple 0.0 "unknown"

segahIntToFuncAndMIDIInt:: Int -> Tuple Number String
segahIntToFuncAndMIDIInt n = case (n`mod`7) of
                          0 -> Tuple 0.0 "note" 
                          1 -> Tuple 2.0 "note" -- remove mote... keep 2.0 -- upwards move to 2.0
                          2 -> Tuple 3.5 "Āghāz, finalis, stop"
                          3 -> Tuple 5.0 "note"
                          4 -> Tuple 7.0 "note"
                          5 -> Tuple 8.5 "note" 
                          6 -> Tuple 10.0 "note"
                          _ -> Tuple 0.0 "unknown"

shurIntToFuncAndMIDIInt:: Int -> Tuple Number String
shurIntToFuncAndMIDIInt n = case (n`mod`7) of
                          0 -> Tuple 0.0 "Āghāz" 
                          1 -> Tuple 2.0 "Finalis"
                          2 -> Tuple 3.5 "note"
                          3 -> Tuple 5.0 "note"
                          4 -> Tuple 7.0 "note"
                          5 -> Tuple 8.5 "Moteghayyer" -- The quarter should be closer to A Natural -- upwards move 9.0
                          6 -> Tuple 10.0 "note"
                          _ -> Tuple 0.0 "unknown"

getDastgahList:: Dastgah -> List Int
getDastgahList (Shur ns) = ns
getDastgahList (Segah ns) = ns
getDastgahList (Nava ns) = ns
getDastgahList (Homayun ns) = ns
getDastgahList (Chahargah ns) = ns
getDastgahList (Mahur ns) = ns
getDastgahList (RastPanjgah ns) = ns
getDastgahList _ = Nil