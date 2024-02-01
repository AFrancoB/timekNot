module AuralSpecsOld where

import Prelude

import Effect (Effect)

import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Map as M
import Data.Foldable (sum)
import Data.Int
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array (filter,fromFoldable,(!!), zipWith, replicate, concat, (..), (:), init, tail, last,head,reverse,zip, cons, uncons, snoc, length, singleton)
import Data.List
import Data.List (fromFoldable,concat,zip,zipWith,length,init,uncons) as L
import Data.Traversable (scanl)

import Record

import Data.Newtype

import Data.Tempo

import AST
import DurationAndIndex
import Parser
import Rhythm
import TestOpsAndDefs
import XenoPitch
import Dastgah

import Data.Rational (Rational(..), (%), fromInt)
import Data.Rational (toNumber) as R
import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration


type Waste = {
  whenPosix:: Number, 
  s:: String, 
  n:: Int,
  gain:: Number,
  pan:: Number,
  speed:: Number,
  begin:: Number,
  end:: Number,
  note:: Number
  }

type AlmostWaste = {
  event:: Event, 
  s:: String, 
  n:: Int,
  gain:: Number,
  pan:: Number,
  speed:: Number,
  begin:: Number,
  end:: Number,
  note:: Number
  }

---- ISSUE SOLVE SOON!  
----- parser should fail if list of values is empty. List should be at least 1


-- transposeWith needs to be implemented in almost all of this (except N)

{-
weight:
1. weight function allows users to change a parameter based on a weight given to a CP. 0% weight will output an unaffected parameter. 100% will modify the paramter as indicated by user. The process will extend by a number of events that is equivalent to the length of the rhythmic.

So: v0.speed = -_- 2 1  will generate a 2 in the event closest to the cFrom and a 1 at the event (cFrom +/- lengthRhythmic). All events in-between will be in a range between these two numbers.  
-}

{-
These are the fields/options for the record passed to playSample:
  buffer :: Foreign, -- Web Audio buffer of sample data to play (ie. new-style)
  s :: String, -- name of sample bank (ie. old-style with sampleMap)
  n :: Int, -- number of sample within a bank (ie. old-style with sampleMap)
  whenPosix :: Number, -- when to play the sample, in POSIX/epoch-1970 time
  when :: Number, -- when to play the sample, in audio context time
  gain :: Number, -- clamped from 0 to 2; 1 is default and full-scale
  overgain :: Number, -- additional gain added to gain to go past clamp at 2
  pan :: Number, -- range: 0 to 1
  nudge :: Number, -- nudge the time of the sample forwards/backwards in seconds
  speed :: Number,
  note :: Number,
  begin :: Number,
  end :: Number,
  cut :: Int,
  shape :: Number,
  cutoff :: Number,
  resonance :: Number,
  hcutoff :: Number,
  hresonance :: Number,
  bandf :: Number,
  bandq :: Number,
  vowel :: String,
  delay :: Number,
  delaytime :: Number,
  delayfeedback :: Number,
  loop :: Number,
  crush :: Number,
  coarse :: Number,
  unit :: String
-}

-- refactoring aural from List Value to List List Value broke the tranbsposition system in aural specs. Fix it!

-- auralSpecs:: Voices -> Rhythmic -> List (List Value) -> M.Map String XenoPitch -> Array Event -> Array AlmostWaste
-- auralSpecs m rhy aurals xenopitch events = concat $ map (\a -> auralSpecs' m rhy a xenopitch events) $ fromFoldable aurals 

-- -- aural specs processes one voice at the time
-- auralSpecs':: Voices -> Rhythmic -> List Value -> M.Map String XenoPitch -> Array Event -> Array AlmostWaste
-- auralSpecs' m rhy aural xenopitch events =
--     let sounds = processSound m rhy (getSound aural) events
--         nS = processN m rhy (getN aural) sounds 
--         gainNS = processGain m rhy (getGain aural) nS
--         panGNS = processPan m rhy (getPan aural) gainNS
--         speedPGNS = processSpeed m rhy (getSpeed aural) panGNS
--         beginSpPGNS = processBegin m rhy (getBegin aural) speedPGNS
--         endBSpPGNS = processEnd m rhy (getEnd aural) beginSpPGNS
--         noteEBSpPGNS = processNote m rhy (getNote aural) (getXeNote aural) xenopitch endBSpPGNS
--         -- cutoffEBSpPGNS = processCutOff
--         -- vowelCEBSpPGNS = processVowel
--     in noteEBSpPGNS


-- -- note
-- processNote:: Voices -> Rhythmic -> Maybe Value -> Maybe Value -> M.Map String XenoPitch ->
--                Array {event::Event, s::String, n::Int, gain::Number, pan::Number, speed::Number, begin::Number, end::Number}  -> 
--                Array {event::Event, s::String, n::Int, gain::Number, pan::Number, speed::Number, begin::Number, end::Number, note:: Number}
-- processNote _ _ Nothing _ _ ws = map (\w -> {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: w.end, note: 0.0}) ws
-- -- processNote m r (Just (TransposedNote id)) xn ws = findReferredNote r ws id m 

--         ----- work here, combine note with pitch system new function will be needed.
--         ----- (List (Tuple String (Maybe Int)))
-- processNote m r (Just (Prog span lista)) xeNotes xp ws = zipWith (\w n -> {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: w.end, note: n}) ws mergedAsMIDI 
--   where prog = processProg m r (Prog span lista) xp ws 
--         xeNote' = case xeNotes of
--                       Nothing -> [2666]
--                       Just val -> processXeNote m r val xp ws  -- :: map 
--         mergedAsMIDI = zipWith (\p xn -> pitchSystemNoteToMIDI p xn xp) prog xeNote'
--         -- merged should be :: Number (which represents a MIDI interval)

-- processNote _ r (Just (Dastgah span d)) _ _ ws = spanDastgah span newList ws r
--   where newList = getMIDIInterval $ analysisDastgahPattern span r $ fromFoldable (getDastgahList d)
-- processNote _ r (Just (Xeno id span lista)) _ xn ws = spanXeno span (fromFoldable midiIntervals) ws r
--   where target = fromMaybe (EDO 0.0 0) $ M.lookup (fst id) xn 
--         midiIntervals = xenoPitchAsAuralPattern (Tuple target (snd id)) $ fromFoldable lista
-- processNote _ _ _ _ _ _ = []

-- pitchSystemNoteToMIDI:: (Tuple String (Maybe Int)) -> Int -> M.Map String XenoPitch -> Number
-- pitchSystemNoteToMIDI (Tuple id subset) nota mapa = xenoPitchAsMIDINum (Tuple xn subset) nota 
--   where xn = fromMaybe (EDO 0.0 0) $ M.lookup id mapa

-- processProg:: Voices -> Rhythmic -> Value -> M.Map String XenoPitch ->  Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number, end:: Number} -> Array (Tuple String (Maybe Int))
-- processProg m r (Prog span xs) mapXeno ws = spanProg span (fromFoldable xs) ws r
-- processProg m r _ mapXeno ws = []

-- spanProg:: Span -> Array (Tuple String (Maybe Int)) -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number, end:: Number} -> Rhythmic -> Array (Tuple String (Maybe Int))
-- spanProg CycleEvent xs ws _ = map (processXN xs) ws
--   where processXN xs w = fromMaybe (Tuple "error" Nothing) $ xs !! (getEventIndex w.event `mod` length xs)
-- spanProg CycleBlock xs ws _ = map (processXN xs) ws
--   where processXN xs w = fromMaybe (Tuple "error" Nothing) $ xs !! (getBlockIndex w.event `mod` length xs)
-- spanProg CycleInBlock xs ws _ = map (processXN xs) ws
--   where processXN xs w = fromMaybe (Tuple "error" Nothing) $ xs !! (getStructureIndex w.event `mod` length xs)
-- spanProg SpreadBlock xs ws rhythmic = map (processXN xs) ws
--   where processXN xs w = n' 
--               where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
--                     modIndex = (getEventIndex w.event) `mod` (length $ fromFoldable percenPositions)
--                     percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
--                     segment = 1.0 / toNumber (length xs)
--                     limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
--                     limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
--                     limits = zip limitsFst limitsSnd
--                     nLimits = zip xs limits
--                     n' = fromMaybe (Tuple "error" Nothing) $ head $ map f $ filter isJust $ spreadProgs percenPos nLimits
--                         where f (Just x) = x
--                               f Nothing = (Tuple "error" Nothing)

-- spreadProgs:: Number -> Array (Tuple (Tuple String (Maybe Int)) (Tuple Number Number)) -> Array (Maybe (Tuple String (Maybe Int)))
-- spreadProgs percenPos nLimits = map (\sLimit -> spreadProg percenPos sLimit) nLimits

-- spreadProg::  Number -> Tuple (Tuple String (Maybe Int)) (Tuple Number Number) -> Maybe (Tuple String (Maybe Int))
-- spreadProg percenPos (Tuple n limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just n else Nothing

-- processXeNote:: Voices -> Rhythmic -> Value -> M.Map String XenoPitch ->  Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number, end:: Number} -> Array Int
-- processXeNote m r (XNotes span xs) mapXeno ws = spanXenote span (fromFoldable xs) ws r
-- processXeNote m r _ mapXeno ws = []

-- spanXenote:: Span -> Array Int -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number, end:: Number} -> Rhythmic -> Array Int
-- spanXenote CycleEvent xs ws _ = map (processXN xs) ws
--   where processXN xs w = fromMaybe 2666 $ xs !! (getEventIndex w.event `mod` length xs)
-- spanXenote CycleBlock xs ws _ = map (processXN xs) ws
--   where processXN xs w = fromMaybe 2666 $ xs !! (getBlockIndex w.event `mod` length xs)
-- spanXenote CycleInBlock xs ws _ = map (processXN xs) ws
--   where processXN xs w = fromMaybe 2666 $ xs !! (getStructureIndex w.event `mod` length xs)
-- spanXenote SpreadBlock xs ws rhythmic = map (processXN xs) ws
--   where processXN xs w = n' 
--               where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
--                     modIndex = (getEventIndex w.event) `mod` (length $ fromFoldable percenPositions)
--                     percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
--                     segment = 1.0 / toNumber (length xs)
--                     limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
--                     limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
--                     limits = zip limitsFst limitsSnd
--                     nLimits = zip xs limits
--                     n' = fromMaybe 2666 $ head $ map f $ filter isJust $ spreadXNs percenPos nLimits
--                         where f (Just x) = x
--                               f Nothing = 2666
  
-- spreadXNs:: Number -> Array (Tuple Int (Tuple Number Number)) -> Array (Maybe Int)
-- spreadXNs percenPos nLimits = map (\sLimit -> spreadXN percenPos sLimit) nLimits

-- spreadXN::  Number -> Tuple Int (Tuple Number Number) -> Maybe Int
-- spreadXN percenPos (Tuple n limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just n else Nothing


-- getXeNote:: List Value -> Maybe Value
-- getXeNote aural = head $ filter isXeNote $ fromFoldable aural

-- isXeNote:: Value -> Boolean
-- isXeNote (XNotes _ _) = true
-- isXeNote _ = false

-- getNote:: List Value -> Maybe Value
-- getNote aural = head $ filter isNote $ fromFoldable aural

-- isNote:: Value -> Boolean
-- isNote (Dastgah _ _) = true
-- -- isDastgah (TransposedDastgah _) = true
-- isNote (Xeno _ _ _) = true
-- isNote (Prog _ _) = true
-- isNote _ = false

-- -- abrir notacion para subsets y relacionarla con spanXeno!!!!
-- spanXeno:: Span -> Array Number -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number, end:: Number} -> Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number, end:: Number, note:: Number}
-- spanXeno span xs ws r = spanDastgah span xs ws r

-- getDastgahList:: Dastgah -> List Int
-- getDastgahList (Shur ns) = ns
-- getDastgahList _ = Nil

-- spanDastgah:: Span -> Array Number -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number, end:: Number} -> Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number, end:: Number, note:: Number}
-- spanDastgah CycleEvent ns ws _ = map (processDastgah ns) ws
--   where processDastgah ns w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: w.end, note: note} 
--               where note = fromMaybe 0.9666 $ ns !! (getEventIndex w.event `mod` length ns)
-- spanDastgah CycleBlock xs ws _ = map (processDastgah xs) ws
--   where processDastgah xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: w.end, note: note}  
--               where note = fromMaybe 0.9666 $ xs !! (getBlockIndex w.event `mod` length xs)
-- spanDastgah CycleInBlock xs ws _ = map (processDastgah xs) ws
--   where processDastgah xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: w.end, note: note} 
--               where note = fromMaybe 0.9666 $ xs !! (getStructureIndex w.event `mod` length xs)
-- spanDastgah SpreadBlock xs ws rhythmic = map (processDastgah xs) ws
--   where processDastgah xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: w.end, note: note} 
--               where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
--                     modIndex = (getEventIndex w.event) `mod` (length $ fromFoldable percenPositions)
--                     percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
--                     segment = 1.0 / toNumber (length xs)
--                     limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
--                     limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
--                     limits = zip limitsFst limitsSnd
--                     noteLimits = zip xs limits
--                     note = fromMaybe 0.9666 $ head $ map f $ filter isJust $ spreadNotes percenPos noteLimits
--                         where f (Just x) = x
--                               f Nothing = 0.9666

-- spreadNotes:: Number -> Array (Tuple Number (Tuple Number Number)) -> Array (Maybe Number)
-- spreadNotes percenPos bLimits = map (\sLimit -> spreadNote percenPos sLimit) bLimits

-- spreadNote::  Number -> Tuple Number (Tuple Number Number) -> Maybe Number
-- spreadNote percenPos (Tuple b limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just b else Nothing


-- -- end
-- processEnd:: Voices -> Rhythmic -> Maybe Value -> 
--                Array {event::Event, s::String, n::Int, gain::Number, pan::Number, speed::Number, begin::Number}  -> 
--                Array {event::Event, s::String, n::Int, gain::Number, pan::Number, speed::Number, begin::Number, end::Number}
-- processEnd _ _ _ [] = []
-- processEnd _ _ Nothing ws = map (\w -> {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: 1.0}) ws
-- processEnd m r (Just (TransposedEnd id n)) ws = findReferredEnd r ws (Tuple id n) m 
-- processEnd _ r (Just (End span eList)) ws = spanEnd span (fromFoldable eList) ws r
-- processEnd _ _ _ _ = []

-- findReferredEnd:: Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number} -> (Tuple String Int) -> Voices -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number, end:: Number}
-- findReferredEnd r ws (Tuple id n) mapa = processEnd mapa r newVal ws
--     where newVal = cycleAurals n $ M.lookup id mapa -- Maybe (Voice Temporal (List Aural))


-- spanEnd:: Span -> Array Number -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number} -> Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number, end:: Number}
-- spanEnd CycleEvent xs ws _ = map (processEnd xs) ws
--   where processEnd xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: end} 
--               where end = fromMaybe 0.9666 $ xs !! (getEventIndex w.event `mod` length xs)
-- spanEnd CycleBlock xs ws _ = map (processEnd xs) ws
--   where processEnd xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: end}  
--               where end = fromMaybe 0.9666 $ xs !! (getBlockIndex w.event `mod` length xs)
-- spanEnd CycleInBlock xs ws _ = map (processEnd xs) ws
--   where processEnd xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: end} 
--               where end = fromMaybe 0.9666 $ xs !! (getStructureIndex w.event `mod` length xs)
-- spanEnd SpreadBlock xs ws rhythmic = map (processEnd xs) ws
--   where processEnd xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: end} 
--               where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
--                     modIndex = (getEventIndex w.event) `mod` (length $ fromFoldable percenPositions)
--                     percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
--                     segment = 1.0 / toNumber (length xs)
--                     limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
--                     limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
--                     limits = zip limitsFst limitsSnd
--                     endLimits = zip xs limits
--                     end = fromMaybe 0.9666 $ head $ map f $ filter isJust $ spreadEnds percenPos endLimits
--                         where f (Just x) = x
--                               f Nothing = 0.9666

-- spreadEnds:: Number -> Array (Tuple Number (Tuple Number Number)) -> Array (Maybe Number)
-- spreadEnds percenPos bLimits = map (\sLimit -> spreadEnd percenPos sLimit) bLimits

-- spreadEnd::  Number -> Tuple Number (Tuple Number Number) -> Maybe Number
-- spreadEnd percenPos (Tuple b limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just b else Nothing

-- getEnd:: List Value -> Maybe Value
-- getEnd aural = head $ filter isEnd $ fromFoldable aural

-- isEnd:: Value -> Boolean
-- isEnd (End _ _) = true
-- isEnd (TransposedEnd _ _) = true
-- isEnd _ = false

-- -- begin
-- processBegin:: Voices -> Rhythmic -> Maybe Value -> 
--                Array {event::Event, s::String, n::Int, gain::Number, pan::Number, speed::Number}  -> 
--                Array {event::Event, s::String, n::Int, gain::Number, pan::Number, speed::Number, begin::Number}
-- processBegin _ _ _ [] = []
-- processBegin _ _ Nothing ws = map (\w -> {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: 0.0}) ws
-- processBegin m r (Just (TransposedBegin id n)) ws = findReferredBegin r ws (Tuple id n) m 
-- processBegin _ r (Just (Begin span bList)) ws = spanBegin span (fromFoldable bList) ws r
-- processBegin _ _ _ _ = []

-- findReferredBegin:: Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number} -> Tuple String Int -> Voices -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number}
-- findReferredBegin r ws (Tuple id n) mapa = processBegin mapa r newVal ws
--     where newVal = cycleAurals n $ M.lookup id mapa

-- spanBegin:: Span -> Array Number -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number} -> Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number}
-- spanBegin CycleEvent xs ws _ = map (processBn xs) ws
--   where processBn xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: bn} 
--               where bn = fromMaybe 0.0666 $ xs !! (getEventIndex w.event `mod` length xs)
-- spanBegin CycleBlock xs ws _ = map (processBn xs) ws
--   where processBn xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: bn}  
--               where bn = fromMaybe 0.0666 $ xs !! (getBlockIndex w.event `mod` length xs)
-- spanBegin CycleInBlock xs ws _ = map (processBn xs) ws
--   where processBn xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: bn} 
--               where bn = fromMaybe 0.0666 $ xs !! (getStructureIndex w.event `mod` length xs)
-- spanBegin SpreadBlock xs ws rhythmic = map (processBn xs) ws
--   where processBn xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: bn} 
--               where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
--                     modIndex = (getEventIndex w.event) `mod` (length $ fromFoldable percenPositions)
--                     percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
--                     segment = 1.0 / toNumber (length xs)
--                     limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
--                     limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
--                     limits = zip limitsFst limitsSnd
--                     bnLimits = zip xs limits
--                     bn = fromMaybe 0.0666 $ head $ map f $ filter isJust $ spreadBegins percenPos bnLimits
--                         where f (Just x) = x
--                               f Nothing = 0.0666

-- spreadBegins:: Number -> Array (Tuple Number (Tuple Number Number)) -> Array (Maybe Number)
-- spreadBegins percenPos bLimits = map (\sLimit -> spreadBegin percenPos sLimit) bLimits

-- spreadBegin::  Number -> Tuple Number (Tuple Number Number) -> Maybe Number
-- spreadBegin percenPos (Tuple b limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just b else Nothing

-- getBegin:: List Value -> Maybe Value
-- getBegin aural = head $ filter isBegin $ fromFoldable aural

-- isBegin:: Value -> Boolean
-- isBegin (Begin _ _) = true
-- isBegin (TransposedBegin _ _) = true
-- isBegin _ = false

-- -- speed
-- processSpeed:: Voices -> Rhythmic -> Maybe Value -> 
--                Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number}  -> 
--                Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number}
-- processSpeed _ _ _ [] = []
-- processSpeed _ _ Nothing ws = map (\w -> {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: 1.0}) ws
-- processSpeed m r (Just (TransposedSpeed id n)) ws = findOtherVoiceSpeed r ws (Tuple id n) m 
-- processSpeed _ r (Just (Speed span spList)) ws = spanSpeed span (fromFoldable spList) ws r
-- processSpeed _ _ _ _ = []

-- findOtherVoiceSpeed:: Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number} -> Tuple String Int -> Voices -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number}
-- findOtherVoiceSpeed r ws (Tuple id n) mapa = processSpeed mapa r newVal ws
--     where newVal = cycleAurals n $ M.lookup id mapa

-- spanSpeed:: Span -> Array Number -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number} -> Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number}
-- spanSpeed CycleEvent xs ws _ = map (processSp xs) ws
--   where processSp xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: sp} 
--               where sp = fromMaybe 1.0666 $ xs !! (getEventIndex w.event `mod` length xs)
-- spanSpeed CycleBlock xs ws _ = map (processSp xs) ws
--   where processSp xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: sp}  
--               where sp = fromMaybe 1.0666 $ xs !! (getBlockIndex w.event `mod` length xs)
-- spanSpeed CycleInBlock xs ws _ = map (processSp xs) ws
--   where processSp xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: sp} 
--               where sp = fromMaybe 1.0666 $ xs !! (getStructureIndex w.event `mod` length xs)
-- spanSpeed SpreadBlock xs ws rhythmic = map (processSp xs) ws
--   where processSp xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: sp} 
--               where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
--                     modIndex = (getEventIndex w.event) `mod` (length $ fromFoldable percenPositions)
--                     percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
--                     segment = 1.0 / toNumber (length xs)
--                     limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
--                     limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
--                     limits = zip limitsFst limitsSnd
--                     spLimits = zip xs limits
--                     sp = fromMaybe 1.0666 $ head $ map f $ filter isJust $ spreadSpeeds percenPos spLimits
--                         where f (Just x) = x
--                               f Nothing = 1.0666

-- spreadSpeeds:: Number -> Array (Tuple Number (Tuple Number Number)) -> Array (Maybe Number)
-- spreadSpeeds percenPos spLimits = map (\sLimit -> spreadSpeed percenPos sLimit) spLimits

-- spreadSpeed::  Number -> Tuple Number (Tuple Number Number) -> Maybe Number
-- spreadSpeed percenPos (Tuple sp limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just sp else Nothing

-- getSpeed:: List Value -> Maybe Value
-- getSpeed aural = head $ filter isSpeed $ fromFoldable aural

-- isSpeed:: Value -> Boolean
-- isSpeed (Speed _ _) = true
-- isSpeed (TransposedSpeed _ _) = true
-- isSpeed _ = false

-- -- pan
-- processPan:: Voices -> Rhythmic -> Maybe Value -> Array {event:: Event, s:: String, n:: Int, gain:: Number}  -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number}
-- processPan _ _ _ [] = []
-- processPan _ _ Nothing ws = map (\w -> {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: 0.5}) ws
-- processPan m r (Just (TransposedPan id n)) ws = findOtherVoicePan r ws (Tuple id n) m 
-- processPan _ r (Just (Pan span pList)) ws = spanPan span (fromFoldable pList) ws r
-- processPan _ _ _ _ = []

-- findOtherVoicePan:: Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number} -> (Tuple String Int) -> Voices -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number}
-- findOtherVoicePan r ws (Tuple id n) mapa = processPan mapa r newVal ws
--     where newVal = cycleAurals n $ M.lookup id mapa

-- spanPan:: Span -> Array Number -> Array {event:: Event, s:: String, n:: Int, gain:: Number} -> Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number}
-- spanPan CycleEvent xs ws _ = map (processP xs) ws
--   where processP xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: p} 
--               where p = fromMaybe 0.5666 $ xs !! (getEventIndex w.event `mod` length xs)
-- spanPan CycleBlock xs ws _ = map (processP xs) ws
--   where processP xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: p}  
--               where p = fromMaybe 0.5666 $ xs !! (getBlockIndex w.event `mod` length xs)
-- spanPan CycleInBlock xs ws _ = map (processP xs) ws
--   where processP xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: p} 
--               where p = fromMaybe 0.5666 $ xs !! (getStructureIndex w.event `mod` length xs)
-- spanPan SpreadBlock xs ws rhythmic = map (processP xs) ws
--   where processP xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: p} 
--               where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
--                     modIndex = (getEventIndex w.event) `mod` (length $ fromFoldable percenPositions)
--                     percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
--                     segment = 1.0 / toNumber (length xs)
--                     limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
--                     limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
--                     limits = zip limitsFst limitsSnd
--                     pLimits = zip xs limits
--                     p = fromMaybe 0.5666 $ head $ map f $ filter isJust $ spreadPans percenPos pLimits
--                         where f (Just x) = x
--                               f Nothing = 0.5666

-- spreadPans:: Number -> Array (Tuple Number (Tuple Number Number)) -> Array (Maybe Number)
-- spreadPans percenPos pLimits = map (\sLimit -> spreadPan percenPos sLimit) pLimits

-- spreadPan::  Number -> Tuple Number (Tuple Number Number) -> Maybe Number
-- spreadPan percenPos (Tuple p limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just p else Nothing

-- getPan:: List Value -> Maybe Value
-- getPan aural = head $ filter isPan $ fromFoldable aural

-- isPan:: Value -> Boolean
-- isPan (Pan _ _) = true
-- isPan (TransposedPan _ _) = true
-- isPan _ = false

-- -- gain
-- processGain:: Voices -> Rhythmic -> Maybe Value -> Array {event:: Event, s:: String, n:: Int}  -> Array {event:: Event, s:: String, n:: Int, gain:: Number}
-- processGain _ _ _ [] = []
-- processGain _ _ Nothing ws = map (\w -> {event: w.event, s: w.s, n: w.n, gain: 1.0}) ws
-- processGain m r (Just (TransposedGain id n)) ws = findOtherVoiceGain r ws (Tuple id n) m 
-- processGain _ r (Just (Gain span gList)) ws = spanGain span (fromFoldable gList) ws r
-- processGain _ _ _ _ = [] 

-- findOtherVoiceGain:: Rhythmic -> Array {event:: Event, s:: String, n:: Int} -> Tuple String Int -> Voices -> Array {event:: Event, s:: String, n:: Int, gain:: Number}
-- findOtherVoiceGain r ws (Tuple id n) mapa = processGain mapa r newVal ws
--     where newVal = cycleAurals n $ M.lookup id mapa

-- spanGain:: Span -> Array Number -> Array {event:: Event, s:: String, n:: Int} -> Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number}
-- spanGain CycleEvent xs ws _ = map (processG xs) ws
--   where processG xs w = {event: w.event, s: w.s, n: w.n, gain: g} 
--               where g = fromMaybe 0.02666 $ xs !! (getEventIndex w.event `mod` length xs)
-- spanGain CycleBlock xs ws _ = map (processG xs) ws
--   where processG xs w = {event: w.event, s: w.s, n: w.n, gain: g}  
--               where g = fromMaybe 0.02666 $ xs !! (getBlockIndex w.event `mod` length xs)
-- spanGain CycleInBlock xs ws _ = map (processG xs) ws
--   where processG xs w = {event: w.event, s: w.s, n: w.n, gain: g} 
--               where g = fromMaybe 0.02666 $ xs !! (getStructureIndex w.event `mod` length xs)
-- spanGain SpreadBlock xs ws rhythmic = map (processG xs) ws
--   where processG xs w = {event: w.event, s: w.s, n: w.n, gain: g} 
--               where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
--                     modIndex = (getEventIndex w.event) `mod` (length $ fromFoldable percenPositions)
--                     percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
--                     segment = 1.0 / toNumber (length xs)
--                     limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
--                     limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
--                     limits = zip limitsFst limitsSnd
--                     gLimits = zip xs limits
--                     g = fromMaybe 0.02666 $ head $ map f $ filter isJust $ spreadGains percenPos gLimits
--                         where f (Just x) = x
--                               f Nothing = 0.02666

-- spreadGains:: Number -> Array (Tuple Number (Tuple Number Number)) -> Array (Maybe Number)
-- spreadGains percenPos gLimits = map (\sLimit -> spreadG percenPos sLimit) gLimits

-- spreadG::  Number -> Tuple Number (Tuple Number Number) -> Maybe Number
-- spreadG percenPos (Tuple g limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just g else Nothing

-- getGain:: List Value -> Maybe Value
-- getGain aural = head $ filter isGain $ fromFoldable aural

-- isGain:: Value -> Boolean
-- isGain (Gain _ _) = true
-- isGain (TransposedGain _ _) = true
-- isGain _ = false

-- -- N
-- processN:: Voices -> Rhythmic -> Maybe Value -> Array {event:: Event, s:: String}  -> Array {event:: Event, s:: String, n:: Int}
-- processN _ _ _ [] = []
-- processN _ _ Nothing ws = map (\w -> {event: w.event, s: w.s, n: 0}) ws
-- -- processN m r (Just (TransposedNWith id l)) ws = findOtherNWith r ws id l m 
-- processN m r (Just (TransposedN id n)) ws = findOtherVoiceN r ws (Tuple id n) m 
-- processN _ r (Just (N span nList _)) ws = spanN span (fromFoldable nList) ws r
-- processN _ _ _ _ = [] 


-- -- findOtherNWithTest = newTransposition
-- --     where lista = [(add 1), (add 3), (add 10)]
-- --           transposed = [0,2,0,2,0,2,0,0]
-- --           patron = bjorklundAsInterval (length lista) (length transposed)
-- --           zipped = concat $ zipWith (\x n -> replicate n x) (fromFoldable lista) patron 
-- --           newTransposition = zipWith (\n newN -> newN n ) transposed zipped


-- -- this events are organised via their index, the array does not represent the position in the cycle. The lista and new transposition need to be organised in the same way

-- -- findOtherNWith:: Rhythmic -> Array {event:: Event, s:: String} -> String -> List Ops -> Voices -> Array {event:: Event, s:: String, n:: Int}
-- -- findOtherNWith r ws id lista mapa = newTransposition
--     -- where 
          


--     --       patron = bjorklundAsInterval (L.length li) (length transposed) 
--     --       zipped = concat $ zipWith (\x n -> replicate n x) (fromFoldable li) patron 
--     --       newTransposition = zipWith (\esn newN -> {event: esn.event, s: esn.s, n: op newN esn.n} ) transposed zipped


-- -- getRefdN:: String -> Voices -> List Int
-- -- getRefdN id mapa = case M.lookup id mapa of
-- --       Nothing -> L.fromFoldable []
-- --       Just (TransposedN id) -> getRefdN 
-- --       Just (TransposedNWith id list) -> 
-- --       Just (N span list) ->

-- findOtherVoiceN:: Rhythmic -> Array {event:: Event, s:: String} -> Tuple String Int -> Voices -> Array {event:: Event, s:: String, n:: Int}
-- findOtherVoiceN r ws (Tuple id n) mapa = processN mapa r newVal ws
--     where newVal = cycleAurals n $ M.lookup id mapa

-- spanN:: Span -> Array Int -> Array {event:: Event, s:: String} -> Rhythmic -> Array {event:: Event, s:: String, n:: Int}
-- spanN CycleEvent xs ws _ = map (processN xs) ws
--   where processN xs w = {event: w.event, s: w.s, n: n'} 
--               where n' = fromMaybe 2666 $ xs !! (getEventIndex w.event `mod` length xs)
-- spanN CycleBlock xs ws _ = map (processN xs) ws
--   where processN xs w = {event: w.event, s: w.s, n: n'} 
--               where n' = fromMaybe 2666 $ xs !! (getBlockIndex w.event `mod` length xs)
-- spanN CycleInBlock xs ws _ = map (processN xs) ws
--   where processN xs w = {event: w.event, s: w.s, n: n'} 
--               where n' = fromMaybe 2666 $ xs !! (getStructureIndex w.event `mod` length xs)
-- spanN SpreadBlock xs ws rhythmic = map (processN xs) ws
--   where processN xs w = {event: w.event, s: w.s, n: n'} 
--               where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
--                     modIndex = (getEventIndex w.event) `mod` (length $ fromFoldable percenPositions)
--                     percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
--                     segment = 1.0 / toNumber (length xs)
--                     limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
--                     limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
--                     limits = zip limitsFst limitsSnd
--                     nLimits = zip xs limits
--                     n' = fromMaybe 2666 $ head $ map f $ filter isJust $ spreadNs percenPos nLimits
--                         where f (Just x) = x
--                               f Nothing = 2666
  
-- spreadNs:: Number -> Array (Tuple Int (Tuple Number Number)) -> Array (Maybe Int)
-- spreadNs percenPos nLimits = map (\sLimit -> spreadN percenPos sLimit) nLimits

-- spreadN::  Number -> Tuple Int (Tuple Number Number) -> Maybe Int
-- spreadN percenPos (Tuple n limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just n else Nothing

-- -- testGetN = getN au
-- --   where au = L.fromFoldable [N CycleBlock $ L.fromFoldable [0,1,2,3], Sound CycleBlock $ L.fromFoldable ["a","b","c"]]

-- getN:: List Value -> Maybe Value
-- getN aural = head $ filter isN $ fromFoldable aural

-- isN:: Value -> Boolean
-- isN (N _ _ _) = true
-- isN (TransposedN _ _) = true
-- -- isN (TransposedNWith _ _ _) = true
-- isN _ = false

-- -- Sound
-- processSound:: Voices -> Rhythmic -> Maybe Value -> Array Event -> Array {event:: Event, s:: String} 
-- processSound _ _ Nothing _ = []
-- processSound m r (Just (TransposedSound id n)) es = findOtherVoiceSound r es (Tuple id n) m 
-- processSound _ r (Just (Sound span sList var)) events = spanSound span (fromFoldable sList) events r
-- processSound _ _ _ _ = [] 

-- findOtherVoiceSound:: Rhythmic -> Array Event -> Tuple String Int -> Voices -> Array {event:: Event, s:: String}
-- findOtherVoiceSound r ws (Tuple id n) mapa = processSound mapa r newVal ws
--     where newVal = cycleAurals n $ M.lookup id mapa

-- spanSound:: Span -> Array String -> Array Event -> Rhythmic -> Array {event:: Event, s:: String}  
-- spanSound CycleEvent xs events _ = map (processSound xs) events
--   where processSound xs event' = {event: event', s: sound'} 
--               where sound' = fromMaybe "error assigning sound" $ xs !! (getEventIndex event' `mod` length xs)
-- spanSound CycleBlock xs events _ = map (processSound xs) events
--   where processSound xs event' = {event: event', s: sound'}
--               where sound' = fromMaybe "error assigning sound" $ xs !! (getBlockIndex event' `mod` length xs)
-- spanSound CycleInBlock xs events _ = map (processSound xs) events
--   where processSound xs event' = {event: event', s: sound'}
--               where sound' = fromMaybe "error assigning sound" $ xs !! (getStructureIndex event' `mod` length xs)
-- spanSound SpreadBlock xs events rhythmic = map (processSound xs) events
--   where processSound xs event' = {event: event', s: sound'}
--               where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
--                     modIndex = (getEventIndex event') `mod` (length $ fromFoldable percenPositions)
--                     percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
--                     segment = 1.0 / toNumber (length xs)
--                     limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
--                     limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
--                     limits = zip limitsFst limitsSnd
--                     soundLimits = zip xs limits
--                     sound' = fromMaybe "error assigning sound SpreadBlock" $ head $ map f $ filter isJust $ spreadSounds percenPos soundLimits
--                         where f (Just x) = x
--                               f Nothing = "error thingy"
  
-- spreadSounds:: Number -> Array (Tuple String (Tuple Number Number)) -> Array (Maybe String)
-- spreadSounds percenPos soundLimits = map (\sLimit -> spreadSound percenPos sLimit) soundLimits

-- spreadSound::  Number -> Tuple String (Tuple Number Number) -> Maybe String
-- spreadSound percenPos (Tuple sound limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just sound else Nothing
  
-- getSound:: List Value -> Maybe Value
-- getSound aural = head $ filter isSound $ fromFoldable aural

-- isSound:: Value -> Boolean
-- isSound (Sound _ _ _) = true
-- isSound (TransposedSound _ _) = true
-- isSound _ = false

---
-- testVoice = Voice defTemporal $ L.fromFoldable [L.fromFoldable [End CycleBlock Nil], L.fromFoldable [Gain CycleEvent Nil, End CycleInBlock Nil]]

-- -- cycleAurals:: Int -> Maybe Voice -> Maybe Value
-- -- cycleAurals n mVoice = do
-- --   voice <- mVoice
-- --   let aurals = (\(Voice t aurals) -> aurals) voice
-- --   let len = L.length aurals 
-- --   newVal <- (fromFoldable aurals) !! (n`mod`len)
-- --   getEnd newVal

-- getStructureIndex:: Event -> Int
-- getStructureIndex (Event _ (Index _ xs _)) = fromMaybe 0 $ head $ xs

-- getBlockIndex:: Event -> Int
-- getBlockIndex (Event _ (Index n _ _)) = n

-- getEventIndex:: Event -> Int
-- getEventIndex (Event _ (Index _ _ n)) = n

-- ---
-- bjorklundAsInterval:: Int -> Int -> Array Int
-- bjorklundAsInterval k n = zipped
--       where bjorkl = fromFoldable $ bjorklund (Tuple k n)
--             limit = map snd $ filter (\(Tuple bool n) -> bool) $ zipWith (\x y -> Tuple x y) bjorkl (0..(length bjorkl))
--             limit1 = snoc limit n
--             limit2 = fromMaybe {head: 2666, tail: []} $ uncons limit1
--             zipped = zipWith (\y x -> x - y) limit1 limit2.tail -- [2,2,3]