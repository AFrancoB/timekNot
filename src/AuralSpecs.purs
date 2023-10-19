module AuralSpecs (auralSpecs) where

import Prelude

import Effect (Effect)

import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Map as M
import Data.Foldable (sum)
import Data.Int
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array (filter,fromFoldable,(!!), zipWith, replicate, concat, (..), (:), init, tail, last,head,reverse,zip, cons, snoc, length, singleton)
import Data.List
import Data.Traversable (scanl)
import Data.List (fromFoldable,concat,zip,zipWith,length,init) as L

import Record

import Data.Newtype

import Data.Tempo

import AST
import DurationAndIndex
import Parser
import Rhythm

import Data.Rational (Rational(..), (%), fromInt)
import Data.Rational (toNumber) as R
import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration


-- issue: the type Value can be a sort of .... functor...?
-- data Value = Sound Span (List String) | TransposedSound String | N Span (List Int) | TransposedN String 
-- Forgeting about Transposed constructors for a moment, the list of Strings of sound and the list of Ints of N are treated exactly the same. They could be:: List a instead of List String and List Int.

-- I could have something like:: Value Span a --where 'a' can be a list of strings, a string, a list of Ints a list of Numbers, a Number, an Int etc... I would need a type like the following:

-- data Transpose = Transpose

-- that would allow me to get the a from another voice. Transpose would be interesting as well because can be:: Transpose func ... what is that? appears like a Functor to me as well...

-- Top of Sound spec is an aural expression:

-- Map String Aural

-- Aural = List (Value)

-- Value = TypeOfValue Span a  ---- a?? or each TypeOfValue should have its own Type?

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

-- type of value could be: Sound, N, Pan, CutOff, HCutOff, Speed, Note, etc...

-- aural specs processes one voice at the time
auralSpecs:: Voices -> Rhythmic -> List Value -> Array Event -> Array AlmostWaste
auralSpecs m rhy aural events =
    let sounds = processSound m rhy (getSound aural) events
        nS = processN m rhy (getN aural) sounds 
        gainNS = processGain m rhy (getGain aural) nS
        panGNS = processPan m rhy (getPan aural) gainNS
        speedPGNS = processSpeed m rhy (getSpeed aural) panGNS
        beginSpPGNS = processBegin m rhy (getBegin aural) speedPGNS
        endBSpPGNS = processEnd m rhy (getEnd aural) beginSpPGNS
        -- cutoffEBSpPGNS = processCutOff
        -- vowelCEBSpPGNS = processVowel
    in endBSpPGNS -- vowelCEBSpPGNS



-- end
processEnd:: Voices -> Rhythmic -> Maybe Value -> 
               Array {event::Event, s::String, n::Int, gain::Number, pan::Number, speed::Number, begin::Number}  -> 
               Array {event::Event, s::String, n::Int, gain::Number, pan::Number, speed::Number, begin::Number, end::Number}
processEnd _ _ _ [] = []
processEnd _ _ Nothing ws = map (\w -> {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: 1.0}) ws
processEnd m r (Just (TransposedEnd id)) ws = findReferredEnd r ws id m 
processEnd _ r (Just (End span eList)) ws = spanEnd span (fromFoldable eList) ws r
processEnd _ _ _ _ = []

findReferredEnd:: Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number} -> String -> Voices -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number, end:: Number}
findReferredEnd r ws id mapa = processEnd mapa r newVal ws
    where maybeVoice = M.lookup id mapa -- Maybe Voice
          newVal = ((\(Voice temporal aural) -> aural) <$> maybeVoice) >>= getEnd -- Maybe Value

spanEnd:: Span -> Array Number -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number} -> Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number, end:: Number}
spanEnd CycleEvent xs ws _ = map (processEnd xs) ws
  where processEnd xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: end} 
              where end = fromMaybe 0.9666 $ xs !! (getEventIndex w.event `mod` length xs)
spanEnd CycleBlock xs ws _ = map (processEnd xs) ws
  where processEnd xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: end}  
              where end = fromMaybe 0.9666 $ xs !! (getBlockIndex w.event `mod` length xs)
spanEnd CycleInBlock xs ws _ = map (processEnd xs) ws
  where processEnd xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: end} 
              where end = fromMaybe 0.9666 $ xs !! (getStructureIndex w.event `mod` length xs)
spanEnd SpreadBlock xs ws rhythmic = map (processEnd xs) ws
  where processEnd xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: w.begin, end: end} 
              where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
                    modIndex = (getEventIndex w.event) `mod` (length $ fromFoldable percenPositions)
                    percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
                    segment = 1.0 / toNumber (length xs)
                    limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
                    limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
                    limits = zip limitsFst limitsSnd
                    endLimits = zip xs limits
                    end = fromMaybe 0.9666 $ head $ map f $ filter isJust $ spreadEnds percenPos endLimits
                        where f (Just x) = x
                              f Nothing = 0.9666

spreadEnds:: Number -> Array (Tuple Number (Tuple Number Number)) -> Array (Maybe Number)
spreadEnds percenPos bLimits = map (\sLimit -> spreadEnd percenPos sLimit) bLimits

spreadEnd::  Number -> Tuple Number (Tuple Number Number) -> Maybe Number
spreadEnd percenPos (Tuple b limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just b else Nothing

getEnd:: List Value -> Maybe Value
getEnd aural = head $ filter isEnd $ fromFoldable aural

isEnd:: Value -> Boolean
isEnd (End _ _) = true
isEnd (TransposedEnd _) = true
isEnd _ = false

-- begin
processBegin:: Voices -> Rhythmic -> Maybe Value -> 
               Array {event::Event, s::String, n::Int, gain::Number, pan::Number, speed::Number}  -> 
               Array {event::Event, s::String, n::Int, gain::Number, pan::Number, speed::Number, begin::Number}
processBegin _ _ _ [] = []
processBegin _ _ Nothing ws = map (\w -> {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: 0.0}) ws
processBegin m r (Just (TransposedBegin id)) ws = findReferredBegin r ws id m 
processBegin _ r (Just (Begin span bList)) ws = spanBegin span (fromFoldable bList) ws r
processBegin _ _ _ _ = []

findReferredBegin:: Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number} -> String -> Voices -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number}
findReferredBegin r ws id mapa = processBegin mapa r newVal ws
    where maybeVoice = M.lookup id mapa -- Maybe Voice
          newVal = ((\(Voice temporal aural) -> aural) <$> maybeVoice) >>= getBegin -- Maybe Value

spanBegin:: Span -> Array Number -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number} -> Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number, begin:: Number}
spanBegin CycleEvent xs ws _ = map (processBn xs) ws
  where processBn xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: bn} 
              where bn = fromMaybe 0.0666 $ xs !! (getEventIndex w.event `mod` length xs)
spanBegin CycleBlock xs ws _ = map (processBn xs) ws
  where processBn xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: bn}  
              where bn = fromMaybe 0.0666 $ xs !! (getBlockIndex w.event `mod` length xs)
spanBegin CycleInBlock xs ws _ = map (processBn xs) ws
  where processBn xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: bn} 
              where bn = fromMaybe 0.0666 $ xs !! (getStructureIndex w.event `mod` length xs)
spanBegin SpreadBlock xs ws rhythmic = map (processBn xs) ws
  where processBn xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: w.speed, begin: bn} 
              where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
                    modIndex = (getEventIndex w.event) `mod` (length $ fromFoldable percenPositions)
                    percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
                    segment = 1.0 / toNumber (length xs)
                    limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
                    limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
                    limits = zip limitsFst limitsSnd
                    bnLimits = zip xs limits
                    bn = fromMaybe 0.0666 $ head $ map f $ filter isJust $ spreadBegins percenPos bnLimits
                        where f (Just x) = x
                              f Nothing = 0.0666

spreadBegins:: Number -> Array (Tuple Number (Tuple Number Number)) -> Array (Maybe Number)
spreadBegins percenPos bLimits = map (\sLimit -> spreadBegin percenPos sLimit) bLimits

spreadBegin::  Number -> Tuple Number (Tuple Number Number) -> Maybe Number
spreadBegin percenPos (Tuple b limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just b else Nothing

getBegin:: List Value -> Maybe Value
getBegin aural = head $ filter isBegin $ fromFoldable aural

isBegin:: Value -> Boolean
isBegin (Begin _ _) = true
isBegin (TransposedBegin _) = true
isBegin _ = false

-- speed
processSpeed:: Voices -> Rhythmic -> Maybe Value -> 
               Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number}  -> 
               Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number}
processSpeed _ _ _ [] = []
processSpeed _ _ Nothing ws = map (\w -> {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: 1.0}) ws
processSpeed m r (Just (TransposedSpeed id)) ws = findOtherVoiceSpeed r ws id m 
processSpeed _ r (Just (Speed span spList)) ws = spanSpeed span (fromFoldable spList) ws r
processSpeed _ _ _ _ = []

findOtherVoiceSpeed:: Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number} -> String -> Voices -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number}
findOtherVoiceSpeed r ws id mapa = processSpeed mapa r newVal ws
    where maybeVoice = M.lookup id mapa -- Maybe Voice
          newVal = ((\(Voice temporal aural) -> aural) <$> maybeVoice) >>= getSpeed -- Maybe Value

spanSpeed:: Span -> Array Number -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number} -> Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number, speed:: Number}
spanSpeed CycleEvent xs ws _ = map (processSp xs) ws
  where processSp xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: sp} 
              where sp = fromMaybe 1.0666 $ xs !! (getEventIndex w.event `mod` length xs)
spanSpeed CycleBlock xs ws _ = map (processSp xs) ws
  where processSp xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: sp}  
              where sp = fromMaybe 1.0666 $ xs !! (getBlockIndex w.event `mod` length xs)
spanSpeed CycleInBlock xs ws _ = map (processSp xs) ws
  where processSp xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: sp} 
              where sp = fromMaybe 1.0666 $ xs !! (getStructureIndex w.event `mod` length xs)
spanSpeed SpreadBlock xs ws rhythmic = map (processSp xs) ws
  where processSp xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: w.pan, speed: sp} 
              where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
                    modIndex = (getEventIndex w.event) `mod` (length $ fromFoldable percenPositions)
                    percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
                    segment = 1.0 / toNumber (length xs)
                    limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
                    limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
                    limits = zip limitsFst limitsSnd
                    spLimits = zip xs limits
                    sp = fromMaybe 1.0666 $ head $ map f $ filter isJust $ spreadSpeeds percenPos spLimits
                        where f (Just x) = x
                              f Nothing = 1.0666

spreadSpeeds:: Number -> Array (Tuple Number (Tuple Number Number)) -> Array (Maybe Number)
spreadSpeeds percenPos spLimits = map (\sLimit -> spreadSpeed percenPos sLimit) spLimits

spreadSpeed::  Number -> Tuple Number (Tuple Number Number) -> Maybe Number
spreadSpeed percenPos (Tuple sp limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just sp else Nothing

getSpeed:: List Value -> Maybe Value
getSpeed aural = head $ filter isSpeed $ fromFoldable aural

isSpeed:: Value -> Boolean
isSpeed (Speed _ _) = true
isSpeed (TransposedSpeed _) = true
isSpeed _ = false

-- pan
processPan:: Voices -> Rhythmic -> Maybe Value -> Array {event:: Event, s:: String, n:: Int, gain:: Number}  -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number}
processPan _ _ _ [] = []
processPan _ _ Nothing ws = map (\w -> {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: 0.5}) ws
processPan m r (Just (TransposedPan id)) ws = findOtherVoicePan r ws id m 
processPan _ r (Just (Pan span pList)) ws = spanPan span (fromFoldable pList) ws r
processPan _ _ _ _ = []

findOtherVoicePan:: Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number} -> String -> Voices -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number}
findOtherVoicePan r ws id mapa = processPan mapa r newVal ws
    where maybeVoice = M.lookup id mapa -- Maybe Voice
          newVal = ((\(Voice temporal aural) -> aural) <$> maybeVoice) >>= getPan -- Maybe Value

spanPan:: Span -> Array Number -> Array {event:: Event, s:: String, n:: Int, gain:: Number} -> Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number, pan:: Number}
spanPan CycleEvent xs ws _ = map (processP xs) ws
  where processP xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: p} 
              where p = fromMaybe 0.5666 $ xs !! (getEventIndex w.event `mod` length xs)
spanPan CycleBlock xs ws _ = map (processP xs) ws
  where processP xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: p}  
              where p = fromMaybe 0.5666 $ xs !! (getBlockIndex w.event `mod` length xs)
spanPan CycleInBlock xs ws _ = map (processP xs) ws
  where processP xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: p} 
              where p = fromMaybe 0.5666 $ xs !! (getStructureIndex w.event `mod` length xs)
spanPan SpreadBlock xs ws rhythmic = map (processP xs) ws
  where processP xs w = {event: w.event, s: w.s, n: w.n, gain: w.gain, pan: p} 
              where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
                    modIndex = (getEventIndex w.event) `mod` (length $ fromFoldable percenPositions)
                    percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
                    segment = 1.0 / toNumber (length xs)
                    limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
                    limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
                    limits = zip limitsFst limitsSnd
                    pLimits = zip xs limits
                    p = fromMaybe 0.5666 $ head $ map f $ filter isJust $ spreadPans percenPos pLimits
                        where f (Just x) = x
                              f Nothing = 0.5666

spreadPans:: Number -> Array (Tuple Number (Tuple Number Number)) -> Array (Maybe Number)
spreadPans percenPos pLimits = map (\sLimit -> spreadPan percenPos sLimit) pLimits

spreadPan::  Number -> Tuple Number (Tuple Number Number) -> Maybe Number
spreadPan percenPos (Tuple p limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just p else Nothing

getPan:: List Value -> Maybe Value
getPan aural = head $ filter isPan $ fromFoldable aural

isPan:: Value -> Boolean
isPan (Pan _ _) = true
isPan (TransposedPan _) = true
isPan _ = false

-- gain
processGain:: Voices -> Rhythmic -> Maybe Value -> Array {event:: Event, s:: String, n:: Int}  -> Array {event:: Event, s:: String, n:: Int, gain:: Number}
processGain _ _ _ [] = []
processGain _ _ Nothing ws = map (\w -> {event: w.event, s: w.s, n: w.n, gain: 1.0}) ws
processGain m r (Just (TransposedGain id)) ws = findOtherVoiceGain r ws id m 
processGain _ r (Just (Gain span gList)) ws = spanGain span (fromFoldable gList) ws r
processGain _ _ _ _ = [] 

findOtherVoiceGain:: Rhythmic -> Array {event:: Event, s:: String, n:: Int} -> String -> Voices -> Array {event:: Event, s:: String, n:: Int, gain:: Number}
findOtherVoiceGain r ws id mapa = processGain mapa r newVal ws
    where maybeVoice = M.lookup id mapa -- Maybe Voice
          newVal = ((\(Voice temporal aural) -> aural) <$> maybeVoice) >>= getGain -- Maybe Value

spanGain:: Span -> Array Number -> Array {event:: Event, s:: String, n:: Int} -> Rhythmic -> Array {event:: Event, s:: String, n:: Int, gain:: Number}
spanGain CycleEvent xs ws _ = map (processG xs) ws
  where processG xs w = {event: w.event, s: w.s, n: w.n, gain: g} 
              where g = fromMaybe 0.02666 $ xs !! (getEventIndex w.event `mod` length xs)
spanGain CycleBlock xs ws _ = map (processG xs) ws
  where processG xs w = {event: w.event, s: w.s, n: w.n, gain: g}  
              where g = fromMaybe 0.02666 $ xs !! (getBlockIndex w.event `mod` length xs)
spanGain CycleInBlock xs ws _ = map (processG xs) ws
  where processG xs w = {event: w.event, s: w.s, n: w.n, gain: g} 
              where g = fromMaybe 0.02666 $ xs !! (getStructureIndex w.event `mod` length xs)
spanGain SpreadBlock xs ws rhythmic = map (processG xs) ws
  where processG xs w = {event: w.event, s: w.s, n: w.n, gain: g} 
              where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
                    modIndex = (getEventIndex w.event) `mod` (length $ fromFoldable percenPositions)
                    percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
                    segment = 1.0 / toNumber (length xs)
                    limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
                    limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
                    limits = zip limitsFst limitsSnd
                    gLimits = zip xs limits
                    g = fromMaybe 0.02666 $ head $ map f $ filter isJust $ spreadGains percenPos gLimits
                        where f (Just x) = x
                              f Nothing = 0.02666


spreadGains:: Number -> Array (Tuple Number (Tuple Number Number)) -> Array (Maybe Number)
spreadGains percenPos gLimits = map (\sLimit -> spreadG percenPos sLimit) gLimits

spreadG::  Number -> Tuple Number (Tuple Number Number) -> Maybe Number
spreadG percenPos (Tuple g limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just g else Nothing

getGain:: List Value -> Maybe Value
getGain aural = head $ filter isGain $ fromFoldable aural

isGain:: Value -> Boolean
isGain (Gain _ _) = true
isGain (TransposedGain _) = true
isGain _ = false

-- N
processN:: Voices -> Rhythmic -> Maybe Value -> Array {event:: Event, s:: String}  -> Array {event:: Event, s:: String, n:: Int}
processN _ _ _ [] = []
processN _ _ Nothing ws = map (\w -> {event: w.event, s: w.s, n: 0}) ws
processN m r (Just (TransposedN id)) ws = findOtherVoiceN r ws id m 
processN _ r (Just (N span nList)) ws = spanN span (fromFoldable nList) ws r
processN _ _ _ _ = [] 

findOtherVoiceN:: Rhythmic -> Array {event:: Event, s:: String} -> String -> Voices -> Array {event:: Event, s:: String, n:: Int}
findOtherVoiceN r ws id mapa = processN mapa r newVal ws
    where maybeVoice = M.lookup id mapa -- Maybe Voice
          newVal = ((\(Voice temporal aural) -> aural) <$> maybeVoice) >>= getN -- Maybe Value

spanN:: Span -> Array Int -> Array {event:: Event, s:: String} -> Rhythmic -> Array {event:: Event, s:: String, n:: Int}
spanN CycleEvent xs ws _ = map (processN xs) ws
  where processN xs w = {event: w.event, s: w.s, n: n'} 
              where n' = fromMaybe 2666 $ xs !! (getEventIndex w.event `mod` length xs)
spanN CycleBlock xs ws _ = map (processN xs) ws
  where processN xs w = {event: w.event, s: w.s, n: n'} 
              where n' = fromMaybe 2666 $ xs !! (getBlockIndex w.event `mod` length xs)
spanN CycleInBlock xs ws _ = map (processN xs) ws
  where processN xs w = {event: w.event, s: w.s, n: n'} 
              where n' = fromMaybe 2666 $ xs !! (getStructureIndex w.event `mod` length xs)
spanN SpreadBlock xs ws rhythmic = map (processN xs) ws
  where processN xs w = {event: w.event, s: w.s, n: n'} 
              where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
                    modIndex = (getEventIndex w.event) `mod` (length $ fromFoldable percenPositions)
                    percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
                    segment = 1.0 / toNumber (length xs)
                    limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
                    limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
                    limits = zip limitsFst limitsSnd
                    nLimits = zip xs limits
                    n' = fromMaybe 2666 $ head $ map f $ filter isJust $ spreadNs percenPos nLimits
                        where f (Just x) = x
                              f Nothing = 2666
  
spreadNs:: Number -> Array (Tuple Int (Tuple Number Number)) -> Array (Maybe Int)
spreadNs percenPos nLimits = map (\sLimit -> spreadN percenPos sLimit) nLimits

spreadN::  Number -> Tuple Int (Tuple Number Number) -> Maybe Int
spreadN percenPos (Tuple n limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just n else Nothing

getN:: List Value -> Maybe Value
getN aural = head $ filter isN $ fromFoldable aural

isN:: Value -> Boolean
isN (N _ _) = true
isN (TransposedN _) = true
isN _ = false

-- Sound
processSound:: Voices -> Rhythmic -> Maybe Value -> Array Event -> Array {event:: Event, s:: String} 
processSound _ _ Nothing _ = []
processSound m r (Just (TransposedSound id)) es = findOtherVoiceSound r es id m 
processSound _ r (Just (Sound span sList)) events = spanSound span (fromFoldable sList) events r
processSound _ _ _ _ = [] 

findOtherVoiceSound:: Rhythmic -> Array Event -> String -> Voices -> Array {event:: Event, s:: String}
findOtherVoiceSound r ws id mapa = processSound mapa r newVal ws
    where maybeVoice = M.lookup id mapa -- Maybe Voice
          newVal = ((\(Voice temporal aural) -> aural) <$> maybeVoice) >>= getSound -- Maybe Value

spanSound:: Span -> Array String -> Array Event -> Rhythmic -> Array {event:: Event, s:: String}  
spanSound CycleEvent xs events _ = map (processSound xs) events
  where processSound xs event' = {event: event', s: sound'} 
              where sound' = fromMaybe "error assigning sound" $ xs !! (getEventIndex event' `mod` length xs)
spanSound CycleBlock xs events _ = map (processSound xs) events
  where processSound xs event' = {event: event', s: sound'}
              where sound' = fromMaybe "error assigning sound" $ xs !! (getBlockIndex event' `mod` length xs)
spanSound CycleInBlock xs events _ = map (processSound xs) events
  where processSound xs event' = {event: event', s: sound'}
              where sound' = fromMaybe "error assigning sound" $ xs !! (getStructureIndex event' `mod` length xs)
spanSound SpreadBlock xs events rhythmic = map (processSound xs) events
  where processSound xs event' = {event: event', s: sound'}
              where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
                    modIndex = (getEventIndex event') `mod` (length $ fromFoldable percenPositions)
                    percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
                    segment = 1.0 / toNumber (length xs)
                    limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
                    limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
                    limits = zip limitsFst limitsSnd
                    soundLimits = zip xs limits
                    sound' = fromMaybe "error assigning sound SpreadBlock" $ head $ map f $ filter isJust $ spreadSounds percenPos soundLimits
                        where f (Just x) = x
                              f Nothing = "error thingy"
  
spreadSounds:: Number -> Array (Tuple String (Tuple Number Number)) -> Array (Maybe String)
spreadSounds percenPos soundLimits = map (\sLimit -> spreadSound percenPos sLimit) soundLimits

spreadSound::  Number -> Tuple String (Tuple Number Number) -> Maybe String
spreadSound percenPos (Tuple sound limit) = if (percenPos >= fst limit) && (percenPos < snd limit) then Just sound else Nothing
  
getSound:: List Value -> Maybe Value
getSound aural = head $ filter isSound $ fromFoldable aural

isSound:: Value -> Boolean
isSound (Sound _ _) = true
isSound (TransposedSound _) = true
isSound _ = false

-- helpers
getStructureIndex:: Event -> Int
getStructureIndex (Event _ (Index _ xs _)) = fromMaybe 0 $ head $ xs

getBlockIndex:: Event -> Int
getBlockIndex (Event _ (Index n _ _)) = n

getEventIndex:: Event -> Int
getEventIndex (Event _ (Index _ _ n)) = n







defTemporal = Temporal (Kairos 0.0 (CPM (120%1))) O false
defVoice = Voice defTemporal defAural
defAural = Nil