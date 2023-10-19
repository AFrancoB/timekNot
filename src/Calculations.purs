module Calculations (programToWaste) where

import Prelude

import Effect (Effect)
import Effect.Console

import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Map as M
import Data.Foldable (sum)
import Data.Int
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array (filter,fromFoldable,(!!), zipWith, replicate, concat, (..), (:), init, tail, last,head,reverse,zip, cons, snoc, length, singleton, splitAt)

import Data.List
import Data.Traversable (scanl)
import Data.List (fromFoldable,concat,zip,zipWith,length,init) as L

import Control.Applicative

import Data.Newtype

import Data.Tempo

import Parsing

import AST
import Parser
import Rhythm
import TestOpsAndDefs
import DurationAndIndex
import TimePacketOps
import AuralSpecs

import Data.Rational (Rational(..), (%), fromInt)
import Data.Rational (toNumber) as R
import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration

import Data.TraversableWithIndex

-- glosario:
---- Voice is an ongoing or finished or yet-to-be-played musical idea enabled by 
---- Block is a rhythmic pattern of onsets
---- Onset a moment in time when a sound is instantiated

---- An Event is an Onset with an Index

---- Index is a mark that allows me to identify the position of the onset so sounds and sound characteristics can be attached to it
---- process-oriented index: an int identifier for each onset on a flow of onsets. 
---- eventIndex is the way I will refer to process oriented indexes
---- structure-oriented index: an int identifier for each segment on a voice and an array to identifier internal events in a voice: The head is the 'natural' subdivisions of the voice, each new element in the array is a new subdivision
---- a structure oriented index has a voice index and a structure index. A voice index is an Int while the Structure Index is an Array Int. The notation I have made for the structure oriented index is: 3-0.2.4  to the left of the (-) is the voice index and to the right of it is the event position in the rhythmic idea. The head of the array is the top level of the nested subdivisions and the last is the deepest level of the subdivisions.  

---
programToWaste:: Program -> DateTime -> DateTime -> DateTime -> Tempo -> Effect (Array Waste)
programToWaste program ws' we' eval' t = waste
  where timePacket = {ws: ws', we: we', eval: eval', origin: origin t, tempo: t}
        voices' = assambleVoice program -- Voices
        calculatedVoices = calculateVoices (getTemporalMap program) voices' timePacket
        -- Effect (M.Map String (Array AlmostWaste))
        almostWs = concat <$> fromFoldable <$> M.values <$> calculatedVoices -- Effect (Array (Array AlmostWaste))
        f:: AlmostWaste -> Boolean
        f w = (\(Event (Onset b _) _) -> b) w.event
        getXs:: Array AlmostWaste -> Array AlmostWaste
        getXs ws = filter (\w -> f w) ws
        toWaste:: AlmostWaste -> Waste
        toWaste almostW =  {whenPosix: posix, s: almostW.s, n: almostW.n, gain: almostW.gain, pan: almostW.pan, speed: almostW.speed, begin: almostW.begin, end: almostW.end}
            where posix = (\(Event (Onset _ p) _) -> p) almostW.event
        waste = map toWaste <$> getXs <$> almostWs
        
assambleVoice:: Program -> Voices
assambleVoice program = M.intersectionWith (\x y -> Voice x y) tempoMap auralMap
  where tempoMap = getTemporalMap program
        auralMap = getAuralMap program

----- down from here is the same as visualiser
calculateVoices:: M.Map String Temporal -> Voices -> TimePacket -> Effect (M.Map String (Array AlmostWaste))
calculateVoices tempoMap voiceMap tp = traverseWithIndex (calculateVoice tempoMap voiceMap tp) voiceMap  -- to get rid of Effect, change traverseWithIndex to mapWithIndex

calculateVoice:: M.Map String Temporal -> Voices -> TimePacket -> String -> Voice -> Effect (Array AlmostWaste)
calculateVoice tempoMap voiceMap tp aKey (Voice temporal aural) = toWaste
  where events = calculateTemporal tempoMap tp aKey temporal -- Array Event
        rhythmic = (\(Temporal p r l) -> r) temporal
        toWaste = auralSpecs voiceMap rhythmic aural <$> events

calculateTemporal:: M.Map String Temporal -> TimePacket -> String -> Temporal -> Effect (Array Event)
calculateTemporal mapa tp aKey (Temporal (Kairos asap tempoMark) rhythmic loop) = do
  let tempo = processTempoMark tempoMark tp.tempo mapa
      posixAtOrigin = fromDateTimeToPosix (origin tp.tempo)
      eval = secsFromOriginAtEval tp
      ws = secsFromOriginAtWS tp
      we = secsFromOriginAtWE tp
      dur = durFromRhythmic rhythmic tempo -- number
      x1 = eval + asap -- always the start of the program
      blocks = getBlocks (ws - dur) we x1 dur -- Array Number
      onsets = onsetsFromBlocks blocks (fromFoldable $ rhythmicToOnsets rhythmic) dur -- Array Onset --- absolute position
      indexes = getIndexes rhythmic (ws - dur) we x1 dur -- Array Index
      events = zipWith Event onsets indexes
      posFromEvent:: Event -> Number
      posFromEvent (Event (Onset _ p) _) = p
      looped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) < we) events
      unlooped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) <= we) $ unloopEvents events
  pure if loop then looped else unlooped

calculateTemporal mapa tp aKey (Temporal (Metric cTo' cFrom' tm) rhythmic loop) = do
  let dur = durFromRhythmic rhythmic $ processTempoMark tm tp.tempo mapa -- correct (change tempo naming to other name)
  -- log ("durCalcTempoMetricTemporal " <> show dur)
  x1 <- x1MetricVoice tp tm cTo' cFrom' rhythmic mapa
  log ("x1 IN metricTemporal " <> show x1)
  let posixAtOrigin = fromDateTimeToPosix (origin tp.tempo)
  -- let eval = secsFromOriginAtEval tp
  let ws = secsFromOriginAtWS tp
  let we = secsFromOriginAtWE tp
  let blocks = getBlocks (ws - dur) we x1 dur -- to check
  -- log ("blocksMetricTemporal: " <> show blocks)
  let onsetPercent = fromFoldable $ rhythmicToOnsets rhythmic -- Array Onsets --- Position in Percentage
  let onsets = onsetsFromBlocks blocks onsetPercent dur -- Array Onset --- absolute position        
  let indexes = getIndexes rhythmic (ws - dur) we x1 dur -- Array Index
  let events = zipWith Event onsets indexes
  let posFromEvent:: Event -> Number
      posFromEvent (Event (Onset _ p) _) = p
  let looped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) < we) events
  let unlooped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) < we) $ unloopEvents events
  pure $ if loop then looped else unlooped
                                  -- v2            --v1
calculateTemporal mapa tp aKey (Temporal (Converge cKey cTo' cFrom' tm) rhythmic loop) = do
  let dur = durFromRhythmic rhythmic $ processTempoMark tm tp.tempo mapa 
  x1 <- x1ConvergeVoice tp tm cKey cTo' cFrom' rhythmic mapa -- v1
  let posixAtOrigin = fromDateTimeToPosix (origin tp.tempo)
  -- let eval = secsFromOriginAtEval tp
  let ws = secsFromOriginAtWS tp
  let we = secsFromOriginAtWE tp

  let blocks = getBlocks (ws - dur) we x1 dur -- to check
  -- log ("blocksMetricTemporal: " <> show blocks)
  let onsetPercent = fromFoldable $ rhythmicToOnsets rhythmic -- Array Onsets --- Position in Percentage
  let onsets = onsetsFromBlocks blocks onsetPercent dur -- Array Onset --- absolute position        
  let indexes = getIndexes rhythmic (ws - dur) we x1 dur -- Array Index
  let events = zipWith Event onsets indexes
  let posFromEvent:: Event -> Number
      posFromEvent (Event (Onset _ p) _) = p
  let looped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) < we) events
  let unlooped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) < we) $ unloopEvents events
  pure $ if loop then looped else unlooped

unloopEvents:: Array Event -> Array Event
unloopEvents es = filter (\(Event _ (Index b _ _)) -> b == 0) es

addPosixOriginToCalculation:: Number -> Array Event -> Array Event
addPosixOriginToCalculation posix es = map (\(Event (Onset bool pos) i) -> Event (Onset bool (pos + posix)) i) es

-- find x1 and dur of referenceVoice for convergent temporal
x1ConvergeVoice:: TimePacket -> TempoMark -> String -> ConvergeTo -> ConvergeFrom -> Rhythmic -> M.Map String Temporal -> Effect Number -- this number is secs from origin
                     -- v1
x1ConvergeVoice  tp tm cKey cTo' cFrom' rhythmic mapa = do
  let referencedTemporal = fromMaybe (Temporal (Kairos 0.0 (CPM (fromInt 120))) O false) $ M.lookup cKey mapa
  let referencedDur = (\(Temporal p rhy _) -> durFromRhythmic rhy $ processTempoMark (getTempoMark p) tp.tempo mapa) referencedTemporal
  let referencedRhythmic = (\(Temporal _ r _) -> r) referencedTemporal
  let referencedTempo = (\(Temporal p _ _) -> processTempoMark (getTempoMark p) tp.tempo mapa) referencedTemporal -- ::Number -- cpm                    -v1
  referencedX1 <- findReferencedX1 tp referencedTemporal mapa
  referencedVoiceAtEval <- elapsedVoiceAtEval tp referencedX1 referencedDur -- not secs but cycles
  log ("referencedVoiceAtEval top " <> show referencedVoiceAtEval)
  let innerPos = innerPosCTo referencedRhythmic referencedTempo cTo'
  let cTo = calculateCToNEW innerPos referencedVoiceAtEval cTo'

  let processedTempoMark = processTempoMark tm tp.tempo mapa
  let cFrom = calculateCFrom cFrom' processedTempoMark rhythmic
  let dur = durFromRhythmic rhythmic processedTempoMark
  
  let x1 = calculateStartConvergent referencedDur cTo dur cFrom  -- result in secs
  log ("x1 converge voice top " <> show (referencedX1 + x1))
  -- cuando empieza la voz en secs, cuanto dura cada bloque en secs, donde esta la voz en eval
  pure (referencedX1 + x1)

findReferencedX1::TimePacket -> Temporal -> M.Map String Temporal -> Effect Number
findReferencedX1 tp (Temporal (Kairos asap tm) rhy _) mapa = do
  let eval = secsFromOriginAtEval tp
  let x1 = eval + asap
  log ("x1 kairos voice " <> show x1)
  pure x1
findReferencedX1 tp (Temporal (Metric cTo cFrom tm) rhy _) mapa = do
  x1 <- x1MetricVoice tp tm cTo cFrom rhy mapa 
  log ("x1 metric voice " <> show x1)
  pure x1            -- v1            --v0
findReferencedX1 tp (Temporal (Converge cKey cTo cFrom tm) rhy l) mapa = do
  let way = keysForReferencePath cKey mapa (Nil) -- Array String
  log ("key " <> show cKey) 
  log ("way: " <> show way)      -- v1               --v0
  recursiveX1 <- recursiveRefX1 tp (Temporal (Converge cKey cTo cFrom tm) rhy l) mapa Nothing way -- v1's x1
  log ("x1 converge voice" <> show recursiveX1)
  pure recursiveX1

recursiveRefX1:: TimePacket -> Temporal -> M.Map String Temporal -> Maybe (Tuple String Number) -> List String -> Effect Number      -- v2                                           -- incoming with [] is v1
recursiveRefX1 tp temporal mapa incomingKeyX1' (Nil) = do
  let cKey = getKey temporal 
  let (Tuple cTo' cFrom') = (\(Temporal p _ _) -> getConvergences p) temporal 
  let rhythmic = (\(Temporal _ r _) -> r) temporal
  let processedTM = (\(Temporal p _ _) -> processTempoMark (getTempoMark p) tp.tempo mapa) temporal
  let dur = durFromRhythmic rhythmic processedTM
  let cFrom = calculateCFrom cFrom' processedTM rhythmic 

  let temporalHack = fromMaybe defTemporal $ M.lookup cKey mapa
  incomingKeyX1 <- if incomingKeyX1' == Nothing then 
            Just <$> (Tuple cKey <$> (findReferencedX1 tp temporalHack mapa)) else pure incomingKeyX1'

  log ("incomingKeyX1 " <> show incomingKeyX1)
  
  let (Tuple refKey refX1) = fromMaybe (Tuple "error" 2.666) incomingKeyX1 
  log ("refX1 " <> show refX1)
  let refTemporal = fromMaybe defTemporal $ M.lookup refKey mapa
  let refRhythmic = (\(Temporal _ r _) ->  r) refTemporal
  let refTM = (\(Temporal p _ _) -> processTempoMark (getTempoMark p) tp.tempo mapa) refTemporal
  let refDur = durFromRhythmic refRhythmic refTM
  refVoiceAtEval <- elapsedVoiceAtEval tp refX1 refDur -- not secs but cycles
  let innerPos = innerPosCTo refRhythmic refTM cTo'
  let cTo = calculateCToNEW innerPos refVoiceAtEval cTo'

  let x1 = calculateStartConvergent refDur cTo dur cFrom
  log ("recursiveRefwithoutWay" <> show (refX1 + x1))
  pure (refX1 + x1)
  
recursiveRefX1 tp temporal mapa incomingKeyX1 (Cons x xs) = do
  let refTemporal = fromMaybe defTemporal $ M.lookup x mapa  -- v0
  let (Tuple cTo' cFrom') = (\(Temporal p _ _) -> getConvergences p) refTemporal 
  let refRhythmic = (\(Temporal _ r _) -> r) refTemporal
  let refProcessedTM = (\(Temporal p _ _) -> processTempoMark (getTempoMark p) tp.tempo mapa) refTemporal
  let refDur = durFromRhythmic refRhythmic refProcessedTM
  let cFrom = calculateCFrom cFrom' refProcessedTM refRhythmic
  refX1 <- case incomingKeyX1 of
          Nothing -> findReferencedX1 tp refTemporal mapa -- result: v0's block1's start point (x1)
          Just prevKeyX1 -> do
                let prevTemporal = fromMaybe defTemporal $ M.lookup (fst prevKeyX1) mapa
                let prevRhythmic = (\(Temporal _ r _) ->  r) prevTemporal
                let prevTM = (\(Temporal p _ _) -> processTempoMark (getTempoMark p) tp.tempo mapa) prevTemporal
                let prevDur = durFromRhythmic prevRhythmic prevTM
                prevVoiceAtEval <- elapsedVoiceAtEval tp (snd prevKeyX1) prevDur -- not secs but cycles
                let innerPos = innerPosCTo prevRhythmic prevTM cTo'
                let cTo = calculateCToNEW innerPos prevVoiceAtEval cTo'
                let refX1 = calculateStartConvergent prevDur cTo refDur cFrom
                pure ((snd prevKeyX1) + refX1) 
  result <- recursiveRefX1 tp temporal mapa (Just (Tuple x refX1)) xs 
  log ("result recursiveRef " <> show result)
  pure result

getConvergences:: Polytemporal -> Tuple ConvergeTo ConvergeFrom
getConvergences (Converge _ cTo cFrom _) = Tuple cTo cFrom
getConvergences _ = Tuple defConvergeTo defConvergeFrom

recursor = keysForReferencePath "v0" defMapTemporals (Nil)

keysForReferencePath:: String -> M.Map String Temporal -> List String -> List String
keysForReferencePath aKey {-v0-} mapa listOfReferences 
  | isNotConvergent aKey mapa = (Nil)
  | otherwise =  -- v0
      if (isNotConvergent nextCheck mapa)
        then (Cons nextCheck listOfReferences)
          else keysForReferencePath nextCheck mapa (Cons nextCheck listOfReferences)
    where nextCheck = getKey $ fromMaybe defTemporal $ M.lookup aKey mapa 

getKey (Temporal (Converge aKey _ _ _) _ _) = aKey
getKey (Temporal _ _ _) = "2666"

isNotConvergent aKey mapa = f $ fromMaybe defTemporal $ M.lookup aKey mapa 
  where f (Temporal (Converge _ _ _ _) _ _) = false
        f (Temporal _ _ _) = true

elapsedVoiceAtEval:: TimePacket -> Number -> Number -> Effect Number
elapsedVoiceAtEval tp x1 dur = do
  let eval = secsFromOriginAtEval tp
      atEval = (eval - x1) / dur   
  pure atEval

---- finding x1 for Metric 
x1MetricVoice:: TimePacket -> TempoMark -> ConvergeTo -> ConvergeFrom -> Rhythmic -> M.Map String Temporal -> Effect Number
x1MetricVoice tp tm cTo' cFrom' rhythmic mapa = do
  let tempo = processTempoMark tm tp.tempo mapa
  -- log ("tempo-X1Metric " <> show tempo)
  let eval = secsFromOriginAtEval tp
  let externalVoiceSecs = 1.0 / (R.toNumber tp.tempo.freq) 
  -- log ("externalVoiceSecs-X1Metric " <> show externalVoiceSecs)
  let cyclesAtEval = R.toNumber $ timeToCount tp.tempo tp.eval 
  -- log ("cyclesAtEva-X1Metric " <> show cyclesAtEval)
  let cTo = calculateCToMetric cyclesAtEval cTo' -- cycles of compared voice
  -- log ("cTo-X1Metric " <> show cTo)
  let cFrom = calculateCFrom cFrom' tempo rhythmic -- ignore for now, test with 0
  let dur = durFromRhythmic rhythmic tempo -- correct (change tempo naming to other name)
  -- log ("dur-X1Metric " <> show dur)
  let x1 = calculateStartConvergent externalVoiceSecs cTo dur cFrom  -- result in secs
  -- log ("x1 (result of X1 Metric)" <> show x1)
  pure x1 

----- this funca!! <3  <3  <3 
calculateStartConvergent:: Number -> Number -> Number -> Number -> Number
calculateStartConvergent durConverged convergeTo durVoice convergeFrom = startOfVoiceInSecs
  where cTo = convergeTo * durConverged
        cFrom = convergeFrom * durVoice
        startOfVoiceInSecs = cTo - cFrom

------  tempomark calculations -----
-- add dur as possibility
processTempoMark:: TempoMark -> Tempo -> M.Map String Temporal -> Number 
processTempoMark (CPM cpm) _ _ = R.toNumber (cpm / (4%1))
processTempoMark (BPM bpm figure) _ _ = R.toNumber ((bpm / (4%1)) / figure)
processTempoMark (CPS cps) _ _ = R.toNumber (cps * (60%1))
processTempoMark XTempo t _ = (R.toNumber (t.freq * (60%1) * (4%1)))
processTempoMark (Prop id x y) t mapa = fromMaybe 120.0 otherTempo
  where prop = (toNumber x / toNumber y)
        otherTempo =  (\(Temporal p _ _) -> calculateRTempo mapa t (getTempoMark p) prop) <$> M.lookup id mapa

getTempoMark:: Polytemporal -> TempoMark
getTempoMark (Kairos _ tm) = tm
getTempoMark (Metric _ _ tm) = tm
getTempoMark (Converge _ _ _ tm) = tm

calculateRTempo:: M.Map String Temporal -> Tempo -> TempoMark -> Number -> Number 
calculateRTempo m t (CPM cpm) prop = (R.toNumber (cpm / (4%1))) * prop
calculateRTempo m t (BPM bpm figure) prop = (R.toNumber ((bpm / (4%1)) / figure)) * prop
calculateRTempo m t (CPS cps) prop = R.toNumber (cps * (60%1)) * prop
calculateRTempo m t XTempo prop = (R.toNumber (t.freq * (60%1) * (4%1))) * prop
calculateRTempo m t (Prop id x y) prop = calculateRTempo m t newTM newProp
  where newProp = (toNumber x / toNumber y) * prop
        newTM = fromMaybe (CPM (fromInt 120)) $ (\(Temporal p _ _) -> (getTempoMark p)) <$> M.lookup id m

-- calculating convergence points
calculateCToMetric:: Number -> ConvergeTo -> Number
calculateCToMetric cyclesAtEval (StructureTo b st a) = (toNumber b) + aligned
  where aligned = aligner cyclesAtEval a
calculateCToMetric cyclesAtEval (ProcessTo i a) = (toNumber i) + aligned
  where aligned = aligner cyclesAtEval a
calculateCToMetric cyclesAtEval (PercenTo p a) =  (p / 100.0) + aligned
  where aligned = aligner cyclesAtEval a

calculateCToNEW:: Number -> Number -> ConvergeTo -> Number
calculateCToNEW innerPos cyclesAtEval (StructureTo b st a) = innerPos + aligned
  where aligned = aligner cyclesAtEval a
calculateCToNEW innerPos cyclesAtEval (ProcessTo i a) = innerPos + aligned
  where aligned = aligner cyclesAtEval a
calculateCToNEW innerPos cyclesAtEval (PercenTo p a) =  (p / 100.0) + aligned
  where aligned = aligner cyclesAtEval a

aligner:: Number -> CPAlign -> Number -- in cycles of external metre
aligner cyclesAtEval Origin = 0.0 
aligner cyclesAtEval Snap = (toNumber $ ceil cyclesAtEval) 
aligner cyclesAtEval (Mod m) = ceiledModInMetre * (toNumber m)
  where modInMetre = cyclesAtEval / (toNumber m)
        ceiledModInMetre = toNumber $ ceil modInMetre

innerPosCTo:: Rhythmic -> Number -> ConvergeTo -> Number
innerPosCTo rhythmic t cTo = percentPos
  where dur = durFromRhythmic rhythmic t
        onsetPercent = fromFoldable $ rhythmicToOnsets rhythmic -- Array Onsets --- Position in Percentage
        lenOnset = length onsetPercent
        structIndexes = rhythmicStructIndex rhythmic [0] -- Array (Array Int)
        eventIndexesPerVoice = (0..(lenOnset-1)) -- Array Int
        structAndPos = zip structIndexes $ map (\(Onset b p) -> p) onsetPercent
        eventsAndPos = zip eventIndexesPerVoice $ map (\(Onset b p) -> p) onsetPercent
        percentPos = filterEventToPosTo cTo structAndPos eventsAndPos lenOnset

calculateCFrom:: ConvergeFrom -> Number -> Rhythmic -> Number
calculateCFrom cp t rhythmic = percentPos
  where dur = durFromRhythmic rhythmic t
        onsetPercent = fromFoldable $ rhythmicToOnsets rhythmic -- Array Onsets --- Position in Percentage
        lenOnset = length onsetPercent
        structIndexes = rhythmicStructIndex rhythmic [0] -- Array (Array Int)
        eventIndexesPerVoice = (0..(lenOnset-1)) -- Array Int
        structAndPos = zip structIndexes $ map (\(Onset b p) -> p) onsetPercent
        eventsAndPos = zip eventIndexesPerVoice $ map (\(Onset b p) -> p) onsetPercent
        percentPos = filterEventToPosFrom cp structAndPos eventsAndPos lenOnset

filterEventToPosTo:: ConvergeTo -> Array (Tuple (Array Int) Number) -> Array (Tuple Int Number) -> Int -> Number
filterEventToPosTo cp structAndPos eventsAndPos lenOnset = result
  where result = case cp of 
                    (StructureTo v st a) -> fromMaybe 0.0 $ head $ map (\x -> cpPos (Left v) (snd x) lenOnset) $ filter (\x -> fst x == st) structAndPos
                    (ProcessTo e a) -> fromMaybe 0.0 $ head $ map (\x -> cpPos (Right e) (snd x) lenOnset) $ filter (\x -> fst x == (e`mod`lenOnset)) eventsAndPos
                    (PercenTo p a) ->  p / 100.0 

filterEventToPosFrom:: ConvergeFrom -> Array (Tuple (Array Int) Number) -> Array (Tuple Int Number) -> Int -> Number
filterEventToPosFrom cp structAndPos eventsAndPos lenOnset = result
  where result = case cp of 
                    (Structure v st) -> fromMaybe 0.0 $ head $ map (\x -> cpPos (Left v) (snd x) lenOnset) $ filter (\x -> fst x == st) structAndPos
                    (Process e) -> fromMaybe 0.0 $ head $ map (\x -> cpPos (Right e) (snd x) lenOnset) $ filter (\x -> fst x == (e`mod`lenOnset)) eventsAndPos
                    (Percen p) ->  p / 100.0 

cpPos:: Either Int Int -> Number -> Int -> Number
cpPos (Left v) x lenOnset = v' + x
  where v' = (toNumber v)
cpPos (Right n) x lenOnset = (toNumber $ floor n') + x
  where n' = (toNumber n)/(toNumber lenOnset)