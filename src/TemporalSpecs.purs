module TemporalSpecs (calculateTemporal) where

import Prelude
import Effect (Effect)
import Effect.Console
import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Map as M
import Data.Foldable (sum)
import Data.Int
import Data.Array (filter,fromFoldable,(!!), zipWith, replicate, concat, (..), (:), init, tail, last,head,reverse,zip, cons, snoc, length, singleton, splitAt)
import Data.List (List(..))

import Data.Tempo

import AST
import Acceleration
import TestOpsAndDefs
import DurationAndIndex
import TimePacketOps

import Data.Rational (Rational(..), (%), fromInt)
import Data.Rational (toNumber) as R -- still need to convert all Number calcs into Rational!!


calculateTemporal:: M.Map String Temporal -> TimePacket -> String -> Temporal -> Effect (Array Event)
calculateTemporal m tp aKey (Temporal (Kairos asap tm) rhythmic loop) = do
  let dur = establishDur tm tp.tempo m rhythmic
      posixAtOrigin = fromDateTimeToPosix (origin tp.tempo)
      eval = secsFromOriginAtEval tp
      ws = secsFromOriginAtWS tp
      we = secsFromOriginAtWE tp
      x1 = eval + asap -- always the start of the program
      blocks = getBlocks (ws - dur) we x1 dur -- Array Number
      -- onsets = onsetsFromBlocks blocks (fromFoldable $ rhythmicToOnsetsAcc rhythmic) dur -- Array Onset --- absolute position
      onsets = onsetsFromBlocks blocks (fromFoldable $ rhythmicToOnsets' tm tp.tempo m rhythmic) dur -- Array Onset --- absolute position
      indexes = getIndexes rhythmic (ws - dur) we x1 dur -- Array Index
      events = zipWith Event onsets indexes
      posFromEvent (Event (Onset _ p) _) = p
      looped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) < we) events
      unlooped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) <= we) $ unloopEvents events
  pure if loop then looped else unlooped

calculateTemporal mapa tp aKey (Temporal (Metric cTo' cFrom' tm) rhythmic loop) = do
  let dur = establishDur tm tp.tempo mapa rhythmic 
  -- log ("durCalcTempoMetricTemporal " <> show dur)
  let lengthRhythm = (length $ fromFoldable $ rhythmicToOnsets rhythmic)-1
  let simCTo = simplifyCTo lengthRhythm cTo'
  let simCFrom = simplifyCFrom lengthRhythm cFrom'
  x1 <- x1MetricVoice tp tm simCTo simCFrom rhythmic mapa
  -- log ("x1 IN metricTemporal " <> show x1)
  let posixAtOrigin = fromDateTimeToPosix (origin tp.tempo)
  -- let eval = secsFromOriginAtEval tp
  let ws = secsFromOriginAtWS tp
  let we = secsFromOriginAtWE tp
  let blocks = getBlocks (ws - dur) we x1 dur -- to check
  -- log ("blocksMetricTemporal: " <> show blocks)
  let onsetPercent = fromFoldable $ rhythmicToOnsets' tm tp.tempo mapa rhythmic -- Array Onsets --- Position in Percentage
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
  let dur = establishDur tm tp.tempo mapa rhythmic
  let lengthRhythm = (length $ fromFoldable $ rhythmicToOnsets rhythmic)-1
  let lengthRhythmTo = (length $ fromFoldable $ rhythmicToOnsets $ getRhythmicFromMap mapa cKey)-1
  let simCTo = simplifyCTo lengthRhythmTo cTo'   
  let simCFrom = simplifyCFrom lengthRhythm cFrom'
  x1 <- x1ConvergeVoice tp tm cKey simCTo simCFrom rhythmic mapa -- v1
  let posixAtOrigin = fromDateTimeToPosix (origin tp.tempo)
  let ws = secsFromOriginAtWS tp
  let we = secsFromOriginAtWE tp
  let blocks = getBlocks (ws - dur) we x1 dur -- to check
  let onsetPercent = fromFoldable $ rhythmicToOnsets' tm tp.tempo mapa rhythmic --[Onsets] Pos in Percentage
  let onsets = onsetsFromBlocks blocks onsetPercent dur --[Onsets] absolute position        
  let indexes = getIndexes rhythmic (ws - dur) we x1 dur -- Array Index
  let events = zipWith Event onsets indexes
  let posFromEvent:: Event -> Number
      posFromEvent (Event (Onset _ p) _) = p
  let looped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) < we) events
  let unlooped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) < we) $ unloopEvents events
  pure $ if loop then looped else unlooped
----- CALCULATE NOVUS!!!!!!!!!!!!!
calculateTemporal mapa tp aKey (Temporal (Novus vKey cFrom' tm) rhythmic loop) = do
  let dur = establishDur tm tp.tempo mapa rhythmic
  let lengthRhythm = (length $ fromFoldable $ rhythmicToOnsets' tm tp.tempo mapa rhythmic)-1
  let simCFrom = simplifyCFrom lengthRhythm cFrom'
  let cp = secsFromOriginAtVantage tp vKey
  -- log ("cp novus: " <> show cp)
  x1 <- x1NovusVoice tp tm cp simCFrom rhythmic mapa -- v1

  let posixAtOrigin = fromDateTimeToPosix (origin tp.tempo)
  let ws = secsFromOriginAtWS tp
  let we = secsFromOriginAtWE tp
  let blocks = getBlocks (ws - dur) we x1 dur -- to check
  let onsetPercent = fromFoldable $ rhythmicToOnsets' tm tp.tempo mapa rhythmic 
  let onsets = onsetsFromBlocks blocks onsetPercent dur --[Onsets] absolute position        
  let indexes = getIndexes rhythmic (ws - dur) we x1 dur -- Array Index
  let events = zipWith Event onsets indexes
  let posFromEvent:: Event -> Number
      posFromEvent (Event (Onset _ p) _) = p
  let looped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) < we) events
  let unlooped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) < we) $ unloopEvents events
  pure $ if loop then looped else unlooped

x1NovusVoice:: TimePacket -> TempoMark -> Number -> ConvergeFrom -> Rhythmic -> M.Map String Temporal -> Effect Number
x1NovusVoice tp tm cp cFrom' rhythmic mapa = do
  let cFrom = calculateCFrom cFrom' rhythmic
  let dur = establishDur tm tp.tempo mapa rhythmic
  let x1 = cp - (cFrom * dur)
  pure x1 

unloopEvents:: Array Event -> Array Event
unloopEvents es = filter (\(Event _ (Index b _ _)) -> b == 0) es

addPosixOriginToCalculation:: Number -> Array Event -> Array Event
addPosixOriginToCalculation posix es = map (\(Event (Onset bool pos) i) -> Event (Onset bool (pos + posix)) i) es

simplifyCTo:: Int -> ConvergeTo -> ConvergeTo
simplifyCTo n (LastTo a) = ProcessTo 0 Origin   ---- 2025::::: Here, that aligner appears odd 
simplifyCTo n cTo = cTo 

simplifyCFrom:: Int -> ConvergeFrom -> ConvergeFrom
simplifyCFrom n Last = Process n  
simplifyCFrom n cfrom = cfrom

-- find x1 and dur of referenceVoice for convergent temporal
---- this needs a simplifyCTo and simplifyCFrom????????????
x1ConvergeVoice:: TimePacket -> TempoMark -> String -> ConvergeTo -> ConvergeFrom -> Rhythmic -> M.Map String Temporal -> Effect Number 
x1ConvergeVoice  tp tm cKey cTo' cFrom' rhythmic mapa = do
  let refTemporal = fromMaybe defTemporal $ M.lookup cKey mapa
  let refRhythmic = getRhythmic refTemporal 
  let refDur = establishDur (tempoMark refTemporal) tp.tempo mapa refRhythmic
  refX1 <- findReferencedX1 tp refTemporal mapa
  refVoiceAtEval <- elapsedVoiceAtEval tp refX1 refDur -- not secs but cycles
  -- log ("refVoiceAtEval top " <> show refVoiceAtEval)
  let innerPos = innerPosCTo refRhythmic cTo'
  let cTo = calculateCToNEW innerPos refVoiceAtEval cTo'
  let cFrom = calculateCFrom cFrom' rhythmic
  let dur = establishDur tm tp.tempo mapa rhythmic 
  let x1 = calculateStartConvergent refDur cTo dur cFrom  -- result in secs
  -- log ("x1 converge voice top " <> show (refX1 + x1))
  -- cuando empieza la voz en secs, cuanto dura cada bloque en secs, donde esta la voz en eval
  pure (refX1 + x1)

findReferencedX1::TimePacket -> Temporal -> M.Map String Temporal -> Effect Number
findReferencedX1 tp (Temporal (Kairos asap tm) rhy _) mapa = do
  let eval = secsFromOriginAtEval tp
  let x1 = eval + asap
  -- log ("x1 kairos voice " <> show x1)
  pure x1
findReferencedX1 tp (Temporal (Metric cTo cFrom tm) rhy _) mapa = do
  x1 <- x1MetricVoice tp tm cTo cFrom rhy mapa 
  -- log ("x1 metric voice " <> show x1)
  pure x1            -- v1            --v0
findReferencedX1 tp (Temporal (Converge cKey cTo cFrom tm) rhy l) mapa = do
  let way = keysForReferencePath cKey mapa (Nil) -- Array String
  -- log ("key " <> show cKey) 
  -- log ("way: " <> show way)      -- v1               --v0
  recursiveX1 <- recursiveRefX1 tp (Temporal (Converge cKey cTo cFrom tm) rhy l) mapa Nothing way -- v1's x1
  -- log ("x1 converge voice" <> show recursiveX1)
  pure recursiveX1
  ---- calculate NOVUS!!!!!!!!!!!!!!!!!!!!!!!!
findReferencedX1 tp (Temporal (Novus vKey cFrom tm) rhy l) mapa = pure 0.0


recursiveRefX1:: TimePacket -> Temporal -> M.Map String Temporal -> Maybe (Tuple String Number) -> List String -> Effect Number      -- v2                                           -- incoming with [] is v1
recursiveRefX1 tp temporal mapa incomingKeyX1' (Nil) = do
  let cKey = getKey mapa temporal 
  let (Tuple cTo' cFrom') = convergences mapa temporal
  let rhythmic = getRhythmic temporal

--- HERE WORK ON implementing establishDur function for acceleration and function

  -- let processedTM = processTempoMark (tempoMark mapa temporal) tp.tempo mapa 
  -- let processedTM = (\(Temporal p _ _) -> processTempoMark (getTempoMark p) tp.tempo mapa) temporal
  -- let dur = durFromRhythmic rhythmic processedTM
  let dur = establishDur (tempoMark temporal) tp.tempo mapa rhythmic
  let cFrom = calculateCFrom cFrom' rhythmic 

  let temporalHack = fromMaybe defTemporal $ M.lookup cKey mapa
  incomingKeyX1 <- if incomingKeyX1' == Nothing then 
            Just <$> (Tuple cKey <$> (findReferencedX1 tp temporalHack mapa)) else pure incomingKeyX1'

  -- log ("incomingKeyX1 " <> show incomingKeyX1)
  
  let (Tuple refKey refX1) = fromMaybe (Tuple "error" 2.666) incomingKeyX1 
  -- log ("refX1 " <> show refX1)
  let refTemporal = fromMaybe defTemporal $ M.lookup refKey mapa

  let refRhythmic = getRhythmic refTemporal

  let refDur = establishDur (tempoMark refTemporal) tp.tempo mapa refRhythmic
  refVoiceAtEval <- elapsedVoiceAtEval tp refX1 refDur -- not secs but cycles
  let innerPos = innerPosCTo refRhythmic cTo'
  let cTo = calculateCToNEW innerPos refVoiceAtEval cTo'

  let x1 = calculateStartConvergent refDur cTo dur cFrom
  -- log ("recursiveRefwithoutWay" <> show (refX1 + x1))
  pure (refX1 + x1)
  
recursiveRefX1 tp temporal mapa incomingKeyX1 (Cons x xs) = do
  let refTemporal = fromMaybe defTemporal $ M.lookup x mapa  -- v0
  let (Tuple cTo' cFrom') = convergences mapa refTemporal
  let refRhythmic = getRhythmic refTemporal
  -- let refProcessedTM = processTempoMark (tempoMark mapa refTemporal) tp.tempo mapa 
  -- let refRhythmic = (\(Temporal _ r _) -> r) refTemporal
  -- let refProcessedTM = (\(Temporal p _ _) -> processTempoMark (getTempoMark p) tp.tempo mapa) refTemporal
  let refDur = establishDur (tempoMark refTemporal) tp.tempo mapa refRhythmic
  -- let refDur = durFromRhythmic refRhythmic refProcessedTM
  let cFrom = calculateCFrom cFrom' refRhythmic
  refX1 <- case incomingKeyX1 of
          Nothing -> findReferencedX1 tp refTemporal mapa -- result: v0's block1's start point (x1)
          Just prevKeyX1 -> do
                let prevTemporal = fromMaybe defTemporal $ M.lookup (fst prevKeyX1) mapa
                let prevRhythmic = getRhythmic prevTemporal
                -- let prevTM = processTempoMark (tempoMark mapa prevTemporal) tp.tempo mapa
                -- let prevDur = durFromRhythmic prevRhythmic prevTM
                let prevDur = establishDur (tempoMark prevTemporal) tp.tempo mapa prevRhythmic
                prevVoiceAtEval <- elapsedVoiceAtEval tp (snd prevKeyX1) prevDur -- not secs but cycles
                let innerPos = innerPosCTo prevRhythmic cTo'
                let cTo = calculateCToNEW innerPos prevVoiceAtEval cTo'
                let refX1 = calculateStartConvergent prevDur cTo refDur cFrom
                pure ((snd prevKeyX1) + refX1) 
  result <- recursiveRefX1 tp temporal mapa (Just (Tuple x refX1)) xs 
  -- log ("result recursiveRef " <> show result)
  pure result

convergences:: M.Map String Temporal -> Temporal -> Tuple ConvergeTo ConvergeFrom
convergences _ (Temporal p _ _) = getConvergences p

getConvergences:: Polytemporal -> Tuple ConvergeTo ConvergeFrom
getConvergences (Converge _ cTo cFrom _) = Tuple cTo cFrom
getConvergences _ = Tuple defConvergeTo defConvergeFrom

keysForReferencePath:: String -> M.Map String Temporal -> List String -> List String
keysForReferencePath aKey {-v0-} mapa listOfReferences 
  | isNotConvergent aKey mapa = (Nil)
  | otherwise =  -- v0
      if (isNotConvergent nextCheck mapa)
        then (Cons nextCheck listOfReferences)
          else keysForReferencePath nextCheck mapa (Cons nextCheck listOfReferences)
    where nextCheck = getKey mapa $ fromMaybe defTemporal $ M.lookup aKey mapa 

getKey _ (Temporal (Converge aKey _ _ _) _ _) = aKey
getKey _ (Temporal _ _ _) = "2666"

isNotConvergent aKey mapa = f' mapa $ fromMaybe defTemporal $ M.lookup aKey mapa 

f' m (Temporal (Converge _ _ _ _) _ _) = false
f' m (Temporal _ _ _) = true

elapsedVoiceAtEval:: TimePacket -> Number -> Number -> Effect Number
elapsedVoiceAtEval tp x1 dur = do
  let eval = secsFromOriginAtEval tp
      atEval = (eval - x1) / dur   
  pure atEval

---- finding x1 for Metric 
x1MetricVoice:: TimePacket -> TempoMark -> ConvergeTo -> ConvergeFrom -> Rhythmic -> M.Map String Temporal -> Effect Number
x1MetricVoice tp tm cTo' cFrom' rhythmic mapa = do
  -- log ("tempo-X1Metric " <> show tempo)
  let eval = secsFromOriginAtEval tp
  let externalVoiceSecs = 1.0 / (R.toNumber tp.tempo.freq) 
  -- log ("externalVoiceSecs-X1Metric " <> show externalVoiceSecs)
  let cyclesAtEval = R.toNumber $ timeToCount tp.tempo tp.eval 
  -- log ("cyclesAtEva-X1Metric " <> show cyclesAtEval)
  let cTo = calculateCToMetric cyclesAtEval cTo' -- cycles of compared voice
  -- log ("cTo-X1Metric " <> show cTo)
  let cFrom = calculateCFrom cFrom' rhythmic -- ignore for now, test with 0
  let dur = establishDur tm tp.tempo mapa rhythmic
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

-- calculating convergence points
calculateCToMetric:: Number -> ConvergeTo -> Number
calculateCToMetric cyclesAtEval (StructureTo b st a) = (toNumber b) + aligned
  where aligned = aligner cyclesAtEval a
calculateCToMetric cyclesAtEval (ProcessTo i a) = (toNumber i) + aligned
  where aligned = aligner cyclesAtEval a
calculateCToMetric cyclesAtEval (PercenTo p a) =  (p / 100.0) + aligned
  where aligned = aligner cyclesAtEval a
calculateCToMetric cyclesAtEval _ = 0.0

calculateCToNEW:: Number -> Number -> ConvergeTo -> Number
calculateCToNEW innerPos cyclesAtEval (StructureTo b st a) = innerPos + aligned
  where aligned = aligner cyclesAtEval a
calculateCToNEW innerPos cyclesAtEval (ProcessTo i a) = innerPos + aligned
  where aligned = aligner cyclesAtEval a
calculateCToNEW innerPos cyclesAtEval (PercenTo p a) =  (p / 100.0) + aligned
  where aligned = aligner cyclesAtEval a
calculateCToNEW innerPos cyclesAtEval _ = 0.0

aligner:: Number -> CPAlign -> Number -- in cycles of external metre
aligner cyclesAtEval Origin = 0.0 
aligner cyclesAtEval Snap = (toNumber $ ceil cyclesAtEval) 
aligner cyclesAtEval (Mod m) = ceiledModInMetre * (toNumber m)
  where modInMetre = cyclesAtEval / (toNumber m)
        ceiledModInMetre = toNumber $ ceil modInMetre

innerPosCTo:: Rhythmic -> ConvergeTo -> Number
innerPosCTo rhythmic cTo = percentPos
  where onsetPercent = fromFoldable $ rhythmicToOnsets rhythmic -- Array Onsets --- Position in Percentage
        lenOnset = length onsetPercent
        structIndexes = rhythmicStructIndex rhythmic [0] -- Array (Array Int)
        eventIndexesPerVoice = (0..(lenOnset-1)) -- Array Int
        structAndPos = zip structIndexes $ map (\(Onset b p) -> p) onsetPercent
        eventsAndPos = zip eventIndexesPerVoice $ map (\(Onset b p) -> p) onsetPercent
        percentPos = filterEventToPosTo cTo structAndPos eventsAndPos lenOnset

calculateCFrom:: ConvergeFrom -> Rhythmic -> Number
calculateCFrom cp rhythmic = percentPos
  where onsetPercent = fromFoldable $ rhythmicToOnsets rhythmic -- Array Onsets --- Position in Percentage
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
                    _ -> 0.0

filterEventToPosFrom:: ConvergeFrom -> Array (Tuple (Array Int) Number) -> Array (Tuple Int Number) -> Int -> Number
filterEventToPosFrom cp structAndPos eventsAndPos lenOnset = result
  where result = case cp of 
                    (Structure v st) -> fromMaybe 0.0 $ head $ map (\x -> cpPos (Left v) (snd x) lenOnset) $ filter (\x -> fst x == st) structAndPos
                    (Process e) -> fromMaybe 0.0 $ head $ map (\x -> cpPos (Right e) (snd x) lenOnset) $ filter (\x -> fst x == (e`mod`lenOnset)) eventsAndPos
                    (Percen p) ->  p / 100.0 
                    _ -> 0.0

cpPos:: Either Int Int -> Number -> Int -> Number
cpPos (Left v) x lenOnset = v' + x
  where v' = (toNumber v)
cpPos (Right n) x lenOnset = (toNumber $ floor n') + x
  where n' = (toNumber n)/(toNumber lenOnset)

-- dur
establishDur:: TempoMark -> Tempo -> M.Map String Temporal -> Rhythmic -> Number
establishDur (Dur n) xT m rhy = R.toNumber n
establishDur (Sin sin) xT m rhy = durInSecs (sum $ rhythmicToSinDur rhy (R.toNumber sin.osc) min max (R.toNumber sin.phase)) min
  where min = processTempoMark sin.min xT m
        max = processTempoMark sin.max xT m
establishDur (Prop id x y) xT m rhy = durProp m xT otherTempoMark otherRhy prop 
  where prop = (toNumber x / toNumber y)
        otherTemporal = fromMaybe defTemporal $ M.lookup id m
        otherTempoMark = tempoMark otherTemporal
        otherRhy = getRhythmic otherTemporal
establishDur tm xT m rhy = durFromRhythmic rhy $ processTempoMark tm xT m

durProp:: M.Map String Temporal -> Tempo -> TempoMark -> Rhythmic -> Number -> Number
durProp m xT (Sin sin) r prop = durOther / prop
  where min = processTempoMark sin.min xT m
        max = processTempoMark sin.max xT m
        durOther = durInSecs (sum $ rhythmicToSinDur r (R.toNumber sin.osc) min max (R.toNumber sin.phase)) min

durProp m xT (Dur d) r prop = (R.toNumber d) / prop
durProp m xT (Prop id x y) _ prop = durProp m xT otherTM otherRhy otherProp 
  where otherProp = (toNumber x / toNumber y) 
        otherTemporal = fromMaybe defTemporal $ M.lookup id m
        otherTM = tempoMark otherTemporal
        otherRhy = getRhythmic otherTemporal
durProp m xT another r prop = (durFromRhythmic r $ processTempoMark another xT m) / prop

rhythmicToOnsets':: TempoMark -> Tempo -> M.Map String Temporal -> Rhythmic -> List Onset 
rhythmicToOnsets' (Sin s) xT m rhy = rhythmicToOnsetsSin rhy (R.toNumber s.osc) min max (R.toNumber s.phase)
  where min = processTempoMark s.min xT m
        max = processTempoMark s.max xT m
rhythmicToOnsets' tm xT m rhy = case tm of 
                              (Prop id x y) -> rhythmicToOnsets' otherTM xT m rhy 
                                where otherTemporal = fromMaybe defTemporal $ M.lookup id m
                                      otherTM = tempoMark otherTemporal
                                      otherRhy = getRhythmic otherTemporal
                              _ -> rhythmicToOnsets rhy

processTempoMark:: TempoMark -> Tempo -> M.Map String Temporal -> Number 
processTempoMark (CPM cpm) _ _ = R.toNumber (cpm / (4%1))
processTempoMark (BPM bpm figure) _ _ = R.toNumber ((bpm / (4%1)) / figure)
processTempoMark (CPS cps) _ _ = R.toNumber (cps * (60%1))
processTempoMark XTempo t _ = (R.toNumber (t.freq * (60%1) * (4%1)))
processTempoMark (Prop id x y) t mapa = fromMaybe 120.0 otherTempo
  where prop = (toNumber x / toNumber y)
        otherTempo = (\temporal -> calculateRTempo mapa t (tempoMark temporal) prop) <$> M.lookup id mapa
processTempoMark other t mapa = 0.0 

calculateRTempo:: M.Map String Temporal -> Tempo -> TempoMark -> Number -> Number 
calculateRTempo m t (CPM cpm) prop = (R.toNumber (cpm / (4%1))) * prop
calculateRTempo m t (BPM bpm figure) prop = (R.toNumber ((bpm / (4%1)) / figure)) * prop
calculateRTempo m t (CPS cps) prop = R.toNumber (cps * (60%1)) * prop
calculateRTempo m t XTempo prop = (R.toNumber (t.freq * (60%1) * (4%1))) * prop
calculateRTempo m t (Prop id x y) prop = calculateRTempo m t newTM newProp
  where newProp = (toNumber x / toNumber y) * prop
        newTM = fromMaybe (CPM (fromInt 120)) $ (\temporal -> tempoMark temporal) <$> M.lookup id m
calculateRTempo m t other prop = 0.0