module Calculations (mapToWaste, countInFreqToSecs, rToNumber, toRat, test', calculateRTempo, processTempoMark, test', getTempoMapFromVoice) where

import Prelude

import Effect (Effect)

import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Map as M
import Data.Foldable (sum)
import Data.Int
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array (filter,fromFoldable,(!!), zipWith, replicate, concat, (..), (:), init, tail, last,head,reverse,zip, snoc, length, singleton)
import Data.List
import Data.Traversable (scanl)
import Data.List (fromFoldable,concat,zip,zipWith,length,init) as L

import Data.Newtype

import Data.Tempo

import AST
import Parsing
import Parser
import Rhythm

import Data.Rational (Rational(..), (%), fromInt)
import Data.Rational (toNumber) as R
import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration


-- for testin

import Data.Enum
import Partial.Unsafe
------- next appointment: Tuesday 19th 3:00 pm

--- Issue: -- ws: 0 -- we: 14 -- eval time: 5
-- tempo <- (origin point: 0) (point: 0) 120bpm | x :|  
-- v1 <- (mod 4 point: 0) (point: 2) 120bpm | xxxx ||
-- v2 <- (mod 4 point: 0) (point: 6) 155bpm | xxxxxxx ||
-- v3 <- v1 (snap point: 0) (point: 0) 100bpm | xxxxx ||
-- the text above produces a valid program. There is a conceptual problem: v3 converges with v1 at (snap point:0), which is a convergence at index 22 of the tempo voice. This is because the 12 index is the correct convergence for (mod 4 point:0) given eval time is 5 (seconds). In this case 22 is (10 units of tempo voice, the eval value is added erroneously to the converging voice). This means that the eval alignment value needs to be conceptualised differently when using a converging voice. 

-- the Converge polytempo should pass recursively the cTo and cFrom rather than calculate it right away and pass them like numbers




-- glosario:
---- Voice is a segment in a sequence (looped). So, a layer defines an array (sequence) of voices that are identical except their position in time (and their voice index)
---- Block is a rhythmic pattern of onsets
---- Onset a moment in time when a sound is instantiated

---- An Event is an Onset with an Index

---- Index is a mark that allows me to identify the position of the onset so sounds and sound characteristics can be attached to it
---- process-oriented index: an int identifier for each onset on a flow of onsets. 
---- eventIndex is the way I will refer to process oriented indexes
---- structure-oriented index: an int identifier for each segment on a voice and an array to identifier internal events in a voice: The head is the 'natural' subdivisions of the voice, each new element in the array is a new subdivision
---- a structure oriented index has a voice index and a structure index. A voice index is an Int while the Structure Index is an Array Int. The notation I have made for the structure oriented index is: 3-0.2.4  to the left of the (-) is the voice index and to the right of it is the event position in the rhythmic idea. The head of the array is the top level of the nested subdivisions and the last is the deepest level of the subdivisions.  

-- helpers between program, voices and tempoMap and auralMap

--Note: The pairing of TempoMaps and AuralMaps is only useful for instantiating sound. The tempoMap information needs to be checked at parsing time and passed over to the calculations since some of the Temporals act as 'metronomes' for convergent voices. Tempo information exist not only as sound sources but as transcient time grids

assambleVoice:: Program -> Voices
assambleVoice program = M.intersectionWith (\x y -> Voice x y) tempoMap auralMap
  where tempoMap = getTemporalMap program
        auralMap = getAuralMap program

getTempoMapFromVoice:: Voices -> M.Map String Temporal
getTempoMapFromVoice voices = M.mapMaybe (\(Voice temporal aural) -> Just temporal) voices

getAuralMapFromVoice:: Voices -> M.Map String Aural
getAuralMapFromVoice voices = M.mapMaybe (\(Voice temporal aural) -> Just aural) voices

mapToWaste:: Program -> DateTime -> DateTime -> DateTime -> Tempo -> Array Waste
mapToWaste program ws' we' eval' t = waste
  where timePacket = {ws: ws', we: we', eval: eval', origin: origin t, tempo: t}
        voices' = assambleVoice program -- Voices
        calculatedVoices = calculateVoices (getTemporalMap program) voices' timePacket

        -- calculatedTime = calculateTemporals (getTemporalMap program) timePacket --Map Str (Array Event)

        
        -- this will have to go eventually
        events = M.values calculatedVoices -- List (Array Event)



        getXs:: Array Event -> Array Event
        getXs events = filter (\(Event (Onset b p) _ ) -> b) events
        xs = fromFoldable $ map getXs events -- Array (Array Event)
        voices = fromFoldable $ (range 0 (L.length events)) -- List Int
        withNs = zip voices xs  -- List (Tuple N (Array Event))
        f n' xs = map (\ (Event (Onset b p) _) -> {whenPosix: p, n: n', s: "tink"}) xs
        waste = concat $ map (\(Tuple n xs) -> f n xs) withNs


test' = calculateTemporals (M.fromFoldable [
  Tuple "v1" (Temporal (Metric (ProcessTo 0 Snap) (Process 0) (BPM 60.0)) (Rhythmics (L.fromFoldable [X,X, (Sd (Rhythmics $ L.fromFoldable [X,X])),X])) true),

  Tuple "v2" (Temporal (Converge "v1" (ProcessTo 1 Snap) (Process 0) (BPM 120.0)) (Rhythmics (L.fromFoldable [X,X, (Sd (Rhythmics $ L.fromFoldable [X,X])),X])) true)
  
  ]
  ) 
  {ws: wP 0.0, we: wP 4.0, eval: wP 0.0, origin: origin t, tempo: t}

--test' = calculateOnsets (M.singleton "v" (Temporal (Metric (PercenTo 0 Origin) (Percen 0) 120.0) (Rhythmics (L.fromFoldable [X])) true)) {ws: wP 0.0, we: wP 6.0, eval: wP 0.4, origin: origin t, tempo: t}


-- (1693237327.024 + o) (1693237327.054 + o + 1.0) 1693237307.024

--- to do on september 14 2023:
-- to make the convergeTo dependant pon eval time you need to:
      -- find the starting point of converged voice
      -- find the relationship between converged voice and eval time:
        -- if eval time is before the voice starts you will go into negative numbers.


calculateTemporals:: M.Map String Temporal -> TimePacket -> M.Map String (Array Event)
calculateTemporals mapa tp = mapWithIndex (calculateTemporal mapa tp) mapa 


----- down from here is the same as visualiser
calculateVoices:: M.Map String Temporal -> Voices -> TimePacket -> M.Map String (Array Event)
calculateVoices tempoMap mapa tp = mapWithIndex (calculateVoice tempoMap tp) mapa 

calculateVoice:: M.Map String Temporal -> TimePacket -> String -> Voice -> Array Event
calculateVoice tempoMap tp aKey (Voice temporal aural) = events
  where events = calculateTemporal tempoMap tp aKey temporal -- Array Event
        sound = processSound (getSound aural) events


processSound:: Maybe Value -> Array Event -> Array {event:: Event, sound:: String} 
processSound Nothing _ = []
processSound (Just (TransposedSound _)) _ = [] -- soon to come
processSound (Just (Sound span sList)) events = spanSound span (fromFoldable sList) events
processSound _ _ = [] 

spanSound:: Span -> Array String -> Array Event -> Array {event:: Event, sound:: String}  
spanSound CycleEvent xs events = map (processSound xs) events
  where processSound xs event' = {event: event', sound: sound'} 
              where sound' = fromMaybe "" $ xs !! (getEventIndex event' `mod` length xs)
spanSound CycleBlock xs events = map (processSound xs) events
  where processSound xs event' = {event: event', sound: sound'}
              where sound' = fromMaybe "" $ xs !! (getBlockIndex event' `mod` length xs)
spanSound CycleInBlock xs events = map (processSound xs) events
  where processSound xs event' = {event: event', sound: sound'}
              where sound' = 
spanSound _ xs events = []

xx[xx]x

Index 0 [0] 0

getBlockIndex:: Event -> Int
getBlockIndex (Event _ (Index n _ _)) = n

getEventIndex:: Event -> Int
getEventIndex (Event _ (Index _ _ n)) = n

showEventIndex (Index _ _ n) = show n 

getSound:: List Value -> Maybe Value
getSound aural = head $ filter isSound $ fromFoldable aural

isSound:: Value -> Boolean
isSound (Sound _ _) = true
isSound (TransposedSound _) = true
isSound _ = false

-- data Value = Sound Span (List String) | TransposedSound String | N Span (List Int) | TransposedN String 
-- data Span = CycleEvent | CycleBlock | CycleInBlock | SpreadBlock
-- data Event = Event Onset Index
-- data Index = Index Int (Array Int) Int


calculateTemporal:: M.Map String Temporal -> TimePacket -> String -> Temporal -> Array Event  
calculateTemporal mapa tp aKey (Temporal (Kairos asap tempoMark) rhythmic loop) = if loop then looped else unlooped
  where tempo = processTempoMark tempoMark tp.tempo mapa
        posixAtOrigin = fromDateTimeToPosix (origin tp.tempo)
        eval = secsFromOriginAtEval tp
        ws = secsFromOriginAtWS tp
        we = secsFromOriginAtWE tp
        dur = durFromRhythmic rhythmic tempo -- number
        x1 = eval + asap -- always the start of the program
        x2 = x1 + dur
        blocks = getBlocks (ws - dur) we x1 dur -- Array Number
        onsets = onsetsFromBlocks blocks (fromFoldable $ rhythmicToOnsets rhythmic) dur posixAtOrigin -- Array Onset --- absolute position
        indexes = getIndexes rhythmic (ws - dur) we x1 dur -- Array Index
        events = zipWith Event onsets indexes
        looped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) < we) events
        unlooped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) <= we) $ eventsInWindowUnlooped events

calculateTemporal mapa tp aKey (Temporal (Metric cTo' cFrom' t) rhythmic loop) = if loop then looped else unlooped  
  where tempo = processTempoMark t tp.tempo mapa
        posixAtOrigin = fromDateTimeToPosix (origin tp.tempo)
        eval = secsFromOriginAtEval tp
        ws = secsFromOriginAtWS tp
        we = secsFromOriginAtWE tp
        defVoiceInSecs = durInSecs 1.0 (R.toNumber (tp.tempo.freq * (60%1)))
        metricAtEval = R.toNumber $ timeToCount tp.tempo tp.eval
        cTo = calculateCToMetric metricAtEval cTo' (R.toNumber (tp.tempo.freq * (60%1))) rhythmic
        cFrom = calculateCFrom eval cFrom' tempo rhythmic
        dur = durFromRhythmic rhythmic tempo
        onsetPercent = fromFoldable $ rhythmicToOnsets rhythmic -- Array Onsets --- Position in Percentage
        onsets = onsetsFromBlocks blocks onsetPercent dur posixAtOrigin-- Array Onset --- absolute position        
        indexes = getIndexes rhythmic (ws - dur) we x1 dur -- Array Index
        x1 = calculateStartConvergent defVoiceInSecs cTo dur cFrom 
        x2 = x1 + dur
        blocks = getBlocks (ws - dur) we x1 dur -- here you can extract the Int for index
        events = zipWith Event onsets indexes
        looped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) < we) events
        unlooped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) <= we) $ eventsInWindowUnlooped events

calculateTemporal mapa tp aKey (Temporal (Converge convergedKey cTo' cFrom' t) rhythmic loop) = if loop then looped else unlooped
  where tempo = processTempoMark t tp.tempo mapa
        posixAtOrigin = fromDateTimeToPosix (origin tp.tempo)
        eval = secsFromOriginAtEval tp
        ws = secsFromOriginAtWS tp
        we = secsFromOriginAtWE tp

        
        convergedValue = fromMaybe (Temporal (Kairos 0.0 (BPM 120.0)) O false) $ M.lookup convergedKey mapa
        rhythmicConverged = (\(Temporal _ rhy _) -> rhy) convergedValue
        tConverged = (\(Temporal p _ _) -> processTempoMark (getTempoMark p) tp.tempo mapa) convergedValue
        -- work here!!! september 10th

        -- evalAndConverged = funca mapa convergedValue tp

-- this cTo does not consider eval time, explore that...
        cTo = calculateCTo eval cTo' tConverged rhythmicConverged
        cFrom = calculateCFrom eval cFrom' tempo rhythmic
        dur = durFromRhythmic rhythmic tempo
        (Tuple x1 x2) = convergeFunc mapa tp.tempo convergedKey eval dur cTo cFrom
        blocks = getBlocks (ws - dur) we x1 dur 
        onsets = onsetsFromBlocks blocks (fromFoldable $ rhythmicToOnsets rhythmic) dur posixAtOrigin-- Array Onset --- absolute position
        indexes = getIndexes rhythmic (ws - dur) we x1 dur -- Array Index
        events = zipWith Event onsets indexes
        looped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) <= we) events
        unlooped = addPosixOriginToCalculation posixAtOrigin $ filter (\e -> (posFromEvent e) >= ws && (posFromEvent e) <= we) $ eventsInWindowUnlooped events


-- tackle convergence before deadline!!!!
-- major consistency problem: if converged voice is Kairos, how snap or mod work? Possible sollutions: a) break consistency and snap at 0 always or (b) give Kairos a convergeFrom value and just start building from that value
-- convergedVoice:: Map String Temporal -> Temporal -> TimePacket -> Tuple Number Number
-- convergedVoice mapa (Temporal (Kairos off tm) rhy _) tp = Tuple x1 x2
--   where eval = secsFromOriginAtEval tp
--         xTempo = tp.tempo
--         tempo = processTempoMark tm xTempo mapa
--         x1 = eval + off 
--         x2 = x1Converged + (durFromRhythmic rhy tempo)

-- convergedVoice mapa (Temporal (Metric cTo cFrom tm) rhy _) tp = 
--   where metricAtEval = R.toNumber $ timeToCount tp.tempo tp.eval
--         cTo = calculateCToMetric metricAtEval cTo' (R.toNumber (tp.tempo.freq * (60%1))) rhythmic
--         cFrom = calculateCFrom eval cFrom' tempo rhythmic

-- findX1AndX2ForConverge mapa tempo' eval (Temporal (Metric cTo' cFrom' tm) rhy _) recursive = recursBack x1Converged x2Converged recursive
--   where tempo = processTempoMark tm tempo' mapa
--         cTo = calculateCTo eval cTo' 120.0 O
--         cFrom = calculateCFrom eval cFrom' tempo rhy 
--         dur = durFromRhythmic rhy tempo
--         x1Converged = calculateStartConvergent defVoiceInSecs' cTo dur cFrom
--         x2Converged = x1Converged + dur

addPosixOriginToCalculation:: Number -> Array Event -> Array Event
addPosixOriginToCalculation posix es = map (\(Event (Onset bool pos) i) -> Event (Onset bool (pos + posix)) i) es

posFromEvent:: Event -> Number
posFromEvent (Event (Onset _ p) _) = p

--
processTempoMark:: TempoMark -> Tempo -> M.Map String Temporal -> Number 
processTempoMark (BPM bpm) _ _ = bpm
processTempoMark (BPM' bpm n d) _ _ = bpm * ((toNumber n) / (toNumber d))
processTempoMark (CPS cps) _ _ = cps * 60.0
processTempoMark XTempo t _ = R.toNumber (t.freq * (toRat 60.0))
processTempoMark (Prop id x y) t mapa = fromMaybe 120.0 otherTempo
  where prop = (toNumber x / toNumber y)
        otherTempo =  (\(Temporal p _ _) -> calculateRTempo mapa t (getTempoMark p) prop) <$> M.lookup id mapa

getTempoMark:: Polytemporal -> TempoMark
getTempoMark (Kairos _ tm) = tm
getTempoMark (Metric _ _ tm) = tm
getTempoMark (Converge _ _ _ tm) = tm

calculateRTempo:: M.Map String Temporal -> Tempo -> TempoMark -> Number -> Number 
calculateRTempo m t (BPM bpm) prop = bpm * prop
calculateRTempo m t (BPM' bpm n d) prop = (bpm * ((toNumber n) / (toNumber d))) * prop
calculateRTempo m t (CPS cps) prop = cps * 60.0 * prop
calculateRTempo m t XTempo prop = (R.toNumber (t.freq * (toRat 60.0))) * prop
calculateRTempo m t (Prop id x y) prop = calculateRTempo m t newTM newProp
  where newProp = (toNumber x / toNumber y) * prop
        newTM = fromMaybe (BPM 120.0) $ (\(Temporal p _ _) -> (getTempoMark p)) <$> M.lookup id m
--

getBlocks:: Number -> Number -> Number -> Number -> Array Number
getBlocks xws we x1 dur = 
  let nOfFstBlock = firstBlock xws x1 dur  -- :: Int
      nOfLstBlock = lastBlock we x1 dur  -- Maybe Int
      nOfBlocks = case nOfLstBlock of 
                    Nothing -> [] --(nOfFstBlock..(nOfFstBlock + 1))
                    (Just n) -> (nOfFstBlock..n) -- [Int]
  in (blockToPos nOfBlocks x1 dur)

blockToPos:: Array Int -> Number -> Number -> Array Number
blockToPos is x1 dur = map (\i -> x1 + ((toNumber i) * dur)) is

firstBlock:: Number -> Number -> Number -> Int
firstBlock _ _ 0.0 = 0
firstBlock xws x1 dur = nOfNxBlock
  where ws' = (xws - x1)/dur 
        nOfNxBlock 
          | ws' < 0.0 = 0
          | otherwise = ceil ws' 

lastBlock:: Number -> Number -> Number -> Maybe Int
lastBlock we x1 dur = nOfLastBlock
  where we' = (we - x1)/dur
        nOfLastBlock 
          -- | x1 + (we'*dur) >= toNumber (floor we') = Nothing
          | x1 + (we'*dur) <= x1 = Nothing 
          | otherwise = Just $ floor we'

onsetsFromBlocks:: Array Number -> Array Onset -> Number -> Number -> Array Onset 
onsetsFromBlocks blocks onsets dur posixO = concat $ map (\block -> onsetsFromBlock onsets block dur posixO) blocks

onsetsFromBlock:: Array Onset -> Number -> Number -> Number -> Array Onset
onsetsFromBlock onsets block dur posixO = map (\(Onset bool pos) -> Onset bool (block + (pos*dur))) onsets 

eventsInWindowUnlooped:: Array Event -> Array Event
eventsInWindowUnlooped es = map unEither $ filter isRight $ map (\e -> eventInWindowUnlooped e) es
  where 
        unEither:: Either String Event -> Event
        unEither (Left x) = Event (Onset false 0.0) (Index 0 [0] 0)
        unEither (Right x) = x

eventInWindowUnlooped:: Event -> Either String Event
eventInWindowUnlooped (Event onset (Index v st e))
  | v == 0 = Right $ Event onset (Index v st e)
  | otherwise = Left "nothing"


---
------ NOTE on September 10: I just refactored TempoMark and there is a chance that the convergence functions are broken
convergeFunc:: M.Map String Temporal -> Tempo -> String -> Number -> Number -> Number -> Number -> Tuple Number Number
convergeFunc mapa tempo convergedKey eval dur cTo cFrom = findX1AndX2ForConverge mapa tempo eval convergedValue (Cons (Triplet dur cTo cFrom) Nil)
  where convergedValue = fromMaybe (Temporal (Kairos 0.0 (BPM 120.0)) O false) $ M.lookup convergedKey mapa

findX1AndX2ForConverge:: M.Map String Temporal -> Tempo -> Number -> Temporal -> List Triplet -> Tuple Number Number
findX1AndX2ForConverge mapa tempo' eval (Temporal (Kairos asap tm) rhy _) recursive = recursBack x1Converged x2Converged recursive
  where tempo = processTempoMark tm tempo' mapa
        x1Converged = eval + asap -- always the start of the program
        x2Converged = x1Converged + (durFromRhythmic rhy tempo)

findX1AndX2ForConverge mapa tempo' eval (Temporal (Metric cTo' cFrom' tm) rhy _) recursive = recursBack x1Converged x2Converged recursive
  where tempo = processTempoMark tm tempo' mapa
        cTo = calculateCTo eval cTo' 120.0 O
        cFrom = calculateCFrom eval cFrom' tempo rhy 
        dur = durFromRhythmic rhy tempo
        x1Converged = calculateStartConvergent defVoiceInSecs' cTo dur cFrom
        x2Converged = x1Converged + dur

findX1AndX2ForConverge mapa tempo' eval (Temporal (Converge convergedKey cTo' cFrom' tm) rhy _) xs = findX1AndX2ForConverge mapa tempo' eval convergedRecursive xs'
  where tempo = processTempoMark tm tempo' mapa
        convergedValue = fromMaybe (Temporal (Kairos 0.0 (BPM 120.0)) O false) $ M.lookup convergedKey mapa
        rhyConverged = (\(Temporal _ rhy _) -> rhy) convergedValue
        tConverged = (\(Temporal p _ _) -> processTempoMark (getTempoMark p) tempo' mapa) convergedValue
        cTo = calculateCTo eval cTo' tConverged rhyConverged
        cFrom = calculateCFrom eval cFrom' tempo rhy
        convergedRecursive = fromMaybe (Temporal (Kairos 0.0 (BPM 120.0)) O false) $ M.lookup convergedKey mapa
        dur = durFromRhythmic rhy tempo
        xs' = Cons (Triplet dur cTo cFrom) xs

recursBack:: Number -> Number -> List Triplet -> Tuple Number Number
recursBack x1Converged x2Converged (Nil) = Tuple x1Converged x2Converged 
recursBack x1Converged x2Converged (Cons x xs) = 
  let convergedTo = (x2Converged - x1Converged) * (snd3 x)
      convergingFrom = (fst3 x) * (thrd x) 
      convergencePoint = x1Converged + convergedTo
      convergingX1 = convergencePoint - convergingFrom
      convergingX2 = convergingX1 + (fst3 x)
  in recursBack convergingX1 convergingX2 xs

calculateStartConvergent:: Number -> Number -> Number -> Number -> Number
calculateStartConvergent durSecsConverged convergeTo durSecsVoice convergeFrom = startOfVoice
  where cTo = convergeTo * durSecsConverged
        cFrom = convergeFrom * durSecsVoice
        startOfVoice = cTo - cFrom


defVoiceInSecs' = durInSecs 1.0 120.0 -- this should be taken from estuary's tempo

findBeats:: Number -> Number -> Number -> Number -> Array (Tuple Number Number)
findBeats ws we dur x1 = removeRemanent we $ addElapsing ws $ addX2 dur $ findBeats' dur x1 ws we

addX2:: Number -> Array Number -> Array (Tuple Number Number)
addX2 dur xs = map (\x -> Tuple x (x + dur)) xs

-- fromMaybe (Tuple 0.0 0.0) $ A.head [Tuple 0.0 0.1, Tuple 0.5 0.6]
removeRemanent:: Number -> Array (Tuple Number Number) -> Array (Tuple Number Number)
removeRemanent we xs = if snd lasty > we then reverse $ (Tuple (fst lasty) we) : (reverse $ fromMaybe ([Tuple 0.0 0.0]) $ init xs) else xs
  where lasty = fromMaybe (Tuple 0.0 0.0) $ last xs

addElapsing:: Number -> Array (Tuple Number Number) -> Array (Tuple Number Number)
addElapsing ws xs = ((\x -> Tuple ws $ fst x) heady) : xs
  where heady = fromMaybe (Tuple 0.0 0.0) $ head xs

findBeats':: Number -> Number -> Number -> Number -> Array Number
findBeats' dur x1 ws we
    | nextBeat dur x1 ws >= we = fromFoldable []
    | otherwise = nextBeat dur x1 ws : findBeats' dur x1 (ws+dur) we

nextBeat:: Number -> Number -> Number -> Number
nextBeat metre offset ws
    | metre == 0.0 = 0.0
    | otherwise =
        let wsInMetre = ws/metre
            offsetInMetre = decimalPart $ offset/metre
            nextBeatInMetre | offsetInMetre >= (decimalPart wsInMetre) = (toNumber $ floor wsInMetre)  + offsetInMetre
                            | otherwise = (toNumber $ ceil wsInMetre) + offsetInMetre
        in nextBeatInMetre * metre

decimalPart:: Number -> Number
decimalPart x = x - (wholePart x)

wholePart:: Number -> Number 
wholePart x = toNumber $ floor x

lineToPercentageInSecs:: Number -> Number -> Number
lineToPercentageInSecs moment dur = moment / dur


aligner:: Number -> Number -> CPAlign -> Number
aligner eval voiceDur  Origin = 0.0
aligner eval voiceDur Snap = (toNumber $ ceil (eval/voiceDur))
aligner eval voiceDur (Mod n) = ceiledModInMetre * (toNumber n)
  where evalInMetre = eval/voiceDur
        modInMetre = evalInMetre / (toNumber n)
        ceiledModInMetre = toNumber $ ceil modInMetre

-- test'' = calculateOnsets (M.singleton "v" (Temporal (Metric (PercenTo 0 Snap) (Percen 25) 120.0) (Rhythmics (L.fromFoldable [X])) true)) 2666.5 2677.5 2666.0 (Tuple (toRat (2666.0/2.0)) (toRat 0.5))

calculateCToMetric:: Number -> ConvergeTo -> Number -> Rhythmic -> Number
calculateCToMetric metricAtEval cTo tempo rhythmic = percentPos+(aligner' metricAtEval $ a cTo)
  where a:: ConvergeTo -> CPAlign
        a (StructureTo v st a') = a'
        a (ProcessTo e a') = a' 
        a (PercenTo p a') = a'
        dur = durFromRhythmic rhythmic tempo
        onsetPercent = fromFoldable $ rhythmicToOnsets rhythmic -- Array Onsets --- Position in Percentage
        lenOnset = length onsetPercent
        structIndexes = rhythmicStructIndex rhythmic [0] -- Array (Array Int)
        eventIndexesPerVoice = (0..(lenOnset-1)) -- Array Int
        structAndPos = zip structIndexes $ map (\(Onset b p) -> p) onsetPercent
        eventsAndPos = zip eventIndexesPerVoice $ map (\(Onset b p) -> p) onsetPercent
        percentPos = filterEventToPosTo cTo structAndPos eventsAndPos lenOnset


calculateCTo:: Number -> ConvergeTo -> Number -> Rhythmic -> Number
calculateCTo eval cp t rhythmic = percentPos
  where dur = durFromRhythmic rhythmic t
        onsetPercent = fromFoldable $ rhythmicToOnsets rhythmic -- Array Onsets --- Position in Percentage
        lenOnset = length onsetPercent
        structIndexes = rhythmicStructIndex rhythmic [0] -- Array (Array Int)
        eventIndexesPerVoice = (0..(lenOnset-1)) -- Array Int
        structAndPos = zip structIndexes $ map (\(Onset b p) -> p) onsetPercent
        eventsAndPos = zip eventIndexesPerVoice $ map (\(Onset b p) -> p) onsetPercent
        percentPos = filterEventToPosFrom (f' cp) structAndPos eventsAndPos lenOnset 

f':: ConvergeTo -> ConvergeFrom
f' (StructureTo x y z) = Structure x y
f' (ProcessTo x y) = Process x
f' (PercenTo x y) = Percen x 

aligner':: Number -> CPAlign -> Number
aligner' metricAtEval Origin = 0.0 
aligner' metricAtEval Snap = toNumber $ ceil metricAtEval 
aligner' metricAtEval (Mod m) = ceiledModInMetre * (toNumber m)
  where modInMetre = metricAtEval / (toNumber m)
        ceiledModInMetre = toNumber $ ceil modInMetre
-- data CPAlign = Mod Int | Snap | Origin  -- this is the first stage

-- cp operations:
calculateCFrom:: Number -> ConvergeFrom -> Number -> Rhythmic -> Number
calculateCFrom eval cp t rhythmic = percentPos
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





durFromRhythmic:: Rhythmic -> Number -> Number
durFromRhythmic X tempo = durInSecs 1.0 tempo 
durFromRhythmic O tempo = durInSecs 1.0 tempo
durFromRhythmic (Sd rhy) tempo = durInSecs 1.0 tempo
durFromRhythmic (Repeat rhy n) tempo = (durFromRhythmic rhy tempo) * (toNumber n) 
durFromRhythmic (Rhythmics xs) tempo = sum $ map (\x -> durFromRhythmic x tempo) xs

rhythmicToVoiceDuration:: Rhythmic -> Number -- does not need Tempo...?
rhythmicToVoiceDuration X = 1.0
rhythmicToVoiceDuration O = 1.0
rhythmicToVoiceDuration (Sd xs) = 1.0
rhythmicToVoiceDuration (Repeat xs n) = foldl (+) 0.0 x
    where x = replicate n $ rhythmicToVoiceDuration xs
rhythmicToVoiceDuration (Rhythmics xs) = foldl (+) 0.0 x
    where x = map (\x -> rhythmicToVoiceDuration x) xs

rhythmicToOnsets:: Rhythmic -> List Onset
rhythmicToOnsets rhy = 
    let voiceDur = rhythmicToVoiceDuration rhy
        rhythmicSegments = onsetDurations 1.0 rhy
        durInPercentOfEvents = Cons 0.0 $ (fromMaybe (L.fromFoldable []) $ L.init $ scanl (+) 0.0 $ map (\x -> x/voiceDur) $ getDur <$> rhythmicSegments) -- List Number
    in L.zipWith (\x y -> Onset x y) (getBool <$> rhythmicSegments) durInPercentOfEvents -- we need to keep the XO -- THIS gives percentage position within voice, 

onsetDurations:: Number -> Rhythmic -> List Onset
onsetDurations dur X =  L.fromFoldable [Onset true dur]
onsetDurations dur O =  L.fromFoldable [Onset false dur]
onsetDurations dur (Sd xs) = onsetDurations' dur xs
onsetDurations dur (Repeat xs n) = L.concat $ map (\x -> onsetDurations dur x) $ L.fromFoldable $ replicate n xs
onsetDurations dur (Rhythmics xs) = L.concat $ map (\x-> onsetDurations dur x) xs

onsetDurations':: Number -> Rhythmic -> List Onset
onsetDurations' dur X = L.fromFoldable [Onset true dur]
onsetDurations' dur O = L.fromFoldable [Onset false dur]
onsetDurations' dur (Sd xs) = onsetDurations' dur xs
onsetDurations' dur (Repeat xs n) = L.concat $ map (\x -> onsetDurations' newDur x) $ L.fromFoldable $ replicate n xs
    where newDur = dur / (toNumber n)
onsetDurations' dur (Rhythmics xs) = L.concat $ map (\x-> onsetDurations' newDur x) xs
    where newDur = dur / (toNumber $ L.length xs)

getDur:: Onset -> Number
getDur (Onset _ x) = x

getBool:: Onset -> Boolean 
getBool (Onset x _) = x

getIndexes:: Rhythmic -> Number -> Number -> Number -> Number -> Array Index
getIndexes rhythmic xws we x1 dur = 
  let lenOnset = L.length $ rhythmicToOnsets rhythmic
      voiceIndexes = getVoiceIndex xws we x1 dur
      structIndexes = rhythmicStructIndex rhythmic [0]
      eventIndexesPerVoice = (0..(lenOnset-1)) 
      eventIndexes = funquilla voiceIndexes eventIndexesPerVoice lenOnset -- Array (Array Int)
  in assambleIndex voiceIndexes structIndexes eventIndexes

assambleIndex:: Array Int -> Array (Array Int) -> Array (Array Int) -> Array Index
assambleIndex vs st es = concat $ zipWith f vs xs 
  where xs = map (\e -> zip st e) es
        f:: Int -> Array (Tuple (Array Int) Int) -> Array Index
        f v xs = map (\x -> Index v (fst x) (snd x)) xs

funquilla:: Array Int -> Array Int -> Int -> Array (Array Int) 
funquilla voicesIndexes onsetIndexes lenOnsets = map (\voiceIndex -> funquilla' onsetIndexes lenOnsets voiceIndex) voicesIndexes
  where funquilla' onsetIndexes lenOnsets voiceIndex = map (\onsetIndex -> (voiceIndex*lenOnsets)+onsetIndex) onsetIndexes

--- !!!! the Repeat constructor is broken.... no clue how to fix it
rhythmicStructIndex:: Rhythmic -> Array Int -> Array (Array Int)
rhythmicStructIndex X i = [i] 
rhythmicStructIndex O i = [i]
rhythmicStructIndex (Rhythmics xs) i = concat $ map (\(Tuple x i') -> rhythmicStructIndex x [i']) zipped
  where zipped = zip (fromFoldable xs) (0..((L.length xs)-1))
-- rhythmicStructIndex (Repeat rhy n) i = 
rhythmicStructIndex (Sd rhy) i = rhythmicStructIndex' rhy i
rhythmicStructIndex _ _ = [[2666]]

rhythmicStructIndex' X i = [i]
rhythmicStructIndex' O i = [i]
rhythmicStructIndex' (Rhythmics xs) i = concat $ map (\(Tuple x i') -> rhythmicStructIndex' x (snoc i i')) zipped
  where zipped = zip (fromFoldable xs) (0..((L.length xs)-1))
rhythmicStructIndex' (Sd rhy) i = rhythmicStructIndex' rhy i

rhythmicStructIndex' _ i = [snoc i 2666]

getVoiceIndex:: Number -> Number -> Number -> Number -> Array Int -- Index for Voice 
getVoiceIndex xws we x1 dur = 
  let nOfFstBlock = firstBlock xws x1 dur  -- :: Int
      nOfLstBlock = lastBlock we x1 dur  -- Maybe Int
      nOfBlocks = case nOfLstBlock of 
                    Nothing -> []
                    (Just n) -> (nOfFstBlock..n) -- [Int]
  in nOfBlocks

----- 
durInSecs:: Number -> Number -> Number
durInSecs dur tempo = dur * (bpmToDur tempo)

bpmToFreq bpm = (1.0/60.0)* bpm

freqToDur freq = 1.0 / freq

bpmToDur bpm = 1.0 / bpmToFreq bpm


countInFreqToSecs:: Rational -> Rational -> Rational
countInFreqToSecs freq x = x / freq

-- {freq: Rational, origin: x, }
rToNumber = R.toNumber

toRat:: Number -> Rational
toRat x = 
    let pFact = 1000000
        floored = floor x -- 12
        fract = x - (toNumber floored) -- 12.5 - 12.0 = 0.5
        fract' = round $ fract * (toNumber pFact) -- 500000
    in (floored%1) + (fract'%pFact) -- 12 + (500000%1000000)



-- origin time operations:

secsFromOriginAtWS:: TimePacket -> Number
secsFromOriginAtWS tp = ws - oPosix
    where oPosix = fromDateTimeToPosix tp.origin
          ws = fromDateTimeToPosix tp.ws

secsFromOriginAtWE:: TimePacket -> Number
secsFromOriginAtWE tp = we - oPosix
    where oPosix = fromDateTimeToPosix tp.origin
          we = fromDateTimeToPosix tp.we

secsFromOriginAtEval:: TimePacket -> Number
secsFromOriginAtEval tp = eval - oPosix
    where oPosix = fromDateTimeToPosix tp.origin
          eval = fromDateTimeToPosix tp.eval

metricFromOriginAtWS:: TimePacket -> Number  -- is this needed anyway?
metricFromOriginAtWS tp = originSecsAtWS / voiceDur
    where originSecsAtWS = secsFromOriginAtWS tp -- :: Number
          vTempo = R.toNumber $ tp.tempo.freq * (60%1) -- hz to bpm
          voiceDur = durInSecs 1.0 vTempo

metricFromOriginAtWE:: TimePacket -> Number
metricFromOriginAtWE tp = originSecsAtWE / voiceDur
    where originSecsAtWE = secsFromOriginAtWE tp -- :: Number
          vTempo = R.toNumber $ tp.tempo.freq * (60%1) -- htz to bpm
          voiceDur = durInSecs 1.0 vTempo

metricFromOriginAtEval:: TimePacket -> Number -- equivalent (in theory) to timeToCount ???
metricFromOriginAtEval tp = originSecsAtEval / voiceDur
    where originSecsAtEval = secsFromOriginAtEval tp -- :: Number
          vTempo = R.toNumber $ tp.tempo.freq * (60%1) -- htz to bpm :: Number
          voiceDur = durInSecs 1.0 vTempo

voiceFromOriginToEval:: TimePacket -> Number -> Number -> Number
voiceFromOriginToEval tp vTempo vUnits = originSecsAtEval / voiceDur
    where originSecsAtEval = secsFromOriginAtEval tp -- :: Number
          voiceDur = durInSecs vUnits vTempo


fromDateTimeToPosix:: DateTime -> Number
fromDateTimeToPosix x = (unwrap $ unInstant $ fromDateTime x)/1000.0000

fromDateTimeToPosixMaybe:: Maybe DateTime -> Maybe Number
fromDateTimeToPosixMaybe (Just x) = Just $ (unwrap $ unInstant $ fromDateTime x)/1000.0000
fromDateTimeToPosixMaybe Nothing = Nothing

---


  ---- testing stuff ---------------
makeDate :: Int -> Month -> Int -> Date
makeDate y m d = 
    unsafePartial $ fromJust $ 
       canonicalDate <$> toEnum y <@> m <*> toEnum d

makeTime :: Int -> Int -> Int -> Int -> Time
makeTime h min sec milisec = 
    unsafePartial $ fromJust $ Time <$> toEnum h <*> toEnum min <*> toEnum sec <*> toEnum milisec

voice:: Number
voice = durInSecs 6.0 120.0

t:: Tempo
t = {freq: (2%1),time: (DateTime (makeDate 2022 June 3) (makeTime 19 11 0 0)), count: fromInt 0 }

wP:: Number -> DateTime
wP sm = (DateTime (makeDate 2022 June 3) (makeTime 19 11 (0 + secs) (0 + mili)))
    where secs = floor sm
          mili = round ((sm - (toNumber (floor sm))) * 1000.0)

eval:: DateTime
eval = (DateTime (makeDate 2022 June 3) (makeTime 19 14 59 0))

o:: DateTime
o = origin t

oPosix:: Number
oPosix = fromDateTimeToPosix o

voiceFromOrigin:: Number -> Number -> Number -> Number
voiceFromOrigin units tempo sm = (fromDateTimeToPosix (wP sm) - oPosix) / (durInSecs units tempo)

-- fromDateTimeToPosix:: DateTime -> Number
-- fromDateTimeToPosix x = (unwrap $ unInstant $ fromDateTime x)/1000.0000

-- fromDateTimeToPosixMaybe:: Maybe DateTime -> Maybe Number
-- fromDateTimeToPosixMaybe (Just x) = Just $ (unwrap $ unInstant $ fromDateTime x)/1000.0000
-- fromDateTimeToPosixMaybe Nothing = Nothing