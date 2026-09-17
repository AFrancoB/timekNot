module TemporalSpecs (calculateTemporal, recursiveChain, startingMomentConvergingVoice, findReferencedX1'', chainConvergences, processTempoMark, keysForReferencePath) where

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
import Data.List as L

import Data.Tempo

import AST
import Acceleration
import TestOpsAndDefs
import DurationAndIndex
import TimePacketOps

import Data.Rational (Rational(..), (%), fromInt)
import Data.Rational (toNumber) as R -- still need to convert all Number calcs into Rational!!

{-
MOD is not transmited through the recursive structure properly. This needs to be checked better in a real program. With a mod that is non-zero

this module is a nightmare, I need to work on this so it is less of a recursive hell

-}


-- caluclateTemporal should be in a Monad that combines State with Effect: EvalTime = StateT s Effect a

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

calculateTemporal mapa tp aKey (Temporal (Converge cKey cTo' cFrom' tm) rhythmic loop) = do 
  -- log ("key of calculateTemporal " <> aKey)
  -- log ("the key of the voice converging " <> aKey)
  -- log ("the key of the converged voice " <> cKey)
  let dur = establishDur tm tp.tempo mapa rhythmic
  -- log ("dur of rhythmic block of converging voice " <> show dur)
  let lengthRhythm = (length $ fromFoldable $ rhythmicToOnsets rhythmic)-1
  -- log ("length of rhythmic block converging (index of last event in rhythmic block) " <> show lengthRhythm) 
  let lengthRhythmTo = (length $ fromFoldable $ rhythmicToOnsets $ getRhythmicFromMap mapa cKey)-1
  -- log ("length of rhythmic block converged (index of last event in rhythmic block) " <> show lengthRhythmTo) 
  let simCTo = simplifyCTo lengthRhythmTo cTo'   
  let simCFrom = simplifyCFrom lengthRhythm cFrom'

  x1 <- startingMomentConvergingVoice tp tm cKey simCTo simCFrom rhythmic mapa  
  -- log ("starting point of converging voice " <> show x1) 




  let posixAtOrigin = fromDateTimeToPosix (origin tp.tempo)
  let ws = secsFromOriginAtWS tp
  let we = secsFromOriginAtWE tp
  let blocks = getBlocks (ws - dur) we x1 dur -- to check
  -- log ("blocksConvergeTemporal: " <> show blocks)
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
  let cp = secsFromOriginAtVantage tp vKey  ----- this needs a MODDDDDDDDD&&&!!U!Y!Y!YUY@U@Y@UY@YU@#TYT#TY#R#R#
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
startingMomentConvergingVoice:: TimePacket -> TempoMark -> String -> ConvergeTo -> ConvergeFrom -> Rhythmic -> M.Map String Temporal -> Effect Number 
startingMomentConvergingVoice tp tm cKey cTo' cFrom' rhythmic mapa = do  
{-
This function calculates the TOP voice in the example D
-}
  -- log ("startingMomentConvergedVoice " <> cKey)
  let refTemporal = fromMaybe defTemporal $ M.lookup cKey mapa
  -- log ("converged from converging temporal (showing the next level of convergence) " <> show refTemporal)
  let refRhythmic = getRhythmic refTemporal 
  let refDur = establishDur (tempoMark refTemporal) tp.tempo mapa refRhythmic
  
  
  refX1 <- findReferencedX1 tp cKey refTemporal mapa  





  -- log ("converged voice starting point: " <> show refX1)
  refVoiceAtEval <- elapsedVoiceAtEval tp refX1 refDur -- not secs but cycles
  -- log ("converged voice at evaluation time (in rhythmic blocks): " <> show refVoiceAtEval)
  -- log ("refVoiceAtEval top " <> show refVoiceAtEval)
  -- log ("key recur 2: " <> cKey)
  let innerPos = innerPosCTo refRhythmic cTo'
  -- log ("innerPos startingMomentConvergedVoice " <> show innerPos)
  let cTo = calculateCToNEW innerPos refVoiceAtEval cTo'
  -- log ("calculated CTo in rhythmic blocks " <> show cTo)
  let cFrom = calculateCFrom cFrom' rhythmic
  -- log ("calculated cFrom in rhythmic blocks " <> show cFrom)
  let dur = establishDur tm tp.tempo mapa rhythmic
  let x1 = calculateStartConvergent refDur cTo dur cFrom  
  -- log ("starting point of converging voice in seconds " <> show x1)
  -- log ("x1 converge voice top " <> show (refX1 + x1))
  -- log ("x1 " <> show (refX1 + x1))
  pure (refX1 + x1)    

-- findReferencedX1:: TimePacket -> Temporal -> M.Map String Temporal -> Effect Number

--   ---- calculate NOVUS!!!!!!!!!!!!!!!!!!!!!!!!
-- findReferencedX1 tp (Temporal (Novus vKey cFrom tm) rhy l) mapa = pure 0.0

-- findReferencedX1 tp (Temporal (Kairos asap tm) rhy _) mapa = do
--   let eval = secsFromOriginAtEval tp
--   let x1 = eval + asap
--   -- log ("x1 kairos voice " <> show x1)
--   pure x1

-- findReferencedX1 tp (Temporal (Metric cTo cFrom tm) rhy _) mapa = do
--   x1 <- x1MetricVoice tp tm cTo cFrom rhy mapa 
--   -- log ("x1 metric voice " <> show x1)
--   pure x1            -- v1            --v0


-- findReferencedX1 tp (Temporal (Converge cKey cTo cFrom tm) rhy l) mapa = do
--   -- log ("converged from converged " <> cKey)
--   let way = keysForReferencePath cKey mapa (Nil) -- Array String
--   -- log ("key " <> show cKey) 
--   -- log ("way: " <> show way)    
--   recursiveX1 <- recursiveRefX1 tp (Temporal (Converge cKey cTo cFrom tm) rhy l) mapa Nothing way -- (Just $ Tuple "a-0" 2.765) way -- v1's x1
--   -- log ("starting point of converged voice: " <> show recursiveX1)   
--   -- log ("converges to " <> cKey)   
--   pure recursiveX1



---------- THIS CALCULATES THE SECOND TOP X1
findReferencedX1:: TimePacket -> String -> Temporal -> M.Map String Temporal -> Effect Number
findReferencedX1 tp key (Temporal (Novus vKey cFrom tm) rhy l) mapa = pure 0.0
findReferencedX1 tp key (Temporal (Kairos asap tm) rhy _) mapa = do
  let eval = secsFromOriginAtEval tp
  let x1 = eval + asap
  pure x1
findReferencedX1 tp key (Temporal (Metric cTo cFrom tm) rhy _) mapa = do
  -- log ("findReferencedX1Metric " <> show key)
  x1 <- x1MetricVoice tp tm cTo cFrom rhy mapa 
  pure x1      
findReferencedX1 tp key (Temporal (Converge cKey cTo cFrom tm) rhy l) mapa = do  
  -- log ("findReferencedX1Converge " <> show key)
  let way = keysForReferencePath key mapa Nil 
  -- log ("way: " <> show way)
  result <- chainConvergences tp mapa key way
  pure result

  -- c <- case way of
  --         Nil -> do
  --           -- log ("this key is Nil in findReferencedX1 (this is temporal to find) " <> show key)
  --           let lastTemporal = fromMaybe defTemporal $ M.lookup cKey mapa 
  --           let lastDur = establishDur (tempoMark lastTemporal) tp.tempo mapa $ getRhythmic lastTemporal
  --           -- log ("here to find the reference of B which is a, a metric voice: " <> show cKey)
  --           lastX1 <- funcionQueArrojaX1deKairosdOMetric tp lastTemporal mapa  --:: TimePacket -> Temporal -> M.Map String Temporal -> Effect Number

  --           let temporalToFind = fromMaybe defTemporal $ M.lookup key mapa -- e
  --           let durToFind = establishDur (tempoMark temporalToFind) tp.tempo mapa $ getRhythmic temporalToFind
  --           let (Tuple cTo' cFrom') = convergences mapa temporalToFind
  --           let cFrom = calculateCFrom cFrom' $ getRhythmic temporalToFind
  --           prevVoiceAtEval <- elapsedVoiceAtEval tp lastX1 lastDur
  --           let innerPos = innerPosCTo (getRhythmic lastTemporal) cTo'
  --           let cTo = calculateCToNEW innerPos prevVoiceAtEval cTo'
  --           -- log ("lastDur " <> (show lastDur) <> " cTo " <> (show cTo) <> " durToFind " <> (show durToFind) <> " cFrom " <> show cFrom)
  --           let result = calculateStartConvergent lastDur cTo durToFind cFrom

  --           -- log ("calculation of a" <> show r)
  --           pure result

  --         ------- the new bug clearly starts here:
  --         _ -> do  --receives from above: tp key( c ) way (C: Temporal (Converge cKey cTo cFrom tm) rhy l) mapa, also: way
  --           log ("way: " <> show way)
  --           log ("key: " <> key <> " temporal: " <> show (Temporal (Converge cKey cTo cFrom tm) rhy l))
  --           -- C this stays at this level as temporalToFind 
  --           -- [a -> b] the recursiveChain should yield the X1 of B

  --           -- think on how to accumulate the X1 in the recursive thingie
  --           pure 2.666


-- a 60cpm | x :|

-- a.s = "muted" .shur = 7;

-- b[0] >< a[3>>] 60cpm | [oxx] ||

-- b.s = "muted";

-- c[0] >< b[3] 60cpm | [oxx] ||

-- c.s = "muted" .shur = 1;

-- d[0] >< c[3] 60cpm | [oxx] ||

-- d.s = "muted" .shur = 2;

-- -- e[0] >< d[3] 60cpm | [oxx] ||

-- -- e.s = "muted" .shur = 3;

-- -- f[0] >< e[3] 60cpm | [oxx] ||

-- -- f.s = "muted" .shur = 4;







            -- log ("this key is NOT NIL in findReferencedX1 (this is temporal to find) " <> show key)
            -- let lastWay = fromMaybe "" $ L.last way   
            -- log ("lastWay " <> show lastWay)                            -- A
            -- let lastTemporal = fromMaybe defTemporal $ M.lookup lastWay mapa
            -- let lastDur = establishDur (tempoMark lastTemporal) tp.tempo mapa $ getRhythmic lastTemporal
            -- recursiveX1 <- chainConvergences tp mapa cKey way
            -- -- log ("recursiveX1 " <> show recursiveX1)
            -- let temporalToFind = fromMaybe defTemporal $ M.lookup key mapa -- B
            -- let durToFind = establishDur (tempoMark temporalToFind) tp.tempo mapa $ getRhythmic temporalToFind
            -- let (Tuple cTo' cFrom') = convergences mapa temporalToFind
            -- let cFrom = calculateCFrom cFrom' $ getRhythmic temporalToFind

            -- prevVoiceAtEval <- elapsedVoiceAtEval tp recursiveX1 lastDur
            -- let innerPos = innerPosCTo (getRhythmic lastTemporal) cTo'
            -- let cTo = calculateCToNEW innerPos prevVoiceAtEval cTo'
            -- -- log ("lastDur " <> (show lastDur) <> " cTo " <> (show cTo) <> " durToFind " <> (show durToFind) <> " cFrom " <> show cFrom)
            -- let result = calculateStartConvergent lastDur cTo durToFind cFrom
            -- -- log ("result: " <> show (recursiveX1 + result))
            -- pure (recursiveX1 + result)

-- test with Key C, cKey B Way: [a: Nil]
--- starting point (X1) of e-0 in the test example
chainConvergences:: TimePacket -> M.Map String Temporal -> String -> List String -> Effect Number 
chainConvergences tp mapa cKey Nil = do
{- 
Th purpose of this function, when receiving a Nil rather than a list, is to assume that cKey represents the bottom value that is metric or kairos. In the example of the chain: D -> C -> B -> A this function (pattern-matched to Nil) would not be used because receiving a Cons sends it to the recursiveChain function where the bottom is computed. This pattern-matched function is used with C -> B -> A; where C stays at the TOP, B is computed at findReferencedX1, and Nil pattern is matched and gets A
-}
  -- log ("Key at chain convergences Nil " <> cKey)
  let temporalMetricOrKairos = fromMaybe defTemporal $ M.lookup cKey mapa
  -- funcionQueArrojaX1deKairosdOMetric:: TimePacket -> Temporal -> M.Map String Temporal -> Effect Number
  x1 <- funcionQueArrojaX1deKairosdOMetric tp temporalMetricOrKairos mapa 
  pure x1

chainConvergences tp mapa cKey (Cons bottom upwardsWay) = do
{-
This receives chains of more than three elements: D -> C -> B -> A (at least), where this function receives C [A -> B -> Nil]. In this function the head of Cons is computed as the bottom (A) and its output (X1, dur and rhythmic of the bottom) is passed to recursiveChain.
-}
  -- log ("key at chain of convergence Cons " <> cKey)
  -- log ("bottom " <> bottom)
  let temporalBottom = fromMaybe defTemporal $ M.lookup bottom mapa
  x1Bottom <- funcionQueArrojaX1deKairosdOMetric tp temporalBottom mapa
  -- log ("x1 of bottom " <> show x1Bottom)
  let durBottom = establishDur (tempoMark temporalBottom) tp.tempo mapa $ getRhythmic temporalBottom
  recurX1 <- recursiveChain tp cKey mapa upwardsWay {dur: durBottom, x1: x1Bottom, rhythmic: getRhythmic temporalBottom}
  -- log ("this is what needs to be modified b >< a: " <> show recurX1)
  pure (recurX1) -- broken on purpose!!!!!!!

-- test with Key C, cKey B Way: [a: Nil]
--- starting point (X1) of e-0 in the test example

-------
recursiveChain:: TimePacket -> String -> M.Map String Temporal -> List String -> {dur:: Number, x1:: Number, rhythmic:: Rhythmic}  ->  Effect Number
recursiveChain tp cKey mapa Nil prev = do
{-
This function calculates the last in the way: in the example B. It should have received the 
-}
  -- log ("secsFromOriginAtEval " <> show (secsFromOriginAtEval tp))
  let temporalToFind = fromMaybe defTemporal $ M.lookup cKey mapa
  let durToFind = establishDur (tempoMark temporalToFind) tp.tempo mapa $ getRhythmic temporalToFind
  let (Tuple cTo' cFrom') = convergences mapa temporalToFind
  let cFrom = calculateCFrom cFrom' $ getRhythmic temporalToFind
  -- log ("cFrom " <> show cFrom)
  prevVoiceAtEval <- elapsedVoiceAtEval tp prev.x1 prev.dur
  let innerPos = innerPosCTo prev.rhythmic cTo'
  let cTo = calculateCToNEW innerPos prevVoiceAtEval cTo'
  -- log ("cTo " <> show cTo)
  -- log ("prevDur " <> show prev.dur)
  -- log ("durToFind " <> show durToFind)\
  let seekedX1 = calculateStartConvergent prev.dur cTo durToFind cFrom
  -- log ("seekedX1 at recusriveChainTEST Nil " <> show (seekedX1 + prev.x1))
  pure $ ((seekedX1) + prev.x1)
--                    c              b           Nil        a
recursiveChain tp k mapa (Cons findThisNow upwardsWay) prev = do
  -- log ("key in Cons " <> show k)
  -- log ("findThisNow in Cons " <> show findThisNow)
  -- log ("prev in Cons " <> show prev)
  -- log ("upwardsway in Cons " <> show upwardsWay)
  let temporalToFind = fromMaybe defTemporal $ M.lookup findThisNow mapa
  let durToFind = establishDur (tempoMark temporalToFind) tp.tempo mapa $ getRhythmic temporalToFind
  let (Tuple cTo' cFrom') = convergences mapa temporalToFind
  let cFrom = calculateCFrom cFrom' $ getRhythmic temporalToFind
  prevVoiceAtEval <- elapsedVoiceAtEval tp prev.x1 prev.dur
  let innerPos = innerPosCTo prev.rhythmic cTo'
  let cTo = calculateCToNEW innerPos prevVoiceAtEval cTo'
  let seekedX1 = calculateStartConvergent prev.dur cTo durToFind cFrom
  -- log ("seeked " <> show seekedX1)
  -- log ("prev x1 " <> show prev.x1)
  -- here I have to do some calculation between prevX1 and seekedX1 that is passed over to the next recursion level
  r <- recursiveChain tp k mapa upwardsWay {dur: durToFind, x1: ((seekedX1) + prev.x1), rhythmic: getRhythmic temporalToFind}
  pure r  -- ????? THIS WILL BREAK

{-
--- simple test for long chain of convergences:
a 60cpm | x :|

b[0] >< a[3>>] 60cpm | [ox] ||

c[0] >< b[2] 120cpm  | [ox] ||

d[0] >< c[2] 120cpm  | [ox] ||

e[0] >< d[2] 120cpm  | [ox] ||

-- 1>> 2>> 3>>
-- X   X   X   X   X   X    A 60cpm   
--             O X 2 3 4    B 60  -> 120cpm 
--                 OX234    C 120 -> 240cpm
--                   OX2    D 120 -> 240cpm
--                     OX   E 120 -> 240cpm

a.s = "muted";

b.s = "muted" .shur = 1;

c.s = "muted" .shur = 2;

d.s = "muted" .shur = 3;

e.s = "muted" .shur = 4;

-- more elaborated test: (substitute the temporal expressions above for these)
-- a 60cpm | x :|

-- b[0] >< a[3>>] 60cpm | [oxx] ||

-- c[0] >< b[3] 120cpm | [oxxo] ||

-- d[0] >< c[4] 120cpm | [(3,5)] ||

-- e[0] >< d[5] 60cpm | [xx]x ||

-}

funcionQueArrojaX1deKairosdOMetric:: TimePacket -> Temporal -> M.Map String Temporal -> Effect Number
funcionQueArrojaX1deKairosdOMetric tp (Temporal (Kairos asap tm) rhy _) mapa = do
  let eval = secsFromOriginAtEval tp
  let x1 = eval + asap
  pure x1
funcionQueArrojaX1deKairosdOMetric tp (Temporal (Metric cTo cFrom tm) rhy _) mapa = do
  x1 <- x1MetricVoice tp tm cTo cFrom rhy mapa 
  pure x1
funcionQueArrojaX1deKairosdOMetric tp _ mapa = pure 2666.0 -- here the pathway for Novus needs to be implemented


convergences:: M.Map String Temporal -> Temporal -> Tuple ConvergeTo ConvergeFrom
convergences _ (Temporal p _ _) = getConvergences p

getConvergences:: Polytemporal -> Tuple ConvergeTo ConvergeFrom
getConvergences (Converge _ cTo cFrom _) = Tuple cTo cFrom
getConvergences _ = Tuple defConvergeTo defConvergeFrom

keysForReferencePath:: String -> M.Map String Temporal -> List String -> List String -- this function seems solid!!
keysForReferencePath aKey mapa listOfReferences 
  | isNotConvergent aKey mapa = (Nil)
  | otherwise =  
      if (isNotConvergent nextCheck mapa)
        then (Cons nextCheck listOfReferences)
          else keysForReferencePath nextCheck mapa (Cons nextCheck listOfReferences)
    where nextCheck = getKey mapa $ fromMaybe defTemporal $ M.lookup aKey mapa 

getKey _ (Temporal (Converge aKey _ _ _) _ _) = aKey
getKey _ (Temporal _ _ _) = "2666"

isNotConvergent aKey mapa = f' mapa $ fromMaybe defTemporalMetric $ M.lookup aKey mapa 

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


---- convergence point calculator

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
aligner cyclesAtEval SnapAfter = (toNumber $ ceil cyclesAtEval) 
aligner cyclesAtEval SnapBefore = (toNumber $ floor cyclesAtEval)
aligner cyclesAtEval (Mod m) = ceiledModInMetre * (toNumber m)
  where modInMetre = cyclesAtEval / (toNumber m)
        ceiledModInMetre = toNumber $ ceil modInMetre

innerPosCTo:: Rhythmic -> ConvergeTo -> Number -- Where within the block you can find the Convergence
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


-- transforms all tempo marks into one tempo frequency: Cycles per minute. So, the freq of any singular onset/offset (X/O). If tempo is 60CPM then the output of this function is 60.0. This output (freq) is the input for establishDur and the output of establishDur is a block's duration in seconds.
processTempoMark:: TempoMark -> Tempo -> M.Map String Temporal -> Number 
processTempoMark (TL tl) _ _ = R.toNumber (tl / (4%1))
processTempoMark (CPM cpm) _ _ = R.toNumber cpm
processTempoMark (CPS cps) _ _ = R.toNumber (cps * (60%1))
processTempoMark (BPM bpm figure) _ _ = R.toNumber ((bpm / (4%1)) / figure)
processTempoMark XTempo t _ = (R.toNumber (t.freq * (60%1) * (4%1)))
processTempoMark (Prop id x y) t mapa = fromMaybe 120.0 otherTempo
  where prop = (toNumber x / toNumber y)
        otherTempo = (\temporal -> calculateRTempo mapa t (tempoMark temporal) prop) <$> M.lookup id mapa
processTempoMark other t mapa = 0.0 


-- This function calculates the freq for tempi derived from other tempi
calculateRTempo:: M.Map String Temporal -> Tempo -> TempoMark -> Number -> Number 
calculateRTempo m t (TL tl) prop = (R.toNumber (tl / (4%1))) * prop
calculateRTempo m t (CPM cpm) prop = (R.toNumber cpm) * prop
calculateRTempo m t (BPM bpm figure) prop = (R.toNumber ((bpm / (4%1)) / figure)) * prop
calculateRTempo m t (CPS cps) prop = R.toNumber (cps * (60%1)) * prop
calculateRTempo m t XTempo prop = (R.toNumber (t.freq * (60%1) * (4%1))) * prop
calculateRTempo m t (Prop id x y) prop = calculateRTempo m t newTM newProp
  where newProp = (toNumber x / toNumber y) * prop
        newTM = fromMaybe (CPM (fromInt 120)) $ (\temporal -> tempoMark temporal) <$> M.lookup id m
calculateRTempo m t other prop = 0.0