module Motor (eventProcess,programToWaste) where

import Prelude
import Data.Int
import Data.List
import Data.List.Lazy (replicate,cycle) --  import both lists as qualified....
import Data.List.Lazy as Lz
import Data.List.NonEmpty (toList)
import Data.Tuple
import Data.Rational as R
import Data.Rational (Rational,(%),fromInt)
import Data.Maybe
import Data.Bifunctor
-- import Data.String (take)
import Data.String as Str

import Effect (Effect)
import Effect.Now (nowDateTime)

import Data.Newtype

import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration
import Data.Tempo
import Data.Enum
import Partial.Unsafe

import Debug
import QuickTestAndDebug

import AST
import Helpers
import QuickTestAndDebug
-- xx[ox]x 

-- xx[ox 1]x 1| xx[ox 2]x 2| xx[ox 3]x 3| xx[ox 4]x4  
-- 12  3   4    12  3   4    12  3   4    12  3   4

-- 1-2 3   4    56  7   8    9-10 11  12 13-14 15  16

-- 2%4.0~~3%4.2.1 ☭ note "0 3 7"

-- ☭ 🐚 samples "bd cp cp"

-- ☭ 🐚 note "0 3 7"

-- estribillo

programToWaste:: Tempo -> DateTime -> DateTime -> DateTime -> Program -> List Waste
programToWaste t ws we eval (Program rhy finitude aurals) = allAurals process aurals
    where process = eventProcess t ws we rhy 
 
allAurals:: Process -> List Aural -> List Waste 
allAurals process aurals = concat $ map (\x -> auraliseMap process x) aurals

auraliseMap:: Process -> Aural -> List Waste
auraliseMap (Events xs) au = map (\x -> auralise x au) xs
auraliseMap _ au = {s:"", whenPosix: 0.0} : Nil

auralise:: Tuple Number (Tuple Boolean Int) -> Aural -> Waste
auralise (Tuple psx (Tuple bool i)) (S xs seqType) 
    | seqType == ByEvent = {s: s, whenPosix: psx}
        where s = fromMaybe "" $ xs !! (i `mod` (length xs))
    | otherwise = {s: "", whenPosix: 0.0}

auralise (Tuple psx (Tuple bool i)) (N xs seqType) = {s: "", whenPosix: 0.0}

testy t ws we = auraliseMap process $ S ("bd" : "cp" : "808" : Nil) ByEvent
    where process = eventProcess t ws we (Rhythmics (x:x:x:x:Nil))

eventProcess:: Tempo -> DateTime -> DateTime -> Rhythmic -> Process
eventProcess t ws we (Rhythmics xs) = Events $ map (\x -> indexedOnsetToEvent x t) $ eventProcessXS t ws we (Rhythmics xs)
eventProcess t ws we rhy =  
    let xs = refrainWithIntraIndexes t ws we rhy -- list (Onset , Num) 
        firstLocation = fromMaybe (Tuple (Onset false 0.0) 0.0) $ head $ xs
        lastLocation = fromMaybe (Tuple (Onset false 0.0) 0.0) $ last $ xs
        eventsLen = toNumber $ (length $ rhythmicToEventsDuration rhy)
        refrainDur = rhythmicToRefrainDuration rhy
        dbg3 = trace ("refrainTotalDur " <> show refrainDur) \_ -> refrainDur
        dbg2 = trace ("eventsPerRefr " <> show eventsLen) \_ -> eventsLen
        dbg1 = trace ("firstLocation " <> show firstLocation) \_ -> firstLocation
        dbg7 = trace ("lastLocation " <> show lastLocation) \_ -> lastLocation
        elapsedRefrains = fst (lmap (\(Onset bool beat)-> beat) firstLocation) / refrainDur
        dbg4 = trace ("elapsedRefrains " <> show elapsedRefrains) \_ -> elapsedRefrains
        offset = (toNumber $ floor elapsedRefrains) * eventsLen -- <- aqui esta el error
        dbg5 = trace ("offset " <> show offset) \_ -> offset
        is = sort $ concat $ map (manyCycles eventsLen) $ map toList $ groupAll $ map (\x -> offset + x) $ map snd xs
        dbg6 = trace ("indices " <> show is) \_ -> is
    in Events $ map (\x -> indexedOnsetToEvent x t) $ zipWith (\x i -> Tuple (fst x) i) xs is

eventProcessXS:: Tempo -> DateTime -> DateTime -> Rhythmic -> List (Tuple Onset Number)
eventProcessXS t ws we rhy = 
    let xs = refrainWithIntraIndexes t ws we rhy
        firstLocation = fromMaybe (Tuple (Onset false 0.0) 0.0) $ head $ xs
        lastLocation = fromMaybe (Tuple (Onset false 0.0) 0.0) $ last $ xs
        amountOfEvents = (length $ rhythmicToEventsDuration rhy)
        refrainDur = rhythmicToRefrainDuration rhy
        elapsedRefrains = fst (lmap (\(Onset bool beat)-> beat) firstLocation) / refrainDur
        dbg0 = trace ("indices " <> show xs) \_ -> xs
        dbg1 = trace ("firstLocation " <> show firstLocation) \_ -> firstLocation
        dbg2 = trace ("lastLocation " <> show lastLocation) \_ -> lastLocation
        dbg3 = trace ("refrainDur " <> show refrainDur) \_ -> refrainDur
        dbg4 = trace ("elapsedRefrains " <> show elapsedRefrains) \_ -> elapsedRefrains
        firstEvent = floor $ snd firstLocation
        lastEvent = floor $ snd lastLocation
        firstBeat = floor $ (\(Onset bool beat) -> beat/refrainDur) $ fst firstLocation -- this should be refrains not Cycles!!
        lastBeat = floor $ (\(Onset bool beat) -> beat/refrainDur) $ fst lastLocation
        dbg5 = trace ("firstBeat " <> show firstBeat) \_ -> firstBeat
        dbg6 = trace ("lastBeat " <> show lastBeat) \_ -> lastBeat
        offset = (floor elapsedRefrains) * amountOfEvents -- <- aqui esta el error
        i = map (\x -> offset + x) $ amountOfRefrainsPerW firstBeat lastBeat firstEvent lastEvent amountOfEvents
    in zipWith (\x y -> Tuple (fst x) $ toNumber y) xs i

amountOfRefrainsPerW:: Int -> Int -> Int -> Int -> Int -> List Int
amountOfRefrainsPerW firstR lastR firstE lastE lenE
    | firstR == lastR = firstE .. lastE
    | (firstR + 1) == (lastR) = concat $ fromFoldable [firstE..(lenE-1),lastEvents]
        where lastEvents = map (\x -> x + lenE) $ 0..lastE
    | otherwise = concat $ fromFoldable [firstE..(lenE -1),zpd,realLast]
    where xs = fromFoldable $ replicate (lastR - firstR - 1) $ 0..(lenE - 1) 
          scl = scanl (+) 0 $ fromFoldable $ replicate (length xs) lenE
          zpd = concat $ zipWith (\x y -> map (\x -> x + y) x) xs scl
          realLast = map (\x -> x + (floor $ (toNumber lenE) * (toNumber lastR))) $ 0..lastE 


manyCycles:: Number -> List Number -> List Number
manyCycles len grupo = (fromMaybe 0.0 $ head grupo) : (fromMaybe Nil $ init n)
    where n = map (\x -> (fromMaybe 0.0 $ head grupo) + x) $ scanl (+) 0.0 $ fromFoldable (replicate (length grupo) len) 

---
indexedOnsetToEvent:: Tuple Onset Number -> Tempo -> (Tuple Number (Tuple Boolean Int))
indexedOnsetToEvent (Tuple (Onset bool beat) i) t = Tuple posix $ Tuple bool $ floor i 
    where countInTime = countToTime t (toRat beat)
          posix = ((unwrap $ unInstant $ fromDateTime countInTime)/1000.0000)

---

durationOfRefrain:: Rhythmic -> Number
durationOfRefrain X = 1.0
durationOfRefrain O = 1.0
durationOfRefrain (Sd x) = 1.0
durationOfRefrain (Rhythmics xs) =  toNumber $ length xs
durationOfRefrain _ = 2.666
-- durationOfRefrain (Repeat xs n) = (toNumber $ length xs) * (toNumber n)


--- this down here is trash       
    --     eventsLen = toNumber $ (length $ rhythmicToEventsDuration rhy)
    -- in map (f eventsLen) xs

    --     firstEventofRefrain = map (\x -> (\(Onset bool beat) -> (toNumber $ floor beat)*eventsLen) $ fst x) xs
    --     indexesInWindows = zipWith (+) (snd <$> xs) firstEventofRefrain
    -- in zipWith onsetAndIndexEvent xs indexesInWindows

-- 
--  :: Number -> Tuple Onset Number -> Tuple Onset Number
-- f offset (Tuple (Onset bool beat) n) = Tuple (Onset bool beat) 
--   where n' = offset + (toNumber (floor beat) * eventsLen)

onsetAndIndexEvent:: (Tuple Onset Number) -> Number -> Tuple Onset Number    
onsetAndIndexEvent x index = Tuple (fst x) index
-- *** I have added the index of the event but I have lost the intraIndex of the event. Info I have: onset kind, position in beats and the index of the event


-- this tuple is XO and index of event
toEventIndex:: Number -> Number ->  Number
toEventIndex refrainindex intraindex = (refrainindex + intraindex)


-- ((Tuple (X dur->beats:860.0) 0.0) : (Tuple (X dur->beats:860.25) 1.0) : (Tuple (O dur->beats:860.5) 2.0) : (Tuple (X dur->beats:860.75) 3.0) : (Tuple (X dur->beats:861.0) 0.0) : (Tuple (X dur->beats:861.25) 1.0) : (Tuple (O dur->beats:861.5) 2.0) : (Tuple (X dur->beats:861.75) 3.0) : Nil)

-- ** this has the info for onset or offset, beatPosition and intra-index. It has to be transformed into an Events. Which is a list of tuplets where fst is whenPosix and snd is the index in the event process.

-- structuredProcess:: Tempo -> DateTime -> DateTime -> Rhythmic -> Process



-- create a function that: a) takes the total duration of a refrain, takes the position of the events in the refrain
-- do we can have an indexed count of the events

---   *** this funca is broken changed the List Number to List Onset in line 92, lines 95 and 97 need to change: make a funca that processes things inside the Onset data structure.
refrainWithIntraIndexes:: Tempo -> DateTime -> DateTime -> Rhythmic -> List (Tuple Onset Number)
refrainWithIntraIndexes t ws we rhy = 
    let indexAtWS = fst $ indexOfRefrain t ws rhy
        eventsPerRefrain = rhythmicToEventsDuration rhy -- List Onset , which has: Bool Number
        eventsPerRefrainandIndex = zip eventsPerRefrain $ 0.. (length eventsPerRefrain - 1) -- [(Onset,Int)]
        refrainDur = rhythmicToRefrainDuration rhy
        processedWsWe = map (\x -> Tuple (findBeatsWithOnset t ws we refrainDur $ fst x) $ snd x) $ map (\x -> Tuple ((\(Onset bool dur) -> Onset bool (dur*refrainDur)) $ fst x) $ snd x) eventsPerRefrainandIndex 
        processTuple tup = zip (fst tup) $ fromFoldable $ replicate (length $ fst tup) $ snd tup 
        toEventProcess = map (\x -> Tuple ((\(Onset bool beatPos)-> (Onset bool (beatPos))) $ fst x) (toNumber $ snd x)) $ concat $ map (\x -> processTuple x) processedWsWe
    in sort toEventProcess

-- Onset is weird, its number represents two things throughout this function duration first and beat position later
findBeatsWithOnset:: Tempo -> DateTime -> DateTime -> Number -> Onset -> List Onset
findBeatsWithOnset t ws we refrainDur (Onset bool dur) = map (\b -> Onset bool b) beats 
    where beats = findBeats t ws we refrainDur dur -- List Number

-- need to attach an index to the onsets found by the findBeats function
-- recipe: zip eventsPerRefrain with a Int representing the number of event in the refrain, use zip but experiment with zipWith. access each event via map, grap the position intrarefrain and produce the operation needed then get back the findBeats operation with the int as a tuplet.

-- findBeats and nextBeat were adapted from the tempi library. 
-- what is understood as metr here is what I understand as proportion. 1:2:3 where 1 is the tempo 2 is twice as fast, 3 is three times as fast, etc. So, metre is the reciprocal of proportion*****
findBeats:: Tempo -> DateTime -> DateTime -> Number -> Number -> List Number
findBeats t ws' we' metre offset = findBeats' metre offset ws we
    where ws = timeToCountNumber t ws'
          we = timeToCountNumber t we'

findBeats':: Number -> Number -> Number -> Number -> List Number
findBeats' metre offset ws we
    | nextBeat metre offset ws >= we = fromFoldable []
    | otherwise = nextBeat metre offset ws : findBeats' metre offset (ws+metre) we

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

indexOfRefrain:: Tempo -> DateTime -> Rhythmic -> Tuple Number Number
indexOfRefrain t wp rhy = 
    let 
    wpCount = timeToCountNumber t wp -- Number
    refrainDur = rhythmicToRefrainDuration rhy -- it is working
    indexAsNum = wpCount / refrainDur
    in Tuple (toNumber $ floor indexAsNum) (rightOfpoint indexAsNum)

rhythmicToRefrainDuration:: Rhythmic -> Number -- does not need Tempo...?
rhythmicToRefrainDuration X = 1.0
rhythmicToRefrainDuration O = 1.0
rhythmicToRefrainDuration (Sd xs) = 1.0
rhythmicToRefrainDuration (Repeat xs n) = foldl (+) 0.0 x
    where x = replicate n $ rhythmicToRefrainDuration xs
rhythmicToRefrainDuration (Rhythmics xs) = foldl (+) 0.0 x
    where x = map (\x -> rhythmicToRefrainDuration x) xs

-- rhythmicToEventsDuration:: Rhythmic -> List Number
-- rhythmicToEventsDuration rhy = 
--     let refrainDur = rhythmicToRefrainDuration rhy
--         rhythmicSegments = eventsDurations 1.0 rhy
--         durInPercentOfEvents = 0.0 : (fromMaybe (0.0:Nil) $  init $ scanl (+) 0.0 $ map (\x -> x/refrainDur) $ getDur <$> rhythmicSegments) -- List Number
--     in durInPercentOfEvents -- we need to keep the XO

rhythmicToEventsDuration:: Rhythmic -> List Onset
rhythmicToEventsDuration rhy = 
    let refrainDur = rhythmicToRefrainDuration rhy
        rhythmicSegments = eventsDurations 1.0 rhy
        durInPercentOfEvents = 0.0 : (fromMaybe (0.0:Nil) $  init $ scanl (+) 0.0 $ map (\x -> x/refrainDur) $ getDur <$> rhythmicSegments) -- List Number
    in zipWith (\x y -> Onset x y) (getBool <$> rhythmicSegments) durInPercentOfEvents -- we need to keep the XO

-- the refrain has only a total duration and can be indexed. The inner life of the refrain is only described by metres and events

getDur:: Onset -> Number
getDur (Onset _ x) = x

getBool:: Onset -> Boolean 
getBool (Onset x _) = x

eventsDurations:: Number -> Rhythmic -> List Onset
eventsDurations dur X = fromFoldable [Onset true dur]
eventsDurations dur O = fromFoldable [Onset false dur]
eventsDurations dur (Sd xs) = eventsDurations' dur xs
eventsDurations dur (Repeat xs n) = concat $ map (\x -> eventsDurations dur x) $ fromFoldable $ replicate n xs
eventsDurations dur (Rhythmics xs) = concat $ map (\x-> eventsDurations dur x) xs


-- data Refrain = Refrain Int Number MetricStructure EventCount
eventsDurations':: Number -> Rhythmic -> List Onset
eventsDurations' dur X = fromFoldable [Onset true dur]
eventsDurations' dur O = fromFoldable [Onset false dur]
eventsDurations' dur (Sd xs) = eventsDurations' dur xs
eventsDurations' dur (Repeat xs n) = concat $ map (\x -> eventsDurations' newDur x) $ fromFoldable $ replicate n xs
    where newDur = dur / (toNumber n)
eventsDurations' dur (Rhythmics xs) = concat $ map (\x-> eventsDurations' newDur x) xs
    where newDur = dur / (toNumber $ length xs)



