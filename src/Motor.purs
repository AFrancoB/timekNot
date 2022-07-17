module Motor (passageToEvents,evalToCountWrapper) where

import Prelude
import Prim.Boolean

import Data.Either
import Data.Identity
import Data.Array as Arr
import Data.List.Lazy hiding (Pattern(..))
import Data.Typelevel.Bool
import Data.Int as I
import Data.Tuple
import Data.Tuple.Nested

import Data.Functor

import Data.Maybe hiding (optional)

import Control.Monad

import Effect (Effect)
import Effect.Console (log)

import Data.Rational
import Data.Ratio

import Effect (Effect)
import Effect.Now (nowDateTime)

import Data.Newtype

import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration
import Data.Tempo
import Data.Enum
import Data.Map as M
import Partial.Unsafe

import AST
import Rhythmic
import Aural

-----

passageToEvents:: Passage -> Tempo -> DateTime -> DateTime -> DateTime -> List (Maybe Event)
passageToEvents (Passage rhy aus conv) t ws we eval = 
    let coords = fromPassageToCoord rhy t ws we eval conv -- Map Int Coord
        lCoord = snd <$> (M.toUnfoldable coords) -- List Coord, es decir: Nu In In
        samples = sampleWithIndex $ last $ filter isSample $ fromFoldable aus --Maybe Aural
        samplesI = auralIndex $ last $ filter isSample $ fromFoldable aus
        -- aqui va una funcion con tupletes de samples y coords con el mismo indice!!
        s = samplesWithPosix samplesI (lenRhyth rhy) samples lCoord
 --       n = last $ filter isN $ au  -- Maybe Aural 
    in map toEvent s

lenRhyth:: Rhythmic -> Int
lenRhyth (Onsets x) = length $ filter (\x -> x==true) $ fromFoldable x
lenRhyth _ = 0 -- esto se debera hacer pronto en el futuro

toEvent:: Maybe (Tuple Number String) -> Maybe Event
toEvent (Just (Tuple posix sample)) = Just {whenPosix: posix, s: sample, n: 0}
toEvent Nothing = Nothing

auralIndex:: Maybe Aural -> Index
auralIndex (Just (Sample _ i)) = i
auralIndex (Just (N _ i)) = i
auralIndex Nothing = EventI -- this feels very wrong...

isSample:: Aural -> Boolean
isSample (Sample _ _) = true
isSample _ = false

isN:: Aural -> Boolean
isN (N _ _) = true
isN _ = false

-- samplesWithCoordinates:: List (Tuple String Int) -> List Coordenada -> List Event
samplesWithPosix:: Index -> Int -> List (Tuple String Int) -> List Coordenada -> List (Maybe (Tuple Number String))
samplesWithPosix index len samples coords = map (eventForSample index len samples) coords

eventForSample:: Index -> Int -> List (Tuple String Int) -> Coordenada -> Maybe (Tuple Number String)
eventForSample EventI len samples (Coord posix p e) = attachPosixWithSample posix $ head $ filter (\s -> (mod (getEventIndex p len e) (length samples)) == (snd s)) samples
eventForSample PassageI len samples (Coord posix p e) = attachPosixWithSample posix $ head $ filter (\s -> (mod p len) == (snd s)) samples
eventForSample MetreI len samples (Coord posix p e) = attachPosixWithSample posix $ head $ fromFoldable []

-- make a test for this stupid ass function
getEventIndex:: Int -> Int -> Int -> Int
getEventIndex p' len' e' = (I.round $ ((p*len) + e))
            where p = I.toNumber p'
                  len = I.toNumber len'
                  e = I.toNumber e'

attachPosixWithSample:: Number -> Maybe (Tuple String Int) -> Maybe (Tuple Number String)
attachPosixWithSample x (Just (Tuple st int)) = Just $ Tuple x st
attachPosixWithSample x Nothing = Nothing

sampleWithIndex:: Maybe Aural -> List (Tuple String Int)
sampleWithIndex (Just (Sample au' i)) = zip au (0..(length au))
        where au = fromFoldable au'
sampleWithIndex _ = fromFoldable []
sampleWithIndex Nothing = fromFoldable []

fromPassageToCoord:: Rhythmic -> Tempo -> DateTime -> DateTime -> DateTime -> Convergence -> M.Map Int Coordenada
fromPassageToCoord rhy t ws we eval convergence = 
    let x = fromRhythmicToList rhy
        passageLength = fromInt $ length x   -- oDur
        onsets = (fromInt <<< snd) <$> (filter (\x -> fst x == true) $ zip x (0..(length x)))
        oPercen = map (toNumber <<< (_/passageLength)) onsets
    in passagePosition oPercen passageLength t ws we eval convergence

fromRhythmicToList:: Rhythmic -> List Boolean
fromRhythmicToList (Onsets x) = fromFoldable x
fromRhythmicToList (Patron x) = concat $ map fromPatternToList $ fromFoldable x
fromRhythmicToList _ = fromFoldable [false]

fromPatternToList:: Rhythmic -> List Boolean
fromPatternToList (Onsets x) = fromFoldable x 
fromPatternToList _ = fromFoldable [false] -- placeholder

passagePosition:: List Number -> Rational -> Tempo -> DateTime -> DateTime -> DateTime -> Convergence -> M.Map Int Coordenada -- change to MMap Instant Int
passagePosition o lenPasaje t ws we eval convergence = 
    let countAtStart = evalToCountWrapper convergence t eval ws -- $ timeToCountNumber t ws --Number
        passageAtStart =  countAtStart/ (toNumber lenPasaje)
        percentAtStart = passageAtStart - (iToN $ floor $ passageAtStart)

        countAtEnd = evalToCountWrapper convergence t eval we -- $ timeToCountNumber t we
        passageAtEnd = countAtEnd/ (toNumber lenPasaje)
        percentAtEnd = passageAtEnd - (iToN $ floor $ passageAtEnd)

        nPassages = (floor $ passageAtEnd) - (floor $ passageAtStart)

        filtrado = filterEvents nPassages percentAtStart percentAtEnd passageAtStart o
        posToTime = map (\x -> positionToTime t lenPasaje x) filtrado
      in M.fromFoldableWithIndex posToTime

evalToCountWrapper:: Convergence -> Tempo -> DateTime -> DateTime -> Number
evalToCountWrapper Origin t eval tp = timeToCountNumber t tp
evalToCountWrapper Eval t eval tp = evalToCountNumber t eval tp
evalToCountWrapper (Prospective _ _) _ _ tp = timeToCountNumber t tp


evalToCountNumber :: Tempo -> DateTime -> DateTime -> Number
evalToCountNumber t eval tp = 
    let x = unwrap (diff tp eval :: Milliseconds)
    in (x * toNumber t.freq) / 1000.0

-- crear instancias de coordenada y en la funcion de abajo debe de ser:
-- :: Tempo -> Rational -> Tuple Number Int -> Coord TimeStamp IPassage IEvent
positionToTime:: Tempo -> Rational -> Tuple Number Int -> Coordenada
positionToTime t lenPasaje (Tuple pos iEvent) = 
    let posInTempo = (toRat pos) * lenPasaje 
        countInTime = countToTime t posInTempo
    in Coord ((unwrap $ unInstant $ fromDateTime countInTime)/1000.0000) (floor pos) iEvent
    

filterEvents:: Int ->  Number -> Number -> Number -> List Number -> List (Tuple Number Int) -- posicion e indiceEvento falta pattern
filterEvents nPassages start end passageAtStart o 
    | nPassages == 0 = zip x $ getIndexSimple start end o
        where x = map (_ + (iToN $ floor passageAtStart)) $ filter (\x -> (x >= start) && (x < end)) o
    | nPassages == 1 =
        let firstList = filter (\x -> (x >= start) && (x < 1.0)) o
            indexFst = getIndexOfFirstList firstList o
            lastList = filter (\x -> (x >= 0.0) && (x < end)) o
            indexLast = getIndexOfLastList lastList o
            listOfEvents = twoPatternW (floor passageAtStart) firstList lastList
            listOfIndexes = concat $ toL [indexFst,indexLast]
        in zip listOfEvents listOfIndexes
    | otherwise =
        let firstList = filter (\x -> (x >= start) && (x < 1.0)) o
            fstIndex = getIndexOfFirstList firstList o
            middleList = take (floor ((iToN (length o))*((iToN nPassages) - 1.0))) $ cycle o
            middleIndex = getIndexOfMiddleList (length middleList) o 
            lastList = filter (\x -> (x >= 0.0) && (x < end)) o
            lastIndex = getIndexOfLastList lastList o
            listOfEvents = multiplePatternW (floor passageAtStart) nPassages firstList o lastList
            listOfIndexes =  concat $ toL [fstIndex,middleIndex,lastIndex]
        in zip listOfEvents listOfIndexes

getIndexSimple:: Number -> Number -> List Number -> List Int
getIndexSimple start end o =   
    let before = (length $ filter (\x -> x < start) o) -1
        between = filter (\x -> (x > start)&&(end>=x)) o
    in if before == 0 then (0..((length between)-1)) else (before..((length between)-1))

getIndexOfFirstList:: List Number -> List Number -> List Int
getIndexOfFirstList x o 
    | x == (toL []) = toL []
    | otherwise = range ((length o) - (length x)) ((length o) - 1)

getIndexOfMiddleList:: Int -> List Number -> List Int
getIndexOfMiddleList middleLen o = take middleLen $ cycle x
    where x = toL (0..((length o)-1))

getIndexOfLastList:: List Number -> List Number -> List Int
getIndexOfLastList x o 
    | x == (toL []) = toL []
    | otherwise = range 0 ((length x)-1)

-- trabajar en esta funcion!!!!!!!!
twoPatternW:: Int -> List Number -> List Number -> List Number
twoPatternW indexPhrase first last' = map (_ + iToN indexPhrase) $ concat $ toL [first,last] 
    where last = if last' == (toL []) then (toL []) else map (_ + 1.0) $ toL last'

multiplePatternW:: Int -> Int -> List Number -> List Number -> List Number -> List Number
multiplePatternW indexAtPhrase mo first o last' = 
    let lenO = length o
        o' = concat $ replicate (mo-1) o
        middle = map (\x -> (fst x) + (snd x)) $ zip o' (iToN <$> (concat $ toL $ map (\x -> replicate lenO x) $ toL (1..(mo-1))))
        last = if last' == (toL []) then (toL []) else map (_ + (iToN mo)) $ last' 
    in map (_ + iToN indexAtPhrase) $ concat $ toL [first,middle,last] 


toL x = fromFoldable x

iToN x = I.toNumber x

floor x = I.floor x


toRat:: Number -> Rational
toRat x = 
    let pFact = 1000000
        floored = floor x -- 12
        fract = x - (iToN floored) -- 12.5 - 12.0 = 0.5
        fract' = I.round $ fract * (iToN pFact) -- 500000
    in (floored%1) + (fract'%pFact) -- 12 + (500000%1000000)


justFractional:: Number -> Number
justFractional x = x - (iToN $ floor x)


---- testing stuff ---------------
makeDate :: Int -> Month -> Int -> Date
makeDate y m d = 
    unsafePartial $ fromJust $ 
       canonicalDate <$> toEnum y <@> m <*> toEnum d

makeTime :: Int -> Int -> Int -> Int -> Time
makeTime h min sec milisec = 
    unsafePartial $ fromJust $ Time <$> toEnum h <*> toEnum min <*> toEnum sec <*> toEnum milisec


t:: Tempo
t = {freq: (2%1),time: (DateTime (makeDate 2022 June 3) (makeTime 19 11 25 100)), count: fromInt 0 }

ws:: Int -> Int -> DateTime
ws x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

we:: Int -> Int -> DateTime
we x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

eval:: DateTime
eval = (DateTime (makeDate 2022 June 3) (makeTime 19 14 59 500))

oDur:: Rational  -- transposition value 2
oDur = (1%2)

o:: List Number
o = fromFoldable [0.0,0.2,0.5] -- at this level a metric unit should be added. For testing: 0,0.1 (metre 1), 0.2,0.3 (metre 2), 0.4,0.5 (metre 3), 0.6,0.7 (metre 4), etc...

countToStart:: Int
countToStart = 327
-------------



-- coordinatesToEvents:: Coordinates -> Events




--- create the option to use eval instead of origin time. This will require diff or something similar.
--- use the non fractional of the number (the floored onset) to give an index to each new onset event
--- create the index within the phrase!!
--- so: first index is a general index of the onset in the general onset scheme
--- the second onset is the onset within the musical idea



--- ojo aqui::::
--Coordinada = Posicion IndiceFrase IndiceMetro IndiceEvento
