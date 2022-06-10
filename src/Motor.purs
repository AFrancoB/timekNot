module Motor where

import Prelude
import Prim.Boolean

import Data.Either
import Data.Identity
import Data.Array as Arr
import Data.List.Lazy
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

import Data.DateTime
import Data.DateTime.Instant
import Data.Tempo
import Data.Enum
import Data.Map as M
import Partial.Unsafe


---- testing stuff ---------------
makeDate :: Int -> Month -> Int -> Date
makeDate y m d = 
    unsafePartial $ fromJust $ 
       canonicalDate <$> toEnum y <@> m <*> toEnum d

makeTime :: Int -> Int -> Int -> Int -> Time
makeTime h min sec milisec = 
    unsafePartial $ fromJust $ Time <$> toEnum h <*> toEnum min <*> toEnum sec <*> toEnum milisec


t:: Tempo
t = {freq: (1%1),time: (DateTime (makeDate 2022 June 3) (makeTime 19 11 25 100)), count: fromInt 0 }

ws:: Int -> Int -> DateTime
ws x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

we:: Int -> Int -> DateTime
we x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

eval:: DateTime
eval = (DateTime (makeDate 2022 June 3) (makeTime 19 13 5 150))

oDur:: Rational  -- transposition value 2
oDur = (1%2)

o:: List Number
o = fromFoldable [0.0,0.2,0.5] -- at this level a metric unit should be added. For testing: 0,0.1 (metre 1), 0.2,0.3 (metre 2), 0.4,0.5 (metre 3), 0.6,0.7 (metre 4), etc...

-------------

onsetPosition:: List Number -> Rational -> Tempo -> DateTime -> DateTime -> DateTime -> List (Tuple Number Int) -- change to MMap Instant Int
onsetPosition o oDur t ws we eval = 
    let countAtStart = timeToCount t ws --rational
        passageAtStart = toNumber (countAtStart/oDur)
        percentAtStart = passageAtStart - (iToN $ floor $ passageAtStart)

        countAtEnd = timeToCount t we
        passageAtEnd = toNumber (countAtEnd/oDur)
        percentAtEnd = passageAtEnd - (iToN $ floor $ passageAtEnd)

        multiOnsets = (floor $ passageAtEnd) - (floor $ passageAtStart)

        filtrado = filterEvents multiOnsets percentAtStart percentAtEnd passageAtStart o
      in filtrado 

--data Indices = Indices Int Int -- index of pattern and index of event

-- NewType wrap and unwrap
-- the first rational actually needs to be the pattern index, make mock tests for this
countToStart:: Int
countToStart = 327
-- estos ya fueron filtrados!!!

-- fromPositionToTimeStamp:: Int -> Number -> Number -> Tempo -> Instant
-- fromPositionToTimeStamp countToStart oDur posicion tempo =
--     let dur = (oDur / (tempo.freq)) * posicion
--     in countToTime tempo $ ((I.toNumber countToStart) + dur)


filterEvents:: Int ->  Number -> Number -> Number -> List Number -> List (Tuple Number Int) -- posicion e indiceEvento falta pattern
filterEvents mo start end passageAtStart o 
    | mo == 0 = zip x $ getIndexSimple start end o
        where x = map (_ + (iToN $ floor passageAtStart)) $ filter (\x -> (x >= start) && (x < end)) o
    | mo == 1 =
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
            middleList = take (floor ((iToN (length o))*((iToN mo) - 1.0))) $ cycle o
            middleIndex = getIndexOfMiddleList (length middleList) o 
            lastList = filter (\x -> (x >= 0.0) && (x < end)) o
            lastIndex = getIndexOfLastList lastList o
            listOfEvents = multiplePatternW (floor passageAtStart) mo firstList o lastList
            listOfIndexes =  concat $ toL [fstIndex,middleIndex,lastIndex]
        in zip listOfEvents listOfIndexes

getIndexSimple:: Number -> Number -> List Number -> List Int
getIndexSimple start end o =   
    let before = length $ filter (\x -> x < start) o
        between = filter (\x -> (x > start)&&(end>=x)) o
    in if before == 0 then (0..(length between)) else (before..(length between))

getIndexOfFirstList:: List Number -> List Number -> List Int
getIndexOfFirstList x o 
    | x == (toL []) = toL []
    | otherwise = range ((length o) - (length x)) ((length o) - 1)

getIndexOfMiddleList:: Int -> List Number -> List Int
getIndexOfMiddleList middleLen o = take middleLen $ cycle x
    where x = toL (0..(length o))

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


-- multiplePatternW mo first middle last = si la primer lista tiene cosas pasa las cosas sin adicion, la lista de enmedio nunca esta vacia pero siempre se le suma un 1 y se acumula cuanto mas mo. La lista ultima si tiene cosas se le suma uno a partir del tail del floor del middle list, si viene vacia no se pasa nada.


toL x = fromFoldable x

iToN x = I.toNumber x

floor x = I.floor x


toRat:: Number -> Rational
toRat x = 
    let 
        pFact = 1000000
        floored = floor x -- 12
        fract = x - (iToN floored) -- 12.5 - 12.0 = 0.5
        fract' = I.round $ fract * (iToN pFact) -- 500000
    in (floored%1) + (fract'%pFact) -- 12 + (500000%1000000)


-- Starting from, for example, the number 4.123: I think one approach might be to take the fractional part of the number (eg. 0.123) and multiply it by some precision factor - say 1000000 - so 0.123 becomes 123000.0 and then convert that to Int and put it over the precision factor to form a Rational - 123000 % 1000000 - then add that to the whole number part as a Rational.


--fromDateTime


type Event =
  { 
  s :: String, -- name of sample bank (ie. old-style with sampleMap)
  n :: Int, -- number of sample within a bank (ie. old-style with sampleMap)
  whenPosix :: Number -- when to play the sample, in POSIX/epoch-1970 time
--   when :: Number, -- when to play the sample, in audio context time
--   gain :: Number, -- clamped from 0 to 2; 1 is default and full-scale
--   overgain :: Number, -- additional gain added to gain to go past clamp at 2
--   pan :: Number
  }


-- coordinatesToEvents:: Coordinates -> Events




--- create the option to use eval instead of origin time. This will require diff or something similar.
--- use the non fractional of the number (the floored onset) to give an index to each new onset event
--- create the index within the phrase!!
--- so: first index is a general index of the onset in the general onset scheme
--- the second onset is the onset within the musical idea



--- ojo aqui::::
--Coordinada = Posicion IndiceFrase IndiceMetro IndiceEvento
