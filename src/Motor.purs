module Motor (passagePosition) where

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
import Data.Tempo
import Data.Enum
import Data.Map as M
import Partial.Unsafe

import AST

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
eval = (DateTime (makeDate 2022 June 3) (makeTime 19 13 5 150))

oDur:: Rational  -- transposition value 2
oDur = (1%2)

o:: List Number
o = fromFoldable [0.0,0.2,0.5] -- at this level a metric unit should be added. For testing: 0,0.1 (metre 1), 0.2,0.3 (metre 2), 0.4,0.5 (metre 3), 0.6,0.7 (metre 4), etc...

countToStart:: Int
countToStart = 327
-------------


passagePosition:: List Number -> Rational -> Tempo -> DateTime -> DateTime -> DateTime -> M.Map Int Coordenada -- change to MMap Instant Int
passagePosition o lenPasaje t ws we eval = 
    let countAtStart = timeToCountNumber t ws --rational
        passageAtStart =  countAtStart/ (toNumber lenPasaje)
        percentAtStart = passageAtStart - (iToN $ floor $ passageAtStart)

        countAtEnd = timeToCountNumber t we
        passageAtEnd = countAtEnd/ (toNumber lenPasaje)
        percentAtEnd = passageAtEnd - (iToN $ floor $ passageAtEnd)

        nPassages = (floor $ passageAtEnd) - (floor $ passageAtStart)

        filtrado = filterEvents nPassages percentAtStart percentAtEnd passageAtStart o
        posToTime = map (\x -> positionToTime t lenPasaje x) filtrado
      in M.fromFoldableWithIndex posToTime


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
    let before = length $ filter (\x -> x < start) o
        between = filter (\x -> (x > start)&&(end>=x)) o
    in if before == 0 then (0..(length between)) else (before..(length between))

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





-- coordinatesToEvents:: Coordinates -> Events




--- create the option to use eval instead of origin time. This will require diff or something similar.
--- use the non fractional of the number (the floored onset) to give an index to each new onset event
--- create the index within the phrase!!
--- so: first index is a general index of the onset in the general onset scheme
--- the second onset is the onset within the musical idea



--- ojo aqui::::
--Coordinada = Posicion IndiceFrase IndiceMetro IndiceEvento
