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


-- ---- testing stuff ---------------
-- makeDate :: Int -> Month -> Int -> Date
-- makeDate y m d = 
--     unsafePartial $ fromJust $ 
--        canonicalDate <$> toEnum y <@> m <*> toEnum d

-- makeTime :: Int -> Int -> Int -> Int -> Time
-- makeTime h min sec milisec = 
--     unsafePartial $ fromJust $ Time <$> toEnum h <*> toEnum min <*> toEnum sec <*> toEnum milisec


-- t:: Tempo
-- t = {freq: (1%1),time: (DateTime (makeDate 2022 June 3) (makeTime 19 11 25 100)), count: fromInt 0 }

-- ws:: Int -> Int -> DateTime
-- ws x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

-- we:: Int -> Int -> DateTime
-- we x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

-- eval:: DateTime
-- eval = (DateTime (makeDate 2022 June 3) (makeTime 19 13 5 150))

-- oDur:: Rational  -- transposition value 2
-- oDur = (1%2)

-- o:: List Number
-- o = fromFoldable [0.0,0.2,0.5] -- at this level a metric unit should be added. For testing: 0,0.1 (metre 1), 0.2,0.3 (metre 2), 0.4,0.5 (metre 3), 0.6,0.7 (metre 4), etc...

-- -------------

-- onsetPosition:: List Number -> Rational -> Tempo -> DateTime -> DateTime -> DateTime -> M.Map Number Int -- change to MMap Instant Int
-- onsetPosition o oDur t ws we eval = 
--     let countAtStart = timeToCount t ws --rational
--         patternAtStart = toNumber (countAtStart/oDur)
--         percentAtStart = patternAtStart - (I.toNumber $ I.floor $ patternAtStart)

--         countAtEnd = timeToCount t we
--         patternAtEnd = toNumber (countAtEnd/oDur)
--         percentAtEnd = patternAtEnd - (I.toNumber $ I.floor $ patternAtEnd)

--         multiOnsets = (I.floor $ patternAtEnd) - (I.floor $ patternAtStart)

--         filtrado = filterEvents multiOnsets percentAtStart percentAtEnd o (Tuple patternAtStart patternAtEnd)
--       in filtrado 

-- --data Indices = Indices Int Int -- index of pattern and index of event

-- -- NewType wrap and unwrap

-- fromPositionToTimeStamp:: Number -> Tempo -> (Tuple Rational Rational) -> Instant
-- fromPositionToTimeStamp oDur posicion tempo =
--     let dur = oDur / (tempo.freq)
--     in countToTime $ (countToStart + dur)


-- filterEvents:: Int ->  Number -> Number -> List Number -> Tuple Number Number -> M.Map Number Int -- posicion e indiceEvento falta pattern
-- filterEvents mo start end o (Tuple patternStart patternEnd)
--     | mo == 0 = M.fromFoldable $ zip x $ getIndexSimple start end o
--         where x = filter (\x -> (x >= start) && (x < end)) o
--               phrase = I.floor start
--     | mo == 1 =
--         let firstList = filter (\x -> (x >= start) && (x < 1.0)) o
--             indexFst = getIndexOfFirstList firstList o
--             lastList = filter (\x -> (x >= 0.0) && (x < end)) o
--             indexLast = getIndexOfLastList lastList o
--             listOfEvents = concat $ fromFoldable [firstList, lastList]
--             listOfIndexes = concat $ fromFoldable [indexFst,indexLast]
--         in M.fromFoldable $ zip listOfEvents listOfIndexes
--     | otherwise =
--         let firstList = filter (\x -> (x >= start) && (x < 1.0)) o
--             fstIndex = getIndexOfFirstList firstList o
--             middleList = take (I.floor ((I.toNumber (length o))*((I.toNumber mo) - 1.0))) $ cycle o
--             middleIndex = getIndexOfMiddleList (length middleList) o 
--             lastList = filter (\x -> (x >= 0.0) && (x < end)) o
--             lastIndex = getIndexOfLastList lastList o
--             listOfEvents = concat $ fromFoldable [firstList, middleList, lastList]
--             listOfIndexes = concat $ fromFoldable [fstIndex, middleIndex, lastIndex]
--         in M.fromFoldable $ zip listOfEvents listOfIndexes

-- getIndexSimple:: Number -> Number -> List Number -> List Int
-- getIndexSimple start end o =   
--     let before = length $ filter (\x -> x < start) o
--         between = filter (\x -> (x > start)&&(end>=x)) o
--     in if before == 0 then (0..(length between)) else (before..(length between))

-- getIndexOfFirstList:: List Number -> List Number -> List Int
-- getIndexOfFirstList x o 
--     | x == (fromFoldable []) = fromFoldable []
--     | otherwise = range ((length o) - (length x)) ((length o) - 1)

-- getIndexOfMiddleList:: Int -> List Number -> List Int
-- getIndexOfMiddleList middleLen o = take middleLen $ cycle x
--     where x = fromFoldable (0..(length o))

-- getIndexOfLastList:: List Number -> List Number -> List Int
-- getIndexOfLastList x o 
--     | x == (fromFoldable []) = fromFoldable []
--     | otherwise = range 0 ((length x)-1)



-- -- fromCoordinateToInstant:: Coordinates -> Instant
-- -- fromCoordinateToInstant (Coord i e) = 
-- --     let num = I.toNumber i

-- --fromDateTime


-- type Event =
--   { 
--   s :: String, -- name of sample bank (ie. old-style with sampleMap)
--   n :: Int, -- number of sample within a bank (ie. old-style with sampleMap)
--   whenPosix :: Number -- when to play the sample, in POSIX/epoch-1970 time
-- --   when :: Number, -- when to play the sample, in audio context time
-- --   gain :: Number, -- clamped from 0 to 2; 1 is default and full-scale
-- --   overgain :: Number, -- additional gain added to gain to go past clamp at 2
-- --   pan :: Number
--   }


-- -- coordinatesToEvents:: Coordinates -> Events




-- --- create the option to use eval instead of origin time. This will require diff or something similar.
-- --- use the non fractional of the number (the floored onset) to give an index to each new onset event
-- --- create the index within the phrase!!
-- --- so: first index is a general index of the onset in the general onset scheme
-- --- the second onset is the onset within the musical idea



-- --- ojo aqui::::
-- --Coordinada = Posicion IndiceFrase IndiceMetro IndiceEvento
