module QuickTestAndDebug (tEval,t,tAncient,ws,we,eval,te,w,evalTi) where

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


makeDate :: Int -> Month -> Int -> Date
makeDate y m d = 
    unsafePartial $ fromJust $ 
       canonicalDate <$> toEnum y <@> m <*> toEnum d

makeTime :: Int -> Int -> Int -> Int -> Time
makeTime h min sec milisec = 
    unsafePartial $ fromJust $ Time <$> toEnum h <*> toEnum min <*> toEnum sec <*> toEnum milisec

tEval:: Tempo 
tEval = {freq: (2%1),time: eval, count: fromInt 0 }

t:: Tempo
t = {freq: (2%1),time: (DateTime (makeDate 2022 June 3) (makeTime 19 11 25 100)), count: fromInt 0 }

tAncient:: Tempo
tAncient = {freq: (2%1),time: (DateTime (makeDate 2012 June 3) (makeTime 19 11 25 100)), count: fromInt 0 }

ws:: Int -> Int -> DateTime
ws x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

we:: Int -> Int -> DateTime
we x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

eval:: DateTime
eval = (DateTime (makeDate 2022 June 3) (makeTime 19 15 0 100))

---
dia = makeDate 2023 May 27
hra = (makeTime 19 43 10 100)

te:: Tempo
te = {freq: (2%1),time: (DateTime dia hra), count: fromInt 0 }

w:: Int -> Int -> DateTime
w x y = (DateTime dia (makeTime 19 43 x y))

evalTi:: Int -> Int -> DateTime
evalTi x y = w x y