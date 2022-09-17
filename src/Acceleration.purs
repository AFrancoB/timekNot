module Acceleration where

import Prelude

import Data.Either
import Data.Identity
import Data.List hiding (many)
import Data.Foldable (foldl)
import Data.Int
import Data.Tuple
import Data.String (singleton, joinWith, take, split, trim, Pattern(..))
import Data.Maybe hiding (optional)
import Data.Functor
import Control.Monad

import Data.Rational (fromInt,(%))
import Data.Rational as R

import Effect (Effect)
import Effect.Console (log)

import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration
import Data.Tempo
import Data.Enum
import Partial.Unsafe

import Data.Newtype


-- acc:: Number -> Number -> Number -> Number
-- acc velF velI tiempo = cambiodeVel / tiempo
--     where cambiodeVel = velF - velI

-- rampWrap:: Acceleration -> Tempo -> DateTime -> DateTime -> DateTime -> Number
-- rampWrap acc t anchor ws we = ramp' acc t countAnchor countWS countWE
--     where countWS = timeToCountNumber t ws
--           countWE = timeToCountNumber t we
--           countAnchor = timeToCountNumber t anchor


-- ramp':: Acceleration -> Tempo -> Number -> Number -> Number -> Number -> Number
-- ramp' (Acc durV startV endV) t startT endT ws we 
--     | startT >= ws = startV
--     | endT <= we = endV
--     | otherwise = 2.666

-- data Acceleration = Acc Number Number Number

-- instance Show Acceleration where
--     show (Acc dur start end) = show dur <> " " <> show start <> " " <> show end


-- ramp':: UTCTime -> UTCTime -> UTCTime -> Rational -> Rational -> Rational
-- ramp' renderTime startTime endTime startVal endVal -- delete what is not needed
--     | startTime >= renderTime = startVal
--     | endTime <= renderTime = endVal
--     | otherwise =    -- args: 3 secs of dur, startval: 0.2, endval: 0.7
--         let segmentVal = endVal - startVal -- 0.5
--             processInterval = realToFrac (diffUTCTime endTime startTime) :: Rational --  3 segs
--             momentAtRender = realToFrac (diffUTCTime renderTime startTime) :: Rational -- assuming render is half way through the process: 1.5 out of 3.0
--             percOfProcessAtRender = getPercentage momentAtRender processInterval segmentVal
--         in startVal + percOfProcessAtRender

--
-- te:: Tempo
-- te = {freq: (2%1),time: (DateTime (makeDate 2022 September 12) (makeTime 22 10 0 0)), count: fromInt 0 }

-- f secs  = timeToCountNumber te (DateTime (makeDate 2022 September 12) (makeTime 22 10 secs 0))

-- testy t x x' = timeToCountNumber newT $ DateTime (makeDate 2022 September 12) (makeTime 22 10 x' 0)
--     where   tc = timeToCount t $ DateTime (makeDate 2022 September 12) (makeTime 22 10 x' 0) 
--             newT = {freq: x,time: (DateTime (makeDate 2022 September 12) (makeTime 22 10 0 0)), count: tc - x }

-- ----
-- startT:: Number
-- startT = (timeToCountNumber t eval) + 1.0

-- endT:: Number
-- endT = (timeToCountNumber t eval) + 3.0

-- startV = 90.0

-- endV = 120.0

-- wsNum:: Int -> Int -> Number
-- wsNum x y = timeToCountNumber t $ ws x y

-- weNum:: Int -> Int -> Number
-- weNum x y = timeToCountNumber t $ we x y

-- makeDate :: Int -> Month -> Int -> Date
-- makeDate y m d = 
--     unsafePartial $ fromJust $ 
--        canonicalDate <$> toEnum y <@> m <*> toEnum d

-- makeTime :: Int -> Int -> Int -> Int -> Time
-- makeTime h min sec milisec = 
--     unsafePartial $ fromJust $ Time <$> toEnum h <*> toEnum min <*> toEnum sec <*> toEnum milisec

-- t:: Tempo
-- t = {freq: (2%1),time: (DateTime (makeDate 2022 June 3) (makeTime 19 11 25 100)), count: fromInt 0 }

-- tAncient:: Tempo
-- tAncient = {freq: (2%1),time: (DateTime (makeDate 2012 June 3) (makeTime 19 11 25 100)), count: fromInt 0 }

-- ws:: Int -> Int -> DateTime
-- ws x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

-- we:: Int -> Int -> DateTime
-- we x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

-- eval:: DateTime
-- eval = (DateTime (makeDate 2022 June 3) (makeTime 19 15 0 0))