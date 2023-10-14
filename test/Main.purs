module Test.Main where

import Prelude
import Data.Boolean
import Data.Either
import Effect (Effect)
import Effect.Now (nowDateTime)
import Effect.Class.Console (log)
import Test.QuickCheck
import AST
import Data.Traversable
import Data.Newtype hiding (traverse)
import Data.DateTime
import Data.DateTime.Instant
import Data.Tempo
import Data.Enum
import Partial.Unsafe
import Data.Rational
-- import Data.Ratio
import Data.Int (floor, round, toNumber) as I
import Data.List as L
import Data.List.Lazy
import Data.Maybe
import Parsing



-- -- tests for:
-- -- Eval Unlooped if voice is in front window span:
-- test1 = voiceInWindowUnlooped 3.5 4.5 1.0 2.0 -- must be 0.0 0.0

-- -- if voice is past window span:
-- test2 = voiceInWindowUnlooped 1.5 2.5 3.0 4.0 -- must be 0.0 0.0

-- -- if voice is showing a segment by ws
-- test3 = voiceInWindowUnlooped 1.5 2.5 2.0 3.0 -- must be

-- -- if voice is showing a segment by we
-- test4 = voiceInWindowUnlooped 2.5 3.5 2.0 3.0 -- must be 

-- -- if voice is bigger than window span
-- test5 = voiceInWindowUnlooped 2.5 4.5 3.0 4.0 -- must be 

-- -- if voice is within window span
-- test6 = voiceInWindowUnlooped 1.5 2.5 1.0 3.0 -- must be 

-- -- Kairos looped

-- -- if voice is in front of window
-- test7 = voiceInWindowLooped 3.5 4.5 1.0 2.0 -- must be 0.0 0.0

-- -- if voice is past window span:
-- test8 = voiceInWindowLooped 1.5 2.5 1.0 2.0 -- must be 3.0 to 3.5 and 3.5 4.0

-- -- -- if mutliple         -- x1  x2  ws we
-- test9 = voiceInWindowLooped 1.5 2.5 1.0 3.0

-- main :: Effect Unit
-- main = do
--   _ <- traverse quickCheck [

--     runParser ("xxxxx :|| sampleSeq \"bd cp 808\"") topPassageParser == (Right $ Passage (Onsets $ L.fromFoldable [true,true,true,true,true]) (L.fromFoldable [Sample (L.fromFoldable ["bd","cp","808"]) EventI]) Origin true) <?> "rhythmic and sampleSeq (the event per sample parser) do not parse properly"
--     ,
--     runParser ("xxxxx :|| sampleSeq\' \"bd cp 808\"") topPassageParser == (Right $ Passage (Onsets $ L.fromFoldable [true,true,true,true,true]) (L.fromFoldable [Sample (L.fromFoldable ["bd","cp","808"]) PassageI]) Origin true) <?> "rhythmic and sampleSeq\' (the passage per sample parser) do not parse properly"

--     ]
--   pure unit
----   unpacking time funcion


-- ------ extracting data from functions



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
t = {freq: (2%1),time: (DateTime (makeDate 2022 June 3) (makeTime 19 11 25 100)), count: fromInt 0 }

wP:: Number -> DateTime
wP sm = (DateTime (makeDate 2022 June 3) (makeTime 19 11 (25 + secs) (100+mili)))
    where secs = I.floor sm
          mili = I.round ((sm - (I.toNumber (I.floor sm))) * 1000.0)

eval:: DateTime
eval = (DateTime (makeDate 2022 June 3) (makeTime 19 14 59 500))

o:: DateTime
o = origin t

oPosix:: Number
oPosix = fromDateTimeToPosix o

voiceFromOrigin:: Number -> Number -> Number -> Number
voiceFromOrigin units tempo sm = (fromDateTimeToPosix (wP sm) - oPosix) / (durInSecs units tempo)

fromDateTimeToPosix:: DateTime -> Number
fromDateTimeToPosix x = (unwrap $ unInstant $ fromDateTime x)/1000.0000

fromDateTimeToPosixMaybe:: Maybe DateTime -> Maybe Number
fromDateTimeToPosixMaybe (Just x) = Just $ (unwrap $ unInstant $ fromDateTime x)/1000.0000
fromDateTimeToPosixMaybe Nothing = Nothing

durInSecs:: Number -> Number -> Number
durInSecs dur tempo = dur * (bpmToDur tempo)

bpmToFreq bpm = (1.0/60.0)* bpm

freqToDur freq = 1.0 / freq

bpmToDur bpm = 1.0 / bpmToFreq bpm
