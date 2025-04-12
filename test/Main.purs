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
import Data.Tuple
import Data.Map (Map(..), empty, fromFoldable)
import Data.Int (floor, round, toNumber) as I
import Data.List as L
import Data.List.Lazy
import Data.Maybe
import Parsing


import Rhythm
import Parser

main :: Effect Unit
main = do
  _ <- traverse quickCheck [
    -- rhythmic parser tests:
    (show <$> runParser "x" rhythmic) == Right "x" <?> "x broken in rhythm parser",
    (show <$> runParser "o" rhythmic) == Right "o" <?> "o broken in rhythm parser",
    (show <$> runParser "xxxxx" rhythmic) == Right "xxxxx" <?> "xxxx broken in rhythm parser",
    (show <$> runParser "xxx xxx" rhythmic) == Right "xxxxxx" <?> "xxx xxx broken in rhythm parser",
    (show <$> runParser "oxox" rhythmic) == Right "oxox" <?> "oxox broken in rhythm parser",
    (show <$> runParser "oxo xox" rhythmic) == Right "oxoxox" <?> "oxo xox broken in rhythm parser",
    (show <$> runParser "[x]" rhythmic) == Right "[x]" <?> "[x] broken in rhythm parser",
    (show <$> runParser "[xxxx]" rhythmic) == Right "[xxxx]" <?> "[xxxx] broken in rhythm parser",
    (show <$> runParser "[oxox]" rhythmic) == Right "[oxox]" <?> "[oxox] broken in rhythm parser",
    (show <$> runParser "[oxox] x" rhythmic) == Right "[oxox]x" <?> " [oxox] x broken in rhythm parser",
    (show <$> runParser "x [oxxxxo]" rhythmic) == Right "x[oxxxxo]" <?> " x[oxxxxo] broken in rhythm parser",
    (show <$> runParser "[xoxox[oxoxo]]" rhythmic) == Right "[xoxox[oxoxo]]" <?> " [xoxox[oxoxo]] broken in rhythm parser",
    (show <$> runParser "[xx[ox[xxx]]]" rhythmic) == Right "[xx[ox[xxx]]]" <?> " [xx[ox[xxx]]] broken in rhythm parser",
    (show <$> runParser "xox  [xx  [ox  [xxx]]]" rhythmic) == Right "xox[xx[ox[xxx]]]" <?> " xox[xx[ox[xxx]]] broken in rhythm parser",
    (show <$> runParser "[[[xx[o[oxo]o]xx]]]" rhythmic) == Right "[[[xx[o[oxo]o]xx]]]" <?> " [[[xx[o[oxo]o]xx]]] broken in rhythm parser",
    (show <$> runParser "[(3,8,5)]" rhythmic) == Right "[(3,8,5)]" <?> " [(3,8,5)] broken in rhythm parser",
    (show <$> runParser "[(x,3,8)]" rhythmic) == Right "[(x,3,8,0)]" <?> " [(x,3,8,0)] broken in rhythm parser",
    (show <$> runParser "[(xx,ox,5,8)]" rhythmic) == Right "[(xx,ox,5,8,0)]" <?> " [(xx,ox,5,8,0)] broken in rhythm parser",
    (show <$> runParser "[_(x,7,12)]" rhythmic) == Right "[_(x,7,12,0)]" <?> " [_(x,7,12,0)] broken in rhythm parser",
    (show <$> runParser "[!oxox#2]" rhythmic) == Right "[!oxox#2]" <?> " [!oxox#2] broken in rhythm parser",
    (show <$> runParser "(3,5)" rhythmic) == Right "(3,5,0)" <?> " (3,5,0) broken in rhythm parser",
    (show <$> runParser "(x,4,7,1)" rhythmic) == Right "(x,4,7,1)" <?> " (x,4,7,1) broken in rhythm parser",
    (show <$> runParser "(ox,xx,3,11)" rhythmic) == Right "(ox,xx,3,11,0)" <?> " (ox,xx,3,11,0) broken in rhythm parser",
    (show <$> runParser "_(xox,5,9)" rhythmic) == Right "_(xox,5,9,0)" <?> " _(xox,5,9,0) broken in rhythm parser",
    (show <$> runParser "(!oxxxo#3,5,7)" rhythmic) == Right "(!oxxxo#3,5,7,0)" <?> " (!oxxxo#3,5,7,0) broken in rhythm parser",
    (show <$> runParser "([xxx],4,7)" rhythmic) == Right "([xxx],4,7,0)" <?> " ([xxx],4,7,0) broken in rhythm parser",
    (show <$> runParser "((3,8),2,7,1)" rhythmic) == Right "((3,8,0),2,7,1)" <?> " ((3,8),2,7,1) broken in rhythm parser",
    (show <$> runParser "![ox]#5" rhythmic) == Right "![ox]#5" <?> "![ox]#5 broken in rhythm parser",
    (show <$> runParser "!_(ox,3,8)#4" rhythmic) == Right "!_(ox,3,8,0)#4" <?> "!_(ox,3,8,0)#4 broken in rhythm parser",
    (show <$> runParser "!oxox xxx#2" rhythmic) == Right "!oxoxxxx#2" <?> "!oxox xxx#2 broken in rhythm parser",
    (show <$> runParser "!ox [xx] (3,8) !xx#3 #4" rhythmic) == Right "!ox[xx](3,8,0)!xx#3#4" <?> "!ox [xx] (3,8) !xx#3 #4 broken in rhythm parser",
    (show <$> runParser "ox (3,8) [xxx] !xx#2 ([xx[(3,5)]],4,7,2) x [(xx,3,5,1) !!xxx [ox] _(ox,2,6,3)#2 [xxx]#5]" rhythmic) == Right "ox(3,8,0)[xxx]!xx#2([xx[(3,5,0)]],4,7,2)x[(xx,3,5,1)!!xxx[ox]_(ox,2,6,3)#2[xxx]#5]" <?> "ox(3,8,0)[xxx]!xx#2([xx[(3,5,0)]],4,7,2)x[(xx,3,5,1)!!xxx[ox]_(ox,2,6,3)#2[xxx]#5] broken in rhythm parser",

    (show <$> runParser "a 300cpm | x :|" parseProgram) == Right "(TimeExpression (fromFoldable [(Tuple \"a-0\" Metric 0 Origin 0 300 % 1cpm x looped)]) : Nil)" <?> "a 300cpm | x :| broken in program parser",

    (show <$> runParser "a [300cpm, 1/4 = 120bpm, mu 3:2, 2cps] | x :|" parseProgram) == Right "(TimeExpression (fromFoldable [(Tuple \"a-0\" Metric 0 Origin 0 300 % 1cpm x looped),(Tuple \"a-1\" Converge \"a-0\" 0 Origin 0 1 % 4 = 120 % 1bpm x looped),(Tuple \"a-2\" Converge \"a-0\" 0 Origin 0 mu-0 3:2 x looped),(Tuple \"a-3\" Converge \"a-0\" 0 Origin 0 2 % 1cps x looped)]) : Nil)" <?> "a [300cpm, 1/4 = 120bpm, mu 3:2, 2cps] | x :| broken in programParser",

    (show <$> runParser "a 300cpm*[1,1.1,1.2] | x :|" parseProgram) == Right "(TimeExpression (fromFoldable [(Tuple \"a-0\" Metric 0 Origin 0 300 % 1cpm x looped),(Tuple \"a-1\" Converge \"a-0\" 0 Origin 0 330 % 1cpm x looped),(Tuple \"a-2\" Converge \"a-0\" 0 Origin 0 360 % 1cpm x looped)]) : Nil)" <?> "a 300cpm*[1,1.1,1.2] | x :| broken at parseProgram",

    (show <$> runParser "a[10] 300cpm | x :|" parseProgram) == Right "(TimeExpression (fromFoldable [(Tuple \"a-0\" Metric 0 Origin 10 300 % 1cpm x looped)]) : Nil)" <?> "a[10] 300cpm | x :| broken at parseProgram",

    (show <$> runParser "a[10] <- [6>>] 300cpm | x :|" parseProgram) == Right "(TimeExpression (fromFoldable [(Tuple \"a-0\" Metric 6 Snap 10 300 % 1cpm x looped)]) : Nil)" <?> "a[10] <- [6>>] 300cpm | x :| broken at parseProgram",

    (show <$> runParser "a[1:0] <- [6>>] [300cpm,500cpm] | x :|" parseProgram) == Right "(TimeExpression (fromFoldable [(Tuple \"a-0\" Converge \"a-1\" 0 Origin 0 300 % 1cpm x looped),(Tuple \"a-1\" Metric 6 Snap 0 500 % 1cpm x looped)]) : Nil)" <?> "a[1:0] <- [6>>] [300cpm,500cpm] | x :| broken at parseProgram",

    (show <$> runParser "a <- mu [300cpm,500cpm] | x :|" parseProgram) == Right "(TimeExpression (fromFoldable [(Tuple \"a-0\" Converge \"mu-0\" 0 Origin 0 300 % 1cpm x looped),(Tuple \"a-1\" Converge \"a-0\" 0 Origin 0 500 % 1cpm x looped)]) : Nil)" <?> "a <- mu [300cpm,500cpm] | x :| broken at parseProgram",

    (show <$> runParser "a[1:13] <- mu[1:17] [300cpm,500cpm] | x :|" parseProgram) == Right "(TimeExpression (fromFoldable [(Tuple \"a-0\" Converge \"a-1\" 13 Origin 13 300 % 1cpm x looped),(Tuple \"a-1\" Converge \"mu-1\" 17 Origin 13 500 % 1cpm x looped)]) : Nil)" <?> "a[1:13] <- mu[1:17] [300cpm,500cpm] | x :| broken at parseProgram"

    ]
  pure unit


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
