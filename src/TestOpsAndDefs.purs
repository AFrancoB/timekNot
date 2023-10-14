module TestOpsAndDefs (defMapTemporals,defTemporal,defPolytemporal,defConvergeTo,defConvergeFrom) where

import Prelude

import Data.Tuple
import Data.Maybe
import Data.Map as M
import Data.Int (floor,round,toNumber)
import Data.List (fromFoldable)

import Data.Tempo

import AST
import DurationAndIndex
import TimePacketOps

import Data.Rational (Rational(..), (%), fromInt)
import Data.DateTime
import Data.Enum
import Partial.Unsafe

-- testProgramToWaste ws we eval bpm str = (\pr -> programToWaste pr (wP ws) (wP we) (wP eval) (t' bpm)) <$> parsed 
--   where parsed = runParser str parseProgram

defMapTemporals = M.fromFoldable [
  Tuple "v0" (Temporal (Metric (ProcessTo 1 Snap) (Process 0) (BPM (135%1) (1%4))) (Rhythmics (fromFoldable [X,X,X,X])) false),
  Tuple "v1" (Temporal (Converge "v0" (ProcessTo 3 Origin) (Process 0) (BPM (150%1) (1%4))) (Rhythmics (fromFoldable [X,X,X,X])) false),
  Tuple "v2" (Temporal (Converge "v1" (ProcessTo 0 Origin) (Process 0) XTempo) (Rhythmics (fromFoldable [X,X,X,X])) false) --,
  -- Tuple "v3" (Temporal (Converge "v2" (ProcessTo 0 Origin) (Process 0) (CPM (120%1))) X false),
  -- Tuple "v4" (Temporal (Kairos 0.0 (CPM (120%1))) O false)
]

defTemporal = Temporal (Kairos 0.0 (CPM (120%1))) O false
defPolytemporal = Kairos 0.0 (CPM (120%1))

defConvergeTo = ProcessTo 0 Origin
defConvergeFrom = Process 0

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

t':: Rational -> Tempo
t' freq = {freq: freq,time: (DateTime (makeDate 2022 June 3) (makeTime 19 11 0 0)), count: fromInt 0 }
  -- where freq = toRat $ bpmToFreq bpm

-- t':: Number -> Tempo
-- t' bpm = {freq: freq,time: (DateTime (makeDate 2022 June 3) (makeTime 19 11 0 0)), count: fromInt 0 }
--   where freq = toRat $ bpmToFreq bpm

t:: Tempo
t = {freq: (2%1),time: (DateTime (makeDate 2022 June 3) (makeTime 19 11 0 0)), count: fromInt 0 }

wP:: Number -> DateTime
wP sm = (DateTime (makeDate 2022 June 3) (makeTime 19 11 (0 + secs) (0 + mili)))
    where secs = floor sm
          mili = round ((sm - (toNumber (floor sm))) * 1000.0)

eval:: DateTime
eval = (DateTime (makeDate 2022 June 3) (makeTime 19 14 59 0))

o:: DateTime
o = origin t

oPosix:: Number
oPosix = fromDateTimeToPosix o