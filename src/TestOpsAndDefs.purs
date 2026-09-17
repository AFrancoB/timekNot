module TestOpsAndDefs (tp,a,b,c,d,e,f,g, rhD, tmD, convergeToD, convergeFromD, getPolytemporal,getRhythmic, getRhythmicFromMap,getLoop,tempoMark,convergeTo,convergeFrom,defMapTemporals,defTemporal,defTemporalMetric,defPolytemporal,defConvergeTo,defConvergeFrom,defVoice,defAural, defEvent, t) where

import Prelude

import Data.Tuple
import Data.Maybe
import Data.Map as M
import Data.Int (floor,round,toNumber)
import Data.List (fromFoldable, List(..))

import Data.Tempo

import AST
import DurationAndIndex
import TimePacketOps

import Data.Rational (Rational(..), (%), fromInt)
import Data.DateTime
import Data.Enum
import Partial.Unsafe

import TimePacketOps
--- helpers

-- if need to get polytempo, loop tempomark follow the model of getRhythmicFromMap
-- get a map and a string: questions 1) if lookup finds nothing what to do? in theory this is addressed already at6 checkpoint

getRhythmicFromMap:: M.Map String Temporal -> String -> Rhythmic
getRhythmicFromMap mapa key = getRhythmic $ fromMaybe defTemporal $ M.lookup key mapa

getPolytemporal:: Temporal -> Polytemporal
getPolytemporal (Temporal p _ _) = p 

getRhythmic:: Temporal -> Rhythmic
getRhythmic (Temporal _ r _) = r

getLoop:: Temporal -> Boolean
getLoop (Temporal _ _ l) = l

tempoMark:: Temporal -> TempoMark
tempoMark (Temporal p _ _) = getTempoMark p 

getTempoMark:: Polytemporal -> TempoMark
getTempoMark (Kairos _ tm) = tm
getTempoMark (Metric _ _ tm) = tm
getTempoMark (Converge _ _ _ tm) = tm
getTempoMark (Novus _ _ tm) = tm

convergeTo:: Temporal -> ConvergeTo
convergeTo (Temporal p _ _) = getConvergeTo p

convergeFrom:: Temporal -> ConvergeFrom
convergeFrom (Temporal p _ _ ) = getConvergeFrom p

getConvergeTo:: Polytemporal -> ConvergeTo
getConvergeTo (Converge _ cTo _ _) = cTo
getConvergeTo (Metric cTo _ _) = cTo
getConvergeTo _ = defConvergeTo

getConvergeFrom:: Polytemporal -> ConvergeFrom
getConvergeFrom (Converge _ _ cFrom _) = cFrom
getConvergeFrom (Novus _ cFrom _) = cFrom
getConvergeFrom (Metric _ cFrom _) = cFrom
getConvergeFrom _ = defConvergeFrom

-- testProgramToWaste ws we eval bpm str = (\pr -> programToWaste pr (wP ws) (wP we) (wP eval) (t' bpm)) <$> parsed 
--   where parsed = runParser str parseProgram


-- Tuple "b-0" Converge "a-0" 10 Snap >> 0 60 % 1cpm xo looped

tmD = CPM (60%1)

convergeToD = ProcessTo 0 Origin

convergeFromD = Process 0

rhD = Rhythmics (fromFoldable [X,O])

-- temporals from above:
-- a = Temporal (Metric (ProcessTo 3 SnapAfter) (Process 0) (CPM (60%1))) (Rhythmics (fromFoldable [X])) true
-- b = Temporal (Converge "a-0" (ProcessTo 1 Origin) (Process 0) (CPM (60%1))) (Rhythmics (fromFoldable [X,O])) true
-- c = Temporal (Converge "b-0" (ProcessTo 1 Origin) (Process 0) (CPM (60%1))) (Rhythmics (fromFoldable [X,O])) true
-- d = Temporal (Converge "c-0" (ProcessTo 1 Origin) (Process 0) (CPM (60%1))) (Rhythmics (fromFoldable [X,O])) true
-- e = Temporal (Converge "d-0" (ProcessTo 1 Origin) (Process 0) (CPM (60%1))) (Rhythmics (fromFoldable [X,O])) true
-- f = Temporal (Converge "e-0" (ProcessTo 1 Origin) (Process 0) (CPM (60%1))) (Rhythmics (fromFoldable [X,O])) true
-- g = Temporal (Converge "f-0" (ProcessTo 1 Origin) (Process 0) (CPM (60%1))) (Rhythmics (fromFoldable [X,O])) true


a = Temporal (Metric (ProcessTo 0 Origin) (Process 0) (CPM (60%1))) (Rhythmics $ fromFoldable [X]) true
b = Temporal (Converge "a-0" (ProcessTo 3 SnapAfter) (Process 0) (CPM (60%1))) (Sd $ Rhythmics $ fromFoldable [O,X]) true
c = Temporal (Converge "b-0" (ProcessTo 2 Origin) (Process 0) (CPM (120%1))) (Sd $ Rhythmics $ fromFoldable [O,X]) true
d = Temporal (Converge "c-0" (ProcessTo 3 Origin) (Process 0) (CPM (60%1))) (Sd $ Rhythmics $ fromFoldable [O,X,X]) true
e = Temporal (Converge "d-0" (ProcessTo 3 Origin) (Process 0) (CPM (60%1))) (Sd $ Rhythmics $ fromFoldable [O,X,X]) true
f = Temporal (Converge "e-0" (ProcessTo 3 Origin) (Process 0) (CPM (60%1))) (Sd $ Rhythmics $ fromFoldable [O,X,X]) true
g = Temporal (Converge "f-0" (ProcessTo 3 Origin) (Process 0) (CPM (60%1))) (Sd $ Rhythmics $ fromFoldable [O,X,X]) true


defMapTemporals = M.fromFoldable [
  Tuple "a-0" a,
  Tuple "b-0" b,
  Tuple "c-0" c,
  Tuple "d-0" d,
  Tuple "e-0" e,
  Tuple "f-0" f,
  Tuple "g-0" g
  -- Tuple "v3" (Temporal (Converge "v2" (ProcessTo 0 Origin) (Process 0) (CPM (120%1))) X false),
  -- Tuple "v4" (Temporal (Kairos 0.0 (CPM (120%1))) O false)
]


defTemporalMetric = Temporal (Metric (ProcessTo 0 Origin) (Process 0) (CPM (60%1))) (Rhythmics (fromFoldable [X,O])) true
-- defTemporal = Temporal (Kairos 0.0 (CPM (120%1))) O false
defTemporal = Temporal (Converge "def-2666" (ProcessTo 5666 SnapAfter) (Process 3666) (CPM (2666%1))) (Rhythmics (fromFoldable [X,O])) true
defTempoMark = CPM (120%1)
defPolytemporal = Kairos 0.0 defTempoMark

defConvergeTo = ProcessTo 0 Origin
defConvergeFrom = Process 0

defVoice = Voice defTemporal defAural
defAural = Nil

defEvent = Event defOnset defIndex

defOnset = Onset true 0.0

defIndex = Index 0 [0] 0

  ---- testing stuff ---------------
tp:: TimePacket
tp = assambleTimePacket (wP 1.0) (wP 1.2) eval t M.empty

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
