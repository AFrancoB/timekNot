module Acceleration (rhythmicToSinDur,sinusoidalAcceleration,rhythmicToOnsetsSin) where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Int (toNumber)
import Data.Number
import Data.Array
import Data.Array (fromFoldable) 
import Data.List (List(..))
import Data.List (fromFoldable, (:)) as L
import Data.Foldable (sum)
import Data.Tuple
import Data.Maybe (fromMaybe)

import AST
import DurationAndIndex

rhythmicToOnsetsSin:: Rhythmic -> Number -> Number -> Number -> Number -> List Onset
rhythmicToOnsetsSin rhy osc min max ph = L.fromFoldable $ zipWith Onset onsets pos
    where onsets = map (\(Onset b p) -> b) $ fromFoldable $ rhythmicToOnsets rhy
          rhyDur = rhythmicToSinDur rhy osc min max ph
          folded' = fromMaybe {init: [], last: 2.666} $ unsnoc $ (scanl (+) 0.0 rhyDur)
          pos = map (\fo -> fo / (sum rhyDur)) $ (0.0 : folded'.init)

acceleration :: Number -> Number -> Number -> Number -> Number -> Number
acceleration startTime finalTime startSpeed endSpeed currentTime =
  let
    deltaTime = finalTime - startTime
    deltaSpeed = endSpeed - startSpeed
    acceleration = deltaSpeed / deltaTime
    initialSpeed = startSpeed
  in
    initialSpeed + acceleration * currentTime

rhythmicToLinDur:: Rhythmic -> Array Number
rhythmicToLinDur rhythmic = map (\(Tuple dur acc) -> (dur / (0.5 + acc))) zipped
    where onsetDur = map (\(Onset b p) -> p) $ fromFoldable $ onsetDurations 1.0 rhythmic
          onsetPos = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic
          onsetAcc = fromFoldable $ map (\pos -> acceleration 0.0 1.0 1.0 2.0 pos) onsetPos
          zipped = zip onsetDur onsetAcc

sinusoidalAcceleration :: Number -> Number -> Number -> Number -> Number
sinusoidalAcceleration frequency currentTime amplitude phase =
  amplitude * sin (2.0 * pi * frequency * currentTime + phase)
  -- amplitude * sin (2.0 * pi * frequency * currentTime + phase)

-- freq is actually cycles per block. 1 means one whole oscilation per block
-- amplitud will determine the range. If amplitude 1 it will go from 0 to 1 and -1

rhythmicToSinDur:: Rhythmic -> Number -> Number -> Number -> Number -> Array Number
rhythmicToSinDur rhythmic freq min' max' ph = map (\(Tuple dur acc) -> dur / (min + ((amp) + acc))) zipped
    where min = 1.0
          max = max' / min'
          amp = (max - min) / 2.0
          phase = if ph == 0.0 then 0.0 else pi / ph
          onsetDur = map (\(Onset b p) -> p) $ fromFoldable $ onsetDurations 1.0 rhythmic
          onsetPos = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic
          onsetAcc = fromFoldable $ map (\pos -> sinusoidalAcceleration freq pos amp phase) onsetPos
          zipped = zip onsetDur onsetAcc



-- trapezoidalRule:: (Number -> Number) -> Number -> Number -> Number
-- trapezoidalRule f a b =
--   let
--     n = 1000
--     h = (b - a) / toNumber n
--     sum = foldl (\acc i -> acc + f (a + toNumber i * h)) 0.0 $ 1..(n-1)
--   in
--     h / 2.0 * (f a + 2.0 * sum + f b)

-- areaUnderCurveSin:: Number -> Number -> Number
-- areaUnderCurveSin start end = trapezoidalRule sin ((start*2.0) * pi) ((end*2.0) * pi)

-- areaUnderCurveLineal:: Number -> Number -> Number
-- areaUnderCurveLineal start end = trapezoidalRule linearAcc start end
--     where 
--             linearAcc:: Number -> Number
--             linearAcc t = 2.0 * t

-- -- rhythmicToSinDur:: Rhythmic -> Array Number
-- rhythmicToSinDur rhythmic = map (\(Tuple dur acc) -> 1.0/(dur * (1.0 + acc))) zipped
--     where onsetDur = map (\(Onset b p) -> p) $ fromFoldable $ onsetDurations 1.0 rhythmic
--           onsetPos = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic
--           onsetAcc = fromFoldable $ map (\pos -> areaUnderCurveSin 0.0 pos) onsetPos
--           zipped = zip onsetDur onsetAcc

-- rhythmicToLinDur rhythmic = map (\(Tuple dur acc) -> 1.0/(dur * (1.0 + acc))) zipped
--     where onsetDur = map (\(Onset b p) -> p) $ fromFoldable $ onsetDurations 1.0 rhythmic
--           onsetPos = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic
--           onsetAcc = fromFoldable $ map (\pos -> areaUnderCurveLineal 0.0 pos) onsetPos
--           zipped = zip onsetDur onsetAcc