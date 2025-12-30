module Novus(processVantage, updateEvalCount) where

import Prelude

import Data.Maybe
import Data.Either
import Data.Map
import Data.Rational

import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration

import Data.Tempo

import AST

-- Eval TimePoint Int
-- 'x'.eval = 5;
-- 'x'.reset;

-- a[100] <- 'x' 300tl | xxxx :|
-- type VantageMap = Map String DateTime

-- data Vantage = Build TimePoint | Move (Either Rational Rational) | Remove | Eval TimePoint Int | Reset
updateEvalCount:: Map String Vantage -> Int -> Int
updateEvalCount novus count = if isEmpty (filter (\v -> isReset v)   novus) then count + 1 else 0
  where isReset Reset = true
        isReset _ = false

processVantage:: Map String Vantage -> VantageMap -> DateTime -> Int -> Tempo -> VantageMap
processVantage novus vm eval evalC t = difference (unions [processed,unprocessed,remainFromBuild,processedNewOnes,processedOldOnes]) remove
  where unprocessed = difference vm novus -- remain the ones that are not altered
        isEval (VEval _) = true
        isEval _ = false
        evalProgram = filter (\v -> isEval v) novus  -- eval from just evaluated Program (as Map String Vantage), could be already existing or new ones
        evalPrAndMap = intersection evalProgram vm  -- eval from evaluated program that were already in the map
        evalJustMap = difference vm evalProgram -- eval from the vantage map
        evalNewOnes = difference evalProgram vm   -- eval just from evaluated program (new ones)

        processedNewOnes = mapMaybeWithKey (\k v -> transformEvalNew k v eval evalC t vm) evalNewOnes
        processedOldOnes = mapMaybeWithKey (\k v -> transformEvalOld k v eval evalC t vm) evalPrAndMap

        isBuild (Build _) = true
        isBuild _ = false
        builds = filter (\v -> isBuild v) novus
        remainFromBuild = intersection vm builds
        toBuild = difference builds vm -- List String
        isMove (Move _) = true
        isMove _ = false
        moves = filter (\v -> isMove v) novus
        toMove1 = intersection moves vm
        toMove2 = intersection moves toBuild
        isRemove Remove = true
        isRemove _ = false
        remove = intersection (filter (\v -> isRemove v) novus) vm
        processed = mapMaybeWithKey (\k v -> transformVtoMaybeDT k v eval t vm) $ unions [toBuild, toMove1, toMove2]

transformEvalNew:: String -> Vantage -> DateTime -> Int -> Tempo -> VantageMap -> Maybe DateTime
transformEvalNew k (VEval m) eval evalC _ vm = Just eval
transformEvalNew _ _ _ _ _ _ = Nothing

transformEvalOld:: String -> Vantage -> DateTime -> Int -> Tempo -> VantageMap -> Maybe DateTime
transformEvalOld k (VEval m) eval evalC _ vm = if evalC`mod`m == 0 then (Just eval) else oldEval
  where oldEval = lookup k vm
transformEvalOld _ _ _ _ _ _ = Nothing


transformVtoMaybeDT:: String -> Vantage -> DateTime -> Tempo -> VantageMap -> Maybe DateTime
transformVtoMaybeDT _ (Build x) eval t _ = result
  where result = case x of  
                Secs secs -> adjust (Seconds $ toNumber secs) eval
                Beat beat -> adjust (Seconds $ toNumber (beat * t.freq)) eval
                UTC dt -> Just dt 
transformVtoMaybeDT k (Move x) eval t vm = current >>= adjust (Seconds x')
  where x' = case x of  
                Right secs -> toNumber secs
                Left beat -> toNumber (beat * t.freq)
        current = lookup k vm -- Maybe DateTime
transformVtoMaybeDT _ Remove _ _ _ = Nothing
transformVtoMaybeDT _ _ _ _ _ = Nothing

