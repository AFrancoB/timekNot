module Novus(processVantage) where

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

processVantage:: Map String Vantage -> VantageMap -> DateTime -> Tempo -> VantageMap
processVantage novus vm eval t = difference (unions [processed,unprocessed,remainFromBuild]) remove
  where unprocessed = difference vm novus -- remain the ones that are not altered
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