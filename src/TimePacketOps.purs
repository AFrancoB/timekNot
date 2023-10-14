module TimePacketOps (secsFromOriginAtWS, secsFromOriginAtWE, secsFromOriginAtEval,metricFromOriginAtWS, metricFromOriginAtWE, metricFromOriginAtEval, voiceFromOriginToEval, fromDateTimeToPosix, fromDateTimeToPosixMaybe) where

import Prelude
import Data.Maybe
import Data.Newtype
import Data.Tempo
import AST
import DurationAndIndex
import Data.Rational (Rational(..), (%), fromInt, toNumber)
import Data.DateTime
import Data.DateTime.Instant

secsFromOriginAtWS:: TimePacket -> Number
secsFromOriginAtWS tp = ws - oPosix
    where oPosix = fromDateTimeToPosix tp.origin
          ws = fromDateTimeToPosix tp.ws

secsFromOriginAtWE:: TimePacket -> Number
secsFromOriginAtWE tp = we - oPosix
    where oPosix = fromDateTimeToPosix tp.origin
          we = fromDateTimeToPosix tp.we

secsFromOriginAtEval:: TimePacket -> Number
secsFromOriginAtEval tp = eval - oPosix
    where oPosix = fromDateTimeToPosix tp.origin
          eval = fromDateTimeToPosix tp.eval

metricFromOriginAtWS:: TimePacket -> Number  -- is this needed anyway?
metricFromOriginAtWS tp = originSecsAtWS / voiceDur
    where originSecsAtWS = secsFromOriginAtWS tp -- :: Number
          vTempo = toNumber $ tp.tempo.freq * (60%1) -- hz to bpm
          voiceDur = durInSecs 1.0 vTempo

metricFromOriginAtWE:: TimePacket -> Number
metricFromOriginAtWE tp = originSecsAtWE / voiceDur
    where originSecsAtWE = secsFromOriginAtWE tp -- :: Number
          vTempo = toNumber $ tp.tempo.freq * (60%1) -- htz to bpm
          voiceDur = durInSecs 1.0 vTempo

metricFromOriginAtEval:: TimePacket -> Number -- equivalent (in theory) to timeToCount ???
metricFromOriginAtEval tp = originSecsAtEval / voiceDur
    where originSecsAtEval = secsFromOriginAtEval tp -- :: Number
          vTempo = toNumber $ tp.tempo.freq * (60%1) -- htz to bpm :: Number
          voiceDur = durInSecs 1.0 vTempo

voiceFromOriginToEval:: TimePacket -> Number -> Number -> Number
voiceFromOriginToEval tp vTempo vUnits = originSecsAtEval / voiceDur
    where originSecsAtEval = secsFromOriginAtEval tp -- :: Number
          voiceDur = durInSecs vUnits vTempo


fromDateTimeToPosix:: DateTime -> Number
fromDateTimeToPosix x = (unwrap $ unInstant $ fromDateTime x)/1000.0000

fromDateTimeToPosixMaybe:: Maybe DateTime -> Maybe Number
fromDateTimeToPosixMaybe (Just x) = Just $ (unwrap $ unInstant $ fromDateTime x)/1000.0000
fromDateTimeToPosixMaybe Nothing = Nothing