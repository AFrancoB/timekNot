module Test.Main where

import Prelude
import Data.Boolean
import Data.Either
import Effect (Effect)
import Effect.Now (nowDateTime)
import Effect.Class.Console (log)
import Test.QuickCheck
import AST
import Rhythmic
import Data.Traversable
import Data.Newtype hiding (traverse)
import Data.DateTime
import Data.DateTime.Instant
import Data.Tempo
import Data.Enum
import Partial.Unsafe
import Data.Rational
import Data.Ratio
import Data.List as L
import Data.List.Lazy
import Data.Maybe
import Parsing

main :: Effect Unit
main = do
  _ <- traverse quickCheck [

    runParser ("xxxxx :|| sampleSeq \"bd cp 808\"") topPassageParser == (Right $ Passage (Onsets $ L.fromFoldable [true,true,true,true,true]) (L.fromFoldable [Sample (L.fromFoldable ["bd","cp","808"]) EventI]) Origin true) <?> "rhythmic and sampleSeq (the event per sample parser) do not parse properly"
    ,
    runParser ("xxxxx :|| sampleSeq\' \"bd cp 808\"") topPassageParser == (Right $ Passage (Onsets $ L.fromFoldable [true,true,true,true,true]) (L.fromFoldable [Sample (L.fromFoldable ["bd","cp","808"]) PassageI]) Origin true) <?> "rhythmic and sampleSeq\' (the passage per sample parser) do not parse properly"

    ]
  pure unit

------ extracting data from functions

extractPosix:: Maybe Event -> String
extractPosix (Just evento) = show evento.whenPosix
extractPosix Nothing = "nada"

extractSample:: Maybe Event -> String
extractSample (Just evento) = evento.s
extractSample Nothing = "nada"


extractEventCoord:: Coordenada -> Int
extractEventCoord (Coord n p e) = e

extractPassageCoord:: Coordenada -> Int
extractPassageCoord (Coord n p e) = p


  ---- testing stuff ---------------
makeDate :: Int -> Month -> Int -> Date
makeDate y m d = 
    unsafePartial $ fromJust $ 
       canonicalDate <$> toEnum y <@> m <*> toEnum d

makeTime :: Int -> Int -> Int -> Int -> Time
makeTime h min sec milisec = 
    unsafePartial $ fromJust $ Time <$> toEnum h <*> toEnum min <*> toEnum sec <*> toEnum milisec


t:: Tempo
t = {freq: (2%1),time: (DateTime (makeDate 2022 June 3) (makeTime 19 11 25 100)), count: fromInt 0 }

ws:: Int -> Int -> DateTime
ws x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

we:: Int -> Int -> DateTime
we x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

eval:: DateTime
eval = (DateTime (makeDate 2022 June 3) (makeTime 19 14 59 500))

oDur:: Rational  -- transposition value 2
oDur = (1%2)

o:: List Number
o = fromFoldable [0.0,0.2,0.5] -- at this level a metric unit should be added. For testing: 0,0.1 (metre 1), 0.2,0.3 (metre 2), 0.4,0.5 (metre 3), 0.6,0.7 (metre 4), etc...

countToStart:: Int
countToStart = 327


-- turn this into tests

-- test = passageToEvents' (Passage (Onsets (L.fromFoldable [true,true,true,true,true])) (L.fromFoldable [Sample (L.fromFoldable ["bd","cp","808"]) EventI]) Eval false) t (ws 0 0) (we 59 0) eval

-- --passageToEvents':: Passage -> Tempo -> DateTime -> DateTime -> DateTime -> List (Maybe Event)
-- passageToEvents' (Passage rhy aus nose rep) t ws we eval = 
--     let coords = fromPassageToCoord rhy t ws we eval nose -- Map Int Coord
--         lCoord = snd <$> (M.toUnfoldable coords) -- List Coord, es decir: Nu In In
--         lCoordRep = repeat rep lCoord
--         samples = sampleWithIndex $ last $ filter isSample $ fromFoldable aus --Maybe Aural
--         samplesI = auralIndex $ last $ filter isSample $ fromFoldable aus
--         -- aqui va una funcion con tupletes de samples y coords con el mismo indice!!
--         s = samplesWithPosix samplesI (lenRhyth rhy) samples lCoord
--  --       n = last $ filter isN $ au  -- Maybe Aural 
--     in lCoordRep