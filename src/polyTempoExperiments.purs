module PolyTemporality where

import Prelude hiding (between)

import Data.Either
import Data.Identity
import Data.List hiding (many,take)
import Data.List.Lazy (replicate,repeat,take)
import Data.Foldable (foldl)
import Data.Int
import Data.Tuple
import Data.String (singleton, joinWith)
import Data.String.CodeUnits (fromCharArray)
import Data.Maybe hiding (optional)
import Data.Functor
import Control.Monad
import Data.List.NonEmpty (toList)
import Data.Map as M
import Data.String as Str

import Effect (Effect)
import Effect.Console (log)

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)

import Parsing 

import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration
import Data.Tempo

import AST
import Motor 
import Helpers

import QuickTestAndDebug

type P = ParserT String Identity 

--  types for polytemporality

--type Tempo = Number

data Convergence = Convergence Number Number  -- numbers should be substituted with a proper coordinate system
instance Show Convergence where
    show (Convergence x y) = show x <> " & " <> show y

data Voice = Voice Number Convergence
instance Show Voice where
    show (Voice t conv) = " t: " <> show t <> " conv: " <> show conv

type PolyTempo = M.Map String Voice


-- !!!  Use Map!!!!

--- what if all the events (or any point in a timeline) is understood as a convergence point from which a timeline grows???

-- OJO: The List Number here represents the Period of a voice. This means: the concrete rhythmic program. For example, XXOX :| and XXXOXOX :| will be represented: (4:7:Nil) where 4 and 7 is the number of time units of the rhythmic figure. 

-- the ouput tuple represents the elapsed percentage of each line at a given moment:: Number

-- converger:: PolyTempo -> List Number -> Number -> Tuple Number Number
-- converger voices lines moment = 
--     where 



-- alignLineWithOrigin:: DateTime -> Tempo -> Tuple -> Number

-- moment is the point in time where the operation is performed, line:: tuplet BeatN tempoN

converge:: Number -> Tuple Number Number -> Number -> Tuple Number Number -> Number -> Tuple Number Number
converge moment line1 convergedAt line2 convergingTo = Tuple conv1 (conv2 + (lineToPercentage ((convergingTo*durInSecs line2) - (convergedAt*durInSecs line1)) line2))
    where conv1 = lineToPercentage moment line1
          conv2 = lineToPercentage moment line2


findPoint:: Number -> Number -> Tuple Number Number -> Number
findPoint moment point line = ((durInSecs line)*point) - moment

lineToPercentage:: Number -> Tuple Number Number -> Number
lineToPercentage moment line = moment / durInSecs line

durInSecs:: Tuple Number Number -> Number
durInSecs line = fst line * (bpmToDur (snd line))

-- te:: Tempo
-- te = {freq: (2%1),time: (DateTime dia hra), count: fromInt 0 }

line1 = Tuple 12.0 90.0 -- 13 time untis at 90 bpm

line2 = Tuple 12.0 120.0 -- 17 time untis at 135 bpm

bpmToFreq bpm = (1.0/60.0)* bpm

freqToDur freq = 1.0 / freq

bpmToDur bpm = 1.0 / bpmToFreq bpm



-- parsing polytemporality
parsePolyTemporality:: P PolyTempo
parsePolyTemporality = do
    x <- many parseVoice 
    eof
    pure $ M.fromFoldable x

parseVoice:: P (Tuple String Voice) 
parseVoice = do
    id <- parseVoiceID
    tempo <- parseTempo
    conv <- parseConvergence
    pure $ Tuple id $ Voice tempo conv

parseConvergence:: P Convergence
parseConvergence = do
    _ <- string "converge at:"
    x <- naturalOrFloat
    _ <- string "converging to:"
    y <- naturalOrFloat
    pure $ Convergence (toNumber' x) (toNumber' y)

parseTempo:: P Number
parseTempo = do
    _ <- string "tempo: "
    x <- naturalOrFloat
    pure $ toNumber' x


parseVoiceID:: P String
parseVoiceID = do
    _ <- pure 1
    x <- between (char '\\') space charForID
    pure $ fromCharArray $ toUnfoldable x

charForID:: P (List Char)
charForID = many $ satisfy (not isSep)

isSep:: Char -> Boolean
isSep x = x == ('\\') || x == (' ')

























tokenParser = makeTokenParser haskellStyle
parens      = tokenParser.parens
braces      = tokenParser.braces
identifier  = tokenParser.identifier
reserved    = tokenParser.reserved
naturalOrFloat = tokenParser.naturalOrFloat
float = tokenParser.float
whitespace = tokenParser.whiteSpace
colon = tokenParser.colon
brackets = tokenParser.brackets
comma = tokenParser.comma
semi = tokenParser.semi
integer = tokenParser.integer
stringLit = tokenParser.stringLiteral




-- parsePolyTemporality:: P String
-- parsePolyTemporality = do 
--   _ <- pure 1
--   _ <- choice [try parseTempi, try parseRatios]
--   pure "hola"

-- tempi: 117, 132, 155    /// tempo: 120 ratios: 1/2, 2.3, (1 + 1/2) 
-- convergence points: expression 0 converges   -- 12 events from eval time on tempo-line 117 will converge with index 30 of tempo-line 132

-- where is index 0? @origin, @evaluation


-- parseTempi:: P (List Number)
-- parseTempi = do
--   _ <- pure 1
--   _ <- strWS "tempi:"
--   x <- many naturalOrFloat
--   pure $ map toNumber' x


-- parseRatios:: P (List Number)
-- parseRatios = do
--   _ <- pure 1
--   _ <- strWS "tempo:"
--   x <- toNumber' <$> naturalOrFloat
--   _ <- strWS "ratios:"
--   y <- map toNumber' <$> many naturalOrFloat
--   pure $ ratiosToTempi x y

-- ratiosToTempi:: Number -> List Number -> List Number
-- ratiosToTempi n rs = map (_*n) rs