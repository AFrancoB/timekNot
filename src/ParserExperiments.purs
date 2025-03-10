module ParserExperiments where

import Prelude

import Data.Identity
import Data.List (List(..), head, tail, elem, (:), concat, (..), range, (!!))
import Data.List (fromFoldable, filter, length, filter) as L
import Data.Array (fromFoldable) as A
import Data.Either
import Data.Int
import Data.String (take, length)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Map (Map(..), filter, lookup, keys, singleton, fromFoldable, toUnfoldable, member, unions, empty)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Maybe
import Data.Rational (Rational(..), toRational, fromInt, (%))

import Data.DateTime (exactDate, Year(..), Month(..), Day(..))

import Data.FunctorWithIndex (mapWithIndex)

import Data.String.CodeUnits (fromCharArray)
import Data.String (split, Pattern)

import Data.Formatter.DateTime (Formatter, parseFormatString, unformat)
import Data.Formatter.Number (Formatter, parseFormatString, unformat) as N

import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration


import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Combinators.Array (many)
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)

import AST
import Rhythm
import Aural

type P = ParserT String Identity

-- timeExpression:: P Expression
-- timeExpression = do
--   _ <- pure 1
--   x <- temporal
--   pure $ TimeExpression x

-- temporal:: P (Map String Temporal)
-- temporal = do
--   _ <- pure 1
--   choice [try replica, try polytemporalRelation]

-- replica:: P (Map String Temporal)
-- replica = do
--   _ <- pure 1
--   id <- voiceId
--   _ <- reserved "<-"
--   id2 <- voiceId 
--   _ <- semi
--   pure $ singleton id $ Replica id2

-- ----- p <- choice [try kairos, try metric, try converge]

-- polytemporalRelation:: P (Map String Polytemporal)
-- polytemporalRelation = do
--   _ <- pure 1
--   id <- voiceId  
--   cFrom <- brackets $ cFrom <|> Process 0  -- tuple int cf
--   cTo <- cTo <|> ProcessTo 0 Origin        -- tuple int ct -- cTo Needs also the ID of the converged voice (or in any case clarify it is external)
--   tm <- choice [tempoMark, tempoMarks] <|> pure XTempo -- 
--   rhyORdurLoop <- choice [try rhythmicWrapper, durWrapper] 
--   -- the result is where you build a func that determines if canonic or not
--   pure $ singleton (fst p) $ Temporal (snd p) (fst rhyORdurLoop) (snd rhyORdurLoop)


-- can1[1:10] <- mu[30] [300cpm, 1.5cps, 500cpm]

-- can1-0[10] <- can1-1[10]    300cpm 
-- can1-1[10] <- mu[30]        1.5cps
-- can1-2[10] <- can1-1[10]    500cpm 

-- --overwriting:
-- can1-2[1:13] <- mi[20] [200cpm, 150cpm] ...

-- can1-2-0[13] <- can1-2-1[13]   200cpm
-- can1-2-1[13] <- mi[20]      150cpm

-- can1-1 <- can1-0 ... -- should fail check!
-- can1-1 <- can1-2 ... -- should fail the check!
-- can1-1 <- mu ... should pass check
-- can1-1 <- can1-2-0 ... should pass the check
-- can1-



g:: List Number -> List Number -> Maybe Number
g x y = do
  m <- x!!0
  n <- y!!2
  pure (m + n)

h:: String -> Tuple Int ConvergeFrom -> String -> Tuple Int ConvergeTo -> List TempoMark -> Map String Polytemporal
h id vIndcFrom idTo vIndcTo tms = 
  



-- Converge String ConvergeTo ConvergeFrom TempoMark |
-- func:: String -> Tuple Int ConvergeFrom -> Tuple Int ConvergeTo -> List TempoMark -> Map String Polytemporal
func id cf idct ct tm = mainVoice -- map funca tm
  where tmWithId = map (\n -> Tuple (id <> "-" <> show n) (f n)) (0..(L.length tm - 1))
          where f n = case tm!!n of
                        Nothing -> "not possible" -- for now would be String
                        Just x -> x   
        mainVoice = case tmWithId!!(fst cf) of
                      Nothing -> Tuple "" ""
                      Just x -> x
        otherVoices = L.filter (\ x -> x /= mainVoice) tmWithId
        -- assambleConvergences = map (\(Tuple id tm) -> Converge (fst mainVoice) (snd cf) (snd ct)) otherVoices
        -- mainVoice converges with convergeTo, remember its id
        -- other voices converge with mainVoice


-- can1[1:10] <- mu[30] [300cpm, 1.5cps, 500cpm]

-- can1-0[10] <- can1-1[10]    300cpm 
-- can1-1[10] <- mu[30]        1.5cps
-- can1-2[10] <- can1-1[10]    500cpm 


----       mu 1/4 = 120bpm | rhythm :| YES
----      mu[25%] 1/4 = 120bpm | rhythm :| YES
----  mu[33-2.1] <- [<< 50] 300cpm  | rhythm :| YES
----     mu[33-2.1] <- [<> 10] 3cps | rhythm :| YES
----      mu <- mi (mi 5:4) | rhythm :| YES
---- mu[50] <- mi[>> 10%4] 1/8 = 150bpm | rhythm :| YES
---- mu[50] <- (-10 secsFromEval) | rhythm :| YES
 
-------- <> = round // >> = after // << = before

-- cFrom:: P (Tuple Int ConvergeFrom)
-- cFrom = do
--   _ <- pure 1
--   voiceIndex <- voiceIndex <|> pure 0
--   cF <- choice [try cFromLast, try cFromPercen, try cFromProcess, try cFromStructure]
--   pure $ Tuple voiceIndex cF


-- voiceIndex:: P Int
-- voiceIndex = do
--   _ <- pure 1
--   n <- integer
--   _ <- reserved ":"
--   pure n

-- tempoMark:: P TempoMark
-- tempoMark = do
--   _ <- pure 1
--   x <- choice [try cpm, try bpm, try cps, try ratio, acceleration]
--   pure x

-- tempoMarks:: P TempoMark
-- tempoMarks = do
--   _ <- pure 1
--   x <- brackets $ many tempoMark
--   pure x

-- --
-- voiceId:: P String 
-- voiceId = do
--     _ <- pure 1
--     x <- identifier -- <|> stringLiteral-- many $ noneOf ['\\','<',' ']
--     pure x

-- rhythmicWrapper:: P (Tuple Rhythmic Boolean) 
-- rhythmicWrapper = do
--   _ <- charWS '|'
--   r <- rhythmic
--   l <- choice [(strWS "||" *> pure false), (strWS ":|" *> pure true)]
--   pure $ Tuple r l

-- durWrapper:: P (Tuple Rhythmic Boolean) 
-- durWrapper = do
--   _ <- charWS '<'
--   r <- rhythmic -- substitute this for duration when duration ready
--   l <- choice [(strWS ">" *> pure false), (strWS ":>" *> pure true)]
--   pure $ Tuple r l









charWS:: Char -> P Char
charWS x = do
  _ <- pure 1
  x <- char x 
  whitespace
  pure x

strWS:: String -> P String
strWS x = do
  _ <- pure 1
  x <- string x 
  whitespace
  pure x

tokenParser = makeTokenParser haskellStyle
parens      = tokenParser.parens
braces      = tokenParser.braces
identifier  = tokenParser.identifier
reserved    = tokenParser.reserved
naturalOrFloat = tokenParser.naturalOrFloat
natural = tokenParser.natural
float = tokenParser.float
whitespace = tokenParser.whiteSpace
colon = tokenParser.colon
brackets = tokenParser.brackets
comma = tokenParser.comma
semi = tokenParser.semi
integer = tokenParser.integer
stringLiteral = tokenParser.stringLiteral
reservedOp = tokenParser.reservedOp
