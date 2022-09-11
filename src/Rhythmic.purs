module Rhythmic (topPassageParser,endOfRhythmic) where

import Prelude
import Prim.Boolean

import Data.Either
import Data.Identity
import Data.Array as Arr
import Data.List.Lazy hiding (many,Pattern)
import Data.List as L
import Data.Typelevel.Bool
import Data.Int as I
import Data.Tuple
import Data.Tuple.Nested

import Data.Functor

import Data.Maybe hiding (optional)

import Control.Monad

import Effect (Effect)
import Effect.Console (log)

import Data.Rational
import Data.Ratio

import Data.NonEmpty as N

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)

import Data.DateTime
import Data.DateTime.Instant
import Data.Tempo
import Data.Enum
import Data.Map as M
import Partial.Unsafe

import AST
import Aural

type P = ParserT String Identity

topPassageParser:: P Passage
topPassageParser = do
    rhy <- topRhythmic
    whitespace
    aur <- samples
    whitespace 
    nose <- noseParser
    _ <- pure 1 
    eof
    pure $ Passage (fst rhy) (L.fromFoldable [aur]) nose $ snd rhy


noseParser:: P Nose
noseParser = do
    x <- choice [(try $ (string "eval")) *> (pure $ Eval),(try $ (string "origin")) *> (pure $ Origin)] <|> (pure $ Origin)
    _ <- pure 1
    pure x

-- topRhythmic:: P Rhythmic

-- relevant structures above this one:
-- Program = [Expression]
-- Expression = Expression Passage Polytemporal
-- Passage = Rhythmic Aural
-- Polytemporal = Canonic Voicing Nose Encounter

-- This file contains the data structures to parse the rhythmic notation that will be the head of the passages. It is based on binary events that are either rests or onsets. The data structure represents all of the possible ways in which these binary options can be invoked by the player. For now, the data structure is called Rhythmic and it should be recursive. It has 5 constructors. 
-- 1) Onset: Represents an event that can be a rest or an onset. 
-- 2) Subdivision: Represents an event that has nested Rhythmic and which the event's duration is going to be the reciprocal of the length of the list of rhythmics. Example: "xxox" are all onset and all have durations of 1. But "[xxox]" are of type ::Subdivision and all have durations of 1/4. "[x[ox]]" consists of an onset with a duration of 1/2, a rest with dur of 1/4 and an onset with dur of 1/4.
-- 3) Repetition: Represnts a Rhythmic that is repeated N times.
-- 4) Euclidean: Represents a Rhythmic that has k number of repetitions in a euclidean space of N.
-- x) Pattern: represents :: [Rhythmic], a list of Rhythmic structures


-- xxox xx[ox]x xx[o[xxx]]x xxox!2 xxox(3,8) [xx](3,8) -- this should be a valid pattern program -- not working

-- xxox (just [onset]) should be 1 1 ~1 1 
-- xx[ox]x should be 1 1 [~1/2 1/2] 1 -- onsets with one subdivision
-- xx[o[xxx]]x should be 1 1 [~1/2[1/6 1/6 1/6]] 1 -- nested subdivisions
-- xxox!2 should be 1 1 ~1 1 1 1 ~1 1 -- a repetition
-- xxox(3,8) should be 1 1 ~1 1 ~1 ~1 ~1 ~1 ~1 ~1 ~1 ~1 1 1 ~1 1 ~1 ~1 ~1 ~1 ~1 ~1 ~1 ~1 1 1 ~1 1 ~1 ~1 ~1 ~1 -- a euclidean
-- [xx(3,8)] should be [1/2 1/2] ~1 ~1 [1/2 1/2] ~1 ~1 [1/2 1/2] ~1 -- euclidean inside a subdivision
-- [xox] 
-- [xox(3,5)](3,4,1)

-- [xo xxxxxxxxxx xxo ooooxxx  xooxooxo]

-- chain!!!! check that out.
-- from complex to simple parsers


topRhythmic:: P (Tuple Rhythmic Boolean)
topRhythmic = do
  r <- choice [onsets]
  whitespace
  end <- endOfRhythmic
  _ <- pure 1 
  pure $ Tuple r end


endOfRhythmic:: P Boolean
endOfRhythmic = do
    x <- choice [(string ":||") *> (pure $ true),(string "||") *> (pure $ false)]
    pure x

-- esto no sirve!!!
-- patterns:: P Rhythmic 
-- patterns = do
--     x <- chainl1 (choice [try onsets]) (char ' ' $> chainRhythms)
--     _ <- pure 1
--     pure x

-- chainRhythms:: Rhythmic -> Rhythmic -> Rhythmic
-- chainRhythms x y =  Patron $ snoc (fromFoldable [x]) y



onsets:: P Rhythmic
-- pattern = ... a List of Rhythmic with no whitespace ...
onsets = do
  xs <- many onset
  _ <- pure 1
  pure $ Onsets $ L.fromFoldable xs 

onset:: P Boolean
onset = do
    x <- choice [(oneOf ['x']) *> (pure $ true),(oneOf ['o']) *> (pure $ false)]
    _ <- pure 1
    pure x

 
-- patternise:: Rhythmic -> Rhythmic -> Rhythmic
-- patternise x y = Pattern $ concat $ fromFoldable [x,y] 

-- como disegnar un operador para dos onsets que luego se pueda generalisar para dos rhythmics?


--   euclideanElement -- [xxox](3,8) OR x(3,8)
--   repetitionElement -- [xxox]!3 OR x!3
--   some other rhythmic wrapped in [ ]
--   ...an x or an o... -> Onset
--   ...fails


-- parsePattern:: P Rhythmic
-- parsePattern = do
--   x <- choice [try parseSubDivision, try parseEuclidean, try parseRepetition, try parseOnset] `sepBy` whitespace
--   pure $ Pattern $ fromFoldable x


-- ---- parseSubDivision should be able to parse patterns
-- parseSubDivision:: P Rhythmic
-- parseSubDivision = do
--   _ <- string "["
--   x <- choice [try parseEuclidean, try parseRepetition, try parseOnset] `sepBy` whitespace
--   _ <- string "]"
--   pure $ Subdivision (Pattern $ fromFoldable x) $ fromFoldable (map  fromRhythmicToSubDivisions x)

-- fromRhythmicToSubDivisions:: Rhythmic -> Maybe Int
-- fromRhythmicToSubDivisions (Onset x) = Nothing
-- fromRhythmicToSubDivisions (Pattern x) = Just $ length x
-- fromRhythmicToSubDivisions (Euclidean on k n o) = Just n
-- fromRhythmicToSubDivisions (Repetition on n) = Just n
-- fromRhythmicToSubDivisions (Subdivision _ _) = Nothing

-- parseOnset:: P Rhythmic
-- parseOnset = do
--   x <- choice [(oneOf ['x']) *> (pure $ true),(oneOf ['o']) *> (pure $ false)]
--   pure $ Onset x

-- -- parseRepetition should be able to parse any other rhythm parser, in theory on should be: on <- parsePattern but it just does not work...
-- parseRepetition:: P Rhythmic
-- parseRepetition = do
--   on <- parseOnset
--   _ <- string "!"
--   n <- integer 
--   pure $ Repetition on n


-- -- data Euclidean = Full Rhythmic Rhythmic | K Rhythmic | InverseK Rhythmic
-- -- same here
-- parseEuclidean:: P Rhythmic
-- parseEuclidean = do
--     x <- choice [try $ parens parseFull, try $ parens parseK, try $ parens parseInv]
--     pure x


-- parseFull:: P Rhythmic
-- parseFull = do
--     kPatt <- parseOnset
--     _ <- comma
--     invPatt <- parseOnset
--     _ <- comma
--     k <- integer
--     _ <- comma
--     n <- integer
--     _ <- optional comma
--     o <- integer <|> pure 0
--     pure $ Euclidean (Full kPatt invPatt) k n o

-- parseK:: P Rhythmic
-- parseK = do
--     kPatt <- parseOnset
--     _ <- comma
--     k <- integer
--     _ <- comma
--     n <- integer
--     _ <- optional comma
--     o <- integer <|> pure 0
--     pure $ Euclidean (K kPatt) k n o

-- parseInv:: P Rhythmic
-- parseInv = do
--     _ <- string "inv"
--     whitespace
--     invPatt <- parseOnset 
--     _ <- comma
--     k <- integer
--     _ <- comma
--     n <- integer
--     _ <- optional comma
--     o <- integer <|> pure 0
--     pure $ Euclidean (InverseK invPatt) k n o



-- Event 

-- rhythmicTo




-- parseEuclidean:: P Rhythmic
-- parseEuclidean = do
--   on <- parseOnset 
--   kno <- parens $ parseKNO
--   pure $ Euclidean on (get1 kno) (get2 kno) (get3 kno)

-- parseKNO:: P (Tuple3 Int Int Int)
-- parseKNO = do
--   k <- integer
--   _ <- comma
--   n <- integer
--   _ <- optional comma
--   o <- integer <|> pure 0
--   pure $ tuple3 k n o






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