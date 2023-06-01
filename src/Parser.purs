module Parser(parseTop) where

import Prelude

import Data.Either
import Data.Identity
import Data.List hiding (many,take)
import Data.List.Lazy (replicate,repeat,take)
import Data.Foldable (foldl)
import Data.Int
import Data.Tuple
import Data.String (singleton, joinWith)
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

import AST
import Motor 
import Helpers

type P = ParserT String Identity 

parseTop:: P Program 
parseTop = do
  _ <- pure 1
  whitespace
--  w <- parsePolyTemporality <|> (pure "hola")
  x <- parseTopRhythmic <|> (pure $ Tuple O false)
  y <- parseTopAural <|> (pure $ S ("" : Nil) ByEvent : Nil) 
  eof
  pure $ Program (fst x) (snd x) y

--
parseTopAural:: P (List Aural)
parseTopAural = do
  _ <- pure 1
  x <- choice [try parseSound]
  pure $ x : Nil

parseSound:: P Aural
parseSound = do
  _ <- pure 1
  x <- choice [(strWS "sXe" *> pure ByEvent), (strWS "sXr" *> pure ByRefrain)]
  samples <- stringLit
  pure $ S (stringToSamples samples) x

stringToSamples:: String -> List String -- what to do with commas??
stringToSamples s = fromFoldable $ Str.split (Str.Pattern " ") $ Str.trim s
--

parseTopRhythmic:: P (Tuple Rhythmic Boolean)
parseTopRhythmic = do
  _ <- pure 1
  x <- choice [try parseRhythmList, try parseSD, try parseRepeat, parseXO]
  y <- choice [(strWS "||" *> pure false), (strWS ":|" *> pure true)]
  pure $ Tuple x y

parseRhythms:: P Rhythmic
parseRhythms = do
  _ <- pure 1
  choice [try parseRepeat, try parseRhythmList, try parseSD, try parseRepeat, parseXO]

parseRhythmList:: P Rhythmic
parseRhythmList = do
  _ <- pure 1
  x <- parseXOorSDorRep
  xs <- toList <$> many1 parseXOorSDorRep
  pure $ Rhythmics $ x:xs

parseXOorSDorRep:: P Rhythmic
parseXOorSDorRep = do
  _ <- pure 1
  choice [try parseSD, try parseRepeat, parseXO]

parseSD:: P Rhythmic
parseSD = do
  _ <- pure 1
  _ <- charWS '['
  x <- parseRhythms
  _ <- charWS ']'
  pure $ Sd x

parseRepeat:: P Rhythmic
parseRepeat = do
  _ <- pure 1
  _ <- charWS '!'
  x <- parseRhythms
  _ <- charWS '#'
  y <- integer
  pure $ Repeat x y

parseXO:: P Rhythmic
parseXO = do
  _ <- pure 1
  x <- choice [charWS 'x' *> pure X, charWS 'o' *> pure O]
  pure x


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
float = tokenParser.float
whitespace = tokenParser.whiteSpace
colon = tokenParser.colon
brackets = tokenParser.brackets
comma = tokenParser.comma
semi = tokenParser.semi
integer = tokenParser.integer
stringLit = tokenParser.stringLiteral