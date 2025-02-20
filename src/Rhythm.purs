module Rhythm(rhythmic) where

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

type P = ParserT String Identity 

-- to do: implement int parser as follows:
-- | <4 4 3> :| -- this should generate: xoooxoooxoo
-- | <4 4 3 , 0> :| -- this should generate: xoooxoooxoo , separates the pattern from a rotation value
-- | <4 4 3 , 0, xx> :| -- this should generate: xxooooooxxooooooxxoooo, instead of assuming a pattern (X) the player gives one to the program
-- this should be added to Rhythmic as a constructor:
-- Numeric Rhythmic (Array Int) Int

rhythmic:: P Rhythmic
rhythmic = do
  _ <- pure 1
  choice [try parseRhythmList, try parseSD, try parseRepeat, try parseBjorklund, parseXO]

parseRhythms:: P Rhythmic
parseRhythms = do
  _ <- pure 1
  choice [try parseRhythmList, try parseSD, try parseBjorklund, try parseRepeat, parseXO]

parseRhythmList:: P Rhythmic
parseRhythmList = do
  _ <- pure 1
  x <- parseXOorSDorReporBjork
  xs <- toList <$> many1 parseXOorSDorReporBjork
  pure $ Rhythmics $ x:xs

parseXOorSDorReporBjork:: P Rhythmic
parseXOorSDorReporBjork = do
  _ <- pure 1
  choice [try parseSD, try parseRepeat, try parseBjorklund, parseXO]

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

parseBjorklund:: P Rhythmic
parseBjorklund = do
    _ <- pure 1
    x <- choice [try $ parens parseFull, try $ parens parseK, try $ parens parseSimpleBl, parseInv]
    pure x

parseFull:: P Rhythmic
parseFull = do
  _ <- pure 1
  kPatt <- parseRhythms
  _ <- comma
  invPatt <- parseRhythms
  _ <- comma
  k <- natural
  _ <- comma
  n <- natural
  _ <- optional comma
  o <- natural <|> pure 0
  pure $ Bjorklund (Full kPatt invPatt) k n o

parseSimpleBl:: P Rhythmic
parseSimpleBl = do
  _ <- pure 1
  k <- natural
  _ <- comma
  n <- natural
  _ <- optional comma
  o <- natural <|> pure 0
  pure $ Bjorklund Simple k n o

parseK:: P Rhythmic
parseK = do
  _ <- pure 1
  p <- bPattern 
  pure $ Bjorklund (K p.patt) p.k p.n p.rotate

parseInv:: P Rhythmic
parseInv = do
  _ <- pure 1
  _ <- string "'("
  p <- bPattern 
  _ <- string ")"
  pure $ Bjorklund (InvK p.patt) p.k p.n p.rotate

bPattern:: P {patt:: Rhythmic, k:: Int, n:: Int, rotate:: Int}
bPattern = do 
  _ <- pure 1
  patt <- parseRhythms
  _ <- comma
  k <- natural
  _ <- comma
  n <- natural
  _ <- optional comma
  o <- natural <|> pure 0
  pure {patt: patt, k: k, n: n, rotate: o}

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
natural = tokenParser.natural
float = tokenParser.float
whitespace = tokenParser.whiteSpace
colon = tokenParser.colon
brackets = tokenParser.brackets
comma = tokenParser.comma
semi = tokenParser.semi
integer = tokenParser.integer
stringLit = tokenParser.stringLiteral