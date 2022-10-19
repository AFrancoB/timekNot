module ParseIdeas where

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

import Effect (Effect)
import Effect.Console (log)

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)


-- 26th at 3:00 next meeting (rework the calendar so it becomes more linear)

type P = ParserT String Identity 

data Rhythmic = 
  X | -- x
  O |
  Sd Rhythmic | -- [x]
  Repeat Rhythmic Int |
  Rhythmics (List Rhythmic) -- xoxo
-- Bjorklund

instance Show Rhythmic where
  show X = "x"
  show O = "o"
  show (Sd xs) = "[" <> show xs <> "]"
  show (Repeat xs n) = "!" <> show xs <> "#" <> show n
  show (Rhythmics xs) = show xs

parseTop:: P Rhythmic
parseTop = do
  _ <- pure 1
  whitespace
  x <- choice [try parseRhythmList, try parseSD, try parseRepeat, parseXO]
  eof
  pure x

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

parseXO:: P Rhythmic
parseXO = do
  _ <- pure 1
  x <- choice [charWS 'x' *> pure X, charWS 'o' *> pure O]
  pure x

parseRepeat:: P Rhythmic
parseRepeat = do
  _ <- pure 1
  _ <- charWS '!'
  x <- parseRhythms
  _ <- charWS '#'
  y <- integer
  pure $ Repeat x y


charWS:: Char -> P Char
charWS x = do
  _ <- pure 1
  x <- char x 
  whitespace
  pure x

toNumber':: Either Int Number -> Number
toNumber' (Left x) = toNumber x 
toNumber' (Right x) = x

attachLast::forall a. a -> List a -> List a 
attachLast a xs = reverse (a :reverse xs)


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