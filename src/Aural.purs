module Aural(aural) where

import Prelude

import Data.Identity
import Data.List (List(..), head, tail, elem, (:), filter, fromFoldable)
import Data.Array (fromFoldable) as A
import Data.Either
import Data.Int
import Data.Tuple (Tuple(..), fst, snd)
import Data.Map (Map(..), lookup, keys, singleton, toUnfoldable, member)
import Data.Map (fromFoldable) as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String as Str

import Data.FunctorWithIndex (mapWithIndex)

import Data.String.CodeUnits (fromCharArray)

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Combinators.Array (many)
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)

import AST
import Rhythm

type P = ParserT String Identity

aural:: P Expression
aural = do 
    _ <- pure 1
    x <- values
    _ <- reserved ";"
    pure $ AuralExpression x -- (Map Strg Aural)

values:: P (Map String Aural)
values = do
    _ <- pure 1
    id <- voiceId
    xs <- many value
    pure $ singleton id (fromFoldable xs)

value:: P Value
value = do
    _ <- pure 1
    _ <- reservedOp "."
    valType <- choice [try sound,n]
    pure valType

n:: P Value
n = do
    _ <- pure 1
    _ <- choice [reserved "n"]
    _ <- reservedOp "="
    n <- choice [try makeN, transposeN]
    pure n

sound:: P Value
sound = do
    _ <- pure 1
    _ <- choice [try $ reserved "sound",reserved "s"]
    _ <- reservedOp "="
    sound <- choice [try makeSound, transposeSound]
    pure sound

transposeN:: P Value
transposeN = do
    id <- voiceId
    pure $ TransposedN id

makeN:: P Value
makeN = do
    _ <- pure 1
    sp <- parseSpan
    strList <- many natural 
    pure $ N sp $ fromFoldable strList


transposeSound:: P Value
transposeSound = do
    id <- voiceId
    pure $ TransposedSound id

makeSound:: P Value
makeSound = do
    _ <- pure 1
    sp <- parseSpan
    strList <- sampleParser 
    pure $ Sound sp strList

parseSpan:: P Span
parseSpan = do
    _ <- pure 1
    x <- choice [
                   reserved "-_" *>  pure CycleInBlock
                 , try $ reserved "_-" *>  pure CycleBlock
                 , try $ reserved "_-_" *> pure SpreadBlock
                 , reserved "_" *>   pure  CycleEvent
                ]
    pure x

sampleParser:: P (List String)
sampleParser = do
    sampleNames <- stringLit
    pure $ stringToSamples sampleNames

stringToSamples:: String -> List String -- what to do with commas??
stringToSamples s = fromFoldable $ Str.split (Str.Pattern " ") $ Str.trim s

voiceId:: P String 
voiceId = do
    _ <- pure 1
    x <- identifier
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
stringLit = tokenParser.stringLiteral


toNumber':: Either Int Number -> Number
toNumber' (Left x) = toNumber x 
toNumber' (Right x) = x


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