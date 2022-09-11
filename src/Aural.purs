module Aural(samples, aural) where

import Prelude hiding (between)
import Prim.Boolean

import Data.Either
import Data.Identity
import Data.Array as Arr
import Data.List.Lazy as Lz
import Data.List hiding (many)
import Data.Typelevel.Bool
import Data.Int
import Data.Tuple
import Data.Tuple.Nested
import Data.String as Str

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

type P = ParserT String Identity


aural:: P (List Aural)
aural = do
    x <- many $ choice [nums, samples]
    pure $ fromFoldable x

nums:: P Aural 
nums = do
    x <- choice [try nSeq,try nSeq']
    pure x

nSeq:: P Aural
nSeq = do 
    _ <- string "nSeq"
    whitespace
    x <- nParser
    pure $ N x EventI

nSeq':: P Aural
nSeq' = do 
    _ <- string "nSeq\'"
    whitespace
    x <- nParser
    pure $ N x PassageI

nParser:: P (List Int)
nParser = do
    x <- brackets $ many integer
    pure $ fromFoldable x

samples:: P Aural
samples = do
    x <- choice [sampleSeq',sampleSeq]
    pure x

sampleSeq':: P Aural
sampleSeq' = do
    _ <- string "sampleSeq\'"
    whitespace
    x <- sampleParser
    pure $ Sample x PassageI

sampleSeq:: P Aural
sampleSeq = do
    _ <- string "sampleSeq"
    whitespace
    x <- sampleParser
    pure $ Sample x EventI

sampleParser:: P (List String)
sampleParser = do
    sampleNames <- stringLit
    pure $ stringToSamples sampleNames

stringToSamples:: String -> List String -- what to do with commas??
stringToSamples s = fromFoldable $ Str.split (Str.Pattern " ") $ Str.trim s

-- get the repetition pattern (which si the length of the array of first value of) so if the array is 4 samples long, seSamples [bd bd cp bd] entonces cada 4 (eventos, metros o pasajes empieza una serie nueva de samples. Luego sumarle a ese numero la posicion del inicio del ciclo, es decir: 0,4,8,12,16 es el inicio de cada ciclo entonces: 0+3,4+3,8+3,12+3 es la posicion del clap













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
