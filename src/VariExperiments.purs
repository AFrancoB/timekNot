module VariExperiments where

import Prelude

import Data.Either
import Data.Identity
import Data.List hiding (many,take)
import Data.List.Lazy (replicate,repeat,take)
import Data.Foldable (foldl)
import Data.Int
import Data.Number (pow)
import Data.Rational (Rational(..), toRational, fromInt, (%))
import Data.Tuple
import Data.String (singleton, joinWith)
import Data.Maybe hiding (optional)
import Data.Functor
import Control.Monad
import Data.List.NonEmpty (toList)
import Data.Map as M
import Data.String as Str
import Data.Rational (Rational(..), toRational, fromInt, (%))

import Effect (Effect)
import Effect.Console (log)

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Combinators as C
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)

import AST
import TimePacketOps (metricToTempoMark)

type P = ParserT String Identity 

-- this is what needs to be implemented integrated with what I have named Variant:
-- expressions that can do basic math, DONE
-- expressions within a list creating a VList DONE
-- expressions that combine lists with expressions like: 10 * [1,2,3] should return [10,20,30]
-- furhtermore: 10 * [1,2,3] /15 should return [0.666, 1.333, 2.0] // TO DO
-- expressions that combine lists with lists:
-- [1,2,3] + [10,20,30]
-- what should be the result of this: zipWith style? or punctual style?
-- zipWith returns: [11,22,33]
-- punctual: [11,21,31,12,22,32,13,23,33] // TO THINK ABOUT AND TO DO

-- and the key to make this work properly: 
-- integrate TempoMark and the lists within Value

-- where variant notation will be useful:
-- 1) in the new tempo marks.
-- 2) to canonise the lists of values in the aural parser.
-- 3) in the duration notation.
-- 4) creating taylor made tuning systems


data Vari = VString String | VNum Number | VInt Int | VList (List Vari)

instance variShow :: Show Vari where
  show (VString st) = st 
  show (VNum x) = show x 
  show (VInt n) = show n 
  show (VList xs) = show xs


varExpr:: P (List Vari)
varExpr = do
  _ <- pure 1
  whitespace
  xs <- brackets $ expr `sepBy` comma
  eof
  pure $ fromFoldable xs

expr:: P Vari
expr = do
  _ <- pure 1
  whitespace
  term `chainl1` addSubOpV

term:: P Vari
term = do 
  _ <- pure 1 
  whitespace
  power `chainl1` mulDivOpV

power:: P Vari
power = do 
  _ <- pure 1 
  whitespace 
  factor `chainr1` expOpV

factor:: P Vari
factor = do
  _ <- pure 1 
  whitespace
  try (parens expr) <|> parseNumVari

addSubOpV:: P (Vari -> Vari -> Vari)
addSubOpV =  do
  _ <- pure 1
  whitespace
  (char '+' *> pure (addVari))
  <|> (char '-' *> pure (subVari))

mulDivOpV:: P (Vari -> Vari -> Vari)
mulDivOpV =  do
  _ <- pure 1
  whitespace
  (char '*' *> pure (mulVari))
  <|> (char '/' *> pure (divVari))

expOpV:: P (Vari -> Vari -> Vari)
expOpV = do
  _ <- pure 1
  whitespace
  (char '^' *> pure (powVari))

addVari:: Vari -> Vari -> Vari
addVari (VNum x) (VNum y) = VNum (x+y) 
addVari _ _ = VString "addVari failed" 

subVari:: Vari -> Vari -> Vari
subVari (VNum x) (VNum y) = VNum (x-y) 
subVari _ _ = VString "subVari failed"

mulVari:: Vari -> Vari -> Vari
mulVari (VNum x) (VNum y) = VNum (x*y) 
mulVari _ _ = VString "mulVari failed"

divVari:: Vari -> Vari -> Vari
divVari (VNum x) (VNum y) = VNum (x/y) 
divVari _ _ = VString "divVari failed"

powVari:: Vari -> Vari -> Vari
powVari (VNum x) (VNum y) = VNum (pow x y) 
powVari _ _ = VString "powVari failed"

parseNumVari:: P Vari
parseNumVari = do
  _ <- pure 0
  whitespace
  x <- choice [try (toNumber' <$> naturalOrFloat), try (parens negNum)]
  pure $ VNum x

negNum:: P Number
negNum = do
  _ <- charWS '-'
  x <- naturalOrFloat
  pure ((-1.0) * toNumber' x)

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


-- trasnform this parser to get rationals rather than numbers
fromEitherToRat:: Either Int Number -> Rational
fromEitherToRat x = toRat $ toNumber' x

toNumber':: Either Int Number -> Number
toNumber' (Left x) = toNumber x 
toNumber' (Right x) = x

toRat:: Number -> Rational
toRat x = 
    let pFact = 1000000
        floored = floor x -- 12
        fract = x - (toNumber floored) -- 12.5 - 12.0 = 0.5
        fract' = round $ fract * (toNumber pFact) -- 500000
    in (floored%1) + (fract'%pFact) -- 12 + (500000%1000000)


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