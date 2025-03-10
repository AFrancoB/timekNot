module Duration where

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
import Data.Rational (Rational(..), toRational, fromInt, (%))

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


-- data Expr = Add Expr Expr
--           | Sub Expr Expr
--           | Lit Number

-- term:: P Expr
-- term = number -- <|> parens

-- op :: P (Expr -> Expr -> Expr)
-- op = (char '+' $> pure Add) <|> (char '-' $> pure Sub)

-- expr :: Parser Expr
-- expr = term >>= rest
--   where
--     rest x = (op >>= \f -> term >>= \y -> rest (f x y)) <|> pure x


-- I have math going on but precedence seems difficult to achieve...

addRat:: Rational -> Rational -> Rational
addRat x y = x + y

subRat:: Rational -> Rational -> Rational
subRat x y = x - y

mulRat:: Rational -> Rational -> Rational
mulRat x y = x * y

divRat:: Rational -> Rational -> Rational
divRat x y = x / y

oper:: P Rational
oper = do
  _ <- pure 1 
  _ <- whitespace
  x <- chainl1 (choice [try func, toRat <$> toNumber' <$> naturalOrFloat]) $ choice [(strWS "/" $> divRat), (strWS "*" $> mulRat),(strWS "-" $> subRat), (strWS "+" $> addRat)]
  pure x


func':: P (List Rational)
func' = do 
  _ <- pure 1
  x <- many func
  eof -- remove this when integrating to the program
  pure x

func:: P Rational
func = do
  _ <- pure 1
  x <- choice [toRat <$> try parseNumber, try $ parens oper, toRat <$> parens parseNumber ]
  pure x

--- negative Numbers
parseNumber:: P Number
parseNumber = choice [
  try $ parens (toNumber' <$> naturalOrFloat),
  try (toNumber' <$> naturalOrFloat),
  negNum
]

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