module Duration where

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

---- Aural:

-- can.sound = "hh cp bd me" *> ["drum hat pum pas", "hi cp", "808"]  .n = 0 1 2 3 4 5 * [2, 10] .segah = 0 3 2 4 5 + [7,14,(-7)]







---- math operations:

durExpr:: P (List Number)
durExpr = do
  _ <- pure 1
  whitespace
  xs <- many expr
  eof
  pure $ fromFoldable xs

expr:: P Number
expr = do
  _ <- pure 1
  whitespace
  term `chainl1` addSubOp

term:: P Number
term = do 
  _ <- pure 1 
  whitespace
  power `chainl1` mulDivOp

power:: P Number
power = do 
  _ <- pure 1 
  whitespace 
  factor `chainr1` expOp

factor:: P Number
factor = do
  _ <- pure 1 
  whitespace
  try (parens expr) <|> parseNumber

addSubOp:: P (Number -> Number -> Number)
addSubOp =  do
  _ <- pure 1
  whitespace
  (char '+' *> pure (+))
  <|> (char '-' *> pure (-))

mulDivOp:: P (Number -> Number -> Number)
mulDivOp =  do
  _ <- pure 1
  whitespace
  (char '*' *> pure (*))
  <|> (char '/' *> pure (/))

expOp:: P (Number -> Number -> Number)
expOp = do
  _ <- pure 1
  whitespace
  (char '^' *> pure (pow))

parseNumber:: P Number
parseNumber = do
  _ <- pure 0
  whitespace
  choice [
  try (toNumber' <$> naturalOrFloat),
  try (parens negNum)
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





---- qwerty notation:

qwertyRange:: P (List Number)
qwertyRange = do 
  _ <- pure 1
  x <- parseNumber
  _ <- reserved ","
  y <- parseNumber
  _ <- reserved "|"
  ns <- many qwertyVal
  _ <- reserved "|"
  pure $ map (\n -> x + (n*(y-x))) $ fromFoldable ns

qwertyVal:: P Number
qwertyVal = do 
  _ <- pure 1
  x <- choice [char 'q' *> pure (0.0/9.0), char 'w' *> pure (1.0/9.0), char 'e' *> pure (2.0/9.0), char 'r' *> pure (3.0/9.0), char 't' *> pure (4.0/9.0), char 'y' *> pure (5.0/9.0), char 'u' *> pure (6.0/9.0), char 'i' *> pure (7.0/9.0), char 'o' *> pure (8.0/9.0), char 'p' *> pure (9.0/9.0)]
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


-- to understand functional programming better:

data Box a = Box a | Boxes (List a)

instance showBox :: Show a => Show (Box a) where
  show :: Box a -> String
  show (Box a) = "box " <> show a
  show (Boxes xs) = "boxes " <> show xs

instance eqBox :: Eq a => Eq (Box a) where
  -- eq :: Box a -> boolean
  eq (Box a) (Box b) = a == b
  eq (Boxes xs) (Boxes ys) = xs == ys
  eq _ _ = false

instance numBox :: Semiring a => Semiring (Box a) where
  add (Box x) (Box y) = Box (x + y)
  add (Box x) (Boxes xs) = Boxes (map (\n -> x + n) xs)
  add (Boxes xs) (Box y) = Boxes (map (\n -> y + n) xs)
  add (Boxes xs) (Boxes ys) = Boxes $ zipWith (\x y -> x + y) xs ys
  mul (Box x) (Box y) = Box (x * y)
  mul (Box x) (Boxes xs) = Boxes (map (\n -> x * n) xs) 
  mul (Boxes xs) (Box y) = Boxes (map (\n -> y * n) xs) 
  mul (Boxes xs) (Boxes ys) = Boxes $ zipWith (\x y -> x * y) xs ys
  zero = Box zero 
  one = Box one   
  
-- instance monBox :: Monoid a => Monoid (Box a) where
--   append (Box x) (Box y) = Box (x <> y)
--   append (Box x) (Boxes ys) = Box (x <> ys)

instance Functor Box where
  map :: forall a b. (a -> b) -> Box a ->  Box  b
  map                 f         (Box a) =  Box (f a)
  map                 f       (Boxes xs) = Boxes (map f xs)



-- instance Apply Box where
--   apply :: forall a b. Box (a -> b) -> Box a -> Box  b
--   apply               (Box   f  )     (Box a)    = Box (f a)
--   apply               (Box   f  )    (Boxes xs)  = Boxes (map f xs) 
--   apply                    _             _       = zero
--   -- apply               (Boxes fs )     (Box a)    = Box a
--   -- apply               (Boxes fs )    (Boxes xs)  = Boxes xs

-- instance Applicative Box where
--   pure :: forall a. a -> Box a
--   pure              a =  Box a

-- instance Bind Box where
--   bind :: forall a b.  Box a -> (a -> Box b) -> Box b
--   bind                (Box a)    f            = f a

-- instance Monad Box


-- Box a -> (a -> Box b) -> Box b

-- f:: String -> Box String
-- f a = pure (a <> "!!!")
