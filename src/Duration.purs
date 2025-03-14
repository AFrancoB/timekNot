module Duration where

import Prelude

import Data.Either
import Data.Identity
import Data.List hiding (many,take)
import Data.List.Lazy (replicate,repeat,take)
import Data.Foldable (foldl)
import Data.Int
import Data.Number (pow)
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

type P = ParserT String Identity 


data Dur a = Dur' a

-- type Signal a = Tempo -> NominalDiffTime -> UTCTime -> UTCTime -> UTCTime -> a

instance durShow:: Show (Dur a) where
  show (Dur' a) = "dur"

instance durFunctor:: Functor Dur where
  map f (Dur' a) = Dur' (f a)

instance durApply:: Apply Dur where
  apply (Dur' fn) a = (fn <$> a)

-- instance applyMaybe :: Apply Maybe where
--   apply (Just fn) x = fn <$> x
--   apply Nothing   _ = Nothing

instance durApplicative:: Applicative Dur where
  pure = Dur'

addDur :: Num a => Dur a -> Dur a -> Dur a
addDur (Dur x) (Dur y) = Dur (x + y)

-- instance durMonad:: Monad Dur where

{- instance Functor Signal where
  fmap f s = \t videoDur renderT evalT anchorT -> f (s t videoDur renderT evalT anchorT)

instance Applicative Signal where
  pure x = \_ _ _ _ _ -> x
  f <*> x = \t videoDur renderT evalT anchorT -> f t videoDur renderT evalT anchorT $ x t videoDur renderT evalT anchorT

instance Monad Signal where
  a >>= f = \t videoDur renderT evalT anchorT -> f (a t videoDur renderT evalT anchorT) a t videoDur renderT evalT anchorT
-}



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