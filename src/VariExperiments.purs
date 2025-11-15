module VariExperiments where

import Prelude

import Data.Either
import Data.Identity
import Data.List hiding (many,take)
import Data.List.Lazy (replicate,repeat,take)
import Data.Foldable (foldl)
import Data.Int
import JS.BigInt (fromInt) as BI
import Data.Number (pow)
import Data.Rational (Rational(..), toRational, fromInt, (%), numerator, denominator)
import Data.Rational (toNumber) as R
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


data Vari = VString String | VNum Number | VInt Int | VList (List Vari) | VTempo TempoMark

instance variShow :: Show Vari where
  show (VString st) = st 
  show (VNum x) = show x 
  show (VInt n) = show n 
  show (VList xs) = show xs
  show (VTempo t) = show t


listExpr:: P Vari
listExpr = do
  _ <- pure 1
  whitespace
  xs <- brackets $ expr `sepBy` comma
  pure $ VList (fromFoldable xs)

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
  try (parens expr) <|> parseVari

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

tempoVarVarAdd:: TempoMark -> Vari -> Vari
tempoVarVarAdd (CPM r) (VNum x) =  VTempo $ CPM (r + (toRat x))
tempoVarVarAdd _ _ = VString "failed at tempoVarVarAdd"

addVari:: Vari -> Vari -> Vari
addVari (VNum x) (VNum y) = VNum (x+y) 
addVari (VNum n) (VList xs) = VList $ map (\x -> addVari (VNum n) x) xs
addVari (VList xs) (VNum n) = VList $ map (\x -> addVari (VNum n) x) xs
addVari (VTempo t) (VNum x) = tempoVarVarAdd t (VNum x)
addVari (VNum x) (VTempo t) = tempoVarVarAdd t (VNum x)
addVari (VTempo t) (VList xs) = VList $ map (\x -> tempoVarVarAdd t x) xs
addVari (VList xs) (VTempo t) = VList $ map (\x -> tempoVarVarAdd t x) xs 
addVari _ _ = VString "addVari failed" 

subVari:: Vari -> Vari -> Vari
subVari (VNum x) (VNum y) = VNum (x-y) 
subVari (VNum n) (VList xs) = VList $ map (\x -> subVari (VNum n) x) xs
subVari (VList xs) (VNum n) = VList $ map (\x -> subVari x (VNum n)) xs
subVari (VTempo t) (VNum x) = tempoVarVarAdd t (VNum (x*(-1.0)))
subVari (VNum x) (VTempo t) = tempoVarVarAdd (changeSignTempo t) (VNum x)
subVari (VTempo t) (VList xs) = VList $ map (\x -> tempoVarVarAdd t x) xs
subVari (VList xs) (VTempo t) = VList $ map (\x -> tempoVarVarAdd t x) xs
subVari _ _ = VString "subVari failed"

changeSignTempo:: TempoMark -> TempoMark
changeSignTempo (CPM r) = CPM (r * (toRat (-1.0)))
changeSignTempo _ = CPM $ toRat 0.0

tempoVarVarMul:: TempoMark -> Vari -> Vari
tempoVarVarMul (CPM r) (VNum x) =  VTempo $ CPM (r * (toRat x))
tempoVarVarMul _ _ = VString "failed at tempoVarVarMul"

mulVari:: Vari -> Vari -> Vari
mulVari (VNum x) (VNum y) = VNum (x*y) 
mulVari (VNum n) (VList xs) = VList $ map (\x -> mulVari (VNum n) x) xs
mulVari (VList xs) (VNum n) = VList $ map (\x -> mulVari (VNum n) x) xs
mulVari (VTempo t) (VNum x) = tempoVarVarMul t (VNum x)
mulVari (VNum x) (VTempo t) = tempoVarVarMul t (VNum x)
mulVari (VTempo t) (VList xs) = VList $ map (\x -> tempoVarVarMul t x) xs
mulVari (VList xs) (VTempo t) = VList $ map (\x -> tempoVarVarMul t x) xs
mulVari _ _ = VString "mulVari failed"

tempoVarVarDiv:: TempoMark -> Vari -> Vari
tempoVarVarDiv (CPM r) (VNum x) =  VTempo (CPM $ divRatioNum r x)
tempoVarVarDiv _ _ = VString "failed at tempoVarVarMul"

tempoVarVarDiv':: TempoMark -> Vari -> Vari
tempoVarVarDiv' (CPM r) (VNum x) =  VTempo (CPM $ divNumRatio x r)
tempoVarVarDiv' _ _ = VString "failed at tempoVarVarMul"

divRatioNum:: Rational -> Number -> Rational
divRatioNum r x = r * (toRational (den*signedBI) num)
  where signed = if x < 0.0 then (-1) else 1
        signedBI = BI.fromInt signed 
        newX = toRat (x*(toNumber signed)) 
        (Tuple num den) = Tuple (numerator newX) (denominator newX)

divNumRatio:: Number -> Rational -> Rational
divNumRatio x r = (toRat x) * (toRational (den*signedBI) num)
  where signed = if r < (0%0) then (-1) else 1 
        signedBI = BI.fromInt signed 
        (Tuple num den) = Tuple (numerator r) (denominator r) 

divVari:: Vari -> Vari -> Vari
divVari (VNum x) (VNum y) = VNum (x/y) 
divVari (VNum n) (VList xs) = VList $ map (\x -> divVari (VNum n) x) xs
divVari (VList xs) (VNum n) = VList $ map (\x -> divVari (VNum n) x) xs
divVari (VTempo t) (VNum x) = tempoVarVarDiv t (VNum x)
divVari (VNum x) (VTempo t) = tempoVarVarDiv' t (VNum x)
divVari (VTempo t) (VList xs) = VList $ map (\x -> tempoVarVarDiv t x) xs
divVari (VList xs) (VTempo t) = VList $ map (\x -> tempoVarVarDiv' t x) xs
divVari _ _ = VString "divVari failed"

tempoVarVarPow:: TempoMark -> Vari -> Vari
tempoVarVarPow (CPM r) (VNum x) = VTempo (CPM $ toRat $ pow (R.toNumber r) x)
tempoVarVarPow _ _ = VString "failed at tempoVarVarMul"

tempoVarVarPow':: TempoMark -> Vari -> Vari
tempoVarVarPow' (CPM r) (VNum x) = VTempo (CPM $ toRat $ pow x (R.toNumber r))
tempoVarVarPow' _ _ = VString "failed at tempoVarVarMul"

powVari:: Vari -> Vari -> Vari
powVari (VNum x) (VNum y) = VNum (pow x y) 
powVari (VNum n) (VList xs) = VList $ map (\x -> powVari (VNum n) x) xs
powVari (VList xs) (VNum n) = VList $ map (\x -> powVari (VNum n) x) xs
powVari (VTempo t) (VNum x) = tempoVarVarPow t (VNum x)
powVari (VNum x) (VTempo t) = tempoVarVarPow' t (VNum x)
powVari (VTempo t) (VList xs) = VList $ map (\x -> tempoVarVarPow t x) xs
powVari (VList xs) (VTempo t) = VList $ map (\x -> tempoVarVarPow' t x) xs
powVari _ _ = VString "powVari failed"

parseVari:: P Vari
parseVari = do
  _ <- pure 0
  whitespace
  choice [VTempo <$> try tempoMark, try (toVariant <$> naturalOrFloat), try (VNum <$> (parens negNum)), listExpr]

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




-- tempo marks
tempoMark:: P TempoMark
tempoMark = do
  _ <- pure 1
  x <- choice [try cpm, try bpm, try cps, try ratio, acceleration]
  pure x

acceleration:: P TempoMark -- (~ 1 << 0 range 100cpm, 1000cpm)
acceleration = do 
  _ <- pure 1
  _ <- reserved "sin"
  freq <- toNumber' <$> naturalOrFloat
  _ <- reserved "phase:"
  ph <- toNumber' <$> naturalOrFloat
  _ <- reservedOp "range"
  max <- choice [try cpm, try bpm, try cps, try ratio]
  _ <- reservedOp ","
  min <- choice [try cpm, try bpm, try cps, try ratio]
  pure $ Sin {osc: toRat freq, min: min, max: max, phase: toRat ph}

cpm:: P TempoMark 
cpm = do
  _ <- pure 1
  x <- toNumber' <$> naturalOrFloat
  _ <- reserved "cpm"
  pure $ CPM (toRat x)

bpm:: P TempoMark 
bpm = do
  _ <- pure 1
  fig <- figure
  _ <- charWS '='
  x <- toNumber' <$> naturalOrFloat
  _ <- reserved "bpm"
  pure $ BPM (toRat x) fig

figure:: P Rational
figure = do
  n <- natural
  _ <- charWS '/'
  d <- natural 
  pure $ toRational n d

cps:: P TempoMark
cps = do
  _ <- pure 1
  x <- toNumber' <$> naturalOrFloat
  _ <- reserved "cps"
  pure $ CPS (toRat x)

ratio:: P TempoMark
ratio = do
  _ <- pure 1 
  id <- voiceId
  indexID <- indexIDParse <|> pure "0"
  x <- natural
  _ <- reservedOp ":"
  y <- natural
  pure $ Prop (id <> "-" <> indexID)  x y

indexIDParse:: P String -- you are here
indexIDParse = do
  _ <- pure 1
  n <- brackets $ natural
  pure $ show n

voiceId:: P String 
voiceId = do
    _ <- pure 1
    x <- identifier -- many $ noneOf ['\\','<',' ']
    pure x

-- trasnform this parser to get rationals rather than numbers
fromEitherToRat:: Either Int Number -> Rational
fromEitherToRat x = toRat $ toNumber' x

toVariant:: Either Int Number -> Vari
toVariant (Left n) = VNum $ toNumber n 
toVariant (Right x) = VNum x

negVariant:: Either Int Number -> Vari
negVariant nx = VNum $ (-1.0) * toNumber' nx 

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
reservedOp = tokenParser.reservedOp