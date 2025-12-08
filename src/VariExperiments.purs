module VariExperiments (V(..), expr, addVari, mulVari, subVari, divVari, powVari, valToV) where

import Prelude

import Data.Either
import Data.Identity
import Data.List hiding (many,take)
import Data.List.Lazy (replicate,repeat,take)
import Data.List.Lazy as Lz
import Data.Foldable (foldl)
import Data.Int
import Data.Int (pow) as I
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
-- import TimePacketOps (metricToTempoMark)

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


data V = VString String | VNum Number | VInt Int | VList (List V) | VTempo TempoMark

instance variShow :: Show V where
  show (VString st) = st 
  show (VNum x) = show x 
  show (VInt n) = show n 
  show (VList xs) = show xs
  show (VTempo t) = show t


listExpr:: P V
listExpr = do
  _ <- pure 1
  whitespace
  xs <- brackets $ expr `sepBy` comma
  pure $ VList (fromFoldable xs)

expr:: P V
expr = do
  _ <- pure 1
  whitespace
  term `chainl1` addSubOpV

term:: P V
term = do 
  _ <- pure 1 
  whitespace
  factor `chainl1` mulDivOpV

-- power:: P V
-- power = do 
--   _ <- pure 1 
--   whitespace 
--   factor `chainr1` expOpV

factor:: P V
factor = do
  _ <- pure 1 
  whitespace
  try (parens expr) <|> parseVari

addSubOpV:: P (V -> V -> V)
addSubOpV =  do
  _ <- pure 1
  whitespace
  char '+' *> pure (addVari)
  <|> char '-' *> pure (subVari)

mulDivOpV:: P (V -> V -> V)
mulDivOpV =  do
  _ <- pure 1
  whitespace
  (char '*' *> pure mulVari)
  <|> (char '/' *> pure divVari)
  <|> (char '^' *> pure powVari)

-- expOpV:: P (V -> V -> V)
-- expOpV = do
--   _ <- pure 1
--   whitespace
--   (char '^' *> pure powVari)


parseVari:: P V
parseVari = do
  _ <- pure 0
  whitespace
  choice [try $ (parens fromThenTo), try $ (parens fromTo), VTempo <$> try tempoMark, try (toVariant <$> naturalOrFloat), try (VNum <$> (parens negNum)), listExpr]


fromTo:: P V
fromTo = do 
  _ <- pure 1
  whitespace
  from <- fromVToInt <$> try expr
  from <- liftEither from
  _ <- reservedOp "til"
  to <- fromVToInt <$> try expr
  to <- liftEither to 
  pure $ step (1 + (to-from)) 1.0 (toNumber from)

fromVToInt:: V -> Either String Int
fromVToInt (VInt n) = Right n
fromVToInt (VNum x) = Right $ round x
fromVToInt _ = Left "not accepted for til, only numbers"
-- needs a function that prevents non integeres to be parsed

fromThenTo:: P V
fromThenTo = do 
  _ <- pure 1
  whitespace
  from <- fromVToNum <$> expr
  from <- liftEither from
  _ <- reservedOp ","
  thenn <- fromVToNum <$> expr
  thenn <- liftEither thenn
  _ <- reservedOp "til"
  to <- fromVToNum <$> expr 
  to <- liftEither to
  pure $ step (1 + (floor $ (to-from)/(thenn-from))) (thenn-from) from  

fromVToNum:: V -> Either String Number
fromVToNum (VInt n) = Right $ toNumber n
fromVToNum (VNum x) = Right x
fromVToNum _ = Left "not accepted for til, only numbers"
-- needs a function that only allows ints and numbers to be parsed (no tempo for now, specially no lists)

step:: forall a. Int -> Number -> Number -> V
step numOfSteps stepSize startPoint = VList $ map (\n -> VNum n) ls
    where ls = fromFoldable $ Lz.take numOfSteps $ Lz.iterate (\nu -> nu + stepSize) startPoint


          -- ls = foldl (\l f -> f l) lista fs  -- this pattern matters, do not erase!!!!

-- foldl (\l f -> f l) (fromFoldable gainList) fs

-- step1:: TempoMark -> Rational -> Int -> List TempoMark
-- step1 (CPM n) upTo numSteps = step1CPM n upTo numSteps
-- step1 (CPS n) upTo numSteps = step1CPS n upTo numSteps
-- step1 (BPM n t) upTo numSteps = step1BPM n t upTo numSteps
-- step1 _ _ _ = Nil

-- step1CPM:: Rational -> Rational -> Int -> List TempoMark
-- step1CPM n upTo 0 = (CPM n) : Nil
-- step1CPM n upTo 1 = (CPM n) : (CPM (n+upTo)) : Nil 
-- step1CPM n upTo numSteps = step numSteps (stepSizeFromUpTo n upTo (toRational numSteps 1)) (CPM n)

-- step1CPS:: Rational -> Rational -> Int -> List TempoMark
-- step1CPS n upTo 0 = (CPS n) : Nil
-- step1CPS n upTo 1 = (CPS n) : (CPS (n+upTo)) : Nil 
-- step1CPS n upTo numSteps = step numSteps (stepSizeFromUpTo n upTo (toRational numSteps 1)) (CPS n)

-- step1BPM:: Rational -> Rational -> Rational -> Int -> List TempoMark
-- step1BPM n t upTo 0 = (BPM n t) : Nil
-- step1BPM n t upTo 1 = (BPM n t) : (BPM n (t+upTo)) : Nil 
-- step1BPM n t upTo numSteps = step numSteps (stepSizeFromUpTo t upTo (toRational numSteps 1)) (BPM n t)

-- stepSizeFromUpTo:: Rational -> Rational -> Rational -> Rational
-- stepSizeFromUpTo t upTo numOfSteps = ((t + upTo) - t) / (numOfSteps- (1%1)) 

-- a.n = 0 1 2 3 * [0,1,2,3]; -- [[0,1,2,3], [1,2,3,4], [2,3,4,5], [3,4,5,6]]



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

--
tempoVarVarAdd:: TempoMark -> V -> V
tempoVarVarAdd (CPM r) (VNum x) = VTempo $ CPM (r + (toRat x))
tempoVarVarAdd t (VTempo t2) = VTempo $ t + t2
tempoVarVarAdd (CPM r) (VInt n) = VTempo $ CPM (r + (toRational n 1))
tempoVarVarAdd _ _ = VString "failed at tempoVarVarAdd"

-- stackVari:: V -> V -> V 
-- stackVari (VString s1) (VString s2) = VList $ s1 |\ s2

----- think about how to stack strings for sound!!!

addVari:: V -> V -> V
addVari (VInt x) (VInt y) = VInt (x+y)
addVari (VInt x) (VNum y) = VInt $ x + (round y)
addVari (VInt n) (VList xs) = VList $ map (\x -> addVari (VInt n) x) xs
addVari (VInt x) (VTempo t) = tempoVarVarAdd t (VInt x)
addVari (VNum x) (VNum y) = VNum (x+y) 
addVari (VNum x) (VInt y) = VNum $ x + (toNumber y)
addVari (VNum x) (VTempo t) = tempoVarVarAdd t (VNum x)
addVari (VNum n) (VList xs) = VList $ map (\x -> addVari (VNum n) x) xs
addVari (VList xs) (VNum n) = VList $ map (\x -> addVari (VNum n) x) xs
addVari (VList xs) (VInt n) = VList $ map (\x -> addVari (VInt n) x) xs
addVari (VList xs) (VTempo t) = VList $ map (\x -> tempoVarVarAdd t x) xs 
addVari (VList xs) (VList ys) = VList $ map (\y -> VList $ map (\x -> addVari x y) xs) ys -- VList (VList:VList:etc)
addVari (VTempo t) (VNum x) = tempoVarVarAdd t (VNum x)
addVari (VTempo t) (VList xs) = VList $ map (\x -> tempoVarVarAdd t x) xs
addVari (VTempo t1) (VTempo t2) = VTempo $ t1 + t2
addVari (VTempo t) (VInt n) = tempoVarVarAdd t (VInt n)

addVari (VString str) _ = VString str
addVari _ _ = VString "addVari failed" 

subVari:: V -> V -> V
subVari (VInt x) (VInt y) = VInt (x-y)
subVari (VInt x) (VNum y) = VInt $ x - (round y)
subVari (VInt n) (VList xs) = VList $ map (\x -> subVari (VInt n) x) xs
subVari (VInt x) (VTempo t) = tempoVarVarAdd (changeSignTempo t) (VInt x)
subVari (VNum x) (VNum y) = VNum (x-y) 
subVari (VNum x) (VInt y) = VNum $ x - (toNumber y)
subVari (VNum n) (VList xs) = VList $ map (\x -> subVari (VNum n) x) xs
subVari (VNum x) (VTempo t) = tempoVarVarAdd (changeSignTempo t) (VNum x)
subVari (VList xs) (VNum n) = VList $ map (\x -> subVari x (VNum n)) xs
subVari (VList xs) (VTempo t) = VList $ map (\x -> tempoVarVarAdd t x) xs
subVari (VList xs) (VInt n) = VList $ map (\x -> subVari x (VInt n)) xs
subVari (VList xs) (VList ys) = VList $ map (\y -> VList $ map (\x -> subVari x y) xs) ys 
subVari (VTempo t) (VNum x) = tempoVarVarAdd t (VNum (x*(-1.0)))
subVari (VTempo t) (VList xs) = VList $ map (\x -> tempoVarVarAdd t x) xs
subVari (VTempo t1) (VTempo t2) = VTempo $ t1 - t2
subVari (VTempo t) (VInt n) = tempoVarVarAdd t $ VInt (n*(-1))
subVari (VString str) _ = VString str
subVari _ _ = VString "subVari failed"

changeSignTempo:: TempoMark -> TempoMark
changeSignTempo (CPM r) = CPM (r * (toRat (-1.0)))
changeSignTempo _ = CPM $ toRat 0.0

tempoVarVarMul:: TempoMark -> V -> V
tempoVarVarMul (CPM r) (VNum x) =  VTempo $ CPM (r * (toRat x))
tempoVarVarMul (CPM r) (VInt n) = VTempo $ CPM (r * (toRational n 1))
tempoVarVarMul t (VTempo t2) = VTempo $ t * t2
tempoVarVarMul _ _ = VString "failed at tempoVarVarMul"

mulVari:: V -> V -> V
mulVari (VInt x) (VInt y) = VInt $ x * y
mulVari (VInt x) (VNum y) = VInt $ x * (round y)
mulVari (VInt n) (VList xs) = VList $ map (\x -> mulVari (VInt n) x) xs 
mulVari (VInt n) (VTempo t) = tempoVarVarMul t $ VInt n
mulVari (VNum x) (VNum y) = VNum $ x * y 
mulVari (VNum n) (VList xs) = VList $ map (\x -> mulVari (VNum n) x) xs
mulVari (VNum x) (VTempo t) = tempoVarVarMul t (VNum x)
mulVari (VNum x) (VInt y) = VNum $ x * (toNumber y)
mulVari (VList xs) (VNum n) = VList $ map (\x -> mulVari (VNum n) x) xs
mulVari (VList xs) (VTempo t) = VList $ map (\x -> tempoVarVarMul t x) xs
mulVari (VList xs) (VInt n) = VList $ map (\x -> mulVari (VInt n) x) xs
mulVari (VList xs) (VList ys) = VList $ map (\y -> VList $ map (\x -> mulVari x y) xs) ys 
mulVari (VTempo t) (VNum x) = tempoVarVarMul t (VNum x)
mulVari (VTempo t) (VList xs) = VList $ map (\x -> tempoVarVarMul t x) xs
mulVari (VTempo t1) (VTempo t2) = VTempo $ t1 * t2
mulVari (VTempo t) (VInt n) = tempoVarVarMul t $ VInt n
mulVari (VString str) _ = VString str
mulVari _ _ = VString "mulVari failed"

tempoVarVarDiv:: TempoMark -> V -> V
tempoVarVarDiv (CPM r) (VNum x) =  VTempo (CPM $ divRatioNum r x)
tempoVarVarDiv t (VTempo t2) = VTempo $ t / t2
tempoVarVarDiv _ _ = VString "failed at tempoVarVarMul"

tempoVarVarDiv':: TempoMark -> V -> V
tempoVarVarDiv' (CPM r) (VNum x) =  VTempo (CPM $ divNumRatio x r)
tempoVarVarDiv' t (VTempo t2) = VTempo $ t2 / t
tempoVarVarDiv' _ _ = VString "failed at tempoVarVarMul"

divRatioNum:: Rational -> Number -> Rational
divRatioNum r x = r / (toRat x)

-- divRatioNum r x = r * (toRational (den*signedBI) num)
--   where signed = if x < 0.0 then (-1) else 1
--         signedBI = BI.fromInt signed 
--         newX = toRat (x*(toNumber signed)) 
--         (Tuple num den) = Tuple (numerator newX) (denominator newX)


divNumRatio:: Number -> Rational -> Rational
divNumRatio x r = (toRat x) / r


divVari:: V -> V -> V
divVari (VInt x) (VInt y) = VInt $ x / y
divVari (VInt x) (VNum y) = VInt $ round $ (toNumber x) / y
divVari (VInt n) (VList xs) = VList $ map (\x -> divVari (VInt n) x) xs
divVari (VInt x) (VTempo t) = tempoVarVarDiv' t $ VInt x
divVari (VNum x) (VNum y) = VNum (x/y) 
divVari (VNum x) (VInt y) = VNum (x / (toNumber y))
divVari (VNum n) (VList xs) = VList $ map (\x -> divVari (VNum n) x) xs
divVari (VNum x) (VTempo t) = tempoVarVarDiv' t (VNum x)
divVari (VList xs) (VNum n) = VList $ map (\x -> divVari (VNum n) x) xs
divVari (VList xs) (VTempo t) = VList $ map (\x -> tempoVarVarDiv' t x) xs
divVari (VList xs) (VInt n) = VList $ map (\x -> divVari (VInt n) x) xs
divVari (VList xs) (VList ys) = VList $ map (\y -> VList $ map (\x -> divVari x y) xs) ys 
divVari (VTempo t) (VNum x) = tempoVarVarDiv t (VNum x)
divVari (VTempo t) (VList xs) = VList $ map (\x -> tempoVarVarDiv t x) xs
divVari (VTempo t1) (VTempo t2) = VTempo $ t1 / t2
divVari (VTempo t) (VInt n) = tempoVarVarDiv' t $ VInt n
divVari (VString str) _ = VString str
divVari _ _ = VString "divVari failed"

tempoVarVarPow:: TempoMark -> V -> V
tempoVarVarPow (CPM r) (VNum x) = VTempo (CPM $ toRat $ pow (R.toNumber r) x)
tempoVarVarPow t1 (VTempo t2) = VTempo $ t1 ** t2
tempoVarVarPow _ _ = VString "failed at tempoVarVarMul"

tempoVarVarPow':: TempoMark -> V -> V
tempoVarVarPow' (CPM r) (VNum x) = VTempo (CPM $ toRat $ pow x (R.toNumber r))
tempoVarVarPow' t1 (VTempo t2) = VTempo $ t2 ** t1
tempoVarVarPow' _ _ = VString "failed at tempoVarVarMul"

powVari:: V -> V -> V
powVari (VInt x) (VInt y) = VInt $ I.pow x y
powVari (VInt x) (VNum y) = VInt $ round $ pow (toNumber x) y  
powVari (VInt n) (VList xs) = VList $ map (\x -> powVari (VInt n) x) xs
powVari (VInt x) (VTempo t) = tempoVarVarPow' t $ VInt x
powVari (VNum x) (VNum y) = VNum (pow x y) 
powVari (VNum x) (VInt y) = VNum $ pow x (toNumber y)
powVari (VNum n) (VList xs) = VList $ map (\x -> powVari (VNum n) x) xs
powVari (VNum x) (VTempo t) = tempoVarVarPow' t (VNum x)
powVari (VList xs) (VNum n) = VList $ map (\x -> powVari (VNum n) x) xs
powVari (VList xs) (VTempo t) = VList $ map (\x -> tempoVarVarPow' t x) xs
powVari (VList xs) (VInt n) = VList $ map (\x -> powVari (VInt n) x) xs
powVari (VList xs) (VList ys) = VList $ map (\y -> VList $ map (\x -> powVari x y) xs) ys 
powVari (VTempo t) (VNum x) = tempoVarVarPow t (VNum x)
powVari (VTempo t) (VList xs) = VList $ map (\x -> tempoVarVarPow t x) xs
powVari (VTempo t1) (VTempo t2) = VTempo $ t1 ** t2
powVari (VTempo t) (VInt n) = tempoVarVarPow t $ VInt n
powVari (VString str) _ = VString str
powVari _ _ = VString "powVari failed"


valToV:: Value -> V -- VList (VList:VList:etc)
valToV (N span lista variations) = VList $ map (\n -> VInt n) lista
valToV (Orbit span lista variations) = VList $ map (\n -> VInt n) lista
valToV (Sound span lista variations) = VList $ map (\n -> VString n) lista
valToV (Vowel span lista variations) = VList $ map (\n -> VString n) lista
valToV (Gain span lista variations) = VList $ map (\n -> VNum n) lista
valToV (Pan span lista variations) = VList $ map (\n -> VNum n) lista
valToV (Speed span lista variations) = VList $ map (\n -> VNum n) lista
valToV (Begin span lista variations) = VList $ map (\n -> VNum n) lista
valToV (End span lista variations) = VList $ map (\n -> VNum n) lista
valToV (CutOff span lista variations) = VList $ map (\n -> VNum n) lista
valToV (CutOffH span lista variations) = VList $ map (\n -> VNum n) lista
valToV (Legato span lista variations) = VList $ map (\n -> VNum n) lista
valToV (MaxW span lista variations) = VList $ map (\n -> VNum n) lista
valToV (MinW span lista variations) = VList $ map (\n -> VNum n) lista
valToV (Inter span lista variations) = VList $ map (\n -> VNum n) lista

valToV (Dastgah span d) = dastgahToV d
valToV (Alpha span lista) = VList $ map (\n -> VInt n) lista
valToV (Beta span lista) = VList $ map (\n -> VInt n) lista
valToV (Gamma span lista) = VList $ map (\n -> VInt n) lista
valToV (Xeno id span lista) = VList $ map (\n -> VInt n) lista 
valToV _ = VInt 2666 -- add pitch stuff!!!

dastgahToV:: Dastgah -> V
dastgahToV (Shur xs) = VList $ map (\n -> VInt n) xs
dastgahToV (Segah xs) = VList $ map (\n -> VInt n) xs
dastgahToV (Nava xs) = VList $ map (\n -> VInt n) xs
dastgahToV (Homayun xs) = VList $ map (\n -> VInt n) xs
dastgahToV (Chahargah xs) = VList $ map (\n -> VInt n) xs
dastgahToV (Mahur xs) = VList $ map (\n -> VInt n) xs
dastgahToV (RastPanjgah xs) = VList $ map (\n -> VInt n) xs




----- for now, CPM, CPS and BPM convert one to another:
-- is this a class instance of TempoMark?????  t͡ɬ    


---- how to articulate all tempo marks together, two problems:
-- 1) XTempo and Proportion rely on external tempi. This means that this operation cannot be performed at parsing level, this needs to be thought in debt
---- 2) The acceleration (sinusoidal) tempo mark has too many bad arguments. It needs to be an operation at Variant level. I need an operator like +- and ~~. Also, what is frequency in this parser? 100cpm +- 20cpm $ sin 1 is: oscilate between 80 and 120 cpms once per second (or block?) 

----- for now: Variant logic will allow CPM, BPM and CPS to be compared and have operations liek addition, substraction, division and multiplication

-- short paper idea: CPS, CPM, BPM, time signatures and the way live coding langs deal with musical time.

-- music is thought on BPMs (beats per minute). for example 120bpm is a beat every 0.5 seconds, this interacts with musical time in strange ways. Time signatures look like this: 4/4. So 1/4 of a time signature is a beat. Thus, a time signature does not mark a beat it marks 4 beats. 

-- TidalCycles introduced the cycle per second as a musical measure. So, 1cps is equivalent to a 1/4 of a 4/4 measure at 120bpms??? it has been unclear how a CPS mark interact with tempo marks. This ambiguous relation has adumbrated experiments with new forms of marking musical time perhaps exlclusive to live coded music: cpm

-- the cpm (as explained in strudel) clearly interacts with music normality with this notation: setcpm = 120/4. 

--- tidalcycles/strudel remain ambiguous towards time signatures and cycles. While the cycle tneds to be favoured more and more musical time creeps in in its notation

---- consider:  setcpm = 120/4

--- now:  the program running is: s "bd cp bd bd cp"

------- the /4 is now meaningless since the beat is going to be felt at /5 (unless another program runs marking 1/4)


-- the perceptive and cognitive space of the tempo mark remains the best way to think of musical time and the cycle measurement remains artifical to the music proposed by live coding musicians

------- timeknot is developing 6 different tempomarks

----- timeknot proposes a mark that already integrates the /4 into the time mark, called cpm all this time. Now just marke by a w






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
  _ <- choice [reserved "tl", reserved "cpm"]
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

toVariant:: Either Int Number -> V
toVariant (Left n) = VNum $ toNumber n 
toVariant (Right x) = VNum x

negVariant:: Either Int Number -> V
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