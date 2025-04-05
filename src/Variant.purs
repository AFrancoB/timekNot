module Variant(mulVar, addVar, getListTempo) where

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
-- import TimePacketOps (metricToTempoMark)


--this:
-- 500cpm * [1,2,3.3]
-- [500cpm, 200cpm*2.3, 200cpm*2.3, 4.5*(1/4 = 120bpm), mu 3:2]
-- (1/4 = 120bpm)*1.1 until (< 1/4 = 500)

-- and this:
-- mu.sound = "bd cn sd" .n = 1 2 4 2 5 6 *[1,2,3,4,5] .pan = 0.3 0.6 0.2 *[1.1,2.2,3.3]


addVar:: Variant -> Variant -> Variant
addVar (VTempo t) (VInt n) = tempoVarVarAdd t (VInt n)
addVar (VTempo t) (VNum x) = tempoVarVarAdd t (VNum x)
addVar (VTempo t) (VList xs) =  tempoVarVarAdd t (VList xs)
addVar (VInt n) (VTempo t) = tempoVarVarAdd t (VInt n)
addVar (VNum x) (VTempo t) = tempoVarVarAdd t (VNum x)
addVar (VList xs) (VTempo t) = tempoVarVarAdd t (VList xs)
addVar (VInt n) (VInt n') = intVarVarAdd n (VInt n')
addVar (VInt n) (VNum x) = intVarVarAdd n (VNum x)
addVar (VInt n) (VList xs) = intVarVarAdd n (VList xs)
addVar (VInt n) (VTempo t) = tempoVarVarAdd t (VInt n)
addVar (VNum x) (VInt n) = intVarVarAdd n (VNum x)
addVar (VNum x) (VNum x') = numVarVarAdd x (VNum x')
addVar (VNum x) (VList xs) = numVarVarAdd x (VList xs)
addVar (VNum x) (VTempo t) = tempoVarVarAdd t (VNum x)
addVar (VInt n) (VString str) = VString str
addVar (VNum x) (VString str) = VString str
addVar (VTempo t) (VString str) =  VString str
addVar (VList xs) (VString str) = VString str
addVar xs ys = listVarVarAdd xs ys
addVar _ _ = VInt 2666

listVarVarAdd:: Variant -> Variant -> Variant
listVarVarAdd (VList xs) ys = VList $ map (\x -> addVar x ys) xs 
listVarVarAdd xs (VList ys) = VList $ map (\y -> addVar y xs) ys
listVarVarAdd _ _ = VString "failed at listVarVarAdd"

numVarVarAdd:: Number -> Variant -> Variant
numVarVarAdd x (VNum x') = VNum (x + x')
numVarVarAdd x (VList xs) = VList $ map (\x' -> numVarVarAdd x x') xs 
numVarVarAdd x (VString str) = VString str
numVarVarAdd x _ = VNum x

intVarVarAdd:: Int -> Variant -> Variant
intVarVarAdd n (VInt n') = VInt (n+n')
intVarVarAdd n (VNum x) = VNum ((toNumber n)+x)
intVarVarAdd n (VList xs) = VList $ map (\x -> intVarVarAdd n x) xs
intVarVarAdd n  (VString str) = VString str
intVarVarAdd n _ = VInt n

tempoVarVarAdd:: TempoMark -> Variant -> Variant
tempoVarVarAdd XTempo variant = VXTempo variant
tempoVarVarAdd t variant = tempVarVarAdd t variant

tempVarVarAdd:: TempoMark -> Variant -> Variant
tempVarVarAdd (CPM r) (VInt n) =  VTempo $ CPM (r + (toRational n 1))
tempVarVarAdd (BPM t fig) (VInt n) = VTempo $ BPM (t + (toRational n 1))  fig
tempVarVarAdd (CPS r) (VInt n) = VTempo $ CPS (r + (toRational n 1))
tempVarVarAdd (Prop st n m) (VInt n') = VTempo $ (Prop st (n+n') m)
tempVarVarAdd (Sin sin) (VInt n) = VTempo $ Sin {min: sin.min, max: sin.max, osc: newFreq, phase: sin.phase}
  where newFreq = sin.osc + (toRational n 1)
tempVarVarAdd (Dur r) (VInt n) = VTempo $ Dur (r + (toRational n 1))
tempVarVarAdd (CPM r) (VNum x) =  VTempo $ CPM (r + (toRat x))
tempVarVarAdd (BPM t fig) (VNum x) = VTempo $ BPM (t + (toRat x))  fig
tempVarVarAdd (CPS r) (VNum x) = VTempo $ CPS (r + (toRat x))
tempVarVarAdd (Prop st n m) (VNum x) = VTempo $ (Prop st (round ((toNumber n)+x)) m)
tempVarVarAdd (Sin sin) (VNum x) = VTempo $ Sin {min: sin.min, max: sin.max, osc: newFreq, phase: sin.phase}
  where   newFreq = sin.osc + (toRat x)
tempVarVarAdd (Dur r) (VNum x) = VTempo $ Dur (r + (toRat x))
tempVarVarAdd t (VList xs) =  VList $ map (\x -> tempVarVarAdd t x) xs
tempVarVarAdd _ _ = VString "nothing to do here in tempVarVar"


-- Multiplication Variant implementation:

mulVar:: Variant -> Variant -> Variant
mulVar (VTempo t) (VInt n) = tempoVarVarMul t (VInt n)
mulVar (VTempo t) (VNum x) = tempoVarVarMul t (VNum x)
mulVar (VTempo t) (VList xs) =  tempoVarVarMul t (VList xs)
mulVar (VInt n) (VTempo t) = tempoVarVarMul t (VInt n)
mulVar (VNum x) (VTempo t) = tempoVarVarMul t (VNum x)
mulVar (VList xs) (VTempo t) = tempoVarVarMul t (VList xs)
mulVar (VInt n) (VInt n') = intVarVarMul n (VInt n')
mulVar (VInt n) (VNum x) = intVarVarMul n (VNum x)
mulVar (VInt n) (VList xs) = intVarVarMul n (VList xs)
mulVar (VInt n) (VTempo t) = tempoVarVarMul t (VInt n)
mulVar (VNum x) (VInt n) = intVarVarMul n (VNum x)
mulVar (VNum x) (VNum x') = numVarVarMul x (VNum x')
mulVar (VNum x) (VList xs) = numVarVarMul x (VList xs)
mulVar (VNum x) (VTempo t) = tempoVarVarMul t (VNum x)
mulVar (VInt n) (VString str) = VString str
mulVar (VNum x) (VString str) = VString str
mulVar (VTempo t) (VString str) =  VString str
mulVar (VList xs) (VString str) = VString str
mulVar xs ys = listVarVarMul xs ys
mulVar _ _ = VInt 2666

listVarVarMul:: Variant -> Variant -> Variant
listVarVarMul (VList xs) ys = VList $ map (\x -> mulVar x ys) xs 
listVarVarMul xs (VList ys) = VList $ map (\y -> mulVar y xs) ys
listVarVarMul _ _ = VString "failed at listVarVarMul"

numVarVarMul:: Number -> Variant -> Variant
numVarVarMul x (VNum x') = VNum (x * x')
numVarVarMul x (VList xs) = VList $ map (\x' -> numVarVarMul x x') xs 
numVarVarMul x (VString str) = VString str
numVarVarMul x _ = VNum x

intVarVarMul:: Int -> Variant -> Variant
intVarVarMul n (VInt n') = VInt (n*n')
intVarVarMul n (VNum x) = VNum ((toNumber n)*x)
intVarVarMul n (VList xs) = VList $ map (\x -> intVarVarMul n x) xs
intVarVarMul n  (VString str) = VString str
intVarVarMul n _ = VInt n

tempoVarVarMul:: TempoMark -> Variant -> Variant
tempoVarVarMul XTempo variant = VXTempo variant
tempoVarVarMul t variant = tempVarVarMul t variant

tempVarVarMul:: TempoMark -> Variant -> Variant
tempVarVarMul (CPM r) (VInt n) =  VTempo $ CPM (r * (toRational n 1))
tempVarVarMul (BPM t fig) (VInt n) = VTempo $ BPM (t * (toRational n 1))  fig
tempVarVarMul (CPS r) (VInt n) = VTempo $ CPS (r * (toRational n 1))
tempVarVarMul (Prop st n m) (VInt n') = VTempo $ (Prop st (n*n') m)
tempVarVarMul (Sin sin) (VInt n) = VTempo $ Sin {min: sin.min, max: sin.max, osc: newFreq, phase: sin.phase}
  where newFreq = sin.osc * (toRational n 1)
tempVarVarMul (Dur r) (VInt n) = VTempo $ Dur (r * (toRational n 1))
tempVarVarMul (CPM r) (VNum x) =  VTempo $ CPM (r * (toRat x))
tempVarVarMul (BPM t fig) (VNum x) = VTempo $ BPM (t * (toRat x))  fig
tempVarVarMul (CPS r) (VNum x) = VTempo $ CPS (r * (toRat x))
tempVarVarMul (Prop st n m) (VNum x) = VTempo $ (Prop st (round ((toNumber n)*x)) m)
tempVarVarMul (Sin sin) (VNum x) = VTempo $ Sin {min: sin.min, max: sin.max, osc: newFreq, phase: sin.phase}
  where   newFreq = sin.osc * (toRat x)
tempVarVarMul (Dur r) (VNum x) = VTempo $ Dur (r * (toRat x))
tempVarVarMul t (VList xs) =  VList $ map (\x -> tempVarVarMul t x) xs
tempVarVarMul _ _ = VString "nothing to do here in tempVarVar"

getTempo:: Variant -> TempoMark
getTempo (VTempo t) = t
-- getTempo (VList xs) = getListTempo xs 
getTempo _ = CPS $ toRat 0.5

getListTempo:: Variant -> List TempoMark
getListTempo (VList xs) = map getTempo xs
getListTempo _ = Nil


toRat:: Number -> Rational
toRat x = 
    let pFact = 1000000
        floored = floor x -- 12
        fract = x - (toNumber floored) -- 12.5 - 12.0 = 0.5
        fract' = round $ fract * (toNumber pFact) -- 500000
    in (floored%1) + (fract'%pFact) -- 12 + (500000%1000000)