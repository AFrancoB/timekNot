module Helpers where

import Prelude
import Data.Int
import Data.List
import Data.List.Lazy (replicate,cycle) --  import both lists as qualified....
import Data.List.Lazy as Lz
import Data.List.NonEmpty (toList)
import Data.Tuple
import Data.Rational as R
import Data.Rational (Rational,(%),fromInt)
import Data.Maybe
import Data.Bifunctor
-- import Data.String (take)
import Data.String as Str
import Data.Either

import Effect (Effect)
import Effect.Now (nowDateTime)

import Data.Newtype

import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration
import Data.Tempo
import Data.Enum
import Partial.Unsafe

import AST

toNumber':: Either Int Number -> Number
toNumber' (Left x) = toNumber x 
toNumber' (Right x) = x

attachLast::forall a. a -> List a -> List a 
attachLast a xs = reverse (a :reverse xs)

x:: Rhythmic
x = X

o:: Rhythmic
o = O

toRat:: Number -> Rational
toRat x = 
    let pFact = 1000000
        floored = floor x -- 12
        fract = x - (toNumber floored) -- 12.5 - 12.0 = 0.5
        fract' = round $ fract * (toNumber pFact) -- 500000
    in (floored%1) + (fract'%pFact) -- 12 + (500000%1000000)


rightOfpoint:: Number -> Number -- partition a Number and get the decimals
rightOfpoint x = x - (toNumber $ floor x)