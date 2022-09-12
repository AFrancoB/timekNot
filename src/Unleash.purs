module Unleash (parseSample,actualise,Program(..),Quant(..),Unleash(..),t,eval,evalTimeandQuantToPsx) where

import Prelude

import Data.Either
import Data.Identity
import Data.List hiding (many)
import Data.Foldable (foldl)
import Data.Int
import Data.Tuple
import Data.String (singleton, joinWith, take, split, trim, Pattern(..))
import Data.Maybe hiding (optional)
import Data.Functor
import Control.Monad

import Effect (Effect)
import Effect.Console (log)
import Effect.Ref

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)

import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration
import Data.Tempo
import Data.Enum
import Partial.Unsafe

import Data.Newtype

import Data.Rational hiding (toNumber)

type P = ParserT String Identity

parseSample:: P Program
parseSample = do
    _ <- pure 1
    x <- sampler
    _ <- string "||"
    whitespace
    q <- parseQuant
    eof
    pure $ P x q

sampler:: P (Tuple String Int)
sampler = do
    _ <- pure 1
    x <- stringLit
    whitespace
    pure $ stringToSamples' x

stringToSamples':: String -> Tuple String Int -- what to do with commas??
stringToSamples' s = 
    let xs = fromFoldable $ split (Pattern ":") $ trim s
        name = fromMaybe " " $ head xs
        n = fromMaybe 0 $ fromTailToMaybeInt xs
    in Tuple name n


fromTailToMaybeInt :: List String -> Maybe Int
fromTailToMaybeInt strs = do
  a <- tail strs -- String -> Maybe (List String)
  b <- head a
  c <- fromString b -- Maybe Int
  pure c

parseQuant:: P Quant
parseQuant = do
    _ <- pure 1
    _ <- string "quant"
    whitespace
    x <- naturalOrFloat
    y <-naturalOrFloat <|> pure (Right 0.85)
    pure $ Q (asNumber x) (asNumber y)

asNumber:: Either Int Number -> Number
asNumber (Left x) = toNumber x
asNumber (Right x) = x


---
actualise:: Program -> Tempo -> DateTime -> DateTime -> DateTime -> {whenPosix:: Number, s:: String, n:: Int}
actualise (P sn q) t eval ws we = singleVirtualToActual sn q t eval ws we


singleVirtualToActual:: (Tuple String Int) -> Quant -> Tempo -> DateTime -> DateTime -> DateTime -> {whenPosix:: Number, s:: String, n:: Int}
singleVirtualToActual (Tuple sample ene) (Q quant calibrate) t eval ws we = {whenPosix: psx, s: sample, n: ene}
    where psx = fromMaybe 0.0 $ fromDateTimeToPosix $ evalTimeandQuantToPsx t eval ws we quant

filterSpan:: DateTime -> DateTime -> DateTime -> Maybe DateTime
filterSpan x ws we = if (x > ws && x < we) then (Just x) else Nothing

evalTimeandQuantToPsx:: Tempo -> DateTime -> DateTime -> DateTime -> Number -> Maybe DateTime
evalTimeandQuantToPsx t eval ws we q = 
    let cuenta = timeToCountNumber t eval
        calibrated = if (justFractional cuenta) <= 0.85 then cuenta else (cuenta + 5.0)
        cuentaInGrid = toNumber $ ceil cuenta
        cuentaQ = cuentaInGrid + q 
        posInTempo = (toRat cuentaQ)
    in filterSpan (countToTime t posInTempo) ws we

fromDateTimeToPosix:: Maybe DateTime -> Maybe Number
fromDateTimeToPosix (Just x) = Just $ (unwrap $ unInstant $ fromDateTime x)/1000.0000
fromDateTimeToPosix Nothing = Nothing

toRat:: Number -> Rational
toRat x = 
    let pFact = 1000000
        floored = floor x -- 12
        fract = x - (toNumber floored) -- 12.5 - 12.0 = 0.5
        fract' = round $ fract * (toNumber pFact) -- 500000
    in (floored%1) + (fract'%pFact) -- 12 + (500000%1000000)


justFractional:: Number -> Number
justFractional x = x - (toNumber $ floor x)

-----

data Program = P (Tuple String Int) Quant 

instance Show Program where
    show (P (Tuple s n) q) = show s <> " " <> show n <> " " <> show q

data Quant = Q Number Number

instance Show Quant where
    show (Q quant calibrate) = show quant <> " " <> show calibrate

-----

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

----
makeDate :: Int -> Month -> Int -> Date
makeDate y m d = 
    unsafePartial $ fromJust $ 
       canonicalDate <$> toEnum y <@> m <*> toEnum d

makeTime :: Int -> Int -> Int -> Int -> Time
makeTime h min sec milisec = 
    unsafePartial $ fromJust $ Time <$> toEnum h <*> toEnum min <*> toEnum sec <*> toEnum milisec

t:: Tempo
t = {freq: (2%1),time: (DateTime (makeDate 2022 June 3) (makeTime 19 11 25 100)), count: fromInt 0 }

tAncient:: Tempo
tAncient = {freq: (2%1),time: (DateTime (makeDate 2012 June 3) (makeTime 19 11 25 100)), count: fromInt 0 }

ws:: Int -> Int -> DateTime
ws x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

we:: Int -> Int -> DateTime
we x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

eval:: DateTime
eval = (DateTime (makeDate 2022 June 3) (makeTime 19 15 0 485))

---

type Unleash = {
  ast :: Ref Program,
  tempo :: Ref Tempo,
  eval :: Ref DateTime
  }