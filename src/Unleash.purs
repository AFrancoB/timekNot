module Unleash (parseProgram,actualise,Program(..),Expression(..),Quant(..),TempoMark(..),Unleash(..),t,eval,ws,we) where

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
import Data.Rational as R

type P = ParserT String Identity

parseProgram:: P Program
parseProgram = do
    _ <- pure 1
    x <- parseExpression `sepEndBy` semi
    eof
    pure $ Program x

parseExpression:: P Expression
parseExpression = do
    _ <- pure 1 
    choice [try parseSample, try parseTempo]    


parseTempo:: P Expression
parseTempo = do
    _ <- pure 1
    x <- sampler
    _ <- string ":|"
    whitespace
    t <- parseTempoChange
    pure $ T x t

parseTempoChange:: P TempoMark
parseTempoChange = do
    _ <- pure 1
    _ <- string "tempo"
    whitespace
    x <- naturalOrFloat
--    y <- naturalOrFloat
    pure $ TempoMark $ asNumber x



--- solve this later
-- parseRhythmFigure:: P Rational
-- parseRhythmFigure = do
--     _ <- pure 1
--     x <- naturalOrFloat
--     _ <- char '/'
--     y <- naturalOrFloat
--     pure $ toRat x y

parseSample:: P Expression
parseSample = do
    _ <- pure 1
    x <- sampler
    _ <- string "||"
    whitespace
    q <- parseQuant
    pure $ S x q

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

actualise:: Program -> Tempo -> DateTime -> DateTime -> DateTime -> List {whenPosix:: Number, s:: String, n:: Int}
actualise (Program exp) t eval ws we = map (\x -> actualiseExps x t eval ws we) exp

actualiseExps:: Expression -> Tempo -> DateTime -> DateTime -> DateTime -> {whenPosix:: Number, s:: String, n:: Int}
actualiseExps (S sn q) t eval ws we = singleVirtualToActual sn q t eval ws we
actualiseExps (T sn newt) t eval ws we = tempoChanger sn newt t eval ws we

singleVirtualToActual:: (Tuple String Int) -> Quant -> Tempo -> DateTime -> DateTime -> DateTime -> {whenPosix:: Number, s:: String, n:: Int}
singleVirtualToActual (Tuple sample ene) (Q quant calibrate) t eval ws we = {whenPosix: psx, s: sample, n: ene}
    where psx = fromMaybe 0.0 $ fromDateTimeToPosix $ evalTimeandQuantToPsx t eval ws we quant calibrate

filterSpan:: DateTime -> DateTime -> DateTime -> Maybe DateTime
filterSpan x ws we = if (x > ws && x < we) then (Just x) else Nothing

evalTimeandQuantToPsx:: Tempo -> DateTime -> DateTime -> DateTime -> Number -> Number -> Maybe DateTime
evalTimeandQuantToPsx t eval ws we q c = 
    let cuenta = timeToCountNumber t eval
        calibrated = if (justFractional cuenta) <= c then cuenta else (cuenta + 1.0)
        cuentaInGrid = toNumber $ ceil cuenta
        cuentaQ = cuentaInGrid + q 
        posInTempo = (toRat cuentaQ)
    in filterSpan (countToTime t posInTempo) ws we

---

tempoMarkToTempo:: Tempo -> Number -> Tempo
tempoMarkToTempo t mark = {freq: newF, time: t.time, count: t.count}
    where newF = toRat $ (1.0 / 60.0) * mark

tempoToTempoMark:: Tempo -> Number
tempoToTempoMark t = R.toNumber $ ((1%1)/t.freq) * (60%1)

tempoChangeWithSameCount:: Tempo -> Tempo -> DateTime -> Tempo
tempoChangeWithSameCount oldT newT moment = {freq: newT.freq, time: newT.time, count: toRat $ newCount}
    where newCount = (timeToCountNumber oldT moment) - (timeToCountNumber newT moment)
    
tempoChangeWithSameCount':: Tempo -> TempoMark -> DateTime -> Tempo
tempoChangeWithSameCount' oldT (TempoMark mark) moment = {freq: newT.freq, time: newT.time, count: toRat $ newCount}
    where newT = tempoMarkToTempo oldT mark
          newCount = (timeToCountNumber oldT moment) - (timeToCountNumber newT moment)

tempoChanger:: (Tuple String Int) -> TempoMark -> Tempo -> DateTime -> DateTime -> DateTime -> {whenPosix:: Number, s:: String, n:: Int}
tempoChanger (Tuple sample ene) mark t eval ws we = 
    let newTempo = tempoChangeWithSameCount' t mark eval
        psx = fromMaybe 0.0 $ fromDateTimeToPosix $ simpleEventsOnTempo newTempo eval ws we
    in {whenPosix: psx, s: sample, n: ene}

simpleEventsOnTempo:: Tempo -> DateTime -> DateTime -> DateTime -> Maybe DateTime
simpleEventsOnTempo t eval ws we =
    let countAtStart = timeToCountNumber t ws -- $ timeToCountNumber t ws --Number
        index = if countAtStart > 0.8 then countAtStart else countAtStart
        start = getPercent countAtStart
        end = getPercent $ timeToCountNumber t we
        onset = 0.0 -- the percent of the moment in which an onset happens in the refrain
        window = onsetForWindow onset countAtStart start end
        ratW = toRat <$> window
    in countToTime t <$> ratW


onsetForWindow:: Number -> Number -> Number -> Number -> Maybe Number
onsetForWindow o countAtStart start end 
    | (start > end) = ((toNumber $ floor countAtStart) + _) <$> onset
        where onset = if ((o+1.0) >= start) && ((o+1.0) < (end+1.0)) then (Just o) else Nothing
    | otherwise = ((toNumber $ floor countAtStart) + _) <$> onset
        where onset = if (o >= start) && (o < end) then (Just o) else Nothing 

getPercent:: Number -> Number
getPercent x = x - (toNumber $ floor x)

----

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
data Program = Program (List Expression)

instance Show Program where
    show (Program xs) = show xs 

data Expression = S (Tuple String Int) Quant | T (Tuple String Int) TempoMark

instance Show Expression where
    show (S (Tuple s n) q) = show s <> " " <> show n <> " " <> show q
    show (T (Tuple s n) t) = show s <> " " <> show n <> " " <> show t

data Quant = Q Number Number

instance Show Quant where
    show (Q quant calibrate) = show quant <> " " <> show calibrate

data TempoMark = TempoMark Number 

instance Show TempoMark where
    show (TempoMark x) = show x

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