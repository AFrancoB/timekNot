module Unleash (parseProgram,actualise,Program(..),Expression(..),Quant(..),TempoMark(..),Unleash(..),simpleEventsOnTempo,tempoChanger,tempoMarkToTempo,nextBeat,findBeats',t,eval,ws,we) where

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

-- parameters are map key 

-- Map Text Thing -> forall opts. Record opts

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
actualise (Program exp) t eval ws we = concat events
    where events = map (\x -> actualiseExps x t eval ws we) exp -- List of Lists

actualiseExps:: Expression -> Tempo -> DateTime -> DateTime -> DateTime -> List {whenPosix:: Number, s:: String, n:: Int}
actualiseExps (S sn q) t eval ws we = fromFoldable $ [singleVirtualToActual sn q t eval ws we]
actualiseExps (T sn newt) t eval ws we = tempoChanger sn newt t eval ws we

singleVirtualToActual:: (Tuple String Int) -> Quant -> Tempo -> DateTime -> DateTime -> DateTime -> {whenPosix:: Number, s:: String, n:: Int}
singleVirtualToActual (Tuple sample ene) (Q quant calibrate) t eval ws we = {whenPosix: psx, s: sample, n: ene}
    where psx = fromMaybe 0.0 $ fromDateTimeToPosixMaybe $ evalTimeandQuantToPsx t eval ws we quant calibrate

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

tempoChanger:: (Tuple String Int) -> TempoMark -> Tempo -> DateTime -> DateTime -> DateTime -> List {whenPosix:: Number, s:: String, n:: Int}
tempoChanger (Tuple sample ene) mark t eval ws we = 
    let newTempo = tempoChangeWithSameCount' t mark eval
        psxs = map fromDateTimeToPosix $ simpleEventsOnTempo newTempo eval ws we -- List Number
    in map (\x -> {whenPosix: x, s: sample, n: ene}) psxs
    
simpleEventsOnTempo:: Tempo -> DateTime -> DateTime -> DateTime -> List DateTime
simpleEventsOnTempo t eval ws we =
    let onset = 0.0
        window = findBeats t ws we 1.0 onset 
        ratW = map toRat window
    in map (\x -> countToTime t x) ratW

-- findBeats and nextBeat were adapted from the tempi library. 
-- what is understood as metr here is what I understand as proportion. 1:2:3 where 1 is the tempo 2 is twice as fast, 3 is three times as fast, etc. So, metre is the reciprocal of proportion*****
findBeats:: Tempo -> DateTime -> DateTime -> Number -> Number -> List Number
findBeats t ws' we' metre offset = findBeats' metre offset ws we
    where ws = timeToCountNumber t ws'
          we = timeToCountNumber t we'

findBeats':: Number -> Number -> Number -> Number -> List Number
findBeats' metre offset ws we
    | nextBeat metre offset ws >= we = fromFoldable []
    | otherwise = nextBeat metre offset ws : findBeats' metre offset (ws+metre) we

nextBeat:: Number -> Number -> Number -> Number
nextBeat metre offset ws
    | metre == 0.0 = 0.0
    | otherwise =
        let wsInMetre = ws/metre
            offsetInMetre = decimalPart $ offset/metre
            nextBeatInMetre | offsetInMetre >= (decimalPart wsInMetre) = (toNumber $ floor wsInMetre)  + offsetInMetre
                            | otherwise = (toNumber $ ceil wsInMetre) + offsetInMetre
        in nextBeatInMetre * metre

decimalPart:: Number -> Number
decimalPart x = x - (wholePart x)

wholePart:: Number -> Number 
wholePart x = toNumber $ floor x

----

fromDateTimeToPosix:: DateTime -> Number
fromDateTimeToPosix x = (unwrap $ unInstant $ fromDateTime x)/1000.0000

fromDateTimeToPosixMaybe:: Maybe DateTime -> Maybe Number
fromDateTimeToPosixMaybe (Just x) = Just $ (unwrap $ unInstant $ fromDateTime x)/1000.0000
fromDateTimeToPosixMaybe Nothing = Nothing

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