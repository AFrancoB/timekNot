module Aural(aural, parseRangeNum, transposeNWith) where

import Prelude

import Data.Identity
import Data.List (List(..), head, tail, elem, (:), filter, fromFoldable, (..))
import Data.Array (fromFoldable) as A
import Data.Either
import Data.Int
import Data.Tuple (Tuple(..), fst, snd)
import Data.Map (Map(..), lookup, keys, singleton, toUnfoldable, member)
import Data.Map (fromFoldable) as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String as Str

import Data.FunctorWithIndex (mapWithIndex)

import Data.String.CodeUnits (fromCharArray)

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Combinators.Array (many)
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)

import AST
import Rhythm

type P = ParserT String Identity

aural:: P Expression
aural = do 
    _ <- pure 1
    x <- values
    _ <- reserved ";" -- this need to be fixed!
    pure $ AuralExpression x -- (Map Strg Aural)

values:: P (Map String Aural)
values = do
    _ <- pure 1
    id <- voiceId
    xs <- many value
    pure $ singleton id (fromFoldable xs)

value:: P Value
value = do
    _ <- pure 1
    _ <- reservedOp "."
    valType <- choice [try sound,try n, try gain, try pan, try speed, try begin, end]
    pure valType

-- chord:: P Value
-- chord = do
--     _ <- pure 1
--     _ <- reserved "chord"
--     _ <- reservedOp "="
--     chords <- [try makeChord, transposeChord]
--     pure n

-- makeChord:: P Value
-- makeChord = do
--     _ <- pure 1
--     sp <- parseSpan
--     spdList <- choice [try (A.fromFoldable <$> parseRangeInt), many parseInt]
--     pure $ Chord sp $ fromFoldable spdList

-- transposeChord:: P Value
-- transposeChord = do
--     id <- voiceId
--     pure $ TransposedChord id


end:: P Value
end = do
    _ <- pure 1
    _ <- choice [reserved "end"]
    _ <- reservedOp "="
    end <- choice [try makeEnd, transposeEnd]
    pure end

-- transposeEndWith:: P Value
-- transposeEndWith = do
--     id <- voiceId
--     with <- parens transNumVal
--     pure $ TransposedEndWith id with

transposeEnd:: P Value
transposeEnd = do
    id <- voiceId
    pure $ TransposedEnd id

makeEnd:: P Value
makeEnd = do
    _ <- pure 1
    sp <- parseSpan
    spdList <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    pure $ End sp $ fromFoldable spdList

begin:: P Value
begin = do
    _ <- pure 1
    _ <- choice [try $ reserved "begin",reserved "begin"]
    _ <- reservedOp "="
    sound <- choice [try makeBegin, transposeBegin]
    pure sound

-- transposeBeginWith:: P Value
-- transposeBeginWith = do
--     id <- voiceId
--     with <- parens transNumVal
--     pure $ TransposedBeginWith id with

transposeBegin:: P Value
transposeBegin = do
    id <- voiceId
    pure $ TransposedBegin id

makeBegin:: P Value
makeBegin = do
    _ <- pure 1
    sp <- parseSpan
    panList <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    pure $ Begin sp $ fromFoldable panList

speed:: P Value
speed = do
    _ <- pure 1
    _ <- choice [reserved "speed"]
    _ <- reservedOp "="
    n <- choice [try makeSpeed, transposeSpeed]
    pure n

-- transposeSpeedWith:: P Value
-- transposeSpeedWith = do
--     id <- voiceId
--     with <- parens transNumVal
--     pure $ TransposedSpeedWith id with

transposeSpeed:: P Value
transposeSpeed = do
    id <- voiceId
    pure $ TransposedSpeed id

makeSpeed:: P Value
makeSpeed = do
    _ <- pure 1
    sp <- parseSpan
    spdList <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    pure $ Speed sp $ fromFoldable spdList

pan:: P Value
pan = do
    _ <- pure 1
    _ <- choice [try $ reserved "pan",reserved "pan"]
    _ <- reservedOp "="
    sound <- choice [try makePan, transposePan]
    pure sound

-- transposePanWith:: P Value
-- transposePanWith = do
--     id <- voiceId
--     with <- parens transNumVal
--     pure $ TransposedPanWith id with

transposePan:: P Value
transposePan = do
    id <- voiceId
    pure $ TransposedPan id

makePan:: P Value
makePan = do
    _ <- pure 1
    sp <- parseSpan
    panList <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    pure $ Pan sp $ fromFoldable panList

gain:: P Value
gain = do
    _ <- pure 1
    _ <- choice [reserved "gain"]
    _ <- reservedOp "="
    n <- choice [try makeGain, transposeGain]
    pure n

-- transposeGainWith:: P Value
-- transposeGainWith = do
--     id <- voiceId
--     with <- parens transNumVal
--     pure $ TransposedGainWith id with

transNumVal:: P (List (Number -> Number))
transNumVal = do 
    _ <- pure 1
    op <- choice [reservedOp "+" *> pure add, reservedOp "*" *> pure mul]
    numList <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    pure $ map op $ fromFoldable numList

transposeGain:: P Value
transposeGain = do
    id <- voiceId
    pure $ TransposedGain id

makeGain:: P Value
makeGain = do
    _ <- pure 1
    sp <- parseSpan
    gainList <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    pure $ Gain sp $ fromFoldable gainList

n:: P Value
n = do
    _ <- pure 1
    _ <- choice [reserved "n"]
    _ <- reservedOp "="
    n <- choice [try makeN, {-try transposeNWith,-} transposeN]
    pure n

transposeNWith:: P Value
transposeNWith = do
    id <- voiceId
    with <- transIntVal
    pure $ TransposedNWith id with

transIntVal:: P (List Ops)
transIntVal = do 
    _ <- pure 1
    n <- choice [try $ parens addInts, try $ parens multInts]
    pure n

addInts:: P (List Ops)
addInts = do
    _ <- pure 1
    _ <- reserved "+"
    ns <- many natural
    pure $ map (\n -> AddInt n) $ fromFoldable ns

multInts:: P (List Ops)
multInts = do
    _ <- pure 1
    _ <- reserved "*"
    ns <- many natural
    pure $ map (\n -> MultInt n) $ fromFoldable ns

mixInts:: P (List Ops)
mixInts = do
    _ <- pure 1
    xs <- many $ choice [parens oneAdd, parens oneMult]
    pure $ fromFoldable xs

oneAdd:: P Ops
oneAdd = do
    _ <- pure 1
    _ <- reserved "+"
    n <- natural
    pure $ AddInt n

oneMult:: P Ops
oneMult = do
    _ <- pure 1
    _ <- reserved "*"
    n <- natural
    pure $ MultInt n

transposeN:: P Value
transposeN = do
    id <- voiceId
    pure $ TransposedN id

makeN:: P Value
makeN = do
    _ <- pure 1
    sp <- parseSpan
    strList <- choice [try (A.fromFoldable <$> parseRangeInt), many natural]
    pure $ N sp $ fromFoldable strList

sound:: P Value
sound = do
    _ <- pure 1
    _ <- choice [try $ reserved "sound",reserved "s"]
    _ <- reservedOp "="
    sound <- choice [try makeSound, transposeSound]
    pure sound


transposeSound:: P Value
transposeSound = do
    id <- voiceId
    pure $ TransposedSound id

makeSound:: P Value
makeSound = do
    _ <- pure 1
    sp <- parseSpan
    strList <- sampleParser 
    pure $ Sound sp strList

--
parseSpan:: P Span
parseSpan = do
    _ <- pure 1
    x <- choice [
                   reserved "-_" *>  pure CycleInBlock
                 , try $ reserved "_-" *>  pure CycleBlock
                 , try $ reserved "_-_" *> pure SpreadBlock
                 , reserved "_" *>   pure  CycleEvent
                ]
    pure x
    ---

sampleParser:: P (List String)
sampleParser = do
    sampleNames <- stringLit
    pure $ stringToSamples sampleNames

stringToSamples:: String -> List String -- what to do with commas??
stringToSamples s = fromFoldable $ Str.split (Str.Pattern " ") $ Str.trim s

voiceId:: P String 
voiceId = do
    _ <- pure 1
    x <- identifier
    pure x

--

toNumber':: Either Int Number -> Number
toNumber' (Left x) = toNumber x 
toNumber' (Right x) = x


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
parseRangeInt:: P (List Int)
parseRangeInt = do
    x <- natural
    _ <- reservedOp ".."
    y <- natural
    pure (x..y)
    
parseRangeNum:: P (List Number)
parseRangeNum = do
    x <- parseSpecialNum
    _ <- reservedOp ".."
    y <- parseSpecialNum
    pure $ specialRange x y

specialRange:: Tuple Int Int -> Tuple Int Int -> List Number
specialRange (Tuple i1 d1) (Tuple i2 d2) = map (\rangeInt -> (toNumber rangeInt) / 10.0) rangeInts
    where n1 = ((toNumber i1) * 10.0) + (toNumber d1) -- Tuple 2 3 will become 23
          n2 = ((toNumber i2) * 10.0) + (toNumber d2) -- Tuple 3 6 will become 36
          rangeInts = ((floor n1)..(floor n2))


parseSpecialNum:: P (Tuple Int Int)
parseSpecialNum = choice [try parseSpecialNum', toSpecial <$> natural]

toSpecial:: Int -> Tuple Int Int
toSpecial n = Tuple n 0

parseSpecialNum':: P (Tuple Int Int)
parseSpecialNum' = do
    x <- natural
    _ <- charWS '.'
    y <- natural 
    pure $ Tuple x y

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


-- check2 :: Map String Aural -> List String -> String -> Aural -> Boolean
-- check2 aMap alreadyRefd aKey (Temporal (Kairos _ _) _ _) = true
-- check2 aMap alreadyRefd aKey (Temporal (Metric _ _ _) _ _) = true
-- check2 aMap alreadyRefd aKey (Temporal (Converge anotherKey _ _ _) _ _) =
--   case lookup anotherKey aMap of
--     Nothing -> false
--     Just anotherValue -> case elem aKey alreadyRefd of
--                            true -> false
--                            false -> check2 aMap (aKey : alreadyRefd) anotherKey anotherValue







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
stringLiteral = tokenParser.stringLiteral
reservedOp = tokenParser.reservedOp
stringLit = tokenParser.stringLiteral