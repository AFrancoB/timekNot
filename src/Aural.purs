module Aural(aural, parseRangeNum, transposeNWith) where

import Prelude

import Data.Identity
import Data.List (List(..), head, tail, elem, (:), filter, fromFoldable, (..), length, zip)
import Data.Array (fromFoldable) as A
import Data.Either
import Data.Int
import Data.Tuple (Tuple(..), fst, snd)
import Data.Map (Map(..), lookup, keys, singleton, toUnfoldable, member, mapMaybe)
import Data.Map (fromFoldable) as M
import Data.Maybe (Maybe(..), fromMaybe)
-- import Data.Set as Set
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
    valType <- choice [try sound,try n, try gain, try pan, try speed, try begin, try end, mayeh]
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

-- Dastgah

-- Intervals: Bozorg 182; Kuchak 114; Tanini 204; Baghie 90.

-- shur: D Eqb F G Aqb Bb C 
-- normalised to 24ET: 0   175      300        500         675        800         1000        1200
-- using intervals:    0   182      114 (296)  204 (500) - 182 (682)- 114 (796) - 204 (1000) - 1200
-- name of interfvals: 0 - Bozorg - Kuchak   -  Tanini   - Bozorg   - Kuchak    - Tanini  ??? how to get to the Octave? 

-- corrected by Mehdad: 0 1.82 2.96 5.0 7.04 7.94 9.98 12.0

mayeh:: P Value
mayeh = do
    _ <- pure 1
    choice [try shur]

shur:: P Value
shur = do
    _ <- pure 1
    _ <- choice [reserved "shur"]
    _ <- reservedOp "="
    shur <- makeShur
    pure shur

-- Dastgah Span Dastgah

makeShur:: P Value 
makeShur = do
    _ <- pure 1
    sp <- parseSpan
    shurList <- choice [try (A.fromFoldable <$> parseRangeInt), many natural]
    pure $ Dastgah sp (Shur $ fromFoldable shurList)


--
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
    n <- brackets natural <|> pure 0
    pure $ TransposedEnd id n

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
    n <- brackets natural <|> pure 0
    pure $ TransposedBegin id n

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
    n <- brackets natural <|> pure 0
    pure $ TransposedSpeed id n

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
    n <- brackets natural <|> pure 0
    pure $ TransposedPan id n

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
    n <- brackets natural <|> pure 0
    pure $ TransposedGain id n

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
    n <- brackets natural <|> pure 0
    with <- transIntVal
    pure $ TransposedNWith id n with

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
    n <- brackets natural <|> pure 0
    pure $ TransposedN id n

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
    n <- brackets natural <|> pure 0
    pure $ TransposedSound id n

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



-- there are three layers that need to be identified: the string that identifies the bounded temporal, the Int that identifies the index of the aural, and then I need a way to identify its type of Value:if it is a sound, gain, speed, etc. For this I could use the constructor of Value...? 

-- checkTransposition:: Voices -> Boolean
-- checkTransposition aMap = not $ elem false $ mapWithIndex (checkTransposition1 aMap Nil) anAuralMap
--     where anAuralMap = mapMaybe (\(Voice _ a) -> Just a) aMap


-- checkTransposition1:: Voices -> List (Tuple String Int) -> String -> List Aural -> Boolean
-- checkTransposition1 aMap refd id aurals = not $ elem false $ mapped
--     where zipped = zip aurals (0..(length aurals))
--           mapped = map (checkTransposition2 aMap refd id) zipped
    
-- checkTransposition2 :: Voices -> List (Tuple String Int) -> String -> Tuple (List Value) Int -> Boolean
-- checkTransposition2 aMap refd id aurals = 






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