module Aural(aural,variationsStr, checkXPitch, getXPitchMap, prog) where

import Prelude

import Data.Identity
import Data.List (List(..), head, tail, elem, (:), filter, fromFoldable, (..), length, zip, concat, mapMaybe)
import Data.Array (fromFoldable, length) as A
import Data.Either
import Data.Int
import Data.Tuple (Tuple(..), fst, snd)
import Data.Map (Map(..), lookup, keys, singleton, toUnfoldable, member, values, unions)
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
    x <- parseValues
    _ <- reserved ";" -- this need to be fixed!
    pure $ AuralExpression x -- (Map Strg Aural)

parseValues:: P (Map String Aural)
parseValues = do
    _ <- pure 1
    id <- voiceId
    xs <- many value
    pure $ singleton id (fromFoldable xs)

value:: P Value
value = do
    _ <- pure 1
    _ <- reservedOp "."
    valType <- choice [try sound,try n, try gain, try pan, try speed, try begin, try end, try vowel, try cutoff, try cutoffh, try inter, try maxw, try minw, try legato, try orbit, try mayeh, try prog, try xeNotes, xeno]
    pure valType

prog:: P Value
prog = do
    _ <- pure 1
    _ <- choice [reserved "prog"]
    _ <- reservedOp "="
    sp <- parseSpan
    xs <- many idOfPitch
    pure $ Prog sp $ fromFoldable xs

idOfPitch:: P (Tuple String (Maybe Int))
idOfPitch = do
    id <- identifier
    n <- (Just <$> brackets natural) <|> pure Nothing
    pure $ Tuple id n

xeNotes:: P Value 
xeNotes = do
    _ <- pure 1
    _ <- choice [reserved "xnotes"]
    _ <- reservedOp "="
    sp <- parseSpan
    l <- choice [try (fromFoldable <$> parseRangeInt), fromFoldable <$> many natural]
    vars <- variationsInt <|> pure Nil
    pure $ XNotes sp l vars

xeno:: P Value 
xeno = do
    _ <- pure 1
    xID <- choice [try shurNot, try centaura, xeno']
    _ <- reservedOp "="
    sp <- parseSpan
    xnL <- choice [try (A.fromFoldable <$> parseRangeInt), many natural]
    pure $ Xeno xID sp $ fromFoldable xnL

centaura:: P (Tuple String (Maybe Int))
centaura = do
    _ <- pure 1
    _ <- reserved "centaura"
    pure $ Tuple "centaura" Nothing

shurNot:: P (Tuple String (Maybe Int))
shurNot = do
    _ <- pure 1
    _ <- reserved "shurNot"
    pure $ Tuple "shurNot" Nothing

xeno':: P (Tuple String (Maybe Int))
xeno' = do
    _ <- pure 1 
    id <- identifier
    n <- (Just <$> brackets natural) <|> pure Nothing
    pure $ Tuple id n

-- Dastgah

-- Intervals: Bozorg 182; Kuchak 114; Tanini 204; Baghie 90.

-- shur: D Eqb F G Aqb Bb C 
-- normalised to 24ET: 0   150      300        500         650        800         1000        1200
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


orbit:: P Value
orbit = do
    _ <- pure 1
    _ <- choice [reserved "orbit"]
    _ <- reservedOp "="
    m <- choice [try makeOrbit, transposeOrbit]
    pure m

transposeOrbit:: P Value
transposeOrbit = do
    id <- voiceId
    n <- brackets natural <|> pure 0
    pure $ TransposedOrbit id n

makeOrbit:: P Value
makeOrbit = do
    _ <- pure 1
    sp <- parseSpan
    nList <- choice [try (A.fromFoldable <$> parseRangeInt), many natural]
    vars <- variationsInt <|> pure Nil
    pure $ Orbit sp (fromFoldable nList) vars

legato:: P Value
legato = do
    _ <- pure 1
    _ <- choice [reserved "legato"]
    _ <- reservedOp "="
    m <- choice [try makeLegato, transposeLegato]
    pure m

transposeLegato:: P Value
transposeLegato = do
    id <- voiceId
    n <- brackets natural <|> pure 0
    pure $ TransposedLegato id n

makeLegato:: P Value
makeLegato = do
    _ <- pure 1
    sp <- parseSpan
    coLs <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ Legato sp (fromFoldable coLs) vars

--
inter:: P Value
inter = do
    _ <- pure 1
    _ <- choice [reserved "inter"]
    _ <- reservedOp "="
    m <- choice [try makeInter, transposeInter]
    pure m

transposeInter:: P Value
transposeInter = do
    id <- voiceId
    n <- brackets natural <|> pure 0
    pure $ TransposedInter id n

makeInter:: P Value
makeInter = do
    _ <- pure 1
    sp <- parseSpan
    coLs <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ Inter sp (fromFoldable coLs) vars

--
minw:: P Value
minw = do
    _ <- pure 1
    _ <- choice [reserved "minw"]
    _ <- reservedOp "="
    m <- choice [try makeMinw, transposeMinw]
    pure m

transposeMinw:: P Value
transposeMinw = do
    id <- voiceId
    n <- brackets natural <|> pure 0
    pure $ TransposedMinW id n

makeMinw:: P Value
makeMinw = do
    _ <- pure 1
    sp <- parseSpan
    coLs <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ MinW sp (fromFoldable coLs) vars

--
maxw:: P Value
maxw = do
    _ <- pure 1
    _ <- choice [reserved "maxw"]
    _ <- reservedOp "="
    m <- choice [try makeMaxw, transposeMaxw]
    pure m

transposeMaxw:: P Value
transposeMaxw = do
    id <- voiceId
    n <- brackets natural <|> pure 0
    pure $ TransposedMaxW id n

makeMaxw:: P Value
makeMaxw = do
    _ <- pure 1
    sp <- parseSpan
    coLs <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ MaxW sp (fromFoldable coLs) vars

--
cutoffh:: P Value
cutoffh = do
    _ <- pure 1
    _ <- choice [reserved "hcutoff"]
    _ <- reservedOp "="
    cutoffh <- choice [try makeCutOffH, transposeCutOffH]
    pure cutoffh

transposeCutOffH:: P Value
transposeCutOffH = do
    id <- voiceId
    n <- brackets natural <|> pure 0
    pure $ TransposedCutOffH id n

makeCutOffH:: P Value
makeCutOffH = do
    _ <- pure 1
    sp <- parseSpan
    coLs <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ CutOffH sp (fromFoldable coLs) vars

cutoff:: P Value
cutoff = do
    _ <- pure 1
    _ <- choice [reserved "cutoff"]
    _ <- reservedOp "="
    cutoff <- choice [try makeCutOff, transposeCutOff]
    pure cutoff

transposeCutOff:: P Value
transposeCutOff = do
    id <- voiceId
    n <- brackets natural <|> pure 0
    pure $ TransposedCutOff id n

makeCutOff:: P Value
makeCutOff = do
    _ <- pure 1
    sp <- parseSpan
    coLs <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ CutOff sp (fromFoldable coLs) vars

vowel:: P Value
vowel = do
    _ <- pure 1
    _ <- choice [reserved "vowel"]
    _ <- reservedOp "="
    vowel <- choice [try makeVowel, transposeVowel]
    pure vowel

transposeVowel:: P Value
transposeVowel = do
    id <- voiceId
    n <- brackets natural <|> pure 0
    pure $ TransposedVowel id n

makeVowel:: P Value
makeVowel = do
    _ <- pure 1
    sp <- parseSpan
    vLs <- choice [many parseVowel]
    vars <- variationsVow <|> pure Nil
    pure $ Vowel sp (fromFoldable vLs) vars

variationsVow:: P (List (Variation String))
variationsVow = do
    _ <- pure 1
    _ <- reserved "&"
    xs <- everyVow `sepBy` (reserved "&")
    pure xs

everyVow:: P (Variation String)
everyVow = do
    _ <- pure 1
    _ <- reserved "every"
    n <- integer
    sp <- parseSpan
    xs <- choice [many parseVowel]
    pure $ Every n sp $ fromFoldable xs

parseVowel:: P String
parseVowel = do
    _ <- pure 1
    x <- choice [charWS 'a' *> pure "a", charWS 'e' *> pure "e", charWS 'i' *> pure "i", charWS 'o' *> pure "o", charWS 'u' *> pure "u"]
    pure x

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
    vars <- variationsNum <|> pure Nil
    pure $ End sp (fromFoldable spdList) vars

begin:: P Value
begin = do
    _ <- pure 1
    _ <- choice [try $ reserved "begin",reserved "begin"]
    _ <- reservedOp "="
    b <- choice [try makeBegin, transposeBegin]
    pure b

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
    vars <- variationsNum <|> pure Nil
    pure $ Begin sp (fromFoldable panList) vars

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
    vars <- variationsNum <|> pure Nil
    pure $ Speed sp (fromFoldable spdList) vars

pan:: P Value
pan = do
    _ <- pure 1
    _ <- choice [try $ reserved "pan",reserved "p"]
    _ <- reservedOp "="
    p <- choice [try makePan, transposePan]
    pure p

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
    vars <- variationsNum <|> pure Nil
    pure $ Pan sp (fromFoldable panList) vars

gain:: P Value
gain = do
    _ <- pure 1
    _ <- choice [reserved "gain"]
    _ <- reservedOp "="
    g <- choice [try makeGain, transposeGain]
    pure g

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
    vars <- variationsNum <|> pure Nil
    pure $ Gain sp (fromFoldable gainList) vars

n:: P Value
n = do
    _ <- pure 1
    _ <- choice [reserved "n"]
    _ <- reservedOp "="
    n <- choice [try makeN, {-try transposeNWith,-} transposeN]
    pure n

-- transposeNWith:: P Value
-- transposeNWith = do
--     id <- voiceId
--     n <- brackets natural <|> pure 0
--     with <- transIntVal
--     pure $ TransposedNWith id n with

transposeN:: P Value
transposeN = do
    id <- voiceId
    n <- brackets natural <|> pure 0
    pure $ TransposedN id n

makeN:: P Value
makeN = do
    _ <- pure 1
    sp <- parseSpan
    nList <- choice [try (A.fromFoldable <$> parseRangeInt), many natural]
    vars <- variationsInt <|> pure Nil
    pure $ N sp (fromFoldable nList) vars

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
    vars <- variationsStr <|> pure Nil
    pure $ Sound sp strList vars

--
variationsStr:: P (List (Variation String))
variationsStr = do
    _ <- pure 1
    _ <- reserved "&"
    xs <- everyStr `sepBy` (reserved "&")
    pure xs

everyStr:: P (Variation String)
everyStr = do
    _ <- pure 1
    _ <- reserved "every"
    n <- integer
    sp <- parseSpan
    xs <- sampleParser
    pure $ Every n sp xs

variationsInt:: P (List (Variation Int))
variationsInt = do
    _ <- pure 1
    _ <- reserved "&"
    xs <- everyInt `sepBy` (reserved "&")
    pure xs

everyInt:: P (Variation Int)
everyInt = do
    _ <- pure 1
    _ <- reserved "every"
    n <- integer
    sp <- parseSpan
    xs <- choice [try parseRangeInt, fromFoldable <$> many natural]
    pure $ Every n sp xs

variationsNum:: P (List (Variation Number))
variationsNum = do
    _ <- pure 1
    _ <- reserved "&"
    xs <- everyNum `sepBy` (reserved "&")
    pure xs

everyNum:: P (Variation Number)
everyNum = do
    _ <- pure 1
    _ <- reserved "every"
    n <- integer
    sp <- parseSpan
    xs <- choice [try parseRangeNum, fromFoldable <$> many parseNumber]
    pure $ Every n sp xs

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


-- tests

-- test' x =
--   case runParser x parseProgram of
--     Left (ParseError err _) -> Left err
--     Right aMap -> Right $ check aMap 

checkXPitch:: List Expression -> Boolean
checkXPitch exs = (checkXPitch' exs) && (checkProg exs)

checkProg :: List Expression -> Boolean
checkProg expressions = not $ elem false $ map (\kn -> func aXenoPitchMap kn) listOfPitchID
  where aXenoPitchMap = getXPitchMap expressions
        listOfPitchID = getProgIDs $ getAuralMap expressions

getProgIDs:: Map String (List Aural) -> List (Tuple String (Maybe Int))
getProgIDs aurals = noteIDs
    where noteIDs = concat $ mapMaybe keepProg $ concat $ concat $ values aurals

keepProg:: Value -> Maybe (List (Tuple String (Maybe Int)))
keepProg (Prog _ lista) = Just lista
keepProg _ = Nothing

checkXPitch' :: List Expression -> Boolean
checkXPitch' expressions = not $ elem false $ map (\kn -> func aXenoPitchMap kn) listOfPitchID
  where aXenoPitchMap = getXPitchMap expressions
        listOfPitchID = getXenoIDs $ getAuralMap expressions

func:: Map String XenoPitch -> Tuple String (Maybe Int) -> Boolean
func mapa (Tuple k Nothing) = case lookup k mapa of
                                Nothing -> false
                                Just xn -> true 
func mapa (Tuple k (Just n)) = case lookup k mapa of
                                Nothing -> false
                                Just xn -> f xn n 

f:: XenoPitch -> Int -> Boolean
f (CPSet s f (Just subs)) indx = indx <= A.length subs  
f Centaura _ = true
f ShurNot  _ = true
f _ _ = false

getXenoIDs:: Map String (List Aural) -> List (Tuple String (Maybe Int))
getXenoIDs aurals = noteIDs
    where noteIDs = mapMaybe keepXeno $ concat $ concat $ values aurals

keepXeno:: Value -> Maybe (Tuple String (Maybe Int))
keepXeno (Xeno id _ _) = Just id
keepXeno _ = Nothing

getAuralMap:: Program -> Map String (List Aural)
getAuralMap program = toListAurals $ map unexpressAural $ filter (\ expression -> isAural expression) program
  where isAural (AuralExpression _) = true
        isAural _ = false

toListAurals:: List (Map String Aural) -> Map String (List Aural)
toListAurals mapas = unions $ map (\k -> toAurals k vals) $ map fst vals
  where vals = concat $ map toUnfoldable mapas
        toAurals key vals = singleton key $ map snd $ filter (\v -> (fst v) == key) vals

unexpressAural:: Expression -> Map String Aural
unexpressAural (AuralExpression x) = x 
unexpressAural _ = empty

getXPitchMap:: Program -> Map String XenoPitch
getXPitchMap program = unions $ map unexpressPitch $ filter (\ expression -> isXPitch expression) program
  where isXPitch (XenoPitchExpression _) = true
        isXPitch _ = false

unexpressPitch:: Expression -> Map String XenoPitch
unexpressPitch (XenoPitchExpression x) = x 
unexpressPitch _ = empty

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