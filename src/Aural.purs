module Aural(aural,variationsStr, checkXPitch, getPitchMap, prog) where

import Prelude

import Data.Identity
import Data.List (List(..), head, tail, elem, (:), filter, fromFoldable, (..), length, zip, concat, mapMaybe, reverse, foldl)
import Data.List.NonEmpty (NonEmptyList, toList)
import Data.List.Lazy as Lz
import Data.Array (fromFoldable, length) as A
import Data.Either
import Data.Int
import Data.Tuple (Tuple(..), fst, snd)
import Data.Map (Map(..), lookup, keys, singleton, toUnfoldable, member, values, unions)
import Data.Map (fromFoldable) as M
import Data.Maybe (Maybe(..), fromMaybe)
-- import Data.Set as Set
import Data.String as Str

import Effect.Unsafe (unsafePerformEffect)
import Effect.Shuffle (shuffle)

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
import Variant
import AuralInACan

type P = ParserT String Identity

aural:: P Expression
aural = do 
    _ <- pure 1
    x <- parseValues
    _ <- reserved ";" -- this need to be fixed!
    pure $ AuralExpression x -- (Map Strg Aural)

-- parseValues:: P (Map String Aural)
-- parseValues = do
--     _ <- pure 1
--     id <- voiceId
--     xs <- many value
--     pure $ singleton id (fromFoldable xs)

-- can[0].s = "bd cp hh" .n = 0 3 2 4 .pan = 0.5 0.4 0.7 0.4 0.5 .gain = 0.9 0.7 0.8 0.9 1.1 0.9 1.2 0.7 0.8 0.9;


parseValues:: P (Map String Aural)
parseValues = do
    _ <- pure 1
    whitespace
    id <- voiceId
    index <- indexForID
    xs <- many value -- List (Tuple Value Variant)
    pure $ auralInACan id index $ fromFoldable xs

indexForID:: P (Maybe (List Int))
indexForID = do 
    _ <- pure 1
    whitespace
    xs <- indexForID' <|> pure Nothing
    pure xs

indexForID':: P (Maybe (List Int))
indexForID' = do 
    _ <- pure 1
    xs <- brackets (naturalOrFloat `sepBy` comma)
    pure $ Just $ map toInt' $ fromFoldable xs

toInt':: Either Int Number -> Int 
toInt' (Left n) = n
toInt' (Right x) = round x

-- value:: P Value
-- value = do
--     _ <- pure 1
--     _ <- reservedOp "."
--     valType <- choice [try sound,try n, try gain, try pan, try speed, try begin, try end, try vowel, try cutoff, try cutoffh, try inter, try maxw, try minw, try legato, try orbit, try mayeh, try prog, try xeNotes, xeno]
--     pure valType

value:: P (Tuple Value Variant) -- WORKS
value = do
    _ <- pure 1
    _ <- reservedOp "."
    val <- choice [try sound, try n, try gain, try pan, try speed, try begin, try end, try vowel, try cutoff, try cutoffh, try inter, try maxw, try minw, try legato, try orbit, try alpha, try beta, try gamma, try mayeh, try prog, try xeNotes, xeno] -- should be a tuple value variant
    op <- operadores <|> (pure mulVar)
    trans <- choice [try transposer, parens $ parsesStepVariant] <|> (pure $ VList (VInt 1:Nil))
    pure $ Tuple val $ operate val trans op



parsesStepVariant:: P Variant
parsesStepVariant = do 
    _ <- pure 1
    whitespace
    -- fs <- funcs 
    _ <- reserved "step" 
    numSteps <- natural
    stepSize <- parseNumber
    startPt <- parseNumber
    pure $ step numSteps stepSize startPt -- create a foldl (\l f -> f l) steps functions pattern for variants!!

step:: Int -> Number -> Number -> Variant
step numOfSteps stepSize startPoint = VList $ map (\n -> VNum n) $ fromFoldable $ Lz.take numOfSteps $ Lz.iterate (\nu -> nu + stepSize) startPoint


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




transposer:: P Variant
transposer = do 
    _ <- pure 0
    xs <- brackets $ (toVariant <$> naturalOrFloat) `sepBy` comma
    pure $ VList $ fromFoldable xs

toVariant:: Either Int Number -> Variant
toVariant (Left n) = VInt n 
toVariant (Right x) = VNum x

operadores:: P (Variant -> Variant -> Variant)
operadores = choice [reservedOp "*" *> pure mulVar, reservedOp "+" *> pure addVar]

--

--
prog:: P Value
prog = do
    _ <- pure 1
    _ <- choice [reserved "prog"]
    _ <- reservedOp "="
    sp <- parseSpan <|> pure CycleEvent
    fs <- funcs
    xs <- many idOfPitch
    pure $ Prog sp $ foldl (\l f -> f l) (fromFoldable xs) fs

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
    sp <- parseSpan <|> pure CycleEvent
    fs <- funcs
    l <- choice [try (fromFoldable <$> parseRangeInt), fromFoldable <$> many natural]
    vars <- variationsInt <|> pure Nil
    pure $ XNotes sp (foldl (\l f -> f l) l fs) vars

alpha:: P Value
alpha = do
    _ <- pure 1
    _ <- choice [reserved "alpha"]
    _ <- reservedOp "="
    a <- choice [try makeAlpha]
    pure a

makeAlpha:: P Value 
makeAlpha = do
    _ <- pure 1
    sp <- parseSpan <|> pure CycleEvent
    fs <- funcs
    alphaList <- choice [try (A.fromFoldable <$> parseRangeInt), many natural]
    pure $ Alpha sp $ foldl (\l f -> f l) (fromFoldable alphaList) fs

beta:: P Value
beta = do
    _ <- pure 1
    _ <- choice [reserved "beta"]
    _ <- reservedOp "="
    b <- choice [try makeBeta]
    pure b

makeBeta:: P Value 
makeBeta = do
    _ <- pure 1
    sp <- parseSpan <|> pure CycleEvent
    fs <- funcs
    betaList <- choice [try (A.fromFoldable <$> parseRangeInt), many natural]
    pure $ Beta sp $ foldl (\l f -> f l) (fromFoldable betaList) fs

gamma:: P Value
gamma = do
    _ <- pure 1
    _ <- choice [reserved "gamma"]
    _ <- reservedOp "="
    g <- choice [try makeGamma]
    pure g

makeGamma:: P Value 
makeGamma = do
    _ <- pure 1
    sp <- parseSpan <|> pure CycleEvent
    fs <- funcs
    gammaList <- choice [try (A.fromFoldable <$> parseRangeInt), many natural]
    pure $ Gamma sp $ foldl (\l f -> f l) (fromFoldable gammaList) fs

xeno:: P Value 
xeno = do
    _ <- pure 1
    xID <- choice [try shurNot, try shurNot8, xeno']
    _ <- reservedOp "="
    sp <- parseSpan <|> pure CycleEvent
    fs <- funcs
    xnL <- choice [try (A.fromFoldable <$> parseRangeInt), many natural]
    pure $ Xeno xID sp $ foldl (\l f -> f l) (fromFoldable xnL) fs

shurNot8:: P (Tuple String (Maybe Int))
shurNot8 = do
    _ <- pure 1
    _ <- reserved "shurNot8"
    pure $ Tuple "shurNot8" Nothing

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

mayeh:: P Value
mayeh = do
    _ <- pure 1
    choice [try shur, try segah, try nava, try homayun, try chahargah, try mahur, rastPanjgah]

shur:: P Value
shur = do
    _ <- pure 1
    _ <- choice [reserved "shur"]
    _ <- reservedOp "="
    shur <- makeShur
    pure shur

makeShur:: P Value 
makeShur = do
    _ <- pure 1
    sp <- parseSpan  <|> pure CycleEvent
    fs <- funcs
    shurList <- choice [try (A.fromFoldable <$> parseRangeInt), many shurNote]
    pure $ Dastgah sp (Shur $ foldl (\l f -> f l) (fromFoldable shurList) fs)

shurNote:: P Int
shurNote = do
    _ <- pure 1
    x <- choice [(try $ reserved "\\a") *> pure 0, (try $ reserved "\\f") *> pure 1, (try $ reserved "\\m") *> pure 5, natural]
    pure x

segah:: P Value
segah = do
    _ <- pure 1
    _ <- choice [reserved "segah"]
    _ <- reservedOp "="
    segah <- makeSegah
    pure segah

makeSegah:: P Value 
makeSegah = do
    _ <- pure 1
    sp <- parseSpan <|> pure CycleEvent
    fs <- funcs
    segahList <- choice [try (A.fromFoldable <$> parseRangeInt), many segahNote]
    pure $ Dastgah sp (Segah $ foldl (\l f -> f l) (fromFoldable segahList) fs)

segahNote:: P Int
segahNote = do
    _ <- pure 1
    x <- choice [(try $ reserved "\\m") *> pure 1, (try $ reserved "\\a") *> pure 2, (try $ reserved "\\f") *> pure 2, (try $ reserved "\\s") *> pure 2, natural]
    pure x

nava:: P Value
nava = do
    _ <- pure 1
    _ <- choice [reserved "nava"]
    _ <- reservedOp "="
    nava <- makeNava
    pure nava

makeNava:: P Value 
makeNava = do
    _ <- pure 1
    sp <- parseSpan <|> pure CycleEvent
    fs <- funcs
    navaList <- choice [try (A.fromFoldable <$> parseRangeInt), many navaNote]
    pure $ Dastgah sp (Nava $ foldl (\l f -> f l) (fromFoldable navaList) fs)

navaNote:: P Int
navaNote = do
    _ <- pure 1
    x <- choice [(try $ reserved "\\i") *> pure 2, (try $ reserved "\\a") *> pure 3, (try $ reserved "\\f") *> pure 4, natural]
    pure x

homayun:: P Value
homayun = do
    _ <- pure 1
    _ <- choice [reserved "homayun"]
    _ <- reservedOp "="
    homayun <- makeHomayun
    pure homayun

makeHomayun:: P Value 
makeHomayun = do
    _ <- pure 1
    sp <- parseSpan <|> pure CycleEvent
    fs <- funcs
    homayunList <- choice [try (A.fromFoldable <$> parseRangeInt), many homayunNote]
    pure $ Dastgah sp (Homayun $ foldl (\l f -> f l) (fromFoldable homayunList) fs)

homayunNote:: P Int
homayunNote = do
    _ <- pure 1
    x <- choice [(try $ reserved "\\i") *> pure 3, (try $ reserved "\\a") *> pure 2, (try $ reserved "\\f") *> pure 4, (try $ reserved "\\s") *> pure 5, natural]
    pure x

chahargah:: P Value
chahargah = do
    _ <- pure 1
    _ <- choice [reserved "chahargah"]
    _ <- reservedOp "="
    chahargah <- makeChahargah
    pure chahargah

makeChahargah:: P Value 
makeChahargah = do
    _ <- pure 1
    sp <- parseSpan <|> pure CycleEvent
    fs <- funcs
    chahargahList <- choice [try (A.fromFoldable <$> parseRangeInt), many chahargahNote]
    pure $ Dastgah sp (Chahargah $ foldl (\l f -> f l) (fromFoldable chahargahList) fs)

chahargahNote:: P Int
chahargahNote = do
    _ <- pure 1
    x <- choice [(try $ reserved "\\f") *> pure 0, (try $ reserved "\\a") *> pure 5, natural]
    pure x

mahur:: P Value
mahur = do
    _ <- pure 1
    _ <- choice [reserved "mahur"]
    _ <- reservedOp "="
    mahur <- makeMahur
    pure mahur

makeMahur:: P Value 
makeMahur = do
    _ <- pure 1
    sp <- parseSpan <|> pure CycleEvent
    fs <- funcs
    mahurList <- choice [try (A.fromFoldable <$> parseRangeInt), many mahurNote]
    pure $ Dastgah sp (Mahur $ foldl (\l f -> f l) (fromFoldable mahurList) fs)

mahurNote:: P Int
mahurNote = do
    _ <- pure 1
    x <- choice [(try $ reserved "\\f") *> pure 0, (try $ reserved "\\a") *> pure 0, (try $ reserved "\\s") *> pure 1, natural]
    pure x

rastPanjgah:: P Value
rastPanjgah = do
    _ <- pure 1
    _ <- choice [reserved "rastPanjgah"]
    _ <- reservedOp "="
    rastPanjgah <- makeRastPanjgah
    pure rastPanjgah

makeRastPanjgah:: P Value 
makeRastPanjgah = do
    _ <- pure 1
    sp <- parseSpan <|> pure CycleEvent
    fs <- funcs
    rastPanjgahList <- choice [try (A.fromFoldable <$> parseRangeInt), many rastPanjgahNote]
    pure $ Dastgah sp $ RastPanjgah $ foldl (\l f -> f l) (fromFoldable rastPanjgahList) fs

rastPanjgahNote:: P Int
rastPanjgahNote = do
    _ <- pure 1
    x <- choice [(try $ reserved "\\f") *> pure 3, (try $ reserved "\\a") *> pure 3, natural]
    pure x

orbit:: P Value
orbit = do
    _ <- pure 1
    _ <- choice [reserved "orbit"]
    _ <- reservedOp "="
    m <- choice [try makeOrbit]
    pure m

makeOrbit:: P Value
makeOrbit = do
    _ <- pure 1
    sp <- parseSpan <|> pure CycleEvent
    fs <- funcs
    nList <- choice [try (A.fromFoldable <$> parseRangeInt), many natural]
    vars <- variationsInt <|> pure Nil
    pure $ Orbit sp (foldl (\l f -> f l) (fromFoldable nList) fs) vars

legato:: P Value
legato = do
    _ <- pure 1
    _ <- choice [reserved "legato"]
    _ <- reservedOp "="
    m <- choice [try makeLegato]
    pure m

makeLegato:: P Value
makeLegato = do
    _ <- pure 1
    sp <- parseSpan <|> pure CycleEvent
    fs <- funcs
    coLs <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ Legato sp (foldl (\l f -> f l) (fromFoldable coLs) fs) vars

--
inter:: P Value
inter = do
    _ <- pure 1
    _ <- choice [reserved "inter"]
    _ <- reservedOp "="
    m <- choice [try makeInter]
    pure m

makeInter:: P Value
makeInter = do
    _ <- pure 1
    sp <- parseSpan <|> pure CycleEvent
    fs <- funcs
    coLs <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ Inter sp (foldl (\l f -> f l) (fromFoldable coLs) fs) vars

--
minw:: P Value
minw = do
    _ <- pure 1
    _ <- choice [reserved "minw"]
    _ <- reservedOp "="
    m <- choice [try makeMinw]
    pure m

makeMinw:: P Value
makeMinw = do
    _ <- pure 1
    sp <- parseSpan  <|> pure CycleEvent
    fs <- funcs
    coLs <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ MinW sp (foldl (\l f -> f l) (fromFoldable coLs) fs) vars

--
maxw:: P Value
maxw = do
    _ <- pure 1
    _ <- choice [reserved "maxw"]
    _ <- reservedOp "="
    m <- choice [try makeMaxw]
    pure m

makeMaxw:: P Value
makeMaxw = do
    _ <- pure 1
    sp <- parseSpan  <|> pure CycleEvent
    fs <- funcs
    coLs <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ MaxW sp (foldl (\l f -> f l) (fromFoldable coLs) fs) vars

--
cutoffh:: P Value
cutoffh = do
    _ <- pure 1
    _ <- choice [reserved "hcutoff"]
    _ <- reservedOp "="
    cutoffh <- choice [try makeCutOffH]
    pure cutoffh

makeCutOffH:: P Value
makeCutOffH = do
    _ <- pure 1
    sp <- parseSpan  <|> pure CycleEvent
    fs <- funcs
    coLs <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ CutOffH sp (foldl (\l f -> f l) (fromFoldable coLs) fs) vars

cutoff:: P Value
cutoff = do
    _ <- pure 1
    _ <- choice [reserved "cutoff"]
    _ <- reservedOp "="
    cutoff <- choice [try makeCutOff]
    pure cutoff

makeCutOff:: P Value
makeCutOff = do
    _ <- pure 1
    sp <- parseSpan  <|> pure CycleEvent
    fs <- funcs
    coLs <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ CutOff sp (foldl (\l f -> f l) (fromFoldable coLs) fs) vars

vowel:: P Value
vowel = do
    _ <- pure 1
    _ <- choice [reserved "vowel"]
    _ <- reservedOp "="
    vowel <- choice [try makeVowel]
    pure vowel

makeVowel:: P Value
makeVowel = do
    _ <- pure 1
    sp <- parseSpan  <|> pure CycleEvent
    fs <- funcs
    vLs <- choice [many parseVowel]
    vars <- variationsVow <|> pure Nil
    pure $ Vowel sp (foldl (\l f -> f l) (fromFoldable vLs) fs) vars

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
    sp <- parseSpan  <|> pure CycleEvent
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
    end <- choice [try makeEnd]
    pure end

makeEnd:: P Value
makeEnd = do
    _ <- pure 1
    sp <- parseSpan  <|> pure CycleEvent
    fs <- funcs
    endList <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ End sp (foldl (\l f -> f l) (fromFoldable endList) fs) vars

begin:: P Value
begin = do
    _ <- pure 1
    _ <- choice [try $ reserved "begin",reserved "begin"]
    _ <- reservedOp "="
    b <- choice [try makeBegin]
    pure b

makeBegin:: P Value
makeBegin = do
    _ <- pure 1
    sp <- parseSpan  <|> pure CycleEvent
    fs <- funcs
    beginList <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ Begin sp (foldl (\l f -> f l) (fromFoldable beginList) fs) vars

speed:: P Value
speed = do
    _ <- pure 1
    _ <- choice [reserved "speed"]
    _ <- reservedOp "="
    n <- choice [try makeSpeed]
    pure n

makeSpeed:: P Value
makeSpeed = do
    _ <- pure 1
    sp <- parseSpan  <|> pure CycleEvent
    fs <- funcs
    spdList <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ Speed sp (foldl (\l f -> f l) (fromFoldable spdList) fs) vars

pan:: P Value
pan = do
    _ <- pure 1
    _ <- choice [try $ reserved "pan",reserved "p"]
    _ <- reservedOp "="
    p <- choice [try makePan]
    pure p

makePan:: P Value
makePan = do
    _ <- pure 1
    sp <- parseSpan  <|> pure CycleEvent
    fs <- funcs
    panList <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ Pan sp (foldl (\l f -> f l) (fromFoldable panList) fs) vars

gain:: P Value
gain = do
    _ <- pure 1
    _ <- choice [reserved "gain"]
    _ <- reservedOp "="
    g <- choice [try makeGain]
    pure g

makeGain:: P Value
makeGain = do
    _ <- pure 1
    sp <- parseSpan  <|> pure CycleEvent
    fs <- funcs
    gainList <- choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    vars <- variationsNum <|> pure Nil
    pure $ Gain sp (foldl (\l f -> f l) (fromFoldable gainList) fs) vars

n:: P Value
n = do
    _ <- pure 1
    _ <- choice [reserved "n"]
    _ <- reservedOp "="
    n <- choice [try makeN]
    pure n

makeN:: P Value
makeN = do
    _ <- pure 1
    sp <- parseSpan  <|> pure CycleEvent
    fs <- funcs
    nList <- choice [try (A.fromFoldable <$> parseRangeInt), A.fromFoldable <$> toList <$> many1 natural]
    vars <- variationsInt <|> pure Nil
    pure $ N sp (foldl (\l f -> f l) (fromFoldable nList) fs) vars

sound:: P Value
sound = do
    _ <- pure 1
    _ <- choice [try $ reserved "sound",reserved "s"]
    _ <- reservedOp "="
    sound <- choice [try makeSound]
    pure sound

makeSound:: P Value
makeSound = do
    _ <- pure 1
    sp <- parseSpan  <|> pure CycleEvent
    fs <- funcs
    strList <- sampleParser 
    vars <- variationsStr <|> pure Nil
    pure $ Sound sp (foldl (\l f -> f l) strList fs) vars

transNumVal:: P (List (Number -> Number))
transNumVal = do 
    _ <- pure 1
    op <- choice [reservedOp "+" *> pure add, reservedOp "*" *> pure mul]
    numList <- brackets $ choice [try (A.fromFoldable <$> parseRangeNum), many parseNumber]
    pure $ map op $ fromFoldable numList
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
funcs:: forall a. P (List (List a -> List a))
funcs = do
    _ <- pure 1
    fs <- fun `sepBy` (reserved "$")
    pure $ fromFoldable fs

fun:: forall a. P (List a -> List a)
fun = do 
    _ <- pure 1
    x <- choice [try rev, try pal, try shuf] <|> pure nada
    pure x

rev:: forall a. P (List a -> List a)
rev = do 
    _ <- pure 1
    r <- reserved "rev" *> pure reverse
    pure r 

pal:: forall a. P (List a -> List a)
pal = do 
    _ <- pure 1
    p <- reserved "pal" *> pure palindrome
    pure p

palindrome:: forall a. List a -> List a
palindrome xs = concat (xs : reverse xs : Nil)

shuf:: forall a. P (List a -> List a) 
shuf = do 
    _ <- pure 1
    shu <- reserved "shuffle" *> pure shuffle'
    pure shu

shuffle' :: forall a. List a -> List a
shuffle' xs = unsafePerformEffect $ shuffle xs

nada:: forall a. List a -> List a
nada xs = xs 


-- funca xs = map (\x -> x) xs

-- List operations:
-- substitute this on every value: choice [try (fromFoldable <$> parseRangeInt), fromFoldable <$> many natural]
-- be able to do: reverse, stutter, pyramid, palindrome, etc...

--- is here the right place to also produce: sine $ range 0 10, etc.?


parseSpan:: P Span
parseSpan = do
    _ <- pure 1
    x <- choice [
                   reserved "-_" *>  pure CycleInBlock <|> reserved ":cycleIn" *> pure CycleInBlock
                 , try $ reserved "_-" *>  pure CycleBlock <|> reserved ":cycleBlock" *> pure CycleBlock
                 , try $ reserved "_-_" *> pure SpreadBlock <|> reserved ":spread" *> pure SpreadBlock
                 , reserved "_" *>   pure  CycleEvent <|> reserved ":cycle" *> pure CycleEvent
                ]  <|> pure CycleEvent
    pure x

--

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
  where aXenoPitchMap = getPitchMap expressions
        listOfPitchID = getProgIDs $ getAuralMap expressions

getProgIDs:: Map String (List Aural) -> List (Tuple String (Maybe Int))
getProgIDs aurals = noteIDs
    where noteIDs = concat $ mapMaybe keepProg $ concat $ concat $ values aurals

keepProg:: Value -> Maybe (List (Tuple String (Maybe Int)))
keepProg (Prog _ lista) = Just lista
keepProg _ = Nothing

checkXPitch' :: List Expression -> Boolean
checkXPitch' expressions = not $ elem false $ map (\kn -> func aXenoPitchMap kn) listOfPitchID
  where aXenoPitchMap = getPitchMap expressions
        listOfPitchID = getXenoIDs $ getAuralMap expressions

func:: Map String Tuning -> Tuple String (Maybe Int) -> Boolean
func mapa (Tuple "shurNot8" Nothing) = true
func mapa (Tuple "shurNot" Nothing) = true
func mapa (Tuple k Nothing) = case lookup k mapa of
                                Nothing -> false
                                Just xn -> true 
func mapa (Tuple k (Just n)) = case lookup k mapa of
                                Nothing -> false
                                Just xn -> f xn n 

f:: Tuning -> Int -> Boolean
f (CPSet s f (Just subs)) indx = indx <= A.length subs  
f ShurNot8 _ = true
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

getPitchMap:: Program -> Map String Tuning
getPitchMap program = unions $ map unexpressPitch $ filter (\ expression -> isPitch expression) program
  where isPitch (PitchExpression _) = true
        isPitch _ = false

unexpressPitch:: Expression -> Map String Tuning
unexpressPitch (PitchExpression x) = x 
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