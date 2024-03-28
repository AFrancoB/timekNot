module Parser(temporal, check, parseProgram, replica, getTemporalMap, getAuralMap, test, testP,xPitchExpression, expression, getVantageMap, parseDate, utcA) where

import Prelude

import Data.Identity
import Data.List (List(..), head, tail, elem, (:), concat)
import Data.List (fromFoldable, filter) as L
import Data.Array (fromFoldable) as A
import Data.Either
import Data.Int
import Data.String (take, length)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Map (Map(..), filter, lookup, keys, singleton, fromFoldable, toUnfoldable, member, unions, empty)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Maybe
import Data.Rational (Rational(..), toRational, fromInt, (%))

import Data.DateTime (exactDate, Year(..), Month(..), Day(..))

import Data.FunctorWithIndex (mapWithIndex)

import Data.String.CodeUnits (fromCharArray)
import Data.String (split, Pattern)

import Data.Formatter.DateTime (Formatter, parseFormatString, unformat)
import Data.Formatter.Number (Formatter, parseFormatString, unformat) as N

import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration


import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Combinators.Array (many)
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)

import AST
import Rhythm
import Aural

type P = ParserT String Identity

testP str = runParser str parseProgram

-- ISSUES
---- range of Numbers is absolutely broken. DO NOT USE
---- Make all tests: start testing all the checks: tempoCheck, idCheck replicaCheck!!!

-- TO DO LIST October 17th:
---- refactor Aural and Value
---- finish refactor of transposeWith 

---- implement keyword last DONE
---- implement copy of temporals: v1 <- v0 DONE
---- implement many aurals for one temporal DONE
---- implement weight

-----
-- minor goals: 

-- for now: pitch from the middle east DONE partially:structParser
-- refactor show of my data types. They mostly suck.

-- implementaciones siguientes:
  -- xenopitch -- PATHWAY OPENED. CPS ARE IMPLEMENTED

  -- refer to the most recent version of tempi-purs DONE (instead of pulling it from the internet I copied the code from the repo) DONE

  -- events specific to concrete indexes: 2-0.1 "cp" -- should generate a cp sound only at 2-0.1

  -- implement unleash parser 

  -- razgado  -- doable after concat
-- r <- razgado 0.2 5 -- donde 0.2 es dur en secs y 5 es numero de notas 
-- r.sound = "grandpiano" .speed = _-_ 1 1.1 1.2 1.3 1.4 1.5;

---- doable after razgado
  -- canonise (tms) cp rhythmic <<- vantage (id index (_-4 means every fourth event in block)) 
-- v0 <- diverge | xxxx :|
-- c <- canonise cpm(100,200,300,400,500) cp: 5 | xxxxx || <<- v0 _-4

  -- concat temporals  -- Doable soon
-- v0 <- (2 afterEval) (3)              | xxox ||
-- v1 <- (2 afterEval) (1) (v0 3:5) | xxx[xx]ox ||
-- w <- v0 <> v1 :|

  -- refactor auralSpecs!!!!!!

  -- acceleration in unlooped events (how to represent this? and calculate the durations of the events??)

  -- Start with post-evaluation CPstry utcA, 
-- Vantage.build = "first" (100 secsFromEval)
-- Vantage.build = "second" (100 xBeatsFromEval)
-- Vantage.move = "first" (100 secsFromEval)
-- Vantage.move = "second" (100 xBeatsFromEval)
-- Vantage.move = "first" (3 fromCurrentPosition)
-- Vantage.remove = "first"

-- v <- first cFrom tm | xxxx :|



  -- grand project:
  -- Monoid programs:: Map ZoneIndex Voices
  ----- each zone has its eval time. Every zone accesses temporals and aurals
  ----- trans-zone relationships: 
        --  two zones cannot name equally a temporal
        --  priority given to referencing (rather than referenced) zones
        -- what are the implications of this in an ensemble?


parseProgram:: P Program
parseProgram = do
  whitespace
  xs <- many expression
  eof
  pure $ L.fromFoldable xs

expression:: P Expression
expression = do
  _ <- pure 1
  choice [try timeExpression, try aural, try vantageExpression, xPitchExpression]
  
xPitchExpression:: P Expression
xPitchExpression = do
  _ <- pure 1
  x <- braces $ many $ xPitch
  pure $ XenoPitchExpression $ unions x

xPitch:: P (Map String XenoPitch)
xPitch = do
  _ <- pure 1
  id <- identifier
  _ <- reserved "<-"
  x <- choice [try cpSet] --, try mos, try edo]
  _ <- reserved ";"
  pure $ singleton id x

cpSet:: P XenoPitch
cpSet = do
  _ <- pure 1
  _ <- reserved "cps"
  sz <- natural
  factors <- parens $ many natural
  subsets' <- subsets <|> pure Nothing
  pure $ CPSet sz factors subsets'

subsets:: P (Maybe (Array Subset))
subsets = do
  _ <- pure 1
  _ <- reserved "|"
  xs <- chooseSubset `sepBy` comma
  pure (Just $ A.fromFoldable xs)

chooseSubset:: P Subset
chooseSubset = choice [try intersection, try difference, try union, includes]

includes:: P Subset
includes = do
  _ <- pure 1
  n <- natural
  pure $ Subset n

union:: P Subset
union = do
  _ <- pure 1
  ns <- natural `sepBy` (reservedOp "u")
  pure $ Unions $ A.fromFoldable ns

intersection:: P Subset
intersection = do 
  _ <- pure 1
  a <- natural
  _ <- reservedOp "n"
  b <- natural
  pure $ Intersection a b

difference:: P Subset
difference = do
  _ <- pure 1
  a <- natural
  _ <- reservedOp "c"
  b <- natural
  pure $ Difference a b

--
vantageExpression:: P Expression
vantageExpression = do
  _ <- pure 1
  x <- vantage
  pure $ VantagePointExpression x

vantage:: P (Map String Vantage)
vantage = do
  _ <- pure 1
  id <- identifier
  x <- choice [buildA, moveA, removeA]
  _ <- charWS ';'
  pure $ singleton id x

-- MyID.lift = 20 beats from eval;
buildA:: P Vantage
buildA = do
  _ <- pure 1
  _ <- reserved ".lift"
  _ <- reservedOp "="
  x <- choice [try beatA, try secsA, utcA]
  pure $ Build x

utcA:: P TimePoint
utcA = do
  _ <- pure 1
  d <- date
  t <- choice [parens timeOfDay, timeOfDay] 
  local <- (parens $ local) <|> (reserved "so-called utc") *> pure 0
  tiempo <- liftEither $ parseDate (d <> " " <> t)
  result <- liftMaybe (\_ -> "Not a local time") $ adjust (Hours $ toNumber local) tiempo
  pure $ UTC result

date:: P String
date = do
  _ <- pure 1
  y <- natural
  m <- identifier
  d <- natural
  pure (show y <> "-" <> m <> "-" <>  show d)

timeOfDay:: P String
timeOfDay = do
  _ <- pure 1
  h <- natural
  _ <- reservedOp ":"
  m <- natural
  _ <- reservedOp ":"
  s <- natural
  pure ((padHour h) <> ":" <> show m <> ":" <>  show s)

local:: P Int
local = do
  _ <- pure 1
  _ <- reserved "so-called utc"
  op <- choice [reservedOp "-" *> pure 1, reservedOp "+" *> pure (-1)]
  n <- natural
  pure (n * op)

padHour:: Int -> String
padHour n = if (length iAsStr) > 1 then iAsStr else "0" <> iAsStr
  where iAsStr = show n 

parseFormatter:: Either String Formatter
parseFormatter = parseFormatString "YYYY-MMMM-DD HH:m:s"

parseDate:: String -> Either String DateTime
parseDate s = case parseFormatter of 
                Left x -> Left x
                Right x -> unformat x s

beatA:: P TimePoint
beatA = do
  num <- naturalOrFloat
  _ <- reserved "beats from eval"
  pure $ Beat $ toRat (toNumber' num)

secsA:: P TimePoint
secsA = do
  num <- naturalOrFloat
  _ <- reserved "secs from eval"
  pure $ Secs $ toRat (toNumber' num)

-- MyID.move = 20 beats from eval;
moveA:: P Vantage
moveA = do
  _ <- pure 1
  _ <- reserved ".move"
  _ <- reservedOp "="
  x <- choice [try beatMoveA, secsMoveA]
  pure $ Move x

beatMoveA:: P (Either Rational Rational)
beatMoveA = do
  num <- naturalOrFloat
  _ <- reserved "beats from current"
  pure $ Left $ toRat (toNumber' num)

secsMoveA:: P (Either Rational Rational)
secsMoveA = do
  num <- naturalOrFloat
  _ <- reserved "secs from current"
  pure $ Right $ toRat (toNumber' num)

removeA:: P Vantage
removeA = do
  _ <- pure 1
  _ <- reserved ".remove"
  pure $ Remove

timeExpression:: P Expression
timeExpression = do
  _ <- pure 1
  x <- temporal
  pure $ TimeExpression x

temporal:: P (Map String Temporal)
temporal = do
  _ <- pure 1
  choice [try replica, polytemporalRelation]

replica:: P (Map String Temporal)
replica = do
  _ <- pure 1
  id <- voiceId
  _ <- reserved "<-"
  id2 <- voiceId 
  _ <- semi
  pure $ singleton id $ Replica id2

polytemporalRelation:: P (Map String Temporal)
polytemporalRelation = do
  _ <- pure 1
  p <- choice [try kairos, try metric, try converge]
  _ <- charWS '|'
  r <- rhythmic
  l <- choice [(strWS "||" *> pure false), (strWS ":|" *> pure true)]
  pure $ singleton (fst p) $ Temporal (snd p) r l

kairos:: P (Tuple String Polytemporal)
kairos = do
  _ <- pure 1
  id <- voiceId
  _ <- reserved "<-"
  n <- choice [secsFromEval, atEval]
  tm <- parens tempoMark <|> pure XTempo
  pure $ Tuple id $ Kairos n tm

secsFromEval:: P Number 
secsFromEval = do
  _ <- pure 1
  n <- naturalOrFloat
  _ <- reserved "secsAfterEval"
  pure $ toNumber' n

atEval:: P Number
atEval = do
  _ <- pure 1
  _ <- reserved "atEval"
  pure 0.01

metric:: P (Tuple String Polytemporal)
metric = do 
  _ <- pure 1
  id <- voiceId
  _ <- reserved "<-"
  polytemporal <- choice [try divergingMetric, convergingMetric]
  pure $ Tuple id polytemporal

divergingMetric:: P Polytemporal
divergingMetric = do
  _ <- pure 1
  _ <- reserved "diverge"
  tm <- parens tempoMark <|> pure XTempo -- the alternative should be same as estuary tempo
  pure $ Metric (ProcessTo 0 Origin) (Process 0) tm

convergingMetric:: P Polytemporal
convergingMetric = do
  _ <- pure 1
  cTo <- choice [try $ parens parsePercenTo, try $ parens parseProcessTo, parens parseStructureTo] <|> (pure $ ProcessTo 0 Snap)
  cFrom <- choice [try $ parens cFromPercen, try $ parens cFromProcess, parens cFromStructure]
  tm <- parens tempoMark <|> pure XTempo -- the alternative should be same as estuary tempo
  pure $ Metric cTo cFrom tm

converge:: P (Tuple String Polytemporal)
converge = do
  _ <- pure 1
  id <- voiceId
  _ <- reserved "<-"
  polytemporal <- choice [try diverging, try converging, novus]
  pure $ Tuple id polytemporal

diverging:: P Polytemporal
diverging = do
  _ <- pure 1
  _ <- whitespace
  voice <- voiceId
  _ <- reserved "diverge"
  tm <- parens tempoMark <|> pure XTempo -- the alternative should be same as estuary tempo
  pure $ Converge voice (ProcessTo 0 Origin) (Process 0) tm

converging:: P Polytemporal
converging = do
  _ <- pure 1
  _ <- whitespace
  voice <- voiceId -- choice between metricVoice or arbitrary name of a voice
  cTo <- choice [try cToLast, try $ parens parsePercenTo, try $ parens parseProcessTo, parens parseStructureTo]
  cFrom <- choice [try cFromLast, try $ parens cFromPercen, try $ parens cFromProcess, parens cFromStructure]
  tm <- parens tempoMark <|> pure XTempo -- the alternative should be same as estuary tempo
  pure $ Converge voice cTo cFrom tm

novus:: P Polytemporal
novus = do
  _ <- pure 1
  _ <- whitespace
  _ <- reserved "Novus"
  _ <- reservedOp "."
  vantID <- voiceId 
  cFrom <- choice [try cFromLast, try $ parens $ cFromPercen, try $ parens $ cFromProcess, parens cFromStructure]
  tm <- parens tempoMark <|> pure XTempo 
  pure $ Novus vantID cFrom tm

cFromLast:: P ConvergeFrom
cFromLast = do
  _ <- pure 1
  _ <- strWS "last"
  pure $ Last 

cFromPercen:: P ConvergeFrom
cFromPercen = do
  _ <- pure 1
  p <- naturalOrFloat
  _ <- charWS '%'
  pure $ Percen (toNumber' p)

cFromProcess:: P ConvergeFrom
cFromProcess = do
  _ <- pure 1
  e <- natural
  pure $ Process e

cFromStructure:: P ConvergeFrom
cFromStructure = do
  _ <- pure 1
  v <- natural
  _ <- string "-"
  st <- structParser
  pure $ Structure v st

--
cToLast:: P ConvergeTo
cToLast = do
  _ <- pure 1
  last <- choice [try lastMod, lastSnap, lastOrigin]
  pure last 

lastOrigin:: P ConvergeTo
lastOrigin = do
  _ <- pure 1
  _ <- strWS "last" 
  pure $ LastTo Origin

lastSnap:: P ConvergeTo
lastSnap = do
  _ <- pure 1
  _ <- strWS "last" 
  _ <- reserved "afterEval"
  pure $ LastTo Snap

lastMod:: P ConvergeTo
lastMod = do
  _ <- pure 1
  _ <- reserved "mod"
  m <- natural
  _ <- strWS "last"
  _ <- reserved "afterEval"
  pure $ LastTo (Mod m)

parsePercenTo:: P ConvergeTo
parsePercenTo = do
  _ <- pure 1
  p <- choice [try percenMod, try percenSnap, percenOrigin]
  pure p

parseProcessTo:: P ConvergeTo
parseProcessTo = do
  _ <- pure 1
  c <- choice [try processMod, try processSnap, processOrigin]
  pure c

parseStructureTo:: P ConvergeTo
parseStructureTo = do
  _ <- pure 1
  c <- choice [try structureMod, try structureSnap, structureOrigin]
  pure c

percenOrigin:: P ConvergeTo
percenOrigin = do
  _ <- pure 1
  n <- naturalOrFloat
  _ <- charWS '%'
  pure $ PercenTo (toNumber' n) Origin

percenSnap:: P ConvergeTo
percenSnap = do
  _ <- pure 1
  n <- naturalOrFloat
  _ <- charWS '%'
  _ <- reserved "afterEval"
  pure $ PercenTo (toNumber' n) Snap

percenMod:: P ConvergeTo
percenMod = do
  _ <- pure 1
  _ <- reserved "mod"
  m <- natural
  n <- naturalOrFloat
  _ <- charWS '%'
  _ <- reserved "afterEval"
  pure $ PercenTo (toNumber' n) (Mod m)

processOrigin:: P ConvergeTo
processOrigin = do
  _ <- pure 1
  n <- natural
  pure $ ProcessTo n Origin

processSnap:: P ConvergeTo
processSnap = do
  _ <- pure 1
  n <- natural
  _ <- reserved "afterEval"
  pure $ ProcessTo n Snap

processMod:: P ConvergeTo
processMod = do
  _ <- pure 1
  _ <- reserved "mod"
  m <- natural
  n <- natural
  _ <- reserved "afterEval"
  pure $ ProcessTo n (Mod m)

structureOrigin:: P ConvergeTo
structureOrigin = do
  _ <- pure 1
  st <- forStructure
  pure $ StructureTo (fst st) (snd st) Origin

structureSnap:: P ConvergeTo
structureSnap = do
  _ <- pure 1
  st <- forStructure
  _ <- reserved "afterEval"
  pure $ StructureTo (fst st) (snd st) Snap

structureMod:: P ConvergeTo
structureMod = do
  _ <- pure 1
  _ <- reserved "mod"
  m <- natural
  st <- forStructure
  _ <- reserved "afterEval"
  pure $ StructureTo (fst st) (snd st) (Mod m)

forStructure:: P (Tuple Int (Array Int)) 
forStructure = do
  _ <- pure 1
  v <- natural
  _ <- string "-"
  st <- structParser
  pure $ Tuple v st

structParser:: P (Array Int)
structParser = do
  _ <- pure 1
  xs <- natural `sepBy` string "."
  pure $ A.fromFoldable xs

voiceId:: P String 
voiceId = do
    _ <- pure 1
    x <- identifier -- many $ noneOf ['\\','<',' ']
    pure x

tempoMark:: P TempoMark
tempoMark = do
  _ <- pure 1
  x <- choice [try cpm, try bpm, try cps, try ratio, acceleration]
  pure x

acceleration:: P TempoMark -- (~ 1 << 0 range 100cpm, 1000cpm)
acceleration = do 
  _ <- pure 1
  _ <- reserved "~"
  freq <- toNumber' <$> naturalOrFloat
  _ <- reserved "<<"
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
  _ <- reserved "cpm"
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
  x <- natural
  _ <- reservedOp ":"
  y <- natural
  pure $ Prop id x y

--
test :: VantageMap -> String -> Either String Program
test vm x =
  case runParser x parseProgram of
    Left (ParseError err _) -> Left err
    Right prog -> case check vm prog of
                    true -> Right prog
                    false -> Left "failed the check"

getTemporalMap:: Program -> Map String Temporal
getTemporalMap program = unions $ map unexpressTempo $ L.filter (\ expression -> isTemporal expression) program
  where isTemporal (TimeExpression _) = true 
        isTemporal _ = false 

unexpressTempo:: Expression -> Map String Temporal
unexpressTempo (TimeExpression x) = x 
unexpressTempo _ = empty

getAuralMap:: Program -> Map String (List Aural)
getAuralMap program = toListAurals $ map unexpressAural $ L.filter (\ expression -> isAural expression) program
  where isAural (AuralExpression _) = true
        isAural _ = false

toListAurals:: List (Map String Aural) -> Map String (List Aural)
toListAurals mapas = unions $ map (\k -> toAurals k vals) $ map fst vals
  where vals = concat $ map toUnfoldable mapas
        toAurals key vals = singleton key $ map snd $ L.filter (\v -> (fst v) == key) vals

unexpressAural:: Expression -> Map String Aural
unexpressAural (AuralExpression x) = x 
unexpressAural _ = empty

--
getVantageMap:: Program -> Map String Vantage
getVantageMap program = unions $ map unexpressVantage $ L.filter (\ expression -> isVantage expression) program
  where isVantage (VantagePointExpression _) = true 
        isVantage _ = false 

unexpressVantage:: Expression -> Map String Vantage
unexpressVantage (VantagePointExpression x) = x 
unexpressVantage _ = empty

-- test' :: String -> Either String (Map String Temporal)
-- test' x =
--   case getTemporalMap <$> runParser x parseProgram of
--     Left (ParseError err _) -> Left err
--     Right aMap -> Right $ check aMap 

check :: VantageMap -> Program -> Boolean
check vm program = checkedTempoAspects && checkedPitch
  where checkedTempoAspects = checkT vm (getVantageMap program) $ getTemporalMap program
        checkedPitch = checkXPitch program


checkT :: VantageMap -> Map String Vantage -> Map String Temporal -> Boolean
checkT vm vMNew aMap' = checkID && checkTempoMark
  where aReplicaMap = filter isReplica aMap'
        aMap = filter (not isReplica) aMap'
        checkID = not $ elem false $ mapWithIndex (check2 vm vMNew aMap' Nil) aMap'  
        checkTempoMark = not $ elem false $ mapWithIndex (checkTempi aMap Nil) aMap

isReplica:: Temporal -> Boolean
isReplica (Replica _) = true
isReplica _ = false

getReplicaKey:: Temporal -> String
getReplicaKey (Replica id) = id
getReplicaKey _ = "2666"

check2 :: VantageMap -> Map String Vantage -> Map String Temporal -> List String -> String -> Temporal -> Boolean
check2 _ _ aMap alreadyRefd aKey (Temporal (Kairos _ _) _ _) = true
check2 _ _ aMap alreadyRefd aKey (Temporal (Metric _ _ _) _ _) = true
check2 vm vMNew aMap alreadyRefd aKey (Temporal (Converge anotherKey _ _ _) _ _) =
  case lookup anotherKey aMap of
    Nothing -> false
    Just anotherValue -> case elem aKey alreadyRefd of
                           true -> false
                           false -> check2 vm vMNew aMap (aKey : alreadyRefd) anotherKey anotherValue

check2 vm vMNew aMap alreadyRefd aKey (Temporal (Novus vantageKey _ _) _ _) = 
  case lookup vantageKey vMNew of
    (Just x) -> if isRemove x then false else true 
    Nothing -> case lookup vantageKey vm of
                  (Just _) -> true
                  Nothing -> false

-- in case of replica
check2 vm vMNew aMap alreadyRefd aKey (Replica id) 
  | aKey == id = false
  | otherwise = 
    case lookup id aMap of
          Nothing -> false
          Just nVal -> case elem id alreadyRefd of
                        true -> false
                        false -> check2 vm vMNew aMap (aKey : alreadyRefd) id nVal

isRemove Remove = true
isRemove _ = false

checkTempi:: Map String Temporal -> List String -> String -> Temporal -> Boolean
checkTempi aMap alreadyRefd aKey temporal = 
    if (getTempoRef temporal) == Nothing then true 
    else case lookup anotherKey aMap of
            Nothing -> false
            Just anotherValue -> case elem aKey alreadyRefd of
                      true -> false
                      false -> checkTempi aMap (aKey : alreadyRefd) anotherKey anotherValue
    where anotherKey = fromMaybe "" $ getTempoRef temporal

getTempoRef:: Temporal -> Maybe String
getTempoRef (Temporal (Kairos _ tm) _ _) = isTempoRefd tm
getTempoRef (Temporal (Metric _ _ tm) _ _) = isTempoRefd tm
getTempoRef (Temporal (Converge _ _ _ tm) _ _) = isTempoRefd tm
getTempoRef (Temporal (Novus _ _ tm) _ _) = isTempoRefd tm
getTempoRef (Replica _) = Nothing

isTempoRefd:: TempoMark -> Maybe String
isTempoRefd (Prop id _ _) = Just id
isTempoRefd _ = Nothing

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

toRat:: Number -> Rational
toRat x = 
    let pFact = 1000000
        floored = floor x -- 12
        fract = x - (toNumber floored) -- 12.5 - 12.0 = 0.5
        fract' = round $ fract * (toNumber pFact) -- 500000
    in (floored%1) + (fract'%pFact) -- 12 + (500000%1000000)




------ this is an attempt to create a Number range using Formatter
-- getProperDigits:: String -> String -> Either String N.Formatter
-- getProperDigits a b =
--   case (length a' <= 2) && (length b' <= 2) of  
--     false -> "not really a number"
--     true -> if a'!0 > b'!0 then 
--   where a' = split (Pattern ".") a
--         b' = split (Pattern ".") b

-- compareAB:: Maybe Int -> Maybe Int -> String
-- compareAB (Just a) (Just b) = if a>=b then a "0" else b
-- compareAB Nothing (Just b) = b
-- compareAB (Just a) Nothing = a
-- compareAB Nothing Nothing = 0

-- parseNumFormatter:: Either String N.Formatter
-- parseNumFormatter = N.parseFormatString "0.000"

-- parseNum:: String -> Either String Number
-- parseNum s = case parseNumFormatter of 
--                 Left x -> Left x
--                 Right x -> N.unformat x s