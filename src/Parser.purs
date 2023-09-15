module Parser(temporal, check, parseProgram, getTemporalMap, getAuralMap, test, testP) where

import Prelude

import Data.Identity
import Data.List (List(..), head, tail, elem, (:), filter)
import Data.List (fromFoldable) as L
import Data.Array (fromFoldable) as A
import Data.Either
import Data.Int
import Data.Tuple (Tuple(..), fst, snd)
import Data.Map (Map(..), lookup, keys, singleton, fromFoldable, toUnfoldable, member, unions, empty)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Maybe

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
import Aural

type P = ParserT String Identity

--1 gather a list of expressions 
--2 loose the expression part and apply an intersectionWith to all of it and build: 
-- Map (String (Voice Temporal Aural))
-- that is what is processed at calculateOnset

testP str = runParser str parseProgram

parseProgram:: P Program
parseProgram = do
  whitespace
  xs <- many expression
  eof
  pure $ L.fromFoldable xs

expression:: P Expression
expression = do
  _ <- pure 1
  x <- choice [try temporal, try aural]
  pure x

-- temporal not polytemporal
temporal:: P Expression
temporal = do
    _ <- pure 1
    x <- polytemporalRelation
    pure $ TimeExpression x

polytemporalRelation:: P (Map String Temporal)
polytemporalRelation = do
    _ <- pure 1
    p <- choice [try kairos, try metric, converge]
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
  tm <- parens tempoMark <|> pure XTempo -- the alternative should be same as estuary tempo
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
  polytemporal <- choice [try diverging, converging]
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
  cTo <- choice [try $ parens parsePercenTo, try $ parens parseProcessTo, parens parseStructureTo] <|> (pure $ ProcessTo 0 Snap)
  cFrom <- choice [try $ parens cFromPercen, try $ parens cFromProcess, parens cFromStructure]
  tm <- parens tempoMark <|> pure XTempo -- the alternative should be same as estuary tempo
  pure $ Converge voice cTo cFrom tm

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
  x <- choice [bpm, bpm', cps, ratio]
  pure x

bpm:: P TempoMark 
bpm = do
  _ <- pure 1
  x <- toNumber' <$> naturalOrFloat
  _ <- reserved "bpm"
  pure $ BPM x

bpm':: P TempoMark 
bpm' = do
  _ <- pure 1
  x <- toNumber' <$> naturalOrFloat
  _ <- reserved "bpm the"
  n <- natural
  d <- natural <|> pure 1
  pure $ BPM' x n d

cps:: P TempoMark
cps = do
  _ <- pure 1
  x <- toNumber' <$> naturalOrFloat
  _ <- reserved "cps"
  pure $ CPS x

ratio:: P TempoMark
ratio = do
  _ <- pure 1 
  id <- voiceId
  x <- natural
  _ <- reservedOp ":"
  y <- natural
  pure $ Prop id x y

test :: String -> Either String (Map String Temporal)
test x =
  case getTemporalMap <$> runParser x parseProgram of
    Left (ParseError err _) -> Left err
    Right aMap -> case check aMap of
                    true -> Right aMap
                    false -> Left "failed the check"

getTemporalMap:: Program -> Map String Temporal
getTemporalMap program = unions $ map unexpressTempo $ filter (\ expression -> isTemporal expression) program
  where isTemporal (TimeExpression _) = true 
        isTemporal _ = false 

unexpressTempo:: Expression -> Map String Temporal
unexpressTempo (TimeExpression x) = x 
unexpressTempo _ = empty

getAuralMap:: Program -> Map String Aural
getAuralMap program = unions $ map unexpressAural $ filter (\ expression -> isAural expression) program
  where isAural (AuralExpression _) = true
        isAural _ = false

unexpressAural:: Expression -> Map String Aural
unexpressAural (AuralExpression x) = x 
unexpressAural _ = empty

check :: Map String Temporal -> Boolean
check aMap = checkID && checkTempoMark
  where checkID = not $ elem false $ mapWithIndex (check2 aMap Nil) aMap   
        checkTempoMark = not $ elem false $ mapWithIndex (checkTempi aMap Nil) aMap

check2 :: Map String Temporal -> List String -> String -> Temporal -> Boolean
check2 aMap alreadyRefd aKey (Temporal (Kairos _ _) _ _) = true
check2 aMap alreadyRefd aKey (Temporal (Metric _ _ _) _ _) = true
check2 aMap alreadyRefd aKey (Temporal (Converge anotherKey _ _ _) _ _) =
  case lookup anotherKey aMap of
    Nothing -> false
    Just anotherValue -> case elem aKey alreadyRefd of
                           true -> false
                           false -> check2 aMap (aKey : alreadyRefd) anotherKey anotherValue


-- this funca needs to be tested. Next time you build the software on estuary you need to do this and it should fail:

-- v0 <- diverge   120bpm | x :|
-- v1 <- diverge (v2 3:4) | x :|
-- v2 <- diverge (v1 5:7) | x :| -- this line can be commented out as first test

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

isTempoRefd:: TempoMark -> Maybe String
isTempoRefd (Prop id _ _) = Just id
isTempoRefd _ = Nothing


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