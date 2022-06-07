module MainParser where

import Prelude
import Prim.Boolean

import Data.Either
import Data.Identity
import Data.Array
import Data.List as L
import Data.Typelevel.Bool
import Data.Int
import Data.Tuple
import Data.Tuple.Nested

import Data.List.Lazy.Types

import Data.Functor

import Data.Maybe hiding (optional)

import Control.Monad

import Effect (Effect)
import Effect.Console (log)

import Data.Rational as Rat
import Data.Ratio as Ra

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)


import Rhythmic

type P = ParserT String Identity

-- bpm (120,1/4) cps [2,1,3,2.2]

-- ratios/bpms/[cps] -- converge Double// eval// origin -- quant

-- canonise (ratios [1,2,3,4]) $ origin $ converge 20 
-- este programa tiene 4 voces todas contando desde un origen comun (que es cuando la proporcion 1 esta en el instante 0) y convergen (o pudieron haber convergido) en el evento 20


-- "xxox oxx[xx] ox(3,8) !x#4" $ dur (reciprocal [2,4,8,8])//(len 12)//(len [12,6,3,1.5])//structure 12 (reciprocal [2,4,8,8]) $ inf/fin $ cp 25

-- onset notation: -- dur substitutes onset notation if no onset given
-- xo ---- recursive!! -- repeat notation
-- euclidean

-- if no dur or len given, it is one beat of the tempo, or one whole cycle
-- structure recieves a length (12 beats of the tempo unit or 12 cycles) and a subdivision

-- chord [0,4,7] (seq "xoo")
-- arpeggio [0,4,7] 0.25 (seq' "xooo")
-- razgado [0,4,7] (0.05,(-0.1)) (bjork "xooxoxo")
-- heterophony [0,4,7] [0,1,1] ("ooxoooxo")
-- event (func blabla) "xooxo"
-- atIndex 4 (func blabla)
-- atIndexMod 4 (func blabla) "xoooxoo"
-- funcs: note, speed, vowel, lpf, hpf, begin, end, crush, 

-- up here tengo q separar el canonic y el [passage] 
-- aki va Program Canonic (Array Passage)

data Program = Program Polytemporal (L.List Passage)

instance programShowInstance :: Show Program where
  show (Program poli pass) = "poly " <> (show poli) <>" "<> "pass " <> (show pass)

data Passage = Passage 

instance passageShowInstance :: Show Passage where
  show Passage = "passage"

data Polytemporal = Polytemporal (L.List Voicing) (L.List Nose) (L.List Converge)

instance polytemporalShowInstance :: Show Polytemporal where
  show (Polytemporal x y z) = "poly " <> (show x) <>" "<> (show y) <>" "<> (show z)

data Voicing = Proportion Number | BPM TempoMark | CPS Number 

instance voicingShowInstance :: Show Voicing where
  show (Proportion x) = show x
  show (BPM x) = show x
  show (CPS x) = show x

data Nose = Origin | Eval -- | Metric Number Number

instance noseShowInstance :: Show Nose where
  show Origin = "origin"
  show Eval = "eval"
--  show (Metric x y) = "metric"

-- converge needs two points of contact the self and above, so Tuplet Self Above
data Converge = Diverge | ConvergeFromOrigin Number Number | ConvergeFromEval Number Number -- | ConvergeFromMetric (Tuple Number Number) (Tuple Number Number)

instance convergeShowInstance :: Show Converge where
  show Diverge = "diverge"
  show (ConvergeFromOrigin x y) = "convergeOrigin" <> (show x) <>" "<> (show y)
  show (ConvergeFromEval x y) = "convergeEval" <> (show x) <>" "<> (show y)
 -- show (ConvergeFromMetric x y) = "convergeMetre" <> (show x) <>" "<> (show y)

-- data Quant = Quant Number Number

-- runParser :: s -> Parser s a -> Either ParseError a

type TempoMark = Tuple Number Number

polytemporal :: P Polytemporal
polytemporal = choice [canonise]

canonise :: P Polytemporal
canonise = do
  whitespace
  _ <- string "canonise "
  whitespace
  x <- parsevoicings-- aqui va ratio/bpms/cps
  whitespace
  y <- parens $ parseNose -- aqui va eval/origin
  whitespace
  z <- parens $ parseConverge -- aqui va converge
  pure $ Polytemporal (x) (y) (z)

---- Converge parser

parseConverge:: P (L.List Converge)
parseConverge = do 
  whitespace
  x <- choice [parseDiv, parseConvO, parseConvE] `sepBy` comma
  pure x

parseDiv:: P Converge
parseDiv = do
  _ <- string "diverge" <|> string "d"
  pure Diverge

parseConvO:: P Converge
parseConvO = do
  _ <- string "convergeFromOrigin" <|> string "c-o"
  whitespace
  x <- naturalOrFloat
  y <- naturalOrFloat
  pure $ ConvergeFromOrigin (nfToNum x) (nfToNum y)

parseConvE:: P Converge
parseConvE = do
  _ <- string "convergeFromEval" <|> string "c-e"
  whitespace
  x <- naturalOrFloat
  y <- naturalOrFloat
  pure $ ConvergeFromEval (nfToNum x) (nfToNum y)
----- Head Parser

parseNose:: P (L.List Nose)
parseNose = do 
  x <- choice [parseOrigin, parseEval] `sepBy` comma
  pure x

parseOrigin:: P Nose
parseOrigin = do 
  _ <- string "origin" <|> string "o"
  pure Origin

parseEval:: P Nose 
parseEval = do
  _ <- string "eval" <|> string "e"
  pure Eval

-- parseMetric:: P Nose 
-- parseMetric = do
--   _ <- string "metric" <|> string "m"
--   whitespace
--   x <- naturalOrFloat
--   whitespace
--   y <- naturalOrFloat
--   pure $ Metric (nfToNum x) (nfToNum y)

----- voicing parser

parsevoicings :: P (L.List Voicing)
parsevoicings = do
  whitespace
  x <- choice [parens parseByVoice, try parsebpms, try parseproportions, try parsecpss]
  pure x

parseByVoice:: P (L.List Voicing)
parseByVoice = do 
  x <- (choice [try parseSingleProp,try parseSingleCPS,try parseSingleTempo]) `sepBy` comma
  pure x

parseSingleProp:: P Voicing
parseSingleProp = do 
  x <- naturalOrFloat
  _ <- string "proportion" <|> string "prop" <|> string "p" 
  pure $ Proportion (nfToNum x)

parseSingleTempo:: P Voicing
parseSingleTempo = do 
  x <- naturalOrFloat
  y <- naturalOrFloat
  _ <- string "tempo" <|> string "bpm" <|> string "t" 
  pure $ BPM $ Tuple (nfToNum x) (nfToNum y)

parseSingleCPS:: P Voicing
parseSingleCPS = do 
  x <- naturalOrFloat
  _ <- string "cycle" <|> string "cps" <|> string "c" 
  pure $ CPS (nfToNum x)

parseproportions:: P (L.List Voicing)
parseproportions = do
  _ <- string "proportions"
  x <- parens $ naturalOrFloat `sepBy` comma
  pure $ map (\x' -> Proportion (nfToNum x')) x

parsebpms:: P (L.List Voicing)
parsebpms = do
  whitespace
  _ <- string "tempi" <|> string "tempos" <|> string "bpms"
  x <- parens $ (choice [parseTempoMark,parseDefTempo]) `sepBy`comma
  pure $ map BPM x

parseDefTempo:: P TempoMark
parseDefTempo = do
  x <- string "global"
  pure defaultTempo

parseTempoMark:: P TempoMark
parseTempoMark = do
  x <- naturalOrFloat
  y <- naturalOrFloat
  pure $ Tuple (nfToNum x) (nfToNum y)

defaultTempo:: TempoMark
defaultTempo = Tuple 120.0 0.25 -- here there should be tempo from platform (estuary)

parsecpss:: P (L.List Voicing)
parsecpss = do
  _ <- string "cycles"
  x <- parens $ (choice [parseCPSMark, parseDefCPS]) `sepBy` comma
  pure $ map CPS x

parseDefCPS:: P Number 
parseDefCPS = do
  x <- string "global"
  pure defaultCPS

defaultCPS:: Number
defaultCPS = 0.5 -- here the default cycles per second from platform (estuary)

parseCPSMark:: P Number
parseCPSMark = do
  x <- naturalOrFloat
  pure (nfToNum x)

-----
-- passage :: P Program
-- passage = do
--   x <- string "passage"
--   pure Passage

discardA :: forall m a b. Monad m => m a -> m b -> m b
discardA m k = m >>= \_ -> k



nfToNum:: (Either Int Number) -> Number
nfToNum (Left x) = toNumber x
nfToNum (Right x) = x

-- toRat:: (Either Int Number) -> Rational
-- toRat (Left x) = fromInt x
-- toRat (Right x) = Ratio x


-- GenLanguageDef :: Type -> (Type -> Type) -> Type

-- LanguageDef { caseSensitive :: Boolean, commentEnd :: String, commentLine :: String, commentStart :: String, identLetter :: ParserT s m Char, identStart :: ParserT s m Char, nestedComments :: Boolean, opLetter :: ParserT s m Char, opStart :: ParserT s m Char, reservedNames :: Array String, reservedOpNames :: Array String }

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



-- type GenTokenParser s m = { 
--   angles :: forall a. ParserT s m a -> ParserT s m a, 
--   braces :: forall a. ParserT s m a -> ParserT s m a, 
--   brackets :: forall a. ParserT s m a -> ParserT s m a, 
--   charLiteral :: ParserT s m Char, 
--   colon :: ParserT s m String, 
--   comma :: ParserT s m String, 
--   commaSep :: forall a. ParserT s m a -> ParserT s m (List a), 
--   commaSep1 :: forall a. ParserT s m a -> ParserT s m (NonEmptyList a), 
--   decimal :: ParserT s m Int, 
--   dot :: ParserT s m String, 
--   float :: ParserT s m Number, 
--   hexadecimal :: ParserT s m Int, 
--   identifier :: ParserT s m String, 
--   integer :: ParserT s m Int, 
--   lexeme :: forall a. ParserT s m a -> ParserT s m a, 
--   natural :: ParserT s m Int, 
--   naturalOrFloat :: ParserT s m (Either Int Number), 
--   octal :: ParserT s m Int, 
--   operator :: ParserT s m String, 
--   parens :: forall a. ParserT s m a -> ParserT s m a, 
--   reserved :: String -> ParserT s m Unit, 
--   reservedOp :: String -> ParserT s m Unit, 
--   semi :: ParserT s m String, 
--   semiSep :: forall a. ParserT s m a -> ParserT s m (List a), 
--   semiSep1 :: forall a. ParserT s m a -> ParserT s m (NonEmptyList a), 
--   stringLiteral :: ParserT s m String, 
--   symbol :: String -> ParserT s m String, 
--   whiteSpace :: ParserT s m Unit }



----- questions to david

-- how can I get arrays and not lists from the parser many?

-- this is due sepBy which output is:: List a. 

-- another issue with sepBy, sepBy1 needs a non empty list. Without the nonempty list there can be risks of exceptions. Should the program fail with empty lists?

---  figure out recursion in parser

---- time?? no UTC? no UNIX?? which time works in purescript????

--- how can I make my onset parser recursive?