module Duration where

import Prelude

import Data.Either
import Data.Identity
import Data.List hiding (many,take)
import Data.List.Lazy (replicate,repeat,take)
import Data.Array as A
import Data.Foldable (foldl)
import Data.Int
import Data.Number (pow)
import Data.Rational (Rational(..), toRational, fromInt, (%))
import Data.Tuple
import Data.String (singleton, joinWith)
import Data.Maybe hiding (optional)
import Data.Functor
import Control.Monad
import Data.List.NonEmpty (toList)
import Data.Map as M
import Data.String as Str
import Data.Rational (Rational(..), toRational, fromInt, (%))

import Effect (Effect)
import Effect.Console (log)

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Combinators as C
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)

import AST
import TimePacketOps (metricToTempoMark)

type P = ParserT String Identity 


-- Implementing Scala Files
-- The rules
-- The files are human readable ASCII or 8-bit character text-files. 1)
-- The file type is .scl .
-- There is one scale per file.
-- Lines beginning with an exclamation mark are regarded as comments and are to be ignored.
-- The first (non comment) line contains a short description of the scale, but long lines are possible and should not give a read error. The description is only one line. If there is no description, there should be an empty line.
-- The second line contains the number of notes. This number indicates the number of lines with pitch values that follow. In principle there is no upper limit to this, but it is allowed to reject files exceeding a certain size. The lower limit is 0, which is possible since degree 0 of 1/1 is implicit. Spaces before or after the number are allowed.
-- After that come the pitch values, each on a separate line, either as a ratio or as a value in cents. If the value contains a period, it is a cents value, otherwise a ratio. Ratios are written with a slash, and only one. Integer values with no period or slash should be regarded as such, for example "2" should be taken as "2/1". Numerators and denominators should be supported to at least 231-1 = 2147483647. Anything after a valid pitch value should be ignored. Space or horizontal tab characters are allowed and should be ignored. Negative ratios are meaningless and should give a read error. For a description of cents, go here.
-- The first note of 1/1 or 0.0 cents is implicit and not in the files.
-- Files for which Scala gives Error in file format are incorrectly formatted. They should give a read error and be rejected.


-- ! meanquar.scl ! 1/4-comma meantone scale. Pietro Aaron's temperament (1523) 12 ! 76.04900 193.15686 310.26471 5/4 503.42157 579.47057 696.57843 25/16 889.73529 1006.84314 1082.89214 2/1

-- ! meanquar.scl
-- !
-- 1/4-comma meantone scale. Pietro Aaron's temperament (1523)
--  12
-- !
--  76.04900
--  193.15686
--  310.26471
--  5/4
--  503.42157
--  579.47057
--  696.57843
--  25/16
--  889.73529
--  1006.84314
--  1082.89214
--  2/1





---- span lang:
-- spr = _

-- seq = &

-- &:& sequence as transposition and sequence one item per block

-- & sequence, one item per block. Infers no transposition (no transposition given will seq as transp)

-- &:&-& sequence as transposition and sequence one item per x/o 

-- &:&-&.&  sequence as trans and sequence one item per first level of recursion: [x/o]

-- &:&-&.&.& sequence as trans and seq one item per second level of recursion [[x/o]]



-- _:_ spread as transposition from first to last element of the block

-- _ impossible, since blocks are looped (and repeat forever) there is no way to spread items. If this written, will act as &, which is sequence one item per block

-- _:_-_ spread as transposition and from first to last element of the block 

-- _:_-_._ spread as trans and from first to last element of the block taking into consideration first level of recursion [x]

-- _:_-_._._ spread as trans and from first to last element of the block taking into consideration second level of recursion [[x]]

-- --------
-- _:_ and _:& spread as transp and one item pero block

-- | xox[oxx][xox]oxx :| .s "cp bd sn"

-- cp cp cp cp cp cp cp cp cp cp cp cp | bd bd bd bd bd bd bd bd bd | sn sn ...

-- &:&-&._ sequence as transposition, sequence one item per block, spreads at first level of recursion

-- | xox[oxx][xox]oxx :| .s "cp bd sn"

-- cp cp cp [bd sn cp] [bd sn cp] bd bd bd

-- _:_-_.& spread as transp, spread from first to last item of x and sequence first level of recursion

-- | xox[oxx][xox]oxx :| .s "cp bd sn"

-- cp cp cp bd sn cp bd sn cp bd sn sn    

-- t:b-x.r1.r2.r3.r4.r5.rn 

-- spr:x-spr.seq.spr



-- _:_-_._ spread as trans and from first to last element of the block taking into consideration first level of recursion [x]

-- | xox[oxx][xox]oxx :| .s "cp bd sn"

-- Three ways here to deal with this:
-- 1) taking into account duration determined by x, in this case, no matter how many subdivisions it will be the same spread 

-- cp cp cp bd bd bd bd bd bd bd sn sn

-- 2) taking into consideration the amount of triggers per block:

-- cp cp cp cp bd bd bd bd sn sn sn sn 

-- 3) each new level of recursion with a spread starts a new spread pattern in rotation of the original:

-- cp cp cp bd sn cp bd sn cp bd sn sn


-- 1 is implicit already in: _-_ and needs no other notation. it is possible for this to be articulated with 3. 

-- 2 would be 







---- Aural:

-- can.sound = "hh cp bd me" *> ["drum hat pum pas", "hi cp", "808"]  .n = 0 1 2 3 4 5 * [2, 10] .segah = 0 3 2 4 5 + [7,14,(-7)]

-- if sound parser :: P (Either (Tuple Value Variant) Value)
-- 

-- if list len is 1 then Right Value, if len > 1 Left (Tuple Value Variant)
samplesParser:: P (List (List String))
samplesParser = do
    sampleNames <- stringLit
    pure $ (stringsToSamples <<< stringToStrings) sampleNames

stringToStrings:: String -> List String
stringToStrings s = fromFoldable $ Str.split (Str.Pattern ",") $ Str.trim s

stringsToSamples:: List String -> List (List String) 
stringsToSamples xs = map f xs
  where f s = fromFoldable $ Str.split (Str.Pattern " ") $ Str.trim s

sampleParser:: P (List String)
sampleParser = do
    sampleNames <- stringLit
    pure $ stringToSamples sampleNames

stringToSamples:: String -> List String -- what to do with commas??
stringToSamples s = fromFoldable $ Str.split (Str.Pattern " ") $ Str.trim s

---- math operations:

durExpr:: P (List Number)
durExpr = do
  _ <- pure 1
  whitespace
  xs <- many expr
  eof
  pure $ fromFoldable xs

expr:: P Number
expr = do
  _ <- pure 1
  whitespace
  term `chainl1` addSubOp

term:: P Number
term = do 
  _ <- pure 1 
  whitespace
  power `chainl1` mulDivOp

power:: P Number
power = do 
  _ <- pure 1 
  whitespace 
  factor `chainr1` expOp

factor:: P Number
factor = do
  _ <- pure 1 
  whitespace
  try (parens expr) <|> parseNumber

addSubOp:: P (Number -> Number -> Number)
addSubOp =  do
  _ <- pure 1
  whitespace
  (char '+' *> pure (+))
  <|> (char '-' *> pure (-))

mulDivOp:: P (Number -> Number -> Number)
mulDivOp =  do
  _ <- pure 1
  whitespace
  (char '*' *> pure (*))
  <|> (char '/' *> pure (/))

expOp:: P (Number -> Number -> Number)
expOp = do
  _ <- pure 1
  whitespace
  (char '^' *> pure (pow))

parseNumber:: P Number
parseNumber = do
  _ <- pure 0
  whitespace
  choice [
  try (toNumber' <$> naturalOrFloat),
  try (parens negNum)
  ]

negNum:: P Number
negNum = do
  _ <- charWS '-'
  x <- naturalOrFloat
  pure ((-1.0) * toNumber' x)

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


-- trasnform this parser to get rationals rather than numbers
fromEitherToRat:: Either Int Number -> Rational
fromEitherToRat x = toRat $ toNumber' x

toNumber':: Either Int Number -> Number
toNumber' (Left x) = toNumber x 
toNumber' (Right x) = x

toRat:: Number -> Rational
toRat x = 
    let pFact = 1000000
        floored = floor x -- 12
        fract = x - (toNumber floored) -- 12.5 - 12.0 = 0.5
        fract' = round $ fract * (toNumber pFact) -- 500000
    in (floored%1) + (fract'%pFact) -- 12 + (500000%1000000)





---- qwerty notation:

-- qwertyRange:: P (List Number)
-- qwertyRange = do 
--   _ <- pure 1
--   x <- parseNumber
--   _ <- reserved ","
--   y <- parseNumber
--   _ <- reserved "|"
--   ns <- many qwertyVal
--   _ <- reserved "|"
--   pure $ map (\n -> x + (n*(y-x))) $ fromFoldable ns

-- qwertyVal:: P Number
-- qwertyVal = do 
--   _ <- pure 1
--   x <- choice [char 'q' *> pure (0.0/9.0), char 'w' *> pure (1.0/9.0), char 'e' *> pure (2.0/9.0), char 'r' *> pure (3.0/9.0), char 't' *> pure (4.0/9.0), char 'y' *> pure (5.0/9.0), char 'u' *> pure (6.0/9.0), char 'i' *> pure (7.0/9.0), char 'o' *> pure (8.0/9.0), char 'p' *> pure (9.0/9.0)]
--   pure x


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
stringLit = tokenParser.stringLiteral


-- to understand functional programming better:

data Box a = Box a | Boxes (List a)

instance showBox :: Show a => Show (Box a) where
  show :: Box a -> String
  show (Box a) = "box " <> show a
  show (Boxes xs) = "boxes " <> show xs

instance eqBox :: Eq a => Eq (Box a) where
  -- eq :: Box a -> boolean
  eq (Box a) (Box b) = a == b
  eq (Boxes xs) (Boxes ys) = xs == ys
  eq _ _ = false

instance numBox :: Semiring a => Semiring (Box a) where
  add (Box x) (Box y) = Box (x + y)
  add (Box x) (Boxes xs) = Boxes (map (\n -> x + n) xs)
  add (Boxes xs) (Box y) = Boxes (map (\n -> y + n) xs)
  add (Boxes xs) (Boxes ys) = Boxes $ zipWith (\x y -> x + y) xs ys
  mul (Box x) (Box y) = Box (x * y)
  mul (Box x) (Boxes xs) = Boxes (map (\n -> x * n) xs) 
  mul (Boxes xs) (Box y) = Boxes (map (\n -> y * n) xs) 
  mul (Boxes xs) (Boxes ys) = Boxes $ zipWith (\x y -> x * y) xs ys
  zero = Box zero 
  one = Box one   
  
-- instance monBox :: Monoid a => Monoid (Box a) where
--   append (Box x) (Box y) = Box (x <> y)
--   append (Box x) (Boxes ys) = Box (x <> ys)

instance Functor Box where
  map :: forall a b. (a -> b) -> Box a ->  Box  b
  map                 f         (Box a) =  Box (f a)
  map                 f       (Boxes xs) = Boxes (map f xs)



-- instance Apply Box where
--   apply :: forall a b. Box (a -> b) -> Box a -> Box  b
--   apply               (Box   f  )     (Box a)    = Box (f a)
--   apply               (Box   f  )    (Boxes xs)  = Boxes (map f xs) 
--   apply                    _             _       = zero
--   -- apply               (Boxes fs )     (Box a)    = Box a
--   -- apply               (Boxes fs )    (Boxes xs)  = Boxes xs

-- instance Applicative Box where
--   pure :: forall a. a -> Box a
--   pure              a =  Box a

-- instance Bind Box where
--   bind :: forall a b.  Box a -> (a -> Box b) -> Box b
--   bind                (Box a)    f            = f a

-- instance Monad Box


-- Box a -> (a -> Box b) -> Box b

-- f:: String -> Box String
-- f a = pure (a <> "!!!")
