module ExperimentosConvergencia where

import Prelude

import Data.Either
import Data.Identity
import Data.List hiding (many,take)
import Data.List.Lazy (replicate,repeat,take)
import Data.List.Lazy as Lz
import Data.Foldable (foldl)
import Data.Int
import Data.Int (pow) as I
import JS.BigInt (fromInt) as BI
import Data.Number (pow)
import Data.Rational (Rational(..), toRational, fromInt, (%), numerator, denominator)
import Data.Rational (toNumber) as R
import Data.Tuple
import Data.String (singleton, joinWith)
import Data.Maybe hiding (optional)
import Data.Functor
import Control.Monad
import Data.List.NonEmpty (toList)
import Data.Map as M
import Data.String as Str

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
import VariExperiments

type P = ParserT String Identity 




































-- cExpr:: P V
-- cExpr = do
--   _ <- pure 1
--   whitespace
--   term `chainl1` convergences

-- convergences:: P (V -> V -> V)
-- convergences = do
--   _ <- pure 1
--   whitespace 
--   (reservedOp "<~" *> pure test1)
--   <|> (reservedOp "~>" *> pure test2)
   

-- test1:: V -> V -> V
-- test1 (VInt x) (VInt y) = VConv $ Convergence (Process x) (ProcessTo y SnapAfter)
-- test1 _ _ = VString "muh"

-- test2:: V -> V -> V
-- test2 (VInt x) (VInt y) = VConv $ Convergence (Process y) (ProcessTo x SnapAfter)
-- test2 _ _ = VString "muh"


-- necesito un parser que tome numeros (estos se vuelven ProcessTO o Process dependiendo) separados por flechas que tienen tipo (Convergence -> Convergence). Es decir: una lista de numeros cuyo separador es una funcion. Una lista 

-- c:: P (List (Tuple (V -> V) V))
-- c = do
--   _ <- pure 1 
--   h <- expr 
--   xs <- many tailConvergences
--   pure $ unzip $ fromFoldable $ ((Tuple 0 h) : (fromFoldable xs))

-- fff:: Tuple (List Int) (List V)
-- fff (Tuple f' s) = result
--   where f = tail $ f'  -- Maybe (List Int)
--         sTail = tail s -- Maybe (List V)
--         result = map (\v -> ff f v sTail s) s

-- ff:: Maybe Int -> V -> Maybe (List V) -> List V -> V
-- ff Nothing var sTail s = var 
-- ff skips var map = 
--   where lenSec = length s
--         cycledSkips = skips%lenSec * (-1)
--         from = fromMaybe (VInt 0) $ vs!!cycledSkips


-- [10 <~ 20 ~> 30   ]

-- from 10 to 20
-- from 20 to external
-- from 30 to 20


-- tailConvergences:: P (Tuple Int V)
-- tailConvergences = do
--   a <- arrow
--   b <- expr
--   pure $ Tuple a $ b

-- arrow:: P Int
-- arrow = do 
--   choice [leftArrow]

-- leftArrow:: P Int
-- leftArrow = do 
--   _ <- char '<'
--   xs <- many1 $ char '~'
--   pure (length $ fromFoldable xs)

-- addVari:: V -> V -> V
-- addVari (VInt x) (VInt y) = VInt (x+y)

-- a>> = ConvergeTo a afterEval

-- <<a = ConvergeTo a beforeEval

-- a <~ b = ConvergeFrom a, ConvergeTo b

-- a ~> b = ConvergeTo a, ConvergeFrom b

-- a ~> b>> = ConvergeTo a afterEval, ConvergeFrom b 

-- a <~ [b,c,d] = ConvergeFrom a, ConvergeTo b,  ConvergeFrom a, ConvergeTo c,   ConvergeFrom a, ConvergeTo d

-- a <~ b <~ c = ConvergeFrom a, ConvergeTo b, ConvergeFrom b, ConvergeTo c, 

-- constructor: Convergence to from

-- data Convergence = CTo Int | Convergence Convergence Int | Convergences (List Convergence) | ArrowLeft Int Int | ArrowRight Int Int

-- instance convShow :: Show Convergence where
--   show (CTo to) = show to
--   show (Convergence from to) = "cTo " <> (show to) <> " cFrom " <> show from
--   show (Convergences cs) = show $ map (\c -> show c) cs
--   show (ArrowLeft pos jump) = "Left " <> show jump <> " from " <> show pos
--   show (ArrowRight pos jump) = "Right " <> show jump <> " from " <> show pos



-- negNum:: P Number
-- negNum = do
--   _ <- charWS '-'
--   x <- naturalOrFloat
--   pure ((-1.0) * toNumber' x)

-- charWS:: Char -> P Char
-- charWS x = do
--   _ <- pure 1
--   x <- char x 
--   whitespace
--   pure x

-- strWS:: String -> P String
-- strWS x = do
--   _ <- pure 1
--   x <- string x 
--   whitespace
--   pure x

-- -- trasnform this parser to get rationals rather than numbers
-- fromEitherToRat:: Either Int Number -> Rational
-- fromEitherToRat x = toRat $ toNumber' x

-- toVariant:: Either Int Number -> V
-- toVariant (Left n) = VNum $ toNumber n 
-- toVariant (Right x) = VNum x

-- negVariant:: Either Int Number -> V
-- negVariant nx = VNum $ (-1.0) * toNumber' nx 

-- toNumber':: Either Int Number -> Number
-- toNumber' (Left x) = toNumber x 
-- toNumber' (Right x) = x

-- toRat:: Number -> Rational
-- toRat x = 
--     let pFact = 1000000
--         floored = floor x -- 12
--         fract = x - (toNumber floored) -- 12.5 - 12.0 = 0.5
--         fract' = round $ fract * (toNumber pFact) -- 500000
--     in (floored%1) + (fract'%pFact) -- 12 + (500000%1000000)


-- tokenParser = makeTokenParser haskellStyle
-- parens      = tokenParser.parens
-- braces      = tokenParser.braces
-- identifier  = tokenParser.identifier
-- reserved    = tokenParser.reserved
-- naturalOrFloat = tokenParser.naturalOrFloat
-- natural = tokenParser.natural
-- float = tokenParser.float
-- whitespace = tokenParser.whiteSpace
-- colon = tokenParser.colon
-- brackets = tokenParser.brackets
-- comma = tokenParser.comma
-- semi = tokenParser.semi
-- integer = tokenParser.integer
-- stringLit = tokenParser.stringLiteral
-- reservedOp = tokenParser.reservedOp