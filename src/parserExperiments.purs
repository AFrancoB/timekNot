module ParserExperiments(funca,topParser,parseIdea,Rhythmic'(..)) where

import Prelude
import Prim.Boolean

import Data.Either
import Data.Identity
import Data.Array hiding (filter,reverse,many,fromFoldable,(:),concat)
import Data.List hiding (many)
import Data.Int
import Data.Tuple
import Data.Tuple.Nested
import Data.String (singleton, joinWith)

--import Data.List.Lazy.Types

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

type P = ParserT String Identity

-- parseo':: P String
-- parseo' = do
--     x <- chainl1 (choice [string "x",string "o", string "[", string "]"]) (whitespace $> funca') 
--     pure x

-- funca':: String -> String -> String
-- funca' x y = x <> y

topParser:: P Rhythmic'
topParser = do
    _ <- pure 1
    x <- parseIdea
    eof
    pure x

parseIdea:: P Rhythmic'
parseIdea = do
    _ <- pure 1
    x <- chainl1 (choice [parseOnset,parseSubDivision]) (whitespace $> funca)   
    pure x

funca:: Rhythmic' -> Rhythmic' -> Rhythmic'
funca (Onset' x) (Onset' y) = Onsets' $ fromFoldable [(Onset' x),(Onset' y)]
funca (Onset' x) (Onsets' y) = Onsets' $ (Onset' x) : y
funca (Onsets' x) (Onset' y) = Onsets' $ (attachAsLast (Onset' y) x)
funca (Onsets' x) (Onsets' y) = Onsets' (x<>y)
funca _ _ = Onsets' $ fromFoldable [Onset' false]

-- funca:: Rhythmic' -> Rhythmic' -> Rhythmic'
-- funca (Onset' x) (Onset' y) = Onsets' $ fromFoldable [Onset' x,Onset' y]
-- funca (Onset' x) (Onsets' y) = Onsets' $ (Onset' x) : y 
-- funca (Onset' x) (Subdivision' y) = funca (Onset' x) y
-- funca (Onsets' x) (Onset' y) = Onsets' (attachAsLast (Onset' y) x)
-- funca (Onsets' x) (Onsets' y) = Onsets' $ x <> y
-- funca (Onsets' x) (Subdivision' y) = funca (Onsets' x) y
-- funca (Subdivision' x) (Onset' y) = funca x (Onset' y)
-- funca (Subdivision' x) (Onsets' y) = funca x (Onsets' y)
-- funca (Subdivision' x) (Subdivision' y) = funca x y

attachAsLast:: forall a. a -> List a -> List a
attachAsLast x xs = reverse (x : (reverse xs))

parseSubDivision:: P Rhythmic'
parseSubDivision = do
    _ <- pure 1
    _ <- string "["
    x <- chainl1 (choice [parseIdea,parseOnset]) (whitespace $> funca)
    _ <- string "]"
    pure x

parseOnset:: P Rhythmic'
parseOnset = do
    _ <- pure 1
    x <- oneOf ['x','o']
    pure $ Onset' (if x == 'x' then true else false)


data Rhythmic' = 
    Onset' Boolean |
    Onsets' (List Rhythmic') | -- piling adjacent things no white space
    Subdivision' (List Rhythmic')

instance rhythmicShowInstance :: Show Rhythmic' where
    show (Onset' on) = "onset " <> show on
    show (Onsets' on) = "onsets " <> show on
    show (Subdivision' on) = "sd " <> show on

instance rhythmicEqInstance :: Eq Rhythmic' where
    eq (Onset' x) (Onset' y) = x == y
    eq _ _ = false





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