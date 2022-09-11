module RecursTest(MyList(..),node) where

import Prelude

import Data.Identity

import Data.Functor

import Data.List

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)


-- a simple (maybe the simplest possible?) recursive data type:

data MyList = Node MyList | EndList

instance Show MyList where
  show (Node l) = "+" <> show l
  show EndList = "."

-- a specific parsing monad that consumes strings
type P = ParserT String Identity



-- parsers that are recursive

node :: P MyList
node = do
  _ <- pure 1
  _ <- char 'x'
  x <- myList
  eof
  pure $ Node x

endList :: P MyList
endList = do
  _ <- pure 1
  char '.' $> EndList

myList :: P MyList
myList = do
  _ <- pure 1
  choice [node,endList]