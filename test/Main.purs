module Test.Main where

import Prelude
import Data.Boolean
import Data.Either
import Effect (Effect)
import Effect.Now (nowDateTime)
import Effect.Class.Console (log)
import Test.QuickCheck
import AST
import Parser
import Data.Traversable
import Data.Newtype hiding (traverse)
import Data.DateTime
import Data.DateTime.Instant
import Data.Tempo
import Data.Enum
import Partial.Unsafe
import Data.Rational
import Data.Ratio
import Data.List
import Data.List.Lazy as Lz
import Data.Maybe
import Parsing

import Parser

main :: Effect Unit
main = do
  _ <- traverse quickCheck [
      runParser "" parseTop == (Right $ Program defR nonRep defAu) <?> "empty program",

      runParser "x :|" parseTop == (Right $ Program X repeat defAu) <?> "simple x with repetition program, no waste produced",

      runParser "o :|" parseTop == (Right $ Program O repeat defAu) <?> "simple o with repetition program, no waste produced",

      runParser "xxox :|" parseTop == (Right $ Program xxox repeat defAu) <?> "a list of rhythmics: xxox, no waste produced",

      runParser "xxo xxx oox :|" parseTop == (Right $ Program xxoxxxoox repeat defAu) <?> "a list of rhythmics with spaces should generate same structure than a program withoutn spaces, no waste produced",

      runParser "[xxox] :|" parseTop == (Right $ Program (Sd xxox) repeat defAu) <?> "a subdivision with a list of rhythmics: xxox, no waste produced",

      runParser "xx[ox]x :|" parseTop == (Right $ Program xxOXx repeat defAu) <?> "a list of rhythmics and a subdivision within: xx[ox]x, no waste produced",

      runParser "[xx[ox]x] :|" parseTop == (Right $ Program (Sd xxOXx) repeat defAu) <?> "a subdivision with a list of rhythmics and a subdivision within: xx[ox]x, no waste produced",

      runParser "[xxxx] [ooox] :|" parseTop == (Right $ Program (Rhythmics (Sd xxxx : Sd ooox : Nil)) repeat defAu) <?> "two parallel subdivision patterns",

      runParser "!xxox#2 :|" parseTop == (Right $ Program (Repeat xxox 2) repeat defAu) <?> "testing repetition parser",

      runParser "[!xxox#2] :|" parseTop == (Right $ Program (Sd $ Repeat xxox 2) repeat defAu) <?> "testing repetition parser",

      runParser "!xxox#2 xx[ox]x :|" parseTop == (Right $ Program (Rhythmics (Repeat xxox 2 : xxOXx : Nil)) repeat defAu) <?> "testing repetition parser",

      runParser "!xx[ox]x#2 ooox :|" parseTop == (Right $ Program (Rhythmics (Repeat xxOXx 2 : ooox : Nil)) repeat defAu) <?> "testing repetition parser"
    
    ]
  pure unit

-- synonyms
xxox = Rhythmics (X:X:O:X:Nil)
xxOXx = Rhythmics (X:X:(Sd (Rhythmics (O:X:Nil))):X:Nil)
xxxx = Rhythmics (X:X:X:X:Nil)
ooox = Rhythmics (O:O:O:X:Nil)
xxoxxxoox = Rhythmics (X:X:O:X:X:X:O:O:X:Nil)

repeat = true
nonRep = false

defAu:: List Aural
defAu = ((S ("":Nil) ByEvent): Nil)

defR:: Rhythmic
defR = O