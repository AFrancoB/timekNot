module Aural where

import Prelude
import Prim.Boolean

import Data.Either
import Data.Identity
import Data.Array as Arr
import Data.List.Lazy hiding (many)
import Data.Typelevel.Bool
import Data.Int
import Data.Tuple
import Data.Tuple.Nested

import Data.Functor

import Data.Maybe hiding (optional)

import Control.Monad

import Effect (Effect)
import Effect.Console (log)

import Data.Rational
import Data.Ratio

import Data.NonEmpty as N

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)

import Data.DateTime
import Data.DateTime.Instant
import Data.Tempo
import Data.Enum
import Data.Map as M
import Partial.Unsafe

import Motor

type P = ParserT String Identity


data Aural = Aural String Int

instance auralShowInstance :: Show Aural where
  show (Aural sample n) = "aural "<>sample <>" n: "<>show n 


