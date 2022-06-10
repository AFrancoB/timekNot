module Mapas where

import Prelude
import Prim.Boolean

import Data.Either
import Data.Identity
import Data.Array as Arr
import Data.List.Lazy
import Data.Typelevel.Bool
import Data.Int as I
import Data.Tuple
import Data.Tuple.Nested

import Data.Functor

import Data.Maybe hiding (optional)

import Control.Monad

import Effect (Effect)
import Effect.Console (log)

import Data.Rational
import Data.Ratio

import Effect (Effect)
import Effect.Now (nowDateTime)

import Data.DateTime
import Data.DateTime.Instant
import Data.Tempo
import Data.Enum
import Partial.Unsafe

import Data.Map as M


-- mapaDePosiciones:: M.Map Int Pruebilla
-- mapaDePosiciones = M.singleton 0 $ Pru 2.666 230 0


func = M.fromFoldableWithIndex $ fromFoldable [0.0,1.0,2.0]






