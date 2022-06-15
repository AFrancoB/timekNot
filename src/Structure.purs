module Structure where

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

import Data.Newtype

import Data.DateTime
import Data.DateTime.Instant
import Data.Tempo
import Data.Enum
import Data.Map as M
import Partial.Unsafe


--- "xxox" this lasts 4 beats so oDur should be 4

-- this as 'o' would look like: [0.0,0.25,0.75]

-- so 

-- fromPassageToTime:: List Boolean -> M.Map Int Coordenada
-- fromPassageToTime x = 
--     let passageLength = fromInt $ length x   -- oDur
--         onsets = (fromInt <<< snd) <$> (filter (\x -> fst x == true) $ zip x (0..(length x)))
--         oPercen = map (_/passageLength) onsets
--     in oPercen



