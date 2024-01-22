module SoundTest where

import Prelude
import Effect (Effect)
import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Array
import Data.Array (fromFoldable) as A
import Data.Map (fromFoldable, intersectionWith, Map(..))


-- intersectionWith :: forall k a b c. Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c

--Compute the intersection of two maps, using the specified function to combine values for duplicate keys.

-- data Val = ValN String Number | ValI String Int | ValStr String String | Vals (Array Val)
-- instance valShow :: Show Val where
--   show (ValN x) = "valN" <> show x
--   show (ValI x) = "valI" <> show x
--   show (ValStr x) = "valStr" <> show x
--   show (Vals xs) = show xs

type ValN = Map String Number

type ValI = Map String Int

type ValStr = Map String String

type Eventos = Array Number


