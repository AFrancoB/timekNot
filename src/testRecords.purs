module Recordy where 

import Prelude

import Effect (Effect)

import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Map as M
import Data.Foldable (sum)
import Data.Int
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array (filter,fromFoldable,(!!), zipWith, replicate, concat, (..), (:), init, tail, last,head,reverse,zip, cons, uncons, snoc, length, singleton)
import Data.List
import Data.List (fromFoldable,concat,zip,zipWith,length,init) as L
import Data.Traversable (scanl)

import Record

import Data.Newtype

import Data.Tempo

import AST
import DurationAndIndex
import Parser
import Rhythm
import TestOpsAndDefs

import Data.Rational (Rational(..), (%), fromInt)
import Data.Rational (toNumber) as R
import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration

type AlmostWaste' = {
  event:: Event, 
  s:: String, 
  n:: Int,
  gain:: Number,
  pan:: Number,
  speed:: Number,
  begin:: Number,
  end:: Number,
  note:: Number
  }

defWaste = {
    event: defEvent, 
    s: "", 
    n: 0,
    gain: 1.0,
    pan: 0.5,
    speed: 1.0,
    begin: 0.0,
    end: 1.0,
    note: 0.0
    -- here I need a bunch of Maybes for filters and effects, how it is solved in CineCer0 is by outputting a text that is "", here I cannot add a default value
    }

setEvent:: Event -> AlmostWaste' -> AlmostWaste'
setEvent event waste = waste {event = event }

setSound:: String -> AlmostWaste' -> AlmostWaste'
setSound sound waste = waste {s = sound }

setN:: Int -> AlmostWaste' -> AlmostWaste'
setN n' waste = waste {n = n' }

setGain:: Number -> AlmostWaste' -> AlmostWaste'
setGain g waste = waste {gain = g }

setPan:: Number -> AlmostWaste' -> AlmostWaste'
setPan p waste = waste {pan = p }

setSpeed:: Number -> AlmostWaste' -> AlmostWaste'
setSpeed sp waste = waste {speed = sp }

setBegin:: Number -> AlmostWaste' -> AlmostWaste'
setBegin b waste = waste {begin = b }

setEnd:: Number -> AlmostWaste' -> AlmostWaste'
setEnd end' waste = waste {end = end' }

setNote:: Number -> AlmostWaste' -> AlmostWaste'
setNote no waste = waste {note = no }

-- process:: Voices -> Rhythmic -> List (Value a) -> Array Event -> Array AlmostWaste'
-- process voices rhythmic aural events =
--     let sounds = [] -- processSound voices rhythmic (getSound aural) events
--         result = case (null sounds) of 
--                         true -> []
--                         false -> []
--     in result


-- processSound:: Voices -> Rhythmic -> Maybe Value -> Array Event -> Array {event:: Event, s:: String} 
-- processSound _ _ Nothing _ = []
-- processSound m r (Just (TransposedSound id n)) es = findOtherVoiceSound r es (Tuple id n) m 
-- processSound _ r (Just (Sound span sList)) events = spanSound span (fromFoldable sList) events r
-- processSound _ _ _ _ = [] 



-- data Testy a = Un (Array a) | Dos (Array a) | Tres

-- instance testShow :: Show (Testy a) where
--     show (Un x) = show x
--     show (Dos y) = show y
--     show Tres = "Tres"

-- data Value a = Sound (Attributes a) | N (Attributes a) | Gain (Attributes a) | Pan (Attributes a) | Speed (Attributes a) | Begin (Attributes a) | End (Attributes a) | Dastgah' (PitchAttributes a) 

-- instance valueShow :: Show (Value a) where
--   show (Sound a) = "s " <> show a
--   show (N a) = "n " <> show a
--   show (Gain a) = "g " <> show a
--   show (Pan a) = "p " <> show a
--   show (Speed a) = "sp " <> show a
--   show (Begin a) = "beg " <> show a
--   show (End a) = "end " <> show a
--   show (Dastgah' a) = "dastgah " <> show a
  

-- data Attributes a =  Pattern Span (List a) | Transposed String Int | TransposedWith String Int (List a)

-- instance attributesShow :: Show (Attributes a) where
--   show (Pattern sp l) = show sp <> " " <> show l
--   show (Transposed st n) = show st <> " " <> show n
--   show (TransposedWith st n l) = show st <> " " <> show n <> " " <> show l


-- data PitchAttributes a = Scale Span Dastgah | TransposedPitch String Int | TransposedPitchWith String Int (List a)

-- instance pitchShow :: Show (PitchAttributes a) where
--   show (Scale sp d) = show sp <> " " <> show d
--   show (TransposedPitch st n) = show st <> " " <> show n
--   show (TransposedPitchWith st n l) = show st <> " " <> show n <> " " <> show l

-- data Dastgah = Shur (List Int) -- 1 to 8 then it cycles back

-- instance showDatsgah :: Show Dastgah where
--   show (Shur l) = "shur " <> show l



-- data Value = 
--   Sound Span (List String) | TransposedSound String Int | 
--   N Span (List Int) | TransposedN String Int | TransposedNWith String Int (List Ops) |
--   Gain Span (List Number) | TransposedGain String Int | TransposedGainWith String Int (List Ops) |
--   Pan Span (List Number) | TransposedPan String Int | TransposedPanWith String Int (List Ops) |
--   Speed Span (List Number) | TransposedSpeed String Int | TransposedSpeedWith String Int (List Ops) |
--   Begin Span (List Number) | TransposedBegin String Int | TransposedBeginWith String Int (List Ops) |
--   End Span (List Number) | TransposedEnd String Int | TransposedEndWith String Int (List Ops) |
--   CutOff Span (List Number) | TransposedCutOff String Int |
--   Vowel Span (List Number) | TransposedVowel String Int | 
--   Chord Span (List Number) | TransposedChord String Int |
--   Dastgah Span Dastgah

-- instance valueShow :: Show Value where
--   show (Sound x l) = show x <> " " <> show l
--   show (TransposedSound voice n) = "s transposed from " <> voice
--   show (N x l) = show x <> " " <> show l
--   show (TransposedN voice n) = "n transposed from " <> voice
--   show (TransposedNWith voice n l) = show l <> "n transposedWith from " <> voice
--   show (Gain x l) = show x <> " " <> show l
--   show (TransposedGain voice n) = "gain transposed from " <> voice
--   show (TransposedGainWith voice n l) = "gain transposedWith from " <> voice
--   show (Pan x l) = show x <> " " <> show l
--   show (TransposedPan voice n) = "pan transposed from " <> voice
--   show (TransposedPanWith voice n l) = "pan transposedWith from " <> voice
--   show (Speed x l) = show x <> " " <> show l
--   show (TransposedSpeed voice n) = "speed transposed from " <> voice
--   show (TransposedSpeedWith voice n l) = "speed transposedWith from " <> voice
--   show (Begin x l) = show x <> " " <> show l
--   show (TransposedBegin voice n) = "begin transposed from " <> voice
--   show (TransposedBeginWith voice n l) = "begin transposedWith from " <> voice
--   show (End x l) = show x <> " " <> show l
--   show (TransposedEnd voice n) = "end transposed from " <> voice
--   show (TransposedEndWith voice n l) = "end transposedWith from " <> voice
--   show (CutOff x l) = show x <> " " <> show l
--   show (TransposedCutOff voice n) = "cutoff transposed from " <> voice
--   show (Vowel x l) = show x <> " " <> show l
--   show (TransposedVowel voice n) = "vowel transposed from " <> voice
--   show (Chord x l) = show x <> " " <> show l
--   show (TransposedChord voice n) = "chord transposed from " <> voice
--   show (Dastgah span d) = show d
