module Voices (programToForeign,canAuralMap,mapA) where

import Prelude
import Effect (Effect)
import Effect.Console
import Data.String (split, Pattern(..))
import Data.Map as M
import Data.Int (fromString)
import Data.Tuple
import Data.List (List(..))
import Data.List (fromFoldable) as L
import Data.Array (filter,fromFoldable,(!!), zipWith, replicate, concat, (..), (:), init, tail, last,head,reverse,zip, cons, snoc, length, singleton, splitAt)
import Data.TraversableWithIndex
import Data.Maybe
import Foreign
import Data.Tempo

import AST
import Parser -- getTemporalMap
import Aural  -- getPitchXMap  --- this two functions do not seem to belong in there
import TestOpsAndDefs
import TemporalSpecs
import AuralSpecs

-- glosario:
---- Voice is an ongoing or finished or yet-to-be-played musical idea enabled by 
---- Block is a rhythmic pattern of onsets
---- Onset a moment in time when a sound is instantiated
---- An Event is an Onset with an Index
---- Index is a mark that allows me to identify the position of the onset so sounds and sound characteristics can be attached to it
---- process-oriented index: an int identifier for each onset on a flow of onsets. 
---- eventIndex is the way I will refer to process oriented indexes
---- structure-oriented index: an int identifier for each segment on a voice and an array to identifier internal events in a voice: The head is the 'natural' subdivisions of the voice, each new element in the array is a new subdivision
---- a structure oriented index has a voice index and a structure index. A voice index is an Int while the Structure Index is an Array Int. The notation I have made for the structure oriented index is: 3-0.2.4  to the left of the (-) is the voice index and to the right of it is the event position in the rhythmic idea. The head of the array is the top level of the nested subdivisions and the last is the deepest level of the subdivisions.  

programToForeign:: Program -> TimePacket -> Effect (Array Foreign)
programToForeign program timePacket = concat <$> calculatedVoices -- waste
  where voices' = programToVoice program -- Voices
        xenoPitches = getXPitchMap program -- Map String XenoPitch
        calculatedVoices = fromFoldable <$> M.values <$> calculateVoices (getTemporalMap program) voices' xenoPitches timePacket

programToVoice:: Program -> Voices
programToVoice program = M.intersectionWith (\x y -> Voice x y) tempoMap auralMap
  where tempoMap = getTemporalMap program
        auralMap = getAuralMap program


-- mapT = M.fromFoldable [Tuple "can-0" (Replica "chuchu"), Tuple "can-1" (Replica "chichi"), Tuple "can-2" (Replica "cheche")]

-- mapT = M.fromFoldable [Tuple "can" (Replica "chuchu"), Tuple "muh" (Replica "chichi")]

-- Sound Span (List String) (List (Variation String))

mapA = M.fromFoldable [Tuple "can" (L.fromFoldable [L.fromFoldable [Sound CycleEvent (L.fromFoldable ["bd"]) (L.fromFoldable [])]]), Tuple "muh" (L.fromFoldable [L.fromFoldable [Sound CycleEvent (L.fromFoldable ["cp"]) (L.fromFoldable [])]])]


canAuralMap:: M.Map String Temporal -> M.Map String (List Aural) -> M.Map String (List Aural)
canAuralMap mapT mapA = M.unions $ M.mapMaybeWithKey (\k v ->  Just $ M.unions $ map (\t -> g k v t) kTime ) mapA
  where kTime = map splitter $ fromFoldable $ M.keys mapT

g:: String -> (List Aural) -> Tuple String (Maybe Int) -> M.Map String (List Aural)
g idAural aural timeEntry = M.singleton newKey aural 
  where sufijo = createSufix (snd timeEntry)
        newKey = if idAural == (fst timeEntry) then idAural <> sufijo else idAural

createSufix:: Maybe Int -> String
createSufix n = fromMaybe "" result
  where result = do
          x <- n
          let m = "-" <> (show x)
          pure m

splitter:: String -> Tuple String (Maybe Int)
splitter str = fromMaybe (Tuple "2666" Nothing) $ funca strList
  where strList = split (Pattern "-") str -- List String
        funca xs = tupCheck
          where tup = Tuple <$> (head xs) <*> (last xs)
                tupCheck = do
                  t <- tup
                  let f = fst t
                  let s = snd t
                  pure $ if f == s then Tuple (f) Nothing else Tuple (f) $ fromString (s)




-- up here needs a a function where tempoMap and auralMap are compared:
---- the keys of the tempoMap should be split in the -, then compare the auralMap with each of the prefixes of the tempoMap, if they coincide add the sufix of the tempoMap key to the key of the auralMap:

-- can-0, can-1, can-2 ...
-- can.s = bla bla...

-- needs to produce an auralMap that is:
-- can-0 aural, can-1 sameAural, can-2 sameAural

-- or:
-- can-0, can-1, can-2 ...
-- can[0].s = bla bla...
-- can-0 aural
--or:
-- can-0, can-1, can-2 ...
-- can[1..2].s = bla bla...
-- can-1 aural
-- can-2 aural


------after this exists, the auralMap should change from:
---------- Map String (List Aural) to:
------------- Map String (List (Tuple Aural Transposition))

calculateVoices:: M.Map String Temporal -> Voices -> M.Map String XenoPitch -> TimePacket -> Effect (M.Map String (Array Foreign)) -- (M.Map String (Array AlmostWaste))
calculateVoices tempoMap voiceMap xenopitches tp = traverseWithIndex (calculateVoice tempoMap voiceMap xenopitches tp) voiceMap  -- to get rid of Effect, change traverseWithIndex to mapWithIndex

calculateVoice:: M.Map String Temporal -> Voices -> M.Map String XenoPitch-> TimePacket -> String -> Voice -> Effect (Array Foreign)-- (Array AlmostWaste)
calculateVoice tempoMap voiceMap xenopitches tp aKey (Voice temporal aurals) = do 
    let events = calculateTemporal tempoMap tp aKey temporal -- Array Event
    let rhythmic = getRhythmic tempoMap temporal
    events >>= (auralSpecs voiceMap rhythmic aurals xenopitches) 