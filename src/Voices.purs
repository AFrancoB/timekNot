module Voices (programToForeign) where

import Prelude
import Effect (Effect)
import Effect.Console
import Data.Map as M
import Data.Array (filter,fromFoldable,(!!), zipWith, replicate, concat, (..), (:), init, tail, last,head,reverse,zip, cons, snoc, length, singleton, splitAt)
import Data.TraversableWithIndex
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

calculateVoices:: M.Map String Temporal -> Voices -> M.Map String XenoPitch -> TimePacket -> Effect (M.Map String (Array Foreign)) -- (M.Map String (Array AlmostWaste))
calculateVoices tempoMap voiceMap xenopitches tp = traverseWithIndex (calculateVoice tempoMap voiceMap xenopitches tp) voiceMap  -- to get rid of Effect, change traverseWithIndex to mapWithIndex

calculateVoice:: M.Map String Temporal -> Voices -> M.Map String XenoPitch-> TimePacket -> String -> Voice -> Effect (Array Foreign)-- (Array AlmostWaste)
calculateVoice tempoMap voiceMap xenopitches tp aKey (Voice temporal aurals) = do 
    let events = calculateTemporal tempoMap tp aKey temporal -- Array Event
    let rhythmic = getRhythmic tempoMap temporal
    events >>= (auralSpecs voiceMap rhythmic aurals xenopitches) 