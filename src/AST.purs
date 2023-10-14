module AST(TimekNot(..),Waste(..),AlmostWaste(..), Voices, Voice(..),Program(..),Expression(..),Aural(..),Value(..),Span(..),Temporal(..),Polytemporal(..),Rhythmic(..), Euclidean(..), Event(..), TimePacket(..), Onset(..), Index(..), TempoMark(..), ConvergeTo(..), ConvergeFrom(..), CPAlign(..), Waste(..),showEventIndex, showStructureIndex) where

import Prelude
import Effect.Ref
import Data.List
import Data.String as Str
import Data.Tempo
import Data.DateTime
import Data.Rational
import Data.Map
import Data.Tuple



type Waste = {
  whenPosix:: Number, 
  s:: String, 
  n:: Int,
  gain:: Number,
  pan:: Number,
  speed:: Number,
  begin:: Number,
  end:: Number
  }

type AlmostWaste = {
  event:: Event, 
  s:: String, 
  n:: Int,
  gain:: Number,
  pan:: Number,
  speed:: Number,
  begin:: Number,
  end:: Number
  }

type TimekNot = {
  ast :: Ref Program,
  tempo :: Ref Tempo,
  eval :: Ref DateTime
  }

-- Temporal values is short for TemporalRelationship and Aural is short for Aural Values. Polytemporal stands for TempoRelationship, Rhythmic stands shor for Rhythmic values

type Voices = Map String Voice

-- aural:: List Value // Temporal, which  type has:: Polytemporal Rhythmic Loop
data Voice = Voice Temporal Aural

instance voiceShow :: Show Voice where
    show (Voice t a) = show t <> " " <> show a 

type Program = List Expression

-- instance programShow :: Show Program where
--   show (Program xs) = show xs

data Expression = TimeExpression (Map String Temporal) | AuralExpression (Map String Aural) -- here add a convergence expression when the non-ephemeral convergence points are properly imagined

instance expressionShow :: Show Expression where
  show (TimeExpression x) = show x
  show (AuralExpression x) = show x

type AudioAttributes = {
  sound:: List Value,
  n:: List Value
}

type Aural = List Value

-- future additions to Value: OSound | OTransposedSound | Full Sound OSound
-- for now only X generates sounds, O should be allowed to invoke sound as well. Full will allow to invoke sound for X and O as pairs

data Value = 
  Sound Span (List String) | TransposedSound String | 
  N Span (List Int) | TransposedN String |
  Gain Span (List Number) | TransposedGain String |
  Pan Span (List Number) | TransposedPan String |
  Speed Span (List Number) | TransposedSpeed String |
  Begin Span (List Number) | TransposedBegin String | 
  End Span (List Number) | TransposedEnd String |
  CutOff Span (List Number) | TransposedCutOff String |
  Vowel Span (List Number) | TransposedVowel String

instance valueShow :: Show Value where
  show (Sound x l) = show x <> " " <> show l
  show (TransposedSound voice) = "s transposed from " <> voice
  show (N x l) = show x <> " " <> show l
  show (TransposedN voice) = "n transposed from " <> voice
  show (Gain x l) = show x <> " " <> show l
  show (TransposedGain voice) = "gain transposed from " <> voice
  show (Pan x l) = show x <> " " <> show l
  show (TransposedPan voice) = "pan transposed from " <> voice
  show (Speed x l) = show x <> " " <> show l
  show (TransposedSpeed voice) = "speed transposed from " <> voice
  show (Begin x l) = show x <> " " <> show l
  show (TransposedBegin voice) = "begin transposed from " <> voice
  show (End x l) = show x <> " " <> show l
  show (TransposedEnd voice) = "end transposed from " <> voice
  show (CutOff x l) = show x <> " " <> show l
  show (TransposedCutOff voice) = "cutoff transposed from " <> voice
  show (Vowel x l) = show x <> " " <> show l
  show (TransposedVowel voice) = "vowel transposed from " <> voice

data Span = CycleEvent | CycleBlock | CycleInBlock | SpreadBlock

instance spanShow :: Show Span where
  show CycleEvent =    "_"
  show CycleBlock =    "_-"
  show CycleInBlock =  "-_"
  show SpreadBlock =   "_-_"

data Temporal = Temporal Polytemporal Rhythmic Boolean

instance temporalShow :: Show Temporal where
    show (Temporal x y z) = show x <> " " <> show y <> (if z then " looped" else " unlooped")

data Polytemporal = 
  Kairos Number TempoMark | -- last arg is tempo -- Arg: universal time unit (miliseconds and datetime in purs)
  -- Kairos starts a program at evaluation time (or as soon as possible), no underlying grid
  Metric ConvergeTo ConvergeFrom TempoMark | -- starts a program attached to a default underlying voice (a tempo grid basically) first number is the point to where the new voice will converge, second number is the point from which it converges. 
  Converge String ConvergeTo ConvergeFrom TempoMark -- Args: String is the voice identifier, convergAt (where this voice converges with the identified voice) and convergedFrom (the point of this voice that converges with the identified voice)
  -- Converge starts a program in relationship with another voice

instance polytemporalShowInstance :: Show Polytemporal where
  show (Kairos asap t) = "kairos: " <> show asap <> " tempo mark: " <> show t
  show (Metric cTo cFrom t) = "(converges to "<>show cTo<>") (from "<>show cFrom <> ") (tempo mark: " <> show t <> ")"
  show (Converge voice cTo cFrom t) = "voice "<>show voice<>" (converges to "<>show cTo<>") (from "<>show cFrom <> ") (tempo mark: " <> show t <> ")"


data Rhythmic =  -- whenPosix, thats it
  X | -- x
  O |
  Sd Rhythmic | -- [x]
  Repeat Rhythmic Int |
  Bjorklund Euclidean Int Int Int | 
  Rhythmics (List Rhythmic) -- xoxo

instance Show Rhythmic where
  show X = "x"
  show O = "o"
  show (Sd xs) = "[" <> show xs <> "]"
  show (Repeat xs n) = "!" <> show xs <> "#" <> show n
  show (Bjorklund eu k n r) = "bjorklund"
  show (Rhythmics xs) = show xs

data Euclidean = Full Rhythmic Rhythmic | K Rhythmic | InvK Rhythmic | Simple -- add simple inverse

instance euclideanShowInstance :: Show Euclidean where
  show (Full x y) = "full: " <> (show x) <> " " <> (show y)
  show (K x) = show x
  show (InvK x) = show x
  show (Simple) = "simple"

-- CPAlign will provide a convergence point in relation to a part of the program.
-- mod 4 will align a cp with the next voice start multiple of 4. The convergenceTo value with 'mod 4' will converge to the other voice at the next voice muliplte of 4. If this would be the convergenceFrom, the voice will align to the other voice from its next voice multiple of 4.

-- data Align = Mod Number Number | Mod' Number | Snap Number | Snap' Number | Origin Number  -- this is the goal

data CPAlign = Mod Int | Snap | Origin  -- this is the first stage

instance Show CPAlign where
  show (Mod m) = "cp after first multiple of " <> show m <> " ahead"
  show  Snap = "closest to eval"
  show  Origin = "diverge at origin"

-- Aligners:
---- Mod Multiple Offset (next start of voice/event multiple of N with an offset number becomes voice 0)
---- Mod' Multiple Offset (closest multiple, can be in the past already)
---- Snap cp happens at closest voice or event.
---- Origin will align the cp at 0 (1st of January, 1970: 12:00 am)

data ConvergeFrom = Structure Int (Array Int) | Process Int | Percen Number

instance Show ConvergeFrom where
  show (Structure x xs) = show x <>"-"<> result <> " "
      where subdivisions = foldl (<>) "" $ map (\x -> show x <> ".") xs
            result = Str.take (Str.length subdivisions - 1) subdivisions
  show (Process e) = show e
  show (Percen p) = show p <> "%"

data ConvergeTo = StructureTo Int (Array Int) CPAlign | ProcessTo Int CPAlign | PercenTo Number CPAlign

instance Show ConvergeTo where
  show (StructureTo x xs a) = show x <>"-"<> result <> " " <> show a
      where subdivisions = foldl (<>) "" $ map (\x -> show x <> ".") xs
            result = Str.take (Str.length subdivisions - 1) subdivisions
  show (ProcessTo e a) = show e <> " " <> show a
  show (PercenTo p a) = show p <> "% " <> show a

-- perhaps this is the output of processTempoMark, this will allow users to declare a total duration of a block (reverting more or less the additive logic to divisive)
type Duration = Rational
type MetricUnit = Rational
data TimeSignature = Either Duration MetricUnit

data TempoMark = XTempo | CPM Rational | BPM Rational Rational | CPS Rational | Prop String Int Int

instance Show TempoMark where
  show XTempo = "external"
  show (CPM cpm) = show cpm <> "cpm"
  show (BPM bpm figure) = show bpm <> "bpm the " <> show figure
  show (CPS cps) = show cps <> "cps"
  show (Prop id x y) = "from voice: " <> id <> " " <> show x <> ":" <> show y 

type TimePacket = {
  ws:: DateTime,
  we:: DateTime,
  eval:: DateTime,
  origin:: DateTime,
  tempo:: Tempo
}

data Event = Event Onset Index

instance Show Event where
    show (Event o i) =  show o <> " " <> show i

showEventIndex (Index _ _ n) = show n 

showStructureIndex (Index x xs _) = show x <>"-"<> result
      where subdivisions = foldl (<>) "" $ map (\x -> show x <> ".") xs
            result = Str.take (Str.length subdivisions - 1) subdivisions

data Onset = Onset Boolean Number

instance Show Onset where
    show (Onset true n) =  "(X" <> " psx:" <> " ..." <>(Str.drop 0 $ show n) <>")"
    show (Onset false n) = "(O" <> " psx:" <> " ..." <>(Str.drop 0 $ show n) <>")"

instance Ord Onset where
    compare (Onset bool1 pos1) (Onset bool2 pos2) = pos1 `compare` pos2  

instance Eq Onset where 
    eq (Onset bool1 pos1) (Onset bool2 pos2) = pos1 == pos2

data Index = Index Int (Array Int) Int

instance indexShow :: Show Index where
    show (Index x xs n) = show x <>"-"<> result <> " (" <> (Str.take 8 $ show n) <> ")"
      where subdivisions = foldl (<>) "" $ map (\x -> show x <> ".") xs
            result = Str.take (Str.length subdivisions - 1) subdivisions