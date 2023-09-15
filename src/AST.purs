module AST(TimekNot(..), Voices, Voice(..),Program(..),Expression(..),Aural(..),Value(..),Span(..),Temporal(..),Polytemporal(..),Rhythmic(..), Event(..), TimePacket(..), Onset(..), Index(..), TempoMark(..), ConvergeTo(..), ConvergeFrom(..), CPAlign(..), Triplet(..), Waste(..), fst3, snd3, thrd, showEventIndex, showStructureIndex) where

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
  n:: Int
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

data Expression = TimeExpression (Map String Temporal) | AuralExpression (Map String Aural) -- here add a convergence expression when the trascendental convergence points are properly imagined

instance expressionShow :: Show Expression where
  show (TimeExpression x) = show x
  show (AuralExpression x) = show x

type AudioAttributes = {
  sound:: List Value,
  n:: List Value
}

type Aural = (List Value)


-- future additions to Value: OSound | OTransposedSound | Full Sound OSound
-- for now only X generates sounds, O should be allowed to invoke sound as well. Full will allow to invoke sound for X and O as pairs

data Value = Sound Span (List String) | TransposedSound String | N Span (List Int) | TransposedN String 

instance valueShow :: Show Value where
  show (Sound sp l) = show sp <> " " <> show l
  show (TransposedSound voice) = "s transposed from " <> voice
  show (N sp l) = show sp <> " " <> show l
  show (TransposedN voice) = "n transposed from " <> voice

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
  show (Kairos timemark t) = "kairos: " <> show timemark <> " tempo: " <> show t
  show (Metric cTo cFrom t) = "voice aligns with metric at "<>show cTo<>" from "<>show cFrom <> " tempo: " <> show t
  show (Converge voice cTo cFrom t) = "voice aligns with "<>show voice<>" at "<>show cTo<>" from "<>show cFrom <> " tempo: " <> show t


data Rhythmic =  -- whenPosix, thats it
  X | -- x
  O |
  Sd Rhythmic | -- [x]
  Repeat Rhythmic Int |
  Rhythmics (List Rhythmic) -- xoxo
-- Bjorklund

instance Show Rhythmic where
  show X = "x"
  show O = "o"
  show (Sd xs) = "[" <> show xs <> "]"
  show (Repeat xs n) = "!" <> show xs <> "#" <> show n
  show (Rhythmics xs) = show xs

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

data TempoMark = XTempo | BPM Number | BPM' Number Int Int | CPS Number | Prop String Int Int

instance Show TempoMark where
  show XTempo = "external tempo"
  show (BPM bpm) = show bpm <> "bpm"
  show (BPM' bpm n d) = show bpm <> "bpm the " <> show n <> "/" <> show d
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
    show (Onset true n) =  "(X" <> " psx:" <> " ..." <>(Str.drop 7 $ show n) <>")"
    show (Onset false n) = "(O" <> " psx:" <> " ..." <>(Str.drop 7 $ show n) <>")"

instance Ord Onset where
    compare (Onset bool1 pos1) (Onset bool2 pos2) = pos1 `compare` pos2  

instance Eq Onset where 
    eq (Onset bool1 pos1) (Onset bool2 pos2) = pos1 == pos2

data Index = Index Int (Array Int) Int

instance indexShow :: Show Index where
    show (Index x xs n) = show x <>"-"<> result <> " (" <> (Str.take 8 $ show n) <> ")"
      where subdivisions = foldl (<>) "" $ map (\x -> show x <> ".") xs
            result = Str.take (Str.length subdivisions - 1) subdivisions




-- helpers
data Triplet = Triplet Number Number Number

instance tripletShow :: Show Triplet where
    show (Triplet x y z) = "triplet " <> show x <> " " <> show y <> " " <> show z

fst3:: Triplet -> Number
fst3 (Triplet x _ _) = x

snd3:: Triplet -> Number
snd3 (Triplet _ y _) = y

thrd:: Triplet -> Number
thrd (Triplet _ _ z) = z