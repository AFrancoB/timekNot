module AST(TimekNot(..),Anchor(..),Waste(..),AlmostWaste(..), AnchorMap(..), Voices(..), Voice(..),Program(..),Expression(..),Aural(..),Value(..),Dastgah(..),Span(..),Ops(..),Temporal(..),Polytemporal(..),Rhythmic(..), Euclidean(..), Event(..), TimePacket(..), Onset(..), Index(..), TempoMark(..), ConvergeTo(..), ConvergeFrom(..), CPAlign(..), XenoPitch(..), XenoNote(..),showEventIndex, showStructureIndex) where

import Prelude
import Effect.Ref
import Data.List
import Data.String as Str
import Data.Tempo
import Data.DateTime
import Data.Rational
import Data.Map
import Data.Tuple
import Data.Either
import Data.Maybe

type Waste = {
  whenPosix:: Number, 
  s:: String, 
  n:: Int,
  gain:: Number,
  pan:: Number,
  speed:: Number,
  begin:: Number,
  end:: Number,
  note:: Number
  }

type AlmostWaste = {
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

type TimekNot = {
  ast :: Ref Program,
  tempo :: Ref Tempo,
  eval :: Ref DateTime,
  anchors :: Ref (Map String DateTime)
  }

type Program = List Expression

data Expression = TimeExpression (Map String Temporal) | AuralExpression (Map String Aural) | AnchorExpression  (Map String Anchor) | XenoPitchExpression (Map String XenoPitch)

-- Temporal values is short for TemporalRelationship and Aural is short for Aural Values. Polytemporal stands for TempoRelationship, Rhythmic stands shor for Rhythmic values

type Voices = Map String Voice

-- aural:: List Value // Temporal, which  type has:: Polytemporal Rhythmic Loop
data Voice = Voice Temporal (List Aural)

instance voiceShow :: Show Voice where
    show (Voice t a) = show t <> " " <> show a 

type Aural = List Value

type AnchorMap = Map String DateTime

data Anchor = Build (Either Number Number) | Move (Either Number Number) | Remove

instance anchorShow :: Show Anchor where
  show (Build num) = "established " <> x 
      where x = case num of
                  (Left beat) -> show beat <> " beats from eval"
                  (Right secs) -> show secs <> " secs from eval"
  show (Move num) = " moved by " <> show num <> " secs" -- fornow secs but enable xBeats
      where x = case num of
                  (Left beat) -> show beat <> " beats"
                  (Right secs) -> show secs <> " secs"
  show Remove = " removed"

instance expressionShow :: Show Expression where
  show (TimeExpression x) = show x
  show (AuralExpression x) = show x
  show (AnchorExpression x) = show x
  show (XenoPitchExpression x) = show x

type AudioAttributes = {
  sound:: List Value,
  n:: List Value
}

-- future additions to Value: OSound | OTransposedSound | Full Sound OSound
-- for now only X generates sounds, O should be allowed to invoke sound as well. Full will allow to invoke sound for X and O as pairs

-- refactor as {sound: string, n: Int, etc...} -- What was going to be auralAttributes
data Value = 
  Sound Span (List String) | TransposedSound String Int | 
  N Span (List Int) | TransposedN String Int | TransposedNWith String Int (List Ops) |
  Gain Span (List Number) | TransposedGain String Int | TransposedGainWith String Int (List Ops) |
  Pan Span (List Number) | TransposedPan String Int | TransposedPanWith String Int (List Ops) |
  Speed Span (List Number) | TransposedSpeed String Int | TransposedSpeedWith String Int (List Ops) |
  Begin Span (List Number) | TransposedBegin String Int | TransposedBeginWith String Int (List Ops) |
  End Span (List Number) | TransposedEnd String Int | TransposedEndWith String Int (List Ops) |
  CutOff Span (List Number) | TransposedCutOff String Int |
  Vowel Span (List Number) | TransposedVowel String Int | 
  Chord Span (List Number) | TransposedChord String Int |
  Dastgah Span Dastgah | Xeno (Tuple String (Maybe Int)) Span (List Int)

data Dastgah = Shur (List Int) -- 1 to 8 then it cycles back

instance showDatsgah :: Show Dastgah where
  show (Shur l) = "shur " <> show l


data Ops = AddInt Int | AddNum Number | MultInt Int | MultNum Number

instance showOps :: Show Ops where
  show (AddInt n) = "_+" <> show n
  show (AddNum n) = "_+" <> show n
  show (MultInt n) = "_*" <> show n
  show (MultNum n) = "_*" <> show n

instance valueShow :: Show Value where
  show (Sound x l) = show x <> " " <> show l
  show (TransposedSound voice n) = "s transposed from " <> voice
  show (N x l) = show x <> " " <> show l
  show (TransposedN voice n) = "n transposed from " <> voice
  show (TransposedNWith voice n l) = show l <> "n transposedWith from " <> voice
  show (Gain x l) = show x <> " " <> show l
  show (TransposedGain voice n) = "gain transposed from " <> voice
  show (TransposedGainWith voice n l) = "gain transposedWith from " <> voice
  show (Pan x l) = show x <> " " <> show l
  show (TransposedPan voice n) = "pan transposed from " <> voice
  show (TransposedPanWith voice n l) = "pan transposedWith from " <> voice
  show (Speed x l) = show x <> " " <> show l
  show (TransposedSpeed voice n) = "speed transposed from " <> voice
  show (TransposedSpeedWith voice n l) = "speed transposedWith from " <> voice
  show (Begin x l) = show x <> " " <> show l
  show (TransposedBegin voice n) = "begin transposed from " <> voice
  show (TransposedBeginWith voice n l) = "begin transposedWith from " <> voice
  show (End x l) = show x <> " " <> show l
  show (TransposedEnd voice n) = "end transposed from " <> voice
  show (TransposedEndWith voice n l) = "end transposedWith from " <> voice
  show (CutOff x l) = show x <> " " <> show l
  show (TransposedCutOff voice n) = "cutoff transposed from " <> voice
  show (Vowel x l) = show x <> " " <> show l
  show (TransposedVowel voice n) = "vowel transposed from " <> voice
  show (Chord x l) = show x <> " " <> show l
  show (TransposedChord voice n) = "chord transposed from " <> voice
  show (Dastgah span d) = show d
  show (Xeno id span l) = show l

data Span = CycleEvent | CycleBlock | CycleInBlock | SpreadBlock -- | Weight

instance spanShow :: Show Span where
  show CycleEvent =    "_"
  show CycleBlock =    "_-"
  show CycleInBlock =  "-_"
  show SpreadBlock =   "_-_"
  -- show BySubdivision = "-"
  -- show Weight = "-_-"

data Temporal = Temporal Polytemporal Rhythmic Boolean | Replica String -- this will require a check and the recursive implementation now very familiar

instance temporalShow :: Show Temporal where
    show (Temporal x y z) = show x <> " " <> show y <> (if z then " looped" else " unlooped")
    show (Replica id) = "replicated from " <> show id

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
  show (Bjorklund eu k n r) = "("<>show k<>","<>show n<>") "<>show eu
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

data ConvergeFrom = Structure Int (Array Int) | Process Int | Percen Number | Last

instance Show ConvergeFrom where
  show (Structure x xs) = show x <>"-"<> result <> " "
      where subdivisions = foldl (<>) "" $ map (\x -> show x <> ".") xs
            result = Str.take (Str.length subdivisions - 1) subdivisions
  show (Process e) = show e
  show (Percen p) = show p <> "%"
  show Last = "last"

data ConvergeTo = StructureTo Int (Array Int) CPAlign | ProcessTo Int CPAlign | PercenTo Number CPAlign | LastTo CPAlign

instance Show ConvergeTo where
  show (StructureTo x xs a) = show x <>"-"<> result <> " " <> show a
      where subdivisions = foldl (<>) "" $ map (\x -> show x <> ".") xs
            result = Str.take (Str.length subdivisions - 1) subdivisions
  show (ProcessTo e a) = show e <> " " <> show a
  show (PercenTo p a) = show p <> "% " <> show a
  show (LastTo a) = "last"

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

-- xenopPitch


data XenoPitch = CPSet Int (Array Int) (Maybe (Array Int)) | MOS Int Int | EDO Number Int

instance xenoShow :: Show XenoPitch where
    show (CPSet s f subs) = "cps " <> show s <> " " <> show f <> " " <> show subs
    show (MOS k n) = "mos " <> show k <> " " <> show n
    show (EDO p d) = "edo " <> show p <> " " <> show d

type XenoNote = {
    set:: Array Int,
    "archi-set":: Array String,
    ratio:: Int,
    "bounded-ratio":: Number,
    "bounding-period":: Int
}