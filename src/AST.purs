module AST(TimekNot(..),Vantage(..), TimePoint(..), VantageMap(..), Voices(..), Voice(..),Program(..),Expression(..),Aural(..),Value(..), Variation(..),Dastgah(..),Span(..),Temporal(..),Polytemporal(..),Rhythmic(..), Euclidean(..), Event(..), TimePacket(..), Onset(..), Index(..), TempoMark(..), Sinusoidal(..), ConvergeTo(..), ConvergeFrom(..), CPAlign(..), XenoPitch(..), CPSNote(..), DastgahNote(..), Interval(..), Subset(..), Variant(..), showEventIndex, showStructureIndex) where

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

type TimekNot = {
  ast :: Ref Program,
  tempo :: Ref Tempo,
  eval :: Ref DateTime,
  vantageMap :: Ref (Map String DateTime)
  }

type Program = List Expression

data Expression = TimeExpression (Map String Temporal) | AuralExpression (Map String Aural) | VantagePointExpression  (Map String Vantage) | XenoPitchExpression (Map String XenoPitch)

instance expressionShow :: Show Expression where
  show (TimeExpression x) = "TimeExpression " <> show x
  show (AuralExpression x) = "AuralExpression " <> show x
  show (XenoPitchExpression x) = "XenoPitchExpression " <> show x
  show (VantagePointExpression x) = "VantagePointExpression " <> show x

-- Temporal values is short for TemporalRelationship and Aural is short for Aural Values. Polytemporal stands for TempoRelationship, Rhythmic stands shor for Rhythmic values

type Voices = Map String Voice

-- aural:: List Value // Temporal, which  type has:: Polytemporal Rhythmic Loop
data Voice = Voice Temporal (List Aural)

instance voiceShow :: Show Voice where
    show (Voice t a) = show t <> " " <> show a 

type Aural = List Value -- aural is a list of aural attributes for a given time layer. tend to be 1 sound, 1 n, 1 gain, 1 pan, etc.

-- :: List Aural is a non-monophonic time layer. Each event will trigger multiple samples with different aural attributes

type VantageMap = Map String DateTime

data Vantage = Build TimePoint | Move (Either Rational Rational) | Remove

instance vantageShow :: Show Vantage where
  show (Build x) = "established " <> show x 
  show (Move num) = " moved by " <> show x -- fornow secs but enable xBeats
      where x = case num of
                  (Left beat) -> show beat <> " beats"
                  (Right secs) -> show secs <> " secs"
  show Remove = " removed"

data TimePoint = Beat Rational | Secs Rational | UTC DateTime

instance timePoint :: Show TimePoint where
  show (Beat beat) = show beat <> " beats from eval"
  show (Secs secs) = show secs <> " secs from eval"
  show (UTC utc) = show utc

-- future additions to Value: OSound | OTransposedSound | Full Sound OSound
-- for now only X generates sounds, O should be allowed to invoke sound as well. Full will allow to invoke sound for X and O as pairs

data Variation a = Every Int Span (List a)

instance showVariation :: Show a => Show (Variation a) where
  show :: Variation a -> String
  show (Every n sp xs) = "every " <> show n <> " " <> show sp <> " " <> show xs 

data Value = 
  Sound Span (List String) (List (Variation String)) |
  N Span (List Int) (List (Variation Int)) |
  Gain Span (List Number) (List (Variation Number)) | 
  Pan Span (List Number) (List (Variation Number)) | 
  Speed Span (List Number) (List (Variation Number)) | 
  Begin Span (List Number) (List (Variation Number)) | 
  End Span (List Number) (List (Variation Number)) | 
  Vowel Span (List String) (List (Variation String)) |
  CutOff Span (List Number) (List (Variation Number)) |
  CutOffH Span (List Number) (List (Variation Number)) |
  MaxW Span (List Number) (List (Variation Number)) |
  MinW Span (List Number) (List (Variation Number)) |
  Inter Span (List Number) (List (Variation Number)) |
  Legato Span (List Number) (List (Variation Number)) |
  Orbit Span (List Int) (List (Variation Int)) |
  Dastgah Span Dastgah | 
  Alpha Span (List Int) |
  Beta Span (List Int) |
  Gamma Span (List Int) |
  Xeno (Tuple String (Maybe Int)) Span (List Int) |
  Prog Span (List (Tuple String (Maybe Int))) | 
  XNotes Span (List Int) (List (Variation Int)) | 
  TransposedPitch String Int

instance valueShow :: Show Value where
  show (Sound x l v) = show x <> " " <> show l <> " " <> show v
  show (N x l v) = show x <> " " <> show l <> " " <> show v
  show (Gain x l v) = show x <> " " <> show l
  show (Pan x l v) = show x <> " " <> show l
  show (Speed x l v) = show x <> " " <> show l
  show (Begin x l v) = show x <> " " <> show l
  show (End x l v) = show x <> " " <> show l
  show (Vowel x l v) = show x <> " " <> show l
  show (CutOff x l v) = show x <> " " <> show l
  show (CutOffH x l v) = show x <> " " <> show l
  show (MaxW x l v) = show x <> " " <> show l
  show (MinW x l v) = show x <> " " <> show l
  show (Inter x l v) = show x <> " " <> show l
  show (Legato x l v) = show x <> " " <> show l
  show (Orbit x l v) = show x <> " " <> show l
  show (Dastgah span d) = show d
  show (Alpha span l) = show l 
  show (Beta span l) = show l 
  show (Gamma span l) = show l 
  show (Xeno id span l) = show l
  show (Prog span l) = "prog" <> show l
  show (XNotes span l v) = "xnotes " <> show l
  show (TransposedPitch voice n) = "pitch transposed from " <> voice

data Span = CycleEvent | CycleBlock | CycleInBlock | SpreadBlock -- | Weight

instance spanShow :: Show Span where
  show CycleEvent =    "_"
  show CycleBlock =    "_-"
  show CycleInBlock =  "-_"
  show SpreadBlock =   "_-_"
  -- show BySubdivision = "-"
  -- show Weight = "-_-"

data Dastgah = Shur (List Int) | Segah (List Int) | Nava (List Int) | Homayun (List Int) | Chahargah (List Int) | Mahur (List Int) | RastPanjgah (List Int)

instance showDatsgah :: Show Dastgah where
  show (Shur l) = "shur " <> show l
  show (Segah l) = "segah " <> show l
  show (Nava l) = "nava " <> show l
  show (Homayun l) = "homayun " <> show l
  show (Chahargah l) = "chahargah " <> show l
  show (Mahur l) = "mahur " <> show l
  show (RastPanjgah l) = "rastPanjgah " <> show l

data Temporal = Temporal Polytemporal Rhythmic Boolean 

 -- this will require a check and the recursive implementation now very familiar

instance temporalShow :: Show Temporal where
    show (Temporal x y z) = show x <> " " <> show y <> (if z then " looped" else " unlooped")

data Polytemporal = 
  Kairos Number TempoMark | -- last arg is tempo -- Arg: universal time unit (miliseconds and datetime in purs)
  -- Kairos starts a program at evaluation time (or as soon as possible), no underlying grid
  Metric ConvergeTo ConvergeFrom TempoMark | -- starts a program attached to a default underlying voice (a tempo grid basically) first number is the point to where the new voice will converge, second number is the point from which it converges. 
  Converge String ConvergeTo ConvergeFrom TempoMark | -- Args: String is the voice identifier, convergAt (where this voice converges with the identified voice) and convergedFrom (the point of this voice that converges with the identified voice)  -- Converge starts a program in relationship with another voice
  Novus String ConvergeFrom TempoMark -- |
--  Canonic (List Polytemporal)
  -- InACan (List Polytemporal)



instance polytemporalShowInstance :: Show Polytemporal where
  show (Kairos asap t) = "Kairos " <> show asap <> " " <> show t
  show (Metric cTo cFrom t) = "Metric "<>show cTo<>" "<>show cFrom<>" "<> show t
  show (Converge id cTo cFrom t) = "Converge "<>show id<>" "<>show cTo<>" "<> show cFrom <>" "<> show t
  show (Novus id cFrom t) = "Novus " <> show id <>" "<> show cFrom <>" "<> show t

-- data Durations = 


data Rhythmic =  -- whenPosix, thats it
  X | -- x
  O | -- o
  Sd Rhythmic | -- [x]
  Repeat Rhythmic Int |     -- ! xoxo #3
  Bjorklund Euclidean Int Int Int |  
  Rhythmics (List Rhythmic) -- xoxo

instance Eq Rhythmic where 
    eq X X = true 
    eq O O = true
    eq (Sd xs1) (Sd xs2) = xs1 == xs2
    eq (Repeat r1 n1)  (Repeat r2 n2) = (r1 == r2) && (n1 == n2)
    eq (Bjorklund eu1 k1 n1 r1) (Bjorklund eu2 k2 n2 r2) = ((eu1 == eu2) && (k1 == k2)) && ((n1 == n2) && (r1 == r2))   
    eq (Rhythmics xs1) (Rhythmics xs2) = xs1 == xs2
    eq _ _ = false

instance Show Rhythmic where
  show X = "x"
  show O = "o"
  show (Sd xs) = "[" <> show xs <> "]"
  show (Repeat xs n) = "!" <> show xs <> "#" <> show n
  show (Bjorklund (Full x o) k n r) = "(" <> show x <> "," <> show o <> "," <> show k <> "," <> show n <> "," <> show r <> ")"
  show (Bjorklund (K x) k n r) = "(" <> show x <> "," <> show k <> "," <> show n <> "," <> show r <> ")"
  show (Bjorklund (InvK x) k n r) = "_(" <> show x <> "," <> show k <> "," <> show n <> "," <> show r <> ")"
  show (Bjorklund (Simple) k n r) = "(" <> show k <> "," <> show n <> "," <> show r <>  ")"
  show (Rhythmics xs) = foldl (<>) "" $ map show xs 
 
data Euclidean = Full Rhythmic Rhythmic | K Rhythmic | InvK Rhythmic | Simple -- add simple inverse

instance Eq Euclidean where 
    eq (Full x1 y1) (Full x2 y2) = (x1 == x2) && (y1 == y2)
    eq (K x1) (K x2) = x1 == x2
    eq (InvK x1) (K x2) = x1 == x2
    eq Simple Simple = true
    eq _ _ = false

instance euclideanShowInstance :: Show Euclidean where
  show (Full x y) = "Full " <> show x <>" "<> show y
  show (K x) = "K " <> show x 
  show (InvK x) = "InvK " <> show x
  show (Simple) = "Simple"

-- CPAlign will provide a convergence point in relation to a part of the program.
-- mod 4 will align a cp with the next voice start multiple of 4. The convergenceTo value with 'mod 4' will converge to the other voice at the next voice muliplte of 4. If this would be the convergenceFrom, the voice will align to the other voice from its next voice multiple of 4.

-- data Align = Mod Number Number | Mod' Number | Snap Number | Snap' Number | Origin Number  -- this is the goal

-- THIS IS DUE NOW:
-- data CPAlign = CeilMod Int | CeilSnap | RoundMod Int | RoundSnap | FloorMod Int | FloorSnap | Origin 

data CPAlign = Mod Int | Snap | Origin  -- this is the first stage

instance Show CPAlign where
  show (Mod m) = "Mod " <> show m
  show  Snap = "Snap"
  show  Origin = "Origin"


-- Aligners:
---- Mod Multiple Offset (next start of voice/event multiple of N with an offset number becomes voice 0)
---- Mod' Multiple Offset (closest multiple, can be in the past already)
---- Snap cp happens at closest voice or event.
---- Origin will align the cp at 0

data ConvergeFrom = Structure Int (Array Int) | Process Int | Percen Number | Last -- | Canonic Int ConvergeFrom

instance Show ConvergeFrom where
  show (Structure x xs) = show x <>"-"<> result <> " "
      where subdivisions = foldl (<>) "" $ map (\x -> show x <> ".") xs
            result = Str.take (Str.length subdivisions - 1) subdivisions
  show (Process e) = show e
  show (Percen p) = show p <> "%"
  show Last = "last"

data ConvergeTo = StructureTo Int (Array Int) CPAlign | ProcessTo Int CPAlign | PercenTo Number CPAlign | LastTo CPAlign -- | CanonicTo Int ConvergeTo 

instance Show ConvergeTo where
  show (StructureTo x xs a) = show x <>"-"<> result <> " " <> show a
      where subdivisions = foldl (<>) "" $ map (\x -> show x <> ".") xs
            result = Str.take (Str.length subdivisions - 1) subdivisions
  show (ProcessTo e a) = show e <> " " <> show a
  show (PercenTo p a) = show p <> "% " <> show a
  show (LastTo a) = "last " <> show a

-- perhaps this is the output of processTempoMark, this will allow users to declare a total duration of a block (reverting more or less the additive logic to divisive)

-- data TimeSignature = Duration Rational | TM TempoMark | Sin Sinusoidal
-- type Sinusoidal = {tempoMark:: TempoMark, freq:: Rational, amp:: Rational}

data TempoMark = XTempo | CPM Rational | BPM Rational Rational | CPS Rational | Prop String Int Int | Sin Sinusoidal | Dur Rational

instance Show TempoMark where
  show XTempo = "XTempo"
  show (CPM cpm) = show cpm <> "cpm"
  show (BPM bpm fig) = show fig <> " = " <> show bpm <> "bpm"
  show (CPS cps) = show cps <> "cps"
  show (Prop id x y) = id <> " " <> show x <> ":" <> show y 
  show (Sin acc) = show acc
  show (Dur n) = "Dur " <> show n

type Sinusoidal = {
  min:: TempoMark,
  max:: TempoMark,
  osc:: Rational,
  phase:: Rational
}

type TimePacket = {
  ws:: DateTime,
  we:: DateTime,
  eval:: DateTime,
  origin:: DateTime,
  tempo:: Tempo,
  vantageMap:: VantageMap
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
    show (Onset true n) =  "(X" <> " psx:" <>(Str.drop 0 $ show n) <>")"
    show (Onset false n) = "(O" <> " psx:" <>(Str.drop 0 $ show n) <>")"

instance Ord Onset where
    compare (Onset bool1 pos1) (Onset bool2 pos2) = pos1 `compare` pos2  

instance Eq Onset where 
    eq (Onset bool1 pos1) (Onset bool2 pos2) = pos1 == pos2

data Index = Index Int (Array Int) Int

instance indexShow :: Show Index where
    show (Index x xs n) = show x <>"-"<> result <> " (" <> (Str.take 8 $ show n) <> ")"
      where subdivisions = foldl (<>) "" $ map (\x -> show x <> ".") xs
            result = Str.take (Str.length subdivisions - 1) subdivisions

-- Dastgah

type DastgahNote = {
  function:: String,
  movement:: Interval,
  midiInterval:: Number
}

-- Wendy Carlos

-- 77.965

-- xenopPitch

data Subset = Subset Int | Unions (Array Int) | Intersection Int Int | Difference Int Int | Nested Subset 

instance subsetShow :: Show Subset where
  show _ = "subset"

data XenoPitch = CPSet Int (Array Int) (Maybe (Array Subset)) | MOS Int Int | EDO Number Int | ShurNot8 | ShurNot

instance xenoShow :: Show XenoPitch where
    show (CPSet s f subs) = "CPSet " <> show s <> " " <> show f <> " " <> show subs
    show (MOS k n) = "MOS " <> show k <> " " <> show n
    show (EDO p d) = "EDO " <> show p <> " " <> show d
    show ShurNot8 = "ShurNot8"
    show ShurNot = "ShurNot"

type CPSNote = {
    set:: Array Int,
    "archi-set":: Array String,
    ratio:: Int,
    "bounded-ratio":: Number,
    "bounding-period":: Int
}

data Interval = UpJump | UpNext | DownJump | DownNext | Unison 

instance intervalShow :: Show Interval where
  show UpJump = "UpJump"
  show UpNext = "UpNext"
  show DownJump = "DownJump"
  show DownNext = "DownNext"
  show Unison = "Unison"

data Variant = VInt Int | VNum Number | VString String | VList (List Variant) | VTempo TempoMark | VXTempo Variant | VFunc (Variant -> Variant)

instance showVariant:: Show Variant where
  show (VInt n) = "VInt " <> show n
  show (VNum x) = "VNum " <> show x
  show (VString s) = "VString " <> s
  show (VList xs) = "VList "<> show xs
  show (VTempo t) = "VTempo " <> show t
  show (VXTempo v) = "VXTempo " <> show v
  show (VFunc _) = "funca"