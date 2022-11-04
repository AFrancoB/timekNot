module AST (TimekNot(..),Program(..),Rhythmic(..),Aural(..),SeqType(..),Process(..),Waste(..),Onset(..)) where

import Prelude
import Effect.Ref
import Data.List
import Data.Tempo
import Data.DateTime
import Data.Maybe
import Data.Tuple
import Data.String as Str

type TimekNot = {
  ast :: Ref Program,
  tempo :: Ref Tempo,
  eval :: Ref DateTime,
  wS :: Ref DateTime,
  wE :: Ref DateTime
  }

data Program = Program Rhythmic Boolean (List Aural) 

instance Show Program where
  show (Program rhy fin au) = show rhy <> show fin <> " " <> show au

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

data SeqType = ByEvent | ByIntraEvent | ByRefrain | Structured

instance Show SeqType where
  show ByEvent = show "byEvent"
  show ByIntraEvent = show "byIntraEvent"
  show ByRefrain = show "byRefrain"
  show Structured = show "structured"

instance Eq SeqType where
  eq ByEvent ByEvent = true
  eq ByIntraEvent ByIntraEvent = true
  eq ByRefrain ByRefrain = true
  eq Structured Structured = true
  eq _ _ = false

-- instance Num a => Num (Value a) where
--     x + y = \t dur renderTime evalTime anchTime -> (x t dur renderTime evalTime anchTime) + (y t dur renderTime evalTime anchTime)

-- Create a Map with a key as string and 'a' as value.


data Aural = S (List String) SeqType | N (List Int) SeqType

instance Show Aural where
  show (S xs st) = show xs <> " " <> show st
  show (N ns st) = show ns <> " " <> show st


type Waste =
  { 
  whenPosix :: Number, -- when to play the sample, in POSIX/epoch-1970 time
  s :: String --, -- name of sample bank (ie. old-style with sampleMap)
--  n :: Int -- number of sample within a bank (ie. old-style with sampleMap)
--   when :: Number, -- when to play the sample, in audio context time
--   gain :: Number, -- clamped from 0 to 2; 1 is default and full-scale
--   overgain :: Number, -- additional gain added to gain to go past clamp at 2
--   pan :: Number
  }

-- process are what I used to call coordinates but now reflect the recursive possibilities embedded in the software
data Process = Structure (Tuple Number Int) (List (Tuple Number Int)) | Events (List (Tuple Number (Tuple Boolean Int)))
-- here Number is the whenPosix value of the event/structure and the Int and list of ints is the coordinate that will

-- fix this instance, it suck a bit....
instance Show Process where
    show (Structure estribillo xs) = show (snd estribillo) <>"."<> events
        where events = Str.joinWith "." $ toUnfoldable $ map (snd >>> show) xs
    show (Events indices) = pox
        where pox = Str.joinWith " - " $ toUnfoldable $ map (fst >>> show) indices
              bool = Str.joinWith "-" $ toUnfoldable $ map (\x -> if x == true then "x" else "o") $ map (snd >>> fst) indices

data Onset = Onset Boolean Number 

instance Show Onset where
    show (Onset true n) =  "(X" <> " dur->beatPos:" <> (Str.take 8 $ show n) <> ")"
    show (Onset false n) = "(O" <> " dur->beatPos:" <> (Str.take 8 $ show n) <> ")"

instance Ord Onset where
    compare (Onset bool1 pos1) (Onset bool2 pos2) = pos1 `compare` pos2  

instance Eq Onset where 
    eq (Onset bool1 pos1) (Onset bool2 pos2) = pos1 == pos2
