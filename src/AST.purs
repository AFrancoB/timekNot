module AST(TimekNot(..),Passage(..),Rhythmic(..),Euclidean(..),Index(..),Aural(..),Coordenada(..),Event(..)) where

import Prelude
import Effect.Ref
import Data.List
import Data.Tempo
import Data.DateTime


type TimekNot = {
  ast :: Ref Passage,
  tempo :: Ref Tempo,
  eval :: Ref DateTime
  }

data Passage = Passage Rhythmic (List Aural)

instance passageShowInstance :: Show Passage where
  show (Passage rhy aur) = "rhy "<>show rhy<>" aur "<>show aur

data Rhythmic = 
  Onsets (List Boolean) |
  Patron (List Rhythmic) | -- piling adjacent things no white space
  Subdivision (List Rhythmic) | -- list separated by spaces -- space as operator
  Euclidean Euclidean Int Int Int | 
  Repetition Rhythmic Int  

instance rhythmicShowInstance :: Show Rhythmic where
  show (Onsets on) = "onsets " <> show on
  show (Patron ons) = "Patron " <> show ons
  show (Subdivision ons) = "subdivision: " <> show ons
  show (Euclidean eu k n off) = show eu <> " euclidean " <> (show k) <> "," <> (show n) <> "," <> (show off)
  show (Repetition on n) = show on <> " times " <> (show n)

-- --data EuclideanType = Full | K | InverseK

data Euclidean = Full Rhythmic Rhythmic | K Rhythmic | InverseK Rhythmic

instance euclideanShowInstance :: Show Euclidean where
  show (Full x y) = (show x) <>"on ks and not on ks " <> (show y)
  show (K x) = show x
  show (InverseK x) = show x

data Index = EventI | MetreI | PassageI

instance indexShowInstance :: Show Index where
  show EventI = "Event"
  show MetreI = "Metre"
  show PassageI = "Passage"

data Aural = Sample (List String) Index | N (List Int) Index

instance auralShowInstance :: Show Aural where
  show (Sample xs i) = "sample "<>show xs <>" i: "<>show i
  show (N xs i) = "n "<>show xs<>" i:"<>show i


data Coordenada = Coord Number Int Int 

instance coordenadaShowInstance :: Show Coordenada where
  show (Coord x y z) = "time: "<>show x<>", iPassage: "<>show y<>", iEvent: "<>show z

type Event =
  { 
  whenPosix :: Number, -- when to play the sample, in POSIX/epoch-1970 time
  s :: String, -- name of sample bank (ie. old-style with sampleMap)
  n :: Int -- number of sample within a bank (ie. old-style with sampleMap)
--   when :: Number, -- when to play the sample, in audio context time
--   gain :: Number, -- clamped from 0 to 2; 1 is default and full-scale
--   overgain :: Number, -- additional gain added to gain to go past clamp at 2
--   pan :: Number
  }