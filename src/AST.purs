module AST(TimekNot(..),Passage(..),Convergence(..),Rhythmic(..),Euclidean(..),Index(..),Aural(..),Coordenada(..),Event(..)) where

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

data Passage = Passage Rhythmic (List Aural) Convergence

instance passageShowInstance :: Show Passage where
  show (Passage rhy aur conv) = "rhy "<>show rhy<>" aur "<>show aur<>" conv "<>show conv

instance passageEqInstance :: Eq Passage where
    eq (Passage x y c) (Passage x' y' c') = (x == x') && (y == y') && (c == c')
    eq _ _ = false

data Convergence = Origin | Eval | Prospective Int Number

instance convergenceShowInstance :: Show Convergence where
  show Origin = "|origin|"
  show Eval = "|eval|"
  show (Prospective index offset) = "|Prospective "<>show index<>" "<>show offset

instance convergenceEqInstance :: Eq Convergence where
    eq Origin Origin = true
    eq Eval Eval = true
    eq (Prospective i o) (Prospective i' o') = (i == i') && (o == o')
    eq _ _ = false

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

instance rhythmicEqInstance :: Eq Rhythmic where
    eq (Onsets x) (Onsets y) = x == y
    eq _ _ = false

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

instance indexEqInstance :: Eq Index where
    eq EventI EventI = true
    eq MetreI MetreI = true
    eq PassageI PassageI = true
    eq _ _ = false

data Aural = Sample (List String) Index | N (List Int) Index

instance auralShowInstance :: Show Aural where
  show (Sample xs i) = "sample "<>show xs <>" i: "<>show i
  show (N xs i) = "n "<>show xs<>" i:"<>show i

instance auralEqInstance :: Eq Aural where
    eq (Sample x i) (Sample y i') = (x == y) && (i == i')
    eq (N x i) (N y i') = (x == y) && (i == i')
    eq _ _ = false

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