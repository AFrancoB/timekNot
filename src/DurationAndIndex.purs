module DurationAndIndex(durFromRhythmic,rhythmicToVoiceDuration,rhythmicToOnsets, getIndexes, rhythmicStructIndex, getVoiceIndex, getBlocks, durInSecs, onsetsFromBlocks, bjorklund) where


import Prelude

import Effect (Effect)
import Effect.Console

import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Foldable (sum)
import Data.Int
-- import Data.FunctorWithIndex (mapWithIndex)
import Data.Array (filter,fromFoldable,(!!), zipWith, replicate, concat, (..), (:), init, tail, last,head,reverse,zip, cons, snoc, length, singleton, splitAt)

import Data.List
import Data.Traversable (scanl)
import Data.List (fromFoldable,concat,zip,zipWith,length,init) as L

import Control.Applicative

import Data.Newtype

import Data.Tempo

import AST
import Parser
import Rhythm

import Data.Rational (Rational(..), (%), fromInt)
import Data.Rational (toNumber) as R
import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration

import Data.TraversableWithIndex

-- for testin

import Data.Enum
import Partial.Unsafe

-- get durations

durFromRhythmic:: Rhythmic -> Number -> Number
durFromRhythmic X tempo = durInSecs 1.0 tempo 
durFromRhythmic O tempo = durInSecs 1.0 tempo
durFromRhythmic (Sd rhy) tempo = durInSecs 1.0 tempo
durFromRhythmic (Repeat rhy n) tempo = (durFromRhythmic rhy tempo) * (toNumber n) 
durFromRhythmic (Rhythmics xs) tempo = sum $ map (\x -> durFromRhythmic x tempo) xs
durFromRhythmic (Bjorklund eu k n r) tempo = durFromRhythmic (simplifyBjorklund eu k n r) tempo

rhythmicToVoiceDuration:: Rhythmic -> Number -- does not need Tempo...?
rhythmicToVoiceDuration X = 1.0
rhythmicToVoiceDuration O = 1.0
rhythmicToVoiceDuration (Sd xs) = 1.0
rhythmicToVoiceDuration (Repeat xs n) = foldl (+) 0.0 x
    where x = replicate n $ rhythmicToVoiceDuration xs
rhythmicToVoiceDuration (Bjorklund eu k n r) = rhythmicToVoiceDuration $ simplifyBjorklund eu k n r
rhythmicToVoiceDuration (Rhythmics xs) = foldl (+) 0.0 x
    where x = map (\x -> rhythmicToVoiceDuration x) xs

rhythmicToOnsets:: Rhythmic -> List Onset
rhythmicToOnsets rhy = 
    let voiceDur = rhythmicToVoiceDuration rhy
        rhythmicSegments = onsetDurations 1.0 rhy
        durInPercentOfEvents = Cons 0.0 $ (fromMaybe (L.fromFoldable []) $ L.init $ scanl (+) 0.0 $ map (\x -> x/voiceDur) $ getDur <$> rhythmicSegments) -- List Number
    in L.zipWith (\x y -> Onset x y) (getBool <$> rhythmicSegments) durInPercentOfEvents -- we need to keep the XO -- THIS gives percentage position within voice, 

onsetDurations:: Number -> Rhythmic -> List Onset
onsetDurations dur X =  L.fromFoldable [Onset true dur]
onsetDurations dur O =  L.fromFoldable [Onset false dur]
onsetDurations dur (Sd xs) = onsetDurations' dur xs
onsetDurations dur (Repeat xs n) = L.concat $ map (\x -> onsetDurations dur x) $ L.fromFoldable $ replicate n xs
onsetDurations dur (Bjorklund eu k n r) = onsetDurations dur (simplifyBjorklund eu k n r)
onsetDurations dur (Rhythmics xs) = L.concat $ map (\x-> onsetDurations dur x) xs

onsetDurations':: Number -> Rhythmic -> List Onset
onsetDurations' dur X = L.fromFoldable [Onset true dur]
onsetDurations' dur O = L.fromFoldable [Onset false dur]
onsetDurations' dur (Sd xs) = onsetDurations' dur xs
onsetDurations' dur (Repeat xs n) = L.concat $ map (\x -> onsetDurations' newDur x) $ L.fromFoldable $ replicate n xs
    where newDur = dur / (toNumber n)
onsetDurations' dur (Bjorklund eu k n r) = onsetDurations dur (simplifyBjorklund eu k n r)
onsetDurations' dur (Rhythmics xs) = L.concat $ map (\x-> onsetDurations' newDur x) xs
    where newDur = dur / (toNumber $ L.length xs)

getDur:: Onset -> Number
getDur (Onset _ x) = x

getBool:: Onset -> Boolean 
getBool (Onset x _) = x

----- index calculations -----

getIndexes:: Rhythmic -> Number -> Number -> Number -> Number -> Array Index
getIndexes rhythmic xws we x1 dur = 
  let lenOnset = L.length $ rhythmicToOnsets rhythmic
      voiceIndexes = getVoiceIndex xws we x1 dur
      structIndexes = rhythmicStructIndex rhythmic [0]
      eventIndexesPerVoice = (0..(lenOnset-1)) 
      eventIndexes = funquilla voiceIndexes eventIndexesPerVoice lenOnset -- Array (Array Int)
  in assambleIndex voiceIndexes structIndexes eventIndexes

assambleIndex:: Array Int -> Array (Array Int) -> Array (Array Int) -> Array Index
assambleIndex vs st es = concat $ zipWith f vs xs 
  where xs = map (\e -> zip st e) es
        f:: Int -> Array (Tuple (Array Int) Int) -> Array Index
        f v xs = map (\x -> Index v (fst x) (snd x)) xs

funquilla:: Array Int -> Array Int -> Int -> Array (Array Int) 
funquilla voicesIndexes onsetIndexes lenOnsets = map (\voiceIndex -> funquilla' onsetIndexes lenOnsets voiceIndex) voicesIndexes
  where funquilla' onsetIndexes lenOnsets voiceIndex = map (\onsetIndex -> (voiceIndex*lenOnsets)+onsetIndex) onsetIndexes
  

rhythmicStructIndex:: Rhythmic -> Array Int -> Array (Array Int)
rhythmicStructIndex X i = [i] 
rhythmicStructIndex O i = [i]
rhythmicStructIndex (Rhythmics xs) i = concat $ map (\(Tuple x i') -> rhythmicStructIndex x [i']) zipped
  where zipped = zip (fromFoldable xs) (0..((L.length xs)-1))
rhythmicStructIndex (Repeat rhy n) i = rhythmicStructIndex (simplifyRepeat rhy n) i
rhythmicStructIndex (Sd rhy) i = rhythmicStructIndex' rhy i
rhythmicStructIndex (Bjorklund eu k n rot) i = rhythmicStructIndex (simplifyBjorklund eu k n rot) i

rhythmicStructIndex':: Rhythmic -> Array Int -> Array (Array Int)
rhythmicStructIndex' X i = [i]
rhythmicStructIndex' O i = [i]
rhythmicStructIndex' (Rhythmics xs) i = concat $ map (\(Tuple x i') -> rhythmicStructIndex' x (snoc i i')) zipped
  where zipped = zip (fromFoldable xs) (0..((L.length xs)-1))
rhythmicStructIndex' (Sd rhy) i = rhythmicStructIndex' rhy i
rhythmicStructIndex' (Repeat rhy n) i = rhythmicStructIndex' (simplifyRepeat rhy n) i
rhythmicStructIndex' (Bjorklund eu k n rot) i = rhythmicStructIndex' (simplifyBjorklund eu k n rot) i

-- simplifyRhythmic:: Rhythmic -> Rhythmic 
-- simplifyRhythmic (Repeat rhy x) = replicateRhythmic rhy x 
-- simplifyRhythmic (Bjorklund eu k n rot) = bjorklundRhythmic eu k n rot
-- simplifyRhythmic rhy = rhy

type STEP a = Tuple (Tuple Int Int) (Tuple (Array (Array a)) (Array (Array a)))

simplifyBjorklund:: Euclidean -> Int -> Int -> Int -> Rhythmic
simplifyBjorklund (Simple) k n rot = Rhythmics xs 
  where xs = L.fromFoldable $ map (\r -> if r == true then X else O) $ blRotated rot $ bjorklund (Tuple k n)
simplifyBjorklund (K patt) k n rot = Rhythmics xs 
  where xs = L.fromFoldable $ map (\r -> if r == true then patt else patto) $ blRotated rot $ bjorklund (Tuple k n)
        patto = Rhythmics $ L.fromFoldable $ replicate (L.length $ rhythmicToOnsets patt) O
simplifyBjorklund (InvK patt) k n rot = Rhythmics xs 
  where xs = L.fromFoldable $ map (\r -> if r == true then patto else patt) $ blRotated rot $ bjorklund (Tuple k n)
        patto = Rhythmics $ L.fromFoldable $ replicate (L.length $ rhythmicToOnsets patt) O
simplifyBjorklund (Full pK pN) k n rot = Rhythmics xs
  where xs = L.fromFoldable $ map (\r -> if r == true then pK else pN) $ blRotated rot $ bjorklund (Tuple k n)

blRotated:: Int -> Array Boolean -> Array Boolean
blRotated rot patt = x.after <> x.before 
  where x = splitAt rot patt

bjorklund:: Tuple Int Int -> Array Boolean
bjorklund (Tuple i j') = (concat x') <> (concat y')
  where j = j' - i
        x = replicate i [true]
        y = replicate j [false]
        (Tuple _ (Tuple x' y')) = bjorklund' (Tuple (Tuple i j) (Tuple x y))

bjorklund':: forall a. STEP a -> STEP a
bjorklund' (Tuple n x) =
    let (Tuple i j) = n
    in if min i j <= 1
       then Tuple n x
       else bjorklund' (if i > j then left (Tuple n x) else right (Tuple n x))

right:: forall a. STEP a -> STEP a
right (Tuple (Tuple i j) (Tuple xs ys)) = Tuple (Tuple i (j-i)) (Tuple (zipWith (<>) xs ys') ys'')
  where splitted = splitAt i ys
        ys' = splitted.before
        ys'' = splitted.after

left:: forall a. STEP a -> STEP a
left (Tuple (Tuple i j) (Tuple xs ys)) = Tuple (Tuple j (i-j)) (Tuple (zipWith (<>) xs' ys) xs'')
    where splitted = splitAt j xs 
          xs' = splitted.before 
          xs'' = splitted.after

-- the output is always Rhythmics constructor
simplifyRepeat:: Rhythmic -> Int -> Rhythmic
simplifyRepeat (X) n = Rhythmics $ L.fromFoldable $ replicate n X
simplifyRepeat (O) n = Rhythmics $ L.fromFoldable $ replicate n O
simplifyRepeat (Sd rhy) n = Rhythmics $ map (\r -> Sd r) $ L.fromFoldable $ replicate n rhy
simplifyRepeat (Repeat rhy n2) n1 = simplifyRepeat rhy n
  where n = round ((toNumber n1) * (toNumber n2))
simplifyRepeat (Bjorklund eu k n' rot) n = simplifyRepeat (simplifyBjorklund eu k n' rot) n
simplifyRepeat (Rhythmics xs) n = Rhythmics $ L.fromFoldable $ concat $ replicate n $ fromFoldable xs


getVoiceIndex:: Number -> Number -> Number -> Number -> Array Int -- Index for Voice 
getVoiceIndex xws we x1 dur = 
  let nOfFstBlock = nFirstBlock xws x1 dur  -- :: Int
      nOfLstBlock = nLastBlock we x1 dur  -- Maybe Int
      nOfBlocks = case nOfLstBlock of 
                    Nothing -> []
                    (Just n) -> (nOfFstBlock..n) -- [Int]
  in nOfBlocks

-- start times of blocks in expanded window, in seconds since origin
getBlocks:: Number -> Number -> Number -> Number -> Array Number
getBlocks xws we x1 dur = 
  let nOfFstBlock = nFirstBlock xws x1 dur  -- :: Int
      nOfLstBlock = nLastBlock we x1 dur  -- Maybe Int
      nsOfBlocks = case nOfLstBlock of 
                    Nothing -> [] --(nOfFstBlock..(nOfFstBlock + 1))
                    (Just n) -> (nOfFstBlock..n) -- [Int]
  in (blockToPos nsOfBlocks x1 dur)

blockToPos:: Array Int -> Number -> Number -> Array Number
blockToPos is x1 dur = map (\i -> x1 + ((toNumber i) * dur)) is

-- n of first block at or after ws, regardless of how far in the future
nFirstBlock:: Number -> Number -> Number -> Int
nFirstBlock _ _ 0.0 = 0
nFirstBlock ws x1 dur = nOfNxBlock
  where xwsB = (ws - x1)/dur -- number of block elapsed at xws
        nOfNxBlock 
          | xwsB <= 0.0 = 0
          | otherwise = ceil xwsB 

nLastBlock:: Number -> Number -> Number -> Maybe Int
nLastBlock we x1 dur = nOfLastBlock
  where wEndBlocks = (we - x1)/dur -- number of blocks elapsed at we
        nOfLastBlock 
          | wEndBlocks < 0.0 = Nothing 
          | otherwise = Just $ floor wEndBlocks

-- posix needs to be removed
onsetsFromBlocks:: Array Number -> Array Onset -> Number -> Array Onset 
onsetsFromBlocks blocks onsets dur = concat $ map (\block -> onsetsFromBlock onsets block dur) blocks

onsetsFromBlock:: Array Onset -> Number -> Number -> Array Onset
onsetsFromBlock onsets block dur = map (\(Onset bool pos) -> Onset bool (block + (pos*dur))) onsets 

----- 
durInSecs:: Number -> Number -> Number
durInSecs dur bpm = dur * (bpmToDur bpm)

bpmToFreq bpm = bpm / 60.0 -- bpmToCPS

freqToDur freq = 1.0 / freq

bpmToDur bpm = 1.0 / bpmToFreq bpm

countInFreqToSecs:: Rational -> Rational -> Rational
countInFreqToSecs freq x = x / freq

toRat:: Number -> Rational
toRat x = 
    let pFact = 1000000
        floored = floor x -- 12
        fract = x - (toNumber floored) -- 12.5 - 12.0 = 0.5
        fract' = round $ fract * (toNumber pFact) -- 500000
    in (floored%1) + (fract'%pFact) -- 12 + (500000%1000000)
