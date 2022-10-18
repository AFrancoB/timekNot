        module RhythmIdeas where

import Prelude
import Data.Int
import Data.List
import Data.List.Lazy (replicate)
import Data.Tuple
import Data.Rational as R
import Data.Rational (Rational,(%),fromInt)
import Data.Maybe
import Data.String (take)
import Data.String as Str

import ParseIdeas

import Effect (Effect)
import Effect.Now (nowDateTime)

import Data.Newtype

import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration
import Data.Tempo
import Data.Enum
import Partial.Unsafe


data Onset = Onset Boolean Number

instance Show Onset where
    show (Onset true n) = "(X" <> " dur:" <> (take 5 $ show n) <> ")"
    show (Onset false n) = "(O" <> " dur:" <> (take 5 $ show n) <> ")"


-- xx[ox]x 

-- xx[ox 1]x 1| xx[ox 2]x 2| xx[ox 3]x 3| xx[ox 4]x4  
-- 12  3   4    12  3   4    12  3   4    12  3   4

-- 1-2 3   4    56  7   8    9-10 11  12 13-14 15  16

-- 2%4.0~~3%4.2.1 ☭ note "0 3 7"

-- ☭ 🐚 samples "bd cp cp"

-- ☭ 🐚 note "0 3 7"

-- estribillo

-- process are what I used to call coordinates but now reflect the recursive possibilities embedded in the software
data Process = Structure Int (List Int) | Events Int

instance Show Process where
    show (Structure estribillo xs) = show estribillo <>"."<> events
        where events = Str.joinWith "." $ toUnfoldable $ map show xs
    show (Events index) = show index

--indexOfMetreAndEvents:: Tempo -> DateTime -> Rhythmic -> Tuple Int Int
events t ws we rhy = 
    let refrainIndex = indexOfRefrain t ws rhy
        tFreqAsDur = 1.0 * (R.toNumber t.freq)
        events = eventsDurations tFreqAsDur rhy
        indexofIntraEvents = (0..(length events -1))
        indexOfEvents = map (\x -> (toNumber $ fst refrainIndex) * (toNumber (length events)) + x) $ toNumber <$> indexofIntraEvents
    in rhythmicToRefrainDuration t rhy -- indexOfEvents -- zip events indexofIntraEvents

-- positionOfEvents:: Tempo -> DateTime -> Rhythmic -> List (Tuple Int Number)
positionOfEvents t ws we rhy = 
    let durRefrain = Tuple (indexOfRefrain t ws rhy) (indexOfRefrain t we rhy)
    in durRefrain


indexOfRefrain:: Tempo -> DateTime -> Rhythmic -> Tuple Int Number
indexOfRefrain t wp rhy = 
    let 
    wpCount = timeToCountNumber t wp -- Number
    refrainDur = rhythmicToRefrainDuration t rhy
    indexAsNum = wpCount / refrainDur
    in Tuple (floor indexAsNum) (rightOfpoint indexAsNum)

rhythmicToRefrainDuration:: Tempo -> Rhythmic -> Number
rhythmicToRefrainDuration t X = 1.0 / (R.toNumber t.freq)
rhythmicToRefrainDuration t O = 1.0 / (R.toNumber t.freq)
rhythmicToRefrainDuration t (Sd xs) = 1.0 / (R.toNumber t.freq)
rhythmicToRefrainDuration t (Repeat xs n) = foldl (+) 0.0 x
    where x = replicate n $ rhythmicToRefrainDuration t xs
rhythmicToRefrainDuration t (Rhythmics xs) = foldl (+) 0.0 x
    where x = map (\x -> rhythmicToRefrainDuration t x) xs


-- the refrain has only a total duration and can be indexed. The inner life of the refrain is only described by metres and events



eventsDurations:: Number -> Rhythmic -> List Onset
eventsDurations dur X = fromFoldable [Onset true dur]
eventsDurations dur O = fromFoldable [Onset false dur]
eventsDurations dur (Sd xs) = eventsDurations' dur xs
eventsDurations dur (Repeat xs n) = concat $ map (\x -> eventsDurations dur x) $ fromFoldable $ replicate n xs
eventsDurations dur (Rhythmics xs) = concat $ map (\x-> eventsDurations dur x) xs


-- data Refrain = Refrain Int Number MetricStructure EventCount
eventsDurations':: Number -> Rhythmic -> List Onset
eventsDurations' dur X = fromFoldable [Onset true dur]
eventsDurations' dur O = fromFoldable [Onset false dur]
eventsDurations' dur (Sd xs) = eventsDurations' dur xs
eventsDurations' dur (Repeat xs n) = concat $ map (\x -> eventsDurations' newDur x) $ fromFoldable $ replicate n xs
    where newDur = dur / (toNumber n)
eventsDurations' dur (Rhythmics xs) = concat $ map (\x-> eventsDurations' newDur x) xs
    where newDur = dur / (toNumber $ length xs)


x:: Rhythmic
x = X

o:: Rhythmic
o = O

toRat:: Number -> Rational
toRat x = 
    let pFact = 1000000
        floored = floor x -- 12
        fract = x - (toNumber floored) -- 12.5 - 12.0 = 0.5
        fract' = round $ fract * (toNumber pFact) -- 500000
    in (floored%1) + (fract'%pFact) -- 12 + (500000%1000000)


rightOfpoint:: Number -> Number
rightOfpoint x = x - (toNumber $ floor x)


---- testing stuff ---------------
makeDate :: Int -> Month -> Int -> Date
makeDate y m d = 
    unsafePartial $ fromJust $ 
       canonicalDate <$> toEnum y <@> m <*> toEnum d

makeTime :: Int -> Int -> Int -> Int -> Time
makeTime h min sec milisec = 
    unsafePartial $ fromJust $ Time <$> toEnum h <*> toEnum min <*> toEnum sec <*> toEnum milisec

t:: Tempo
t = {freq: (2%1),time: (DateTime (makeDate 2022 June 3) (makeTime 19 11 25 100)), count: fromInt 0 }

tAncient:: Tempo
tAncient = {freq: (2%1),time: (DateTime (makeDate 2012 June 3) (makeTime 19 11 25 100)), count: fromInt 0 }

ws:: Int -> Int -> DateTime
ws x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

we:: Int -> Int -> DateTime
we x y = (DateTime (makeDate 2022 June 3) (makeTime 19 15 x y))

eval:: DateTime
eval = (DateTime (makeDate 2022 June 3) (makeTime 19 15 0 0))

oDur:: Rational  -- transposition value 2
oDur = (1%2)

onsets:: List Number
onsets = fromFoldable [0.0,0.2,0.5] -- at this level a metric unit should be added. For testing: 0,0.1 (metre 1), 0.2,0.3 (metre 2), 0.4,0.5 (metre 3), 0.6,0.7 (metre 4), etc...

countToStart:: Int
countToStart = 327
-------------