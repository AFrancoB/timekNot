module SpanOperations (spreadInBlock,cycleInSubDivision, uno, dos, tres, cuatro, cinco, seis, siete, ocho,nueve, recursiveCycleStructInBlock, thresholdOfCycle, indexSliceStructure,cycleInSubDivisionStruct) where

import Prelude

import Data.Maybe
import Data.Tuple
import Data.Array (filter,fromFoldable,(!!), zipWith, replicate, take, concat, (..), (:), init, tail, last,head,reverse,zip, cons, uncons, snoc, length, singleton)

import Data.Int (toNumber)

import Data.Traversable (scanl,traverseDefault)

import AST
import DurationAndIndex


-- FROM HERE I START TO CHECK SPREAD and a new cycle within the structure
-- there is an alternative function here that I will not use. It is simpler and easier to understand but the functinality so far is the same as the one already in use. 
-- Will double check if I find further bugs in spread.

-- data Event = Event Onset Index

-- data Onset = Onset Boolean Number

-- data Index = Index Int (Array Int) Int


-- xx[xx[xx[xx]]]
-- 1: 

-- uno = Event (Onset true 0.0) (Index 0 [0] 0) -- a = 0.0 >= 0 && 0.0 < 0.25
-- dos = Event (Onset true 0.33333) (Index 0 [1] 1)   -- b = 0.33 >= 0.25 && 0.33 < 0.5 
-- tres = Event (Onset true 0.66666) (Index 0 [2,0] 2) -- 
-- cuatro = Event (Onset true 0.777777) (Index 0 [2,1] 3)
-- cinco = Event (Onset true 0.8888888) (Index 0 [2,2,0] 4)
-- seis = Event (Onset true 0.9259259259259258) (Index 0 [2,2,1] 5)
-- siete = Event (Onset true 0.9629629629629628) (Index 0 [2,2,2,0] 6)
-- ocho = Event (Onset true 0.9814814814814813) (Index 0 [2,2,2,1] 7)

-- this first case xx[xx[xx[xx]]]x
-- uno = Event (Onset true 0.0) (Index 0 [0] 0) -- a = 0.0 >= 0 && 0.0 < 0.25
-- dos = Event (Onset true 0.25) (Index 0 [1] 1)   -- b = 0.33 >= 0.25 && 0.33 < 0.5 
-- tres = Event (Onset true 0.5) (Index 0 [2,0] 2) -- 
-- cuatro = Event (Onset true 0.5833333333333334) (Index 0 [2,1] 3)
cinco = Event (Onset true 0.6666666666666667) (Index 0 [2,2,0] 4)
seis = Event (Onset true 0.6944444444444445) (Index 0 [2,2,1] 5)
siete = Event (Onset true 0.7222222222222223) (Index 0 [2,2,2,0] 6)
ocho = Event (Onset true 0.7361111111111112) (Index 0 [2,2,2,1] 7)
nueve = Event (Onset true 0.75) (Index 0 [3] 8)

-- ((X psx:0.0) : (X psx:0.25) : (X psx:0.5) : (X psx:0.5833333333333334) : (X psx:0.6666666666666667) : (X psx:0.6944444444444445) : (X psx:0.7222222222222223) : (X psx:0.7361111111111112) : (X psx:0.75) : Nil)

-- rh = Rhythmics (X:X:(Sd (Rhythmics (X:X:(Sd (Rhythmics (X:X:(Sd (Rhythmics (X:X:Nil))):Nil))):Nil))):X:Nil)


-- [[xx]xx]xx - Second case
-- rh = Rhythmics (Sd (Rhythmics (Sd (Rhythmics (X:X:Nil)):X:X:Nil)):X:X:Nil)
-- uno = Event (Onset true 0.0) (Index 0 [0,0,0] 0) -- a = 0.0 >= 0 && 0.0 < 0.25
-- dos = Event (Onset true 0.05555555555555555) (Index 0 [0,0,1] 1)   -- b = 0.33 >= 0.25 && 0.33 < 0.5 
-- tres = Event (Onset true 0.1111111111111111) (Index 0 [0,1] 2) -- 
-- cuatro = Event (Onset true 0.2222222222222222) (Index 0 [0,2] 3)
-- cinco = Event (Onset true 0.3333333333333333) (Index 0 [1] 4)
-- seis = Event (Onset true 0.6666666666666666) (Index 0 [2] 5)    

-- x[xx]x - Third Case
-- rh = Rhythmics (X:Sd (Rhythmics (X:X:Nil)):X:Nil)
uno = Event (Onset true 0.0) (Index 0 [0] 0) -- a = 0.0 >= 0 && 0.0 < 0.25
dos = Event (Onset true 0.3333333333333333) (Index 0 [1,0] 1)   -- b = 0.33 >= 0.25 && 0.33 < 0.5 
tres = Event (Onset true 0.5) (Index 0 [1,1] 2) -- 
cuatro = Event (Onset true 0.6666666666666666) (Index 0 [2] 3)



------
-- here this function only works for a spread at the index level strange to solve it
-- spread functions are now general for all values!!! Bliss
-- spreadInBlockProgress:: forall a. Array a -> Event -> Rhythmic -> Maybe a
-- spreadInBlockProgress xs event rhythmic = fufu xsLimits percenPos
--     where xsLimits = zip xs $ getLimits xs                            -- fun:: Array a -> Array (Tuple Num Num)
--           percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic -- gets the pos relative to a block 
--           modIndex = (getEventIndex event) `mod` ((length $ fromFoldable percenPositions)) -- aqui deberia de haber un ln -1 despues del mod, falta el len
--           percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
          

-- fifi onsets xsLimits = map (fufu xsLimits) onsets

-- fufu xsLimits onset = filter isJust $ map (\xsL -> f xsL ) xsLimits 
--     where f (Tuple a n) = if (onset >= fst n) && (onset < snd n) then (Just a) else Nothing


-- getLimits:: forall a. Array a -> Array (Tuple Number Number)
-- getLimits xs = xsLimits
--     where lenXS = length xs
--           xsPoints = map (\x -> (toNumber x) / (toNumber lenXS)) $ 0..(lenXS -1)
--           tailsXS = fromMaybe [1.0] $ tail (snoc xsPoints 1.0)
--           xsLimits = zip xsPoints tailsXS

--- END OF POTENTIAL REFACTOR
-- spanMaybe CycleInBlock xs event _ = xs !! (getStructureIndexHead event `mod` length xs)


-- spanMaybe CycleInBlock xs event _ = xs !! (getStructureIndexHead event `mod` length xs)

-- -- ['a','b','c','d'] (Event (Onset true 0.5) (Index 0 [1] 1)) XX
-- spanMaybe CycleInBlock xs event rhy = 
--     where 



-- fromMaybe Nothing $ head $ filter isJust uno
--     where uno = zipWith (\x n -> if indx == x then Just n else Nothing ) xs $ 0..(length xs)


-- [0] [1] [2,0] [2,1] [2,2,0] [2,2,1] [2,2,2,0] [2,2,2,1] [3]

-- x[xx[xx[xx]]]x

-- this re-maps the array of structure indices considering the deepest level of circulation desired by user: Level 1 is xxxx, level 2 is xx[xx], level 3 is xx[xx[xx]], etc...
recursiveCycleStructInBlock:: Array (Array Int) -> Int -> Int -> Int
recursiveCycleStructInBlock [] level indx = indx
recursiveCycleStructInBlock struct level indx = 
    case uncons struct of 
        Just obj -> recursiveCycleStructInBlock obj.tail level $ thresholdOfCycle obj.head (fromMaybe [0] $ head $ obj.tail) level indx
        Nothing -> indx -- this case never happens
    
thresholdOfCycle:: Array Int -> Array Int -> Int -> Int -> Int
thresholdOfCycle a b lev indx 
    | (length b > lev) && (length a >= length b) = indx --  [0,0,0] [1,0]
    | (length b > lev) && (length a > lev) = indx 
    | (length b > lev) && (length a <= lev) = indx + 1
    | otherwise = indx + 1


-- = if (length b) <= level then indx + 1 else indx 


{-- NOTE ON THIS: 

cycleInSubDivision has worked exactly in ONE case (the one with the events: uno, dos, tres, etc)

The algorithm needs further testing!!

TEST: recursiveCycleStructInBlock

case with [a,b] seems strange. Test with [[0],[1,0]] and [[0,0],[0,1]], etc.... the case with two structure indices seems fragile

--}

-- m takes the structure index of the event and the index struct produced by -- rhythmicStructIndex rhythmic [0] -- then this becomes the index to 'cut' the array to use recursiveCycleStructInBlock to get the new index for cycling the values
indexSliceStructure:: Array Int -> Array (Array Int) -> Array (Maybe Int)
indexSliceStructure indx xs = zipWith (\x n -> if indx == x then Just n else Nothing ) xs $ 0..(length xs)
    
-- this makes the thing circulate IN a subdivision
cycleInSubDivisionStruct:: forall a. Array a -> Rhythmic -> Int -> Maybe (Array a)
cycleInSubDivisionStruct xs rhythmic level = do 
    let struct = rhythmicStructIndex rhythmic [0]
    let indeces =  map (\indx -> (recursiveCycleStructInBlock (take (indx+1) struct) level 0)-1) $ 0..((length struct)-1) 
    let xs' = map (\i -> i `mod` (length xs)) indeces
    result <- traverseDefault (\x' -> xs !! x') xs' 
    pure result


cycleInSubDivision:: forall a. Array a -> Event -> Rhythmic -> Int -> Maybe a
cycleInSubDivision xs event rhythmic level = do
    let indx = structIndexFromEvent event 
    let struct = rhythmicStructIndex rhythmic [0]
    i <- head $ filter isJust $ indexSliceStructure indx struct
    indxStr <- i 
    let newStruct = take (indxStr+1) struct  -- new struct: from the start of the block to the event.
    let index =  (recursiveCycleStructInBlock newStruct level 0)-1
    let xs' = index `mod` (length xs)
    result <- xs !! xs' 
    pure result

-- structIndexFromEvent':: Event -> Int -> Maybe Int
-- structIndexFromEvent' (Event _ (Index _ xs _)) n = xs !! n
  
structIndexFromEvent:: Event -> Array Int
structIndexFromEvent (Event _ (Index _ xs _)) = xs
  
  -- rhythmicStructIndex rhythmic [0]








-- old trusty spread in block:

spreadInBlock:: forall a. Array a -> Event -> Rhythmic -> Maybe a
spreadInBlock xs event rhythmic = spreadWrap percenPos xsLimits 
  where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic -- gets the pos relative to a block 
        modIndex = ((\(Event _ (Index _ _ n)) -> n) event) `mod` ((length $ fromFoldable percenPositions))
        percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
        segment = 1.0 / toNumber (length xs)
        limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
        limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
        xsLimits = zip xs $ zip limitsFst limitsSnd

spreadWrap:: forall a. Number -> Array (Tuple a (Tuple Number Number)) -> Maybe a
spreadWrap percenPos asWithlimits = fromMaybe Nothing $ head $ filter isJust $ map (\(Tuple as limits) -> spread percenPos as limits) asWithlimits

spread:: forall a. Number -> a -> (Tuple Number Number) -> Maybe a
spread percenPos a limits = if (percenPos >= fst limits) && (percenPos < snd limits) then (Just a) else Nothing
