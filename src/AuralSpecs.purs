module AuralSpecs (auralSpecs) where 

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Tuple
import Data.Maybe
import Data.Maybe.First
import Data.Either
import Data.Map as M
import Data.Foldable (sum)
import Data.Int
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array (filter,fromFoldable,(!!), zipWith, replicate, concat, (..), (:), init, tail, last,head,reverse,zip, cons, uncons, snoc, length, singleton)
import Data.List
import Data.List (fromFoldable,concat,zip,zipWith,length,init) as L
import Data.Traversable (scanl,traverseDefault,sequence)

import Data.Newtype
import Foreign 

import Data.Tempo

import AST
import DurationAndIndex
import Parser
import Rhythm
import TestOpsAndDefs
import AssambleWebdirt
import XenoPitch
import Dastgah
   
import Data.Rational (Rational(..), (%), fromInt)
import Data.Rational (toNumber) as R
import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration

-- aural specs maps on the list of aurals. One aural attribute at the time to process
auralSpecs:: Voices -> Rhythmic -> List Aural -> M.Map String XenoPitch -> Array Event -> Effect (Array Foreign)
auralSpecs v r aurals x es' = map concat <$> traverseDefault (\a -> auralSpecs' v r a x es) $ fromFoldable aurals
    where es = filter checkOnset es' -- here O get removed!

auralSpecs':: Voices -> Rhythmic -> Aural -> M.Map String XenoPitch -> Array Event -> Effect (Array Foreign)
auralSpecs' voices rhy aural xenopitch events 
   | (checkForSound aural) = pure []
   | otherwise = traverseDefault (processEvent voices rhy aural xenopitch) events

checkForSound:: List Value -> Boolean
checkForSound aural = not $ elem true $ map isSound aural 

checkOnset:: Event -> Boolean
checkOnset (Event o i) = (\(Onset b p) -> b) o 

processEvent:: Voices -> Rhythmic -> List Value -> M.Map String XenoPitch -> Event -> Effect Foreign
processEvent v r vals xp ev = do
  let when = processWhen ev 
  let s = processSound v r (getS vals) ev 
  let n = processN v r (getN vals) ev 
  let gain = processGain v r (getG vals) ev 
  let pan = processPan v r (getP vals) ev 
  let speed = processSpeed v r (getSpeed vals) ev
  let begin = processBegin v r (getBegin vals) ev
  let end = processEnd v r (getEnd vals) ev
  let vowel = processVowel v r (getVowel vals) ev
  let cutoff = processCutOff v r (getCutOff vals) ev
  let cutoffh = processCutOffH v r (getCutOffH vals) ev
  let maxw = processMaxW v r (getMaxW vals) ev
  let minw = processMinW v r (getMinW vals) ev
  let inter = processInter v r (getInter vals) ev
  let legato = processLegato v r (getLegato vals) ev
  let orbit = processOrbit v r (getOrbit vals) ev
  let note = processNote v xp r (getNote vals) (getXNote vals) ev
  makeWebDirtEvent when s n gain pan speed begin end vowel cutoff cutoffh maxw minw inter legato orbit note

makeWebDirtEvent:: Number -> String -> Int -> Maybe Number -> Maybe Number -> Maybe Number -> Maybe Number -> Maybe Number -> Maybe String -> Maybe Number -> Maybe Number -> Maybe Number -> Maybe Number -> Maybe Number -> Maybe Number -> Maybe Int -> Maybe Number -> Effect Foreign
makeWebDirtEvent when s n gain pan speed begin end vowel cutoff cutoffh maxw minw inter legato orbit note = do
  oEvent <- objectWithWhenSN when s n
  oG <- optVNum oEvent gain addGain
  oP <- optVNum oG pan addPan 
  oSp <- optVNum oP speed addSpeed
  oB <- optVNum oSp begin addBegin
  oE <- optVNum oB end addEnd
  oCOff <- optVNum oE cutoff addCutOff
  oCOffH <- optVNum oCOff cutoffh addCutOffH
  oMax <- optVNum oCOffH maxw addMaxW  
  oMin <- optVNum oMax minw addMinW  
  oInter <- optVNum oMin inter addInter
  oLeg <- optVNum oInter legato addLegato
  oOrbit <- optVInt oLeg orbit addOrbit
  oV <- optVStr oOrbit vowel addVowel
  oN <- optVNum oV note addNote
  pure oN


optVNum:: Foreign -> Maybe Number -> (Foreign -> Number -> Effect Foreign) -> Effect Foreign
optVNum o Nothing _ = pure o
optVNum o (Just x) f = f o x

optVStr:: Foreign -> Maybe String -> (Foreign -> String -> Effect Foreign) -> Effect Foreign
optVStr o Nothing _ = pure o
optVStr o (Just x) f = f o x

optVInt:: Foreign -> Maybe Int -> (Foreign -> Int -> Effect Foreign) -> Effect Foreign
optVInt o Nothing _ = pure o
optVInt o (Just x) f = f o x

processNote:: Voices -> M.Map String XenoPitch -> Rhythmic -> Maybe Value -> Maybe Value -> Event -> Maybe Number 
processNote _ xp r Nothing xNotes e = Nothing
processNote _ xp r (Just (Prog span lista)) xNotes e = mergeProgWithNote xp r (Prog span lista) xNotes e
processNote _ xp r (Just (Alpha span lista)) _ e = spanMaybe span newList e r
  where newList = fromFoldable $ map alpha lista
processNote _ xp r (Just (Beta span lista)) _ e = spanMaybe span newList e r
  where newList = fromFoldable $ map beta lista
processNote _ xp r (Just (Gamma span lista)) _ e = spanMaybe span newList e r
  where newList = fromFoldable $ map gamma lista
processNote _ xp r (Just (Dastgah span d)) _ e = spanMaybe span newList e r
  where newList = getMIDIInterval $ analysisDastgahPattern span r d
processNote _ xp r (Just (Xeno id span lista)) _ e = spanMaybe span (fromFoldable midiIntervals) e r
  where target = getXPTarget (fst id) xp
        -- target = fromMaybe (EDO 0.0 0) $ M.lookup (fst id) xp 
        midiIntervals = xenoPitchAsAuralPattern (Tuple target (snd id)) (fromFoldable lista) span r
processNote _ _ _ _ _ _ = Nothing


getXPTarget:: String -> M.Map String XenoPitch -> XenoPitch
getXPTarget "shurNot8" _ = ShurNot8
getXPTarget "shurNot" _ = ShurNot
getXPTarget id xp = fromMaybe (EDO 0.0 0) $ M.lookup id xp

findRefdNote:: Voices -> M.Map String XenoPitch -> Rhythmic -> Event -> Tuple String Int -> Maybe Value -> Maybe Number
findRefdNote m xp r e (Tuple id n) xn = processNote m xp r newVal xn e
    where newVal = cycleAurals n (M.lookup id m) getNote

mergeProgWithNote:: M.Map String XenoPitch -> Rhythmic -> Value -> Maybe Value -> Event -> Maybe Number 
mergeProgWithNote xp r prog xnote ev = pitchSystemNoteToMIDI xp prog' <$> xnote'
    where prog' = processProg r prog ev
          xnote' = processXNotes r xnote ev

pitchSystemNoteToMIDI:: M.Map String XenoPitch -> (Tuple String (Maybe Int)) -> Int -> Number
pitchSystemNoteToMIDI mapa (Tuple id subset) nota = xenoPitchAsMIDINum (Tuple xn subset) nota 
  where xn = fromMaybe (EDO 0.0 0) $ M.lookup id mapa

processProg:: Rhythmic -> Value ->  Event -> Tuple String (Maybe Int)
processProg r (Prog span xs) ev = fromMaybe (Tuple "error" Nothing) $ spanMaybe span (fromFoldable xs) ev r
processProg r _ ev = Tuple "error" Nothing

processXNotes:: Rhythmic -> Maybe Value ->  Event -> Maybe Int
processXNotes r (Just (XNotes sp xs vars)) ev = processVarsMaybe vars sp xs ev r
processXNotes r _ ev = Nothing

--
processOrbit:: Voices -> Rhythmic -> Maybe Value -> Event -> Maybe Int 
processOrbit vs r Nothing ev = Nothing
processOrbit _  r (Just (Orbit sp xList vars)) ev = processVarsMaybe vars sp xList ev r
processOrbit _ _ _ _ = Nothing

findRefdOrbit:: Rhythmic -> Event -> Tuple String Int -> Voices -> Maybe Int
findRefdOrbit r ws (Tuple id n) mapa = processOrbit mapa r newVal ws
    where newVal = cycleAurals n (M.lookup id mapa) getOrbit

--
processLegato:: Voices -> Rhythmic -> Maybe Value -> Event -> Maybe Number 
processLegato vs r Nothing ev = Nothing
processLegato _  r (Just (Legato sp xList vars)) ev = processVarsMaybe vars sp xList ev r
processLegato _ _ _ _ = Nothing

findRefdLegato:: Rhythmic -> Event -> Tuple String Int -> Voices -> Maybe Number
findRefdLegato r ws (Tuple id n) mapa = processLegato mapa r newVal ws
    where newVal = cycleAurals n (M.lookup id mapa) getLegato

--
processInter:: Voices -> Rhythmic -> Maybe Value -> Event -> Maybe Number 
processInter vs r Nothing ev = Nothing
processInter _  r (Just (Inter sp xList vars)) ev = processVarsMaybe vars sp xList ev r
processInter _ _ _ _ = Nothing

findRefdInter:: Rhythmic -> Event -> Tuple String Int -> Voices -> Maybe Number
findRefdInter r ws (Tuple id n) mapa = processInter mapa r newVal ws
    where newVal = cycleAurals n (M.lookup id mapa) getInter

--
processMinW:: Voices -> Rhythmic -> Maybe Value -> Event -> Maybe Number 
processMinW vs r Nothing ev = Nothing
processMinW _  r (Just (MinW sp xList vars)) ev = processVarsMaybe vars sp xList ev r
processMinW _ _ _ _ = Nothing

findRefdMinW:: Rhythmic -> Event -> Tuple String Int -> Voices -> Maybe Number
findRefdMinW r ws (Tuple id n) mapa = processMinW mapa r newVal ws
    where newVal = cycleAurals n (M.lookup id mapa) getMinW

--
processMaxW:: Voices -> Rhythmic -> Maybe Value -> Event -> Maybe Number 
processMaxW vs r Nothing ev = Nothing
processMaxW _  r (Just (MaxW sp xList vars)) ev = processVarsMaybe vars sp xList ev r
processMaxW _ _ _ _ = Nothing

findRefdMaxW:: Rhythmic -> Event -> Tuple String Int -> Voices -> Maybe Number
findRefdMaxW r ws (Tuple id n) mapa = processMaxW mapa r newVal ws
    where newVal = cycleAurals n (M.lookup id mapa) getMaxW

--
processCutOffH:: Voices -> Rhythmic -> Maybe Value -> Event -> Maybe Number 
processCutOffH vs r Nothing ev = Nothing
processCutOffH _  r (Just (CutOffH sp xList vars)) ev = processVarsMaybe vars sp xList ev r
processCutOffH _ _ _ _ = Nothing

findRefdCutOffH:: Rhythmic -> Event -> Tuple String Int -> Voices -> Maybe Number
findRefdCutOffH r ws (Tuple id n) mapa = processCutOffH mapa r newVal ws
    where newVal = cycleAurals n (M.lookup id mapa) getCutOffH

--
processCutOff:: Voices -> Rhythmic -> Maybe Value -> Event -> Maybe Number 
processCutOff vs r Nothing ev = Nothing
processCutOff _  r (Just (CutOff sp xList vars)) ev = processVarsMaybe vars sp xList ev r
processCutOff _ _ _ _ = Nothing

findRefdCutOff:: Rhythmic -> Event -> Tuple String Int -> Voices -> Maybe Number
findRefdCutOff r ws (Tuple id n) mapa = processCutOff mapa r newVal ws
    where newVal = cycleAurals n (M.lookup id mapa) getCutOff

--
processVowel:: Voices -> Rhythmic -> Maybe Value -> Event -> Maybe String 
processVowel vs r Nothing ev = Nothing
processVowel _  r (Just (Vowel sp xList vars)) ev = processVarsMaybe vars sp xList ev r
processVowel _ _ _ _ = Nothing

findRefdVowel:: Rhythmic -> Event -> Tuple String Int -> Voices -> Maybe String
findRefdVowel r ws (Tuple id n) mapa = processVowel mapa r newVal ws
    where newVal = cycleAurals n (M.lookup id mapa) getVowel

--
processEnd:: Voices -> Rhythmic -> Maybe Value -> Event -> Maybe Number 
processEnd vs r Nothing ev = Nothing
processEnd _  r (Just (End sp xList vars)) ev = processVarsMaybe vars sp xList ev r
processEnd _ _ _ _ = Nothing

findRefdEnd:: Rhythmic -> Event -> Tuple String Int -> Voices -> Maybe Number
findRefdEnd r ws (Tuple id n) mapa = processEnd mapa r newVal ws
    where newVal = cycleAurals n (M.lookup id mapa) getEnd

--
processBegin:: Voices -> Rhythmic -> Maybe Value -> Event -> Maybe Number 
processBegin vs r Nothing ev = Nothing
processBegin _  r (Just (Begin sp xList vars)) ev = processVarsMaybe vars sp xList ev r
processBegin _ _ _ _ = Nothing

findRefdBegin:: Rhythmic -> Event -> Tuple String Int -> Voices -> Maybe Number
findRefdBegin r ws (Tuple id n) mapa = processBegin mapa r newVal ws
    where newVal = cycleAurals n (M.lookup id mapa) getBegin

--
processSpeed:: Voices -> Rhythmic -> Maybe Value -> Event -> Maybe Number 
processSpeed vs r Nothing ev = Nothing
processSpeed _  r (Just (Speed sp xList vars)) ev = processVarsMaybe vars sp xList ev r
processSpeed _ _ _ _ = Nothing

findRefdSpeed:: Rhythmic -> Event -> Tuple String Int -> Voices -> Maybe Number
findRefdSpeed r ws (Tuple id n) mapa = processSpeed mapa r newVal ws
    where newVal = cycleAurals n (M.lookup id mapa) getSpeed

--
processPan:: Voices -> Rhythmic -> Maybe Value -> Event -> Maybe Number 
processPan vs r Nothing ev = Nothing
processPan _  r (Just (Pan sp xList vars)) ev = processVarsMaybe vars sp xList ev r
processPan _ _ _ _ = Nothing

findRefdP:: Rhythmic -> Event -> Tuple String Int -> Voices -> Maybe Number
findRefdP r ws (Tuple id n) mapa = processPan mapa r newVal ws
    where newVal = cycleAurals n (M.lookup id mapa) getP
--
processGain:: Voices -> Rhythmic -> Maybe Value -> Event -> Maybe Number 
processGain vs r Nothing ev = Nothing
processGain _  r (Just (Gain sp xList vars)) ev = processVarsMaybe vars sp xList ev r
processGain _ _ _ _ = Nothing

findRefdG:: Rhythmic -> Event -> Tuple String Int -> Voices -> Maybe Number
findRefdG r ws (Tuple id n) mapa = processGain mapa r newVal ws
    where newVal = cycleAurals n (M.lookup id mapa) getG
--
processN:: Voices -> Rhythmic -> Maybe Value -> Event -> Int 
processN vs r Nothing ev = 0
processN _  r (Just (N span nList vars)) ev = processVarsInt vars span nList ev r
processN _ _ _ _ = 2666

findRefdN:: Rhythmic -> Event -> Tuple String Int -> Voices -> Int
findRefdN r ws (Tuple id n) mapa = processN mapa r newVal ws
    where newVal = cycleAurals n (M.lookup id mapa) getN
--
processSound:: Voices -> Rhythmic -> Maybe Value -> Event -> String 
processSound vs r Nothing ev = "no sound value even with check!"
processSound _  r (Just (Sound span sList vars)) ev = processVarsStr vars span sList ev r
processSound _ _ _ _ = "processSound failed at pattern matching"

--
processVarsStr:: List (Variation String) -> Span -> List String -> Event -> Rhythmic -> String
processVarsStr Nil sp xs ev r = spanStr sp (fromFoldable xs) ev r
processVarsStr (Cons v vs) sp xs ev r = 
    if isVar v ev 
    then spanStr vSpan (fromFoldable vList) ev r
    else processVarsStr vs sp xs ev r
        where vSpan = getVSpan v
              vList = getVListStr v

processVarsInt:: List (Variation Int) -> Span -> List Int -> Event -> Rhythmic -> Int
processVarsInt Nil sp xs ev r = spanInt sp (fromFoldable xs) ev r
processVarsInt (Cons v vs) sp xs ev r = 
    if isVar v ev 
    then spanInt vSpan (fromFoldable vList) ev r
    else processVarsInt vs sp xs ev r
        where vSpan = getVSpan v
              vList = getVListInt v

---- work here with the maybes!!!!
processVarsMaybe:: forall a. List (Variation a) -> Span -> List a -> Event -> Rhythmic -> Maybe a
processVarsMaybe Nil sp xs ev r = spanMaybe sp (fromFoldable xs) ev r
processVarsMaybe (Cons v vs) sp xs ev r = 
    if isVar v ev 
    then spanMaybe vSpan (fromFoldable vList) ev r
    else processVarsMaybe vs sp xs ev r
        where vSpan = getVSpan v
              vList = getVList v

getVSpan:: forall a. Variation a -> Span
getVSpan (Every _ sp _) = sp

getVListStr:: Variation String -> List String 
getVListStr (Every _ _ xs) = xs

getVListInt:: Variation Int -> List Int
getVListInt (Every _ _ ns) = ns

getVList:: forall a. Variation a -> List a
getVList (Every _ _ xs) = xs

isVar (Every n _ _) ev = ((getBlockIndex ev)`mod`n) == 0 
isVar _ _ = false

findRefdSound:: Rhythmic -> Event -> Tuple String Int -> Voices -> String
findRefdSound r ws (Tuple id n) mapa = processSound mapa r newVal ws
    where newVal = cycleAurals n (M.lookup id mapa) getS

-- Potential here for crazyness: delay, anticipate, swing, contratiempo, microrhythm, snap...
processWhen:: Event -> Number
processWhen (Event o i) = (\(Onset b p) -> p) o 

---- span functions, three flavours: String, Int, Num
spanMaybe:: forall a. Span -> Array a -> Event -> Rhythmic -> Maybe a
spanMaybe CycleEvent xs event _ = xs !! (getEventIndex event `mod` length xs)
spanMaybe CycleBlock xs event _ = xs !! (getBlockIndex event `mod` length xs)
spanMaybe CycleInBlock xs event _ = xs !! (getStructureIndex event `mod` length xs)
spanMaybe SpreadBlock xs event rhythmic = spreadInBlock xs event rhythmic
spanMaybe _ _ _ _ = Nothing

spanStr:: Span -> Array String -> Event -> Rhythmic -> String  
spanStr CycleEvent xs event _ = strMaybe $ xs !! (getEventIndex event `mod` length xs)
spanStr CycleBlock xs event _ = strMaybe $ xs !! (getBlockIndex event `mod` length xs)
spanStr CycleInBlock xs event _ = strMaybe $ xs !! (getStructureIndex event `mod` length xs)
spanStr SpreadBlock xs event rhythmic = strMaybe $ spreadInBlock xs event rhythmic
spanStr _ _ _ _ = "error at spanStr, invalid span constructor"

spanInt:: Span -> Array Int -> Event -> Rhythmic -> Int
spanInt CycleEvent xs event _ = intMaybe $ xs !! (getEventIndex event `mod` length xs)
spanInt CycleBlock xs event _ = intMaybe $ xs !! (getBlockIndex event `mod` length xs)
spanInt CycleInBlock xs event _ = intMaybe $ xs !! (getStructureIndex event `mod` length xs)
spanInt SpreadBlock xs event rhythmic = intMaybe $ spreadInBlock xs event rhythmic
spanInt _ _ _ _ = 2666

------
-- here this function only works for a spread at the index level strange to solve it
-- spread functions are now general for all values!!! Bliss
spreadInBlock:: forall a. Array a -> Event -> Rhythmic -> Maybe a
spreadInBlock xs event rhythmic = spreadWrap percenPos xsLimits 
  where percenPositions = map (\(Onset b p) -> p) $ rhythmicToOnsets rhythmic 
        modIndex = (getEventIndex event) `mod` (length $ fromFoldable percenPositions) -- aqui deberia de haber un ln -1 despues del mod, falta el len
        percenPos = fromMaybe 0.0 $ (fromFoldable percenPositions) !! modIndex
        segment = 1.0 / toNumber (length xs)
        limitsFst = cons 0.0 (scanl (+) 0.0 $ replicate ((length xs) - 1) segment)
        limitsSnd = snoc (scanl (+) 0.0 $ replicate ((length xs) - 1) segment) 1.0
        xsLimits = zip xs $ zip limitsFst limitsSnd

spreadWrap:: forall a. Number -> Array (Tuple a (Tuple Number Number)) -> Maybe a
spreadWrap percenPos asWithlimits = fromMaybe Nothing $ head $ filter isJust $ map (\(Tuple as limits) -> spread percenPos as limits) asWithlimits

spread:: forall a. Number -> a -> (Tuple Number Number) -> Maybe a
spread percenPos a limits = if (percenPos >= fst limits) && (percenPos < snd limits) then (Just a) else Nothing

---- helpers
strMaybe:: Maybe String -> String
strMaybe x = fromMaybe "error" x

intMaybe:: Maybe Int -> Int
intMaybe x = fromMaybe 2666 x

numMaybe:: Maybe Number -> Number
numMaybe x = fromMaybe 2.666 x

cycleAurals:: Int -> Maybe Voice -> (List Value -> Maybe Value) -> Maybe Value
cycleAurals n mVoice f = do
  voice <- mVoice
  let aurals = (\(Voice t aurals) -> aurals) voice
  let len = L.length aurals 
  newVal <- (fromFoldable aurals) !! (n`mod`len)
  f newVal

-- getters
----- structure index is weird, think of nested levels
getStructureIndex:: Event -> Int
getStructureIndex (Event _ (Index _ xs _)) = fromMaybe 0 $ head $ xs

getBlockIndex:: Event -> Int
getBlockIndex (Event _ (Index n _ _)) = n

getEventIndex:: Event -> Int
getEventIndex (Event _ (Index _ _ n)) = n

getXNote:: List Value -> Maybe Value
getXNote aural = head $ filter isXNote $ fromFoldable aural

isXNote:: Value -> Boolean
isXNote (XNotes _ _ _) = true
isXNote _ = false

getNote:: List Value -> Maybe Value
getNote aural = head $ filter isNote $ fromFoldable aural

isNote:: Value -> Boolean
isNote (Dastgah _ _) = true
isNote (Alpha _ _) = true
isNote (Beta _ _) = true
isNote (Gamma _ _) = true
isNote (Xeno _ _ _) = true
isNote (Prog _ _) = true
isNote _ = false

getMaxW:: List Value -> Maybe Value
getMaxW aural = head $ filter isMaxW $ fromFoldable aural

isMaxW:: Value -> Boolean
isMaxW (MaxW _ _ _) = true
isMaxW _ = false

getMinW:: List Value -> Maybe Value
getMinW aural = head $ filter isMinW $ fromFoldable aural

isMinW:: Value -> Boolean
isMinW (MinW _ _ _) = true
isMinW _ = false

getOrbit:: List Value -> Maybe Value
getOrbit aural = head $ filter isOrbit $ fromFoldable aural

isOrbit:: Value -> Boolean
isOrbit (Orbit _ _ _) = true
isOrbit _ = false

getLegato:: List Value -> Maybe Value
getLegato aural = head $ filter isLegato $ fromFoldable aural

isLegato:: Value -> Boolean
isLegato (Legato _ _ _) = true
isLegato _ = false

getInter:: List Value -> Maybe Value
getInter aural = head $ filter isInter $ fromFoldable aural

isInter:: Value -> Boolean
isInter (Inter _ _ _) = true
isInter _ = false

getCutOffH:: List Value -> Maybe Value
getCutOffH aural = head $ filter isCutOffH $ fromFoldable aural

isCutOffH:: Value -> Boolean
isCutOffH (CutOffH _ _ _) = true
isCutOffH _ = false

getCutOff:: List Value -> Maybe Value
getCutOff aural = head $ filter isCutOff $ fromFoldable aural

isCutOff:: Value -> Boolean
isCutOff (CutOff _ _ _) = true
isCutOff _ = false

getVowel:: List Value -> Maybe Value
getVowel aural = head $ filter isVowel $ fromFoldable aural

isVowel:: Value -> Boolean
isVowel (Vowel _ _ _) = true
isVowel _ = false

getEnd:: List Value -> Maybe Value
getEnd aural = head $ filter isEnd $ fromFoldable aural

isEnd:: Value -> Boolean
isEnd (End _ _ _) = true
isEnd _ = false

getBegin:: List Value -> Maybe Value
getBegin aural = head $ filter isBegin $ fromFoldable aural

isBegin:: Value -> Boolean
isBegin (Begin _ _ _) = true
isBegin _ = false

getSpeed:: List Value -> Maybe Value
getSpeed aural = head $ filter isSpeed $ fromFoldable aural

isSpeed:: Value -> Boolean
isSpeed (Speed _ _ _) = true
isSpeed _ = false

getP:: List Value -> Maybe Value
getP aural = head $ filter isP $ fromFoldable aural

isP:: Value -> Boolean
isP (Pan _ _ _) = true
isP _ = false

getG:: List Value -> Maybe Value
getG aural = head $ filter isG $ fromFoldable aural

isG:: Value -> Boolean
isG (Gain _ _ _) = true
isG _ = false

getN:: List Value -> Maybe Value
getN aural = head $ filter isN $ fromFoldable aural

isN:: Value -> Boolean
isN (N _ _ _) = true
isN _ = false

getS:: List Value -> Maybe Value
getS aural = head $ filter isSound $ fromFoldable aural

isSound:: Value -> Boolean
isSound (Sound _ _ _) = true
isSound _ = false