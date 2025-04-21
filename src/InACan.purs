module InACan (canonise) where

import Prelude

import Data.Identity
import Data.List (List(..), zipWith, head, tail, elem, (:), concat, (..), range, (!!))
import Data.List (fromFoldable, filter, length, filter) as L
import Data.Array (fromFoldable) as A
import Data.Either
import Data.Int
import Data.String (take, length)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Map (Map(..), filter, lookup, keys, singleton, fromFoldable, toUnfoldable, member, unions, empty, alter)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Maybe
import Data.Rational (Rational(..), toRational, fromInt, (%))

import Data.DateTime (exactDate, Year(..), Month(..), Day(..))

import Data.FunctorWithIndex (mapWithIndex)

import Data.String.CodeUnits (fromCharArray)
import Data.String (split, Pattern)

import Data.Formatter.DateTime (Formatter, parseFormatString, unformat)
import Data.Formatter.Number (Formatter, parseFormatString, unformat) as N

import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration


import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Combinators.Array (many)
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)

import AST
import Rhythm
import Aural


canonise:: String -> Maybe (Either String String) -> Maybe (Tuple Int ConvergeTo) -> Tuple Int ConvergeFrom -> List TempoMark -> Map String Polytemporal
canonise id mIDTo mIndxCTo indxCFrom tempi = 
  let main = createMainVoice id indxCFrom mIDTo mIndxCTo tempi 
      canon = createCanonicPolytempi id indxCFrom tempi    
  in alter (\n -> Just (snd main)) (fst main) canon

createCanonicPolytempi:: String -> Tuple Int ConvergeFrom ->  List TempoMark -> Map String Polytemporal
createCanonicPolytempi idOg indxCFrom tempi = nPoly
  where genIDs = genericIDS idOg tempi  -- List Tuple Str TM
        nPoly = fromFoldable $ map (\(Tuple id t) -> Tuple id $ createPolytemporal idOg indxCFrom tempi t) genIDs

createPolytemporal:: String -> Tuple Int ConvergeFrom ->  List TempoMark -> TempoMark -> Polytemporal
createPolytemporal idOg indxCFrom tempi t = Converge idCTo cTo cFrom t
  where idCTo = createID idOg (fst indxCFrom)
        cTo = convergeToFromconverge (snd indxCFrom)
        cFrom = snd indxCFrom

convergeToFromconverge:: ConvergeFrom -> ConvergeTo
convergeToFromconverge (Process n) = ProcessTo n Origin 
convergeToFromconverge (Structure n ns) = StructureTo n ns Origin 
convergeToFromconverge (Percen x) = PercenTo x Origin 
convergeToFromconverge Last = LastTo Origin

genericIDS:: String -> List TempoMark -> List (Tuple String TempoMark)
genericIDS id tempi = map (\n -> createIDandTM id n tempi) n 
  where n = (0..(L.length tempi - 1))

createMainVoice:: String -> Tuple Int ConvergeFrom -> Maybe (Either String String) -> Maybe (Tuple Int ConvergeTo) -> List TempoMark -> Tuple String Polytemporal
createMainVoice id cFrom idCTo cTo tempi = 
  let Tuple newID t = createIDandTM id (fst cFrom) tempi
      nPolytemporal = newPolytemporal (snd cFrom) idCTo cTo tempi t
  in Tuple newID nPolytemporal

createIDandTM:: String -> Int -> List TempoMark -> Tuple String TempoMark
createIDandTM id n tempi = Tuple (id <> "-" <> (show m)) tempo
  where m = n `mod` (L.length tempi)
        tempo = fromMaybe XTempo $ tempi!!m

newPolytemporal:: ConvergeFrom -> Maybe (Either String String) -> Maybe (Tuple Int ConvergeTo) -> List TempoMark -> TempoMark -> Polytemporal
newPolytemporal cFrom Nothing Nothing tempi t = Metric cTo cFrom t
  where cTo = ProcessTo 0 Origin
newPolytemporal cFrom Nothing mCTo tempi t = Metric cTo cFrom t
  where cTo = fromMaybe (ProcessTo 2666 Origin) $ (snd <$> mCTo)
newPolytemporal cFrom mIDCTo Nothing tempi t = r
  where r = case mIDCTo of
              Nothing -> Metric (ProcessTo 0 Origin) (Process 0) t
              Just x -> case x of
                          Left str -> Novus str cFrom t 
                          Right str -> Converge idTo (ProcessTo 0 Origin) cFrom t 
                              where idTo = createID str 0
newPolytemporal cFrom mIDCTo mCTo tempi t = r
  where r = case mIDCTo of
              Nothing -> newPolytemporal cFrom Nothing mCTo tempi t  
              Just x -> case x of
                          Left str -> Novus str cFrom t 
                          Right str -> Converge idTo cTo cFrom t 
                              where cTo = fromMaybe (ProcessTo 0 Origin) $ (snd <$> mCTo)
                                    idTo = createID str $ fromMaybe 0 $ (fst <$> mCTo) 
newPolytemporal cFrom _ _ _ t = Metric (ProcessTo 0 Origin) (Process 0) t

createID:: String -> Int -> String
createID id n = id <> "-" <> show n