module InACan (canonise) where

import Prelude

import Data.Identity
import Data.List (List(..), zipWith, head, tail, elem, (:), concat, (..), range, (!!), snoc, uncons)
import Data.List (fromFoldable, filter, length, filter) as L
import Data.List.Lazy (take, cycle, fromFoldable) as Lz
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


-- id: id for the bundle of voices
-- mIDTo is the name of the external voice that the main bundled voice converges to. It is Either Angel External. It is wrapped in a Maybe
-- mIndxCTo if the external voice is a bundle: which of the voices will it take (index) and its convergeTo, It is wrapped in Maybe
-- indxCFrom same as above but for convergeFrom.
-- the List of Tempo to generate voices!

-- a[2:20] <- re[3:12>>] 10,12 til 20 | xxxx :|

-- a-2 (Tuple 2 $ Process 20) (Tuple 3 $ ProcessTo 12 After)  --- main voice --- OJO that 20 is not really what is needed, that needs to be the cycled number derived from the many convergenceFrom look at the following example:

-- a[2:[20,13,17,11]] <- re[3:12>>] 10,12 til 20 | xxxx :|

-- 20 <~ 13, 13 <~ 17, 17 <~ 11, 11 <~ 20

-- a-0 (Tuple 0 $ Process 20) (Tuple 1 $ ProcessTo 13 Origin)
-- a-1 (Tuple 1 $ Process 13) (Tuple 2 $ ProcessTo 17 Origin)
-- -- a-2 (Tuple 2 $ Process 17) (Tuple 3 $ ProcessTo 11 Origin)
-- a-2 (Tuple 2 $ Process 17) external (Tuple 3 $ ProcessTo 12 After)
-- a-3 (Tuple 3 $ Process 11) (Tuple 4 $ ProcessTo 20 Origin)


canonise:: String -> Maybe (Either String String) -> Maybe (Tuple Int ConvergeTo) -> Tuple Int (List ConvergeFrom) -> List TempoMark -> Map String Polytemporal
canonise id mIDTo mIndxCTo indxCFrom tempi = 
  let main = createMainVoice id indxCFrom mIDTo mIndxCTo tempi 
      canon = createCanonicPolytempi id indxCFrom tempi    
  in alter (\n -> Just (snd main)) (fst main) canon

createCanonicPolytempi:: String -> Tuple Int (List ConvergeFrom) ->  List TempoMark -> Map String Polytemporal
createCanonicPolytempi idOg indxCFrom tempi = nPoly
  where genIDs = genericIDS idOg tempi  -- List Tuple Str TM
        cFroms = snd indxCFrom
        convergences = zipWith (\a b -> Tuple a $ convergeToFromconverge b) cFroms $ snoc x.tail x.head
          where x = fromMaybe {head: Process 0, tail: Nil} $ uncons $ snd indxCFrom
        cycledConv = L.fromFoldable $ Lz.take (L.length genIDs) $ Lz.cycle $ Lz.fromFoldable convergences  -- List (Tuple From To)
        namedConvTo = zipWith (\(Tuple from to) (Tuple tag _) -> Tuple from (Tuple to tag)) cycledConv $ snoc genIDs'.tail genIDs'.head
          where genIDs' = fromMaybe {head: (Tuple "errorCanonicPolyTempi" (CPM (0%0))), tail: Nil} $ uncons $ genIDs
        nPoly = fromFoldable $ zipWith (\(Tuple id t) (Tuple from to) -> Tuple id $ createPolytemporal' from to t) genIDs namedConvTo

createPolytemporal':: ConvergeFrom -> Tuple ConvergeTo String -> TempoMark -> Polytemporal
createPolytemporal' from (Tuple to idCTo) t = Converge idCTo to from t


-- createPolytemporal:: String -> Tuple Int (List ConvergeFrom) ->  List TempoMark -> TempoMark -> Polytemporal
-- createPolytemporal idOg indxCFrom tempi t = Converge idCTo cTo (fromMaybe (Process 0) $ head $ snd indxCFrom) t
--   where idCTo = createID idOg (fst indxCFrom)
--         cTo = convergeToFromconverge (fromMaybe (Process 0) $ head $ snd indxCFrom)
--         cFrom = snd indxCFrom

convergeToFromconverge:: ConvergeFrom -> ConvergeTo   -- this conversion indicates that convergeFrom can also have a >> or << value. Reason tis further
convergeToFromconverge (Process n) = ProcessTo n Origin 
convergeToFromconverge (Structure n ns) = StructureTo n ns Origin 
convergeToFromconverge (Percen x) = PercenTo x Origin 
convergeToFromconverge Last = LastTo Origin

genericIDS:: String -> List TempoMark -> List (Tuple String TempoMark)
genericIDS id tempi = map (\n -> createIDandTM id n tempi) n 
  where n = (0..(L.length tempi - 1))

createMainVoice:: String -> Tuple Int (List ConvergeFrom) -> Maybe (Either String String) -> Maybe (Tuple Int ConvergeTo) -> List TempoMark -> Tuple String Polytemporal
createMainVoice id cFrom idCTo cTo tempi = 
  let Tuple newID t = createIDandTM id (fst cFrom) tempi
      nPolytemporal = newPolytemporal (chooseVoiceToExternal cFrom tempi) idCTo cTo tempi t 
      -- nPolytemporal = newPolytemporal (snd cFrom) idCTo cTo tempi t
  in Tuple newID nPolytemporal

chooseVoiceToExternal:: Tuple Int (List ConvergeFrom) -> List TempoMark -> ConvergeFrom
chooseVoiceToExternal (Tuple n cfs) tempi = fromMaybe (Process 0) $ cfs!!m 
  where cycledCFList = L.fromFoldable $ Lz.take (L.length tempi) $ Lz.cycle $ Lz.fromFoldable cfs
        m = n `mod` (L.length tempi)

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





-- 