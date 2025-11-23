module AuralInACan (auralInACan) where

import Prelude

import Data.Identity
import Data.List (List(..), head, tail, elem, (:), filter, fromFoldable, (..), length, zip, concat, mapMaybe, zipWith, transpose, (!!))
import Data.List.Lazy (cycle)
import Data.List.Lazy as Lz
import Data.Array (fromFoldable, length) as A
import Data.Either
import Data.Int
import Data.Tuple (Tuple(..), fst, snd)
import Data.Map (Map(..), lookup, keys, singleton, toUnfoldable, member, values, unions, empty, intersectionWith)
import Data.Map (fromFoldable) as M
import Data.Maybe (Maybe(..), fromMaybe)
-- import Data.Set as Set
import Data.String as Str
import Data.Traversable (maximum)

import Data.FunctorWithIndex (mapWithIndex)

import Data.String.CodeUnits (fromCharArray)

import Parsing
import Parsing.String
import Parsing.String.Basic
import Parsing.Combinators
import Parsing.Combinators.Array (many)
import Parsing.Language (haskellStyle)
import Parsing.Token (makeTokenParser)

import AST
-- import Variant
import VariExperiments as X

-- pitch is unattended also!

auralInACan:: String -> Maybe (List Int) -> List (Tuple Value X.V) -> Map String Aural
auralInACan id Nothing xs = asMap
    where len = fromMaybe 0 $ maximum $ map (\(Tuple _ x) -> maxLenVLists x ) xs
          ids = map (\n -> id <> "-" <> show n) (0..(len -1))
          shaped = map (\(Tuple _ x) -> cycleVLists x len) xs
          varToVal = zipWith (\(Tuple x _) s -> valInACan x s) xs shaped
          asMap = M.fromFoldable $ zipWith (\id x -> Tuple id x ) ids $ transpose varToVal
auralInACan id (Just ns) xs = selectAuralInACan id ns xs

-- singleAuralInACan:: String -> Int -> List (Tuple Value Variant) -> Map String Aural
-- singleAuralInACan id n xs = asMap
--     where newID = id <> "-" <> show n 
--           newXS = xs !! n -- Maybe (Tuple Value Variant)
--           varToVal = map (\(Tuple val var) -> valInACan val var) newXS -- Maybe (List Value)
--           asMap = fromMaybe empty $ singleton newID <$> varToVal

selectAuralInACan:: String -> List Int -> List (Tuple Value X.V) -> Map String Aural
selectAuralInACan id ns xs = intersectionWith (\a b -> b) newMap fullMap
    where ids = map (\n -> id <> "-" <> show n) ns
          newMap = M.fromFoldable $ map (\id -> Tuple id Nil) ids
          fullMap = auralInACan id Nothing xs -- Map String Aural

cycleVLists:: X.V -> Int -> X.V
cycleVLists (X.VList ns) len = X.VList $ fromFoldable $ Lz.take len $ cycle $ Lz.fromFoldable ns
cycleVLists _ len = X.VList Nil

-- this acts in top VList
maxLenVLists:: X.V -> Int
maxLenVLists (X.VList xs) = length xs--fromMaybe 0 $ maximum $ map lenVList xs 
maxLenVLists _ = 0

lenVList:: X.V -> Int
lenVList (X.VList xs) = length xs 
lenVList _ = 0

valInACan:: Value -> X.V -> List Value
valInACan (N spn _ v') (X.VList ns) = map (\n -> N spn (getVListInt n) v') ns
valInACan (Orbit spn _ v') (X.VList ns) = map (\n -> Orbit spn (getVListInt n) v') ns
valInACan (Gain spn _ v') (X.VList xs) = map (\x -> Gain spn (getVListNum x) v') xs
valInACan (Pan spn _ v') (X.VList xs) = map (\x -> Pan spn (getVListNum x) v') xs
valInACan (Speed spn _ v') (X.VList xs) = map (\x -> Speed spn (getVListNum x) v') xs
valInACan (Begin spn _ v') (X.VList xs) = map (\x -> Begin spn (getVListNum x) v') xs
valInACan (End spn _ v') (X.VList xs) = map (\x -> End spn (getVListNum x) v') xs
valInACan (CutOff spn _ v') (X.VList xs) = map (\x -> CutOff spn (getVListNum x) v') xs
valInACan (CutOffH spn _ v') (X.VList xs) = map (\x -> CutOffH spn (getVListNum x) v') xs
valInACan (Legato spn _ v') (X.VList xs) = map (\x -> Legato spn (getVListNum x) v') xs
valInACan (MaxW spn _ v') (X.VList xs) = map (\x -> MaxW spn (getVListNum x) v') xs
valInACan (MinW spn _ v') (X.VList xs) = map (\x -> MinW spn (getVListNum x) v') xs
valInACan (Inter spn _ v') (X.VList xs) = map (\x -> Inter spn (getVListNum x) v') xs
valInACan (Sound spn _ v') (X.VList sts) = map (\st -> Sound spn (getVListStr st) v') sts
valInACan (Vowel spn _ v') (X.VList sts) = map (\st -> Vowel spn (getVListStr st) v') sts
valInACan (Dastgah spn d) (X.VList ns) = dastgahInACan spn d ns
valInACan (Alpha spn _) (X.VList ns) = map (\n -> Alpha spn (getVListInt n)) ns 
valInACan (Beta spn _) (X.VList ns) = map (\n -> Beta spn (getVListInt n)) ns 
valInACan (Gamma spn _) (X.VList ns) = map (\n -> Gamma spn (getVListInt n)) ns 
valInACan (Xeno id spn _) (X.VList ns) = map (\n -> Xeno id spn (getVListInt n)) ns
valInACan _ _ = Nil

dastgahInACan:: Span -> Dastgah -> List X.V -> List Value
-- dastgahInACan _ _ _ = Nil
dastgahInACan sp (Shur _) ns = map (\n -> Dastgah sp (Shur (getVListInt n))) ns
dastgahInACan sp (Segah _) ns = map (\n -> Dastgah sp (Segah (getVListInt n))) ns
dastgahInACan sp (Nava _) ns = map (\n -> Dastgah sp (Nava (getVListInt n))) ns
dastgahInACan sp (Homayun _) ns = map (\n -> Dastgah sp (Homayun (getVListInt n))) ns
dastgahInACan sp (Chahargah _) ns = map (\n -> Dastgah sp (Chahargah (getVListInt n))) ns
dastgahInACan sp (Mahur _) ns = map (\n -> Dastgah sp (Mahur (getVListInt n))) ns
dastgahInACan sp (RastPanjgah _) ns = map (\n -> Dastgah sp (RastPanjgah (getVListInt n))) ns


getVarStr:: X.V -> String
getVarStr (X.VString str) = str
getVarStr (X.VNum x) = show x
getVarStr (X.VInt n) = show n
getVarStr _ = "2666"

getVListStr:: X.V -> List String
getVListStr (X.VList ns) = map getVarStr ns
getVListStr _ = Nil

getVarInt:: X.V -> Int
getVarInt (X.VInt n) = n
getVarInt (X.VNum x) = round x
getVarInt _ = 2666

getVListInt:: X.V -> List Int
getVListInt (X.VList ns) = map getVarInt ns
getVListInt _ = Nil

getVarNum:: X.V -> Number
getVarNum (X.VInt n) = toNumber n
getVarNum (X.VNum x) = x
getVarNum _ = 2666.0

getVListNum:: X.V -> List Number
getVListNum (X.VList ns) = map getVarNum ns
getVListNum _ = Nil

-- List (Tuple Value Variant)
-- need to get all Variants (which all must be VList) and get their length
-- the biggest length determines the length of the rest. If the VList is smaller than this, then cycle through it until reaching this number:
-- so this:
-- [s]
-- [n, n, n, n]
-- [p, p]
-- [g]

-- becomes this:
-- [s, s, s, s]
-- [n, n, n, n]
-- [p, p, p, p]
-- [g, g, g, g]

-- then we need to produce a Map String (List Value), where we get:
-- (mu-0, [s,n,p,g])    (mu-2, [s,n,g,p])
-- (mu-1, [s,n,p,g])    (mu-3, [s,n,g,p])

-- where index 0 of each list becomes id-0, index 1 becomes id-1, etc...



------------------------------------------------
-- to open sample parser with variants and InACanization

-- sound, n, pan, gain, etc, 
--parser from :: P Value to :: P (Either (Tuple Value Variant) Value)

-- then before operate need a function that checks Left (Tuple Value Variant) / Right Value. 
-- Right goes to operate (:: Tuple Value Variant), Left returns the Tuple in the Left

--



tokenParser = makeTokenParser haskellStyle
parens      = tokenParser.parens
braces      = tokenParser.braces
identifier  = tokenParser.identifier
reserved    = tokenParser.reserved
naturalOrFloat = tokenParser.naturalOrFloat
natural = tokenParser.natural
float = tokenParser.float
whitespace = tokenParser.whiteSpace
colon = tokenParser.colon
brackets = tokenParser.brackets
comma = tokenParser.comma
semi = tokenParser.semi
integer = tokenParser.integer
stringLiteral = tokenParser.stringLiteral
reservedOp = tokenParser.reservedOp
stringLit = tokenParser.stringLiteral