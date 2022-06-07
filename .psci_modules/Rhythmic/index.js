import * as Parsing_Language from "../Parsing.Language/index.js";
import * as Parsing_Token from "../Parsing.Token/index.js";

// relevant structures above this one:
// Program = [Expression]
// Expression = Expression Passage Polytemporal
// Passage = Rhythmic Aural
// Polytemporal = Canonic Voicing Nose Encounter
// This file contains the data structures to parse the rhythmic notation that will be the head of the passages. It is based on binary events that are either rests or onsets. The data structure represents all of the possible ways in which these binary options can be invoked by the player. For now, the data structure is called Rhythmic and it should be recursive. It has 5 constructors. 
// 1) Onset: Represents an event that can be a rest or an onset. 
// 2) Subdivision: Represents an event that has nested Rhythmic and which the event's duration is going to be the reciprocal of the length of the list of rhythmics. Example: "xxox" are all onset and all have durations of 1. But "[xxox]" are of type ::Subdivision and all have durations of 1/4. "[x[ox]]" consists of an onset with a duration of 1/2, a rest with dur of 1/4 and an onset with dur of 1/4.
// 3) Repetition: Represnts a Rhythmic that is repeated N times.
// 4) Euclidean: Represents a Rhythmic that has k number of repetitions in a euclidean space of N.
// x) Pattern: represents :: [Rhythmic], a list of Rhythmic structures
// xxox xx[ox]x xx[o[xxx]]x xxox!2 xxox(3,8) [xx](3,8) -- this should be a valid pattern program -- not working
// xxox (just [onset]) should be 1 1 ~1 1 
// xx[ox]x should be 1 1 [~1/2 1/2] 1 -- onsets with one subdivision
// xx[o[xxx]]x should be 1 1 [~1/2[1/6 1/6 1/6]] 1 -- nested subdivisions
// xxox!2 should be 1 1 ~1 1 1 1 ~1 1 -- a repetition
// xxox(3,8) should be 1 1 ~1 1 ~1 ~1 ~1 ~1 ~1 ~1 ~1 ~1 1 1 ~1 1 ~1 ~1 ~1 ~1 ~1 ~1 ~1 ~1 1 1 ~1 1 ~1 ~1 ~1 ~1 -- a euclidean
// [xx(3,8)] should be [1/2 1/2] ~1 ~1 [1/2 1/2] ~1 ~1 [1/2 1/2] ~1 -- euclidean inside a subdivision
// [xox] 
// [xox(3,5)](3,4,1)
// [xo xxxxxxxxxx xxo ooooxxx  xooxooxo]
// chain!!!! check that out.
// from complex to simple parsers
// data Rhythmic = 
//   Onset Boolean |
//   Pattern (List Rhythmic) | -- piling adjacent things no white space
//   Subdivision (List Rhythmic) | -- list separated by spaces -- space as operator
//   Euclidean Euclidean Int Int Int | 
//   Repetition Rhythmic Int  
// --data EuclideanType = Full | K | InverseK
// -- data Euclidean = Full Rhythmic Rhythmic | K Rhythmic | InverseK Rhythmic
// instance euclideanShowInstance :: Show Euclidean where
//   show (Full x y) = (show x) <>"on ks and not on ks " <> (show y)
//   show (K x) = show x
//   show (InverseK x) = show x
// instance rhythmicShowInstance :: Show Rhythmic where
//   show (Onset on) = show on
//   show (Pattern on) = show on
//   show (Subdivision on n) = (show on) <>" subdivided in "<> (show n)
//   show (Euclidean eu k n off) = show eu <> " euclidean " <> (show k) <> "," <> (show n) <> "," <> (show off)
//   show (Repetition on n) = show on <> " times " <> (show n)
// parsePattern:: P Rhythmic
// parsePattern = do
//   x <- choice [try parseSubDivision, try parseEuclidean, try parseRepetition, try parseOnset] `sepBy` whitespace
//   pure $ Pattern $ fromFoldable x
// ---- parseSubDivision should be able to parse patterns
// parseSubDivision:: P Rhythmic
// parseSubDivision = do
//   _ <- string "["
//   x <- choice [try parseEuclidean, try parseRepetition, try parseOnset] `sepBy` whitespace
//   _ <- string "]"
//   pure $ Subdivision (Pattern $ fromFoldable x) $ fromFoldable (map  fromRhythmicToSubDivisions x)
// fromRhythmicToSubDivisions:: Rhythmic -> Maybe Int
// fromRhythmicToSubDivisions (Onset x) = Nothing
// fromRhythmicToSubDivisions (Pattern x) = Just $ length x
// fromRhythmicToSubDivisions (Euclidean on k n o) = Just n
// fromRhythmicToSubDivisions (Repetition on n) = Just n
// fromRhythmicToSubDivisions (Subdivision _ _) = Nothing
// parseOnset:: P Rhythmic
// parseOnset = do
//   x <- choice [(oneOf ['x']) *> (pure $ true),(oneOf ['o']) *> (pure $ false)]
//   pure $ Onset x
// -- parseRepetition should be able to parse any other rhythm parser, in theory on should be: on <- parsePattern but it just does not work...
// parseRepetition:: P Rhythmic
// parseRepetition = do
//   on <- parseOnset
//   _ <- string "!"
//   n <- integer 
//   pure $ Repetition on n
// -- data Euclidean = Full Rhythmic Rhythmic | K Rhythmic | InverseK Rhythmic
// -- same here
// parseEuclidean:: P Rhythmic
// parseEuclidean = do
//     x <- choice [try $ parens parseFull, try $ parens parseK, try $ parens parseInv]
//     pure x
// parseFull:: P Rhythmic
// parseFull = do
//     kPatt <- parseOnset
//     _ <- comma
//     invPatt <- parseOnset
//     _ <- comma
//     k <- integer
//     _ <- comma
//     n <- integer
//     _ <- optional comma
//     o <- integer <|> pure 0
//     pure $ Euclidean (Full kPatt invPatt) k n o
// parseK:: P Rhythmic
// parseK = do
//     kPatt <- parseOnset
//     _ <- comma
//     k <- integer
//     _ <- comma
//     n <- integer
//     _ <- optional comma
//     o <- integer <|> pure 0
//     pure $ Euclidean (K kPatt) k n o
// parseInv:: P Rhythmic
// parseInv = do
//     _ <- string "inv"
//     whitespace
//     invPatt <- parseOnset 
//     _ <- comma
//     k <- integer
//     _ <- comma
//     n <- integer
//     _ <- optional comma
//     o <- integer <|> pure 0
//     pure $ Euclidean (InverseK invPatt) k n o
// Event 
// rhythmicTo
// parseEuclidean:: P Rhythmic
// parseEuclidean = do
//   on <- parseOnset 
//   kno <- parens $ parseKNO
//   pure $ Euclidean on (get1 kno) (get2 kno) (get3 kno)
// parseKNO:: P (Tuple3 Int Int Int)
// parseKNO = do
//   k <- integer
//   _ <- comma
//   n <- integer
//   _ <- optional comma
//   o <- integer <|> pure 0
//   pure $ tuple3 k n o
var tokenParser = /* #__PURE__ */ Parsing_Token.makeTokenParser(Parsing_Language.haskellStyle);
var whitespace = /* #__PURE__ */ (function () {
    return tokenParser.whiteSpace;
})();
var semi = /* #__PURE__ */ (function () {
    return tokenParser.semi;
})();
var reserved = /* #__PURE__ */ (function () {
    return tokenParser.reserved;
})();
var parens = /* #__PURE__ */ (function () {
    return tokenParser.parens;
})();
var naturalOrFloat = /* #__PURE__ */ (function () {
    return tokenParser.naturalOrFloat;
})();
var integer = /* #__PURE__ */ (function () {
    return tokenParser.integer;
})();
var identifier = /* #__PURE__ */ (function () {
    return tokenParser.identifier;
})();
var $$float = /* #__PURE__ */ (function () {
    return tokenParser["float"];
})();
var comma = /* #__PURE__ */ (function () {
    return tokenParser.comma;
})();
var colon = /* #__PURE__ */ (function () {
    return tokenParser.colon;
})();
var brackets = /* #__PURE__ */ (function () {
    return tokenParser.brackets;
})();
var braces = /* #__PURE__ */ (function () {
    return tokenParser.braces;
})();
export {
    tokenParser,
    parens,
    braces,
    identifier,
    reserved,
    naturalOrFloat,
    $$float as float,
    whitespace,
    colon,
    brackets,
    comma,
    semi,
    integer
};
