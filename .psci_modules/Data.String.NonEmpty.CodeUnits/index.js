import * as Data_Array_NonEmpty from "../Data.Array.NonEmpty/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Semigroup_Foldable from "../Data.Semigroup.Foldable/index.js";
import * as Data_String_CodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data_String_NonEmpty_Internal from "../Data.String.NonEmpty.Internal/index.js";
import * as Data_String_Unsafe from "../Data.String.Unsafe/index.js";

// For internal use only. Do not export.
var toNonEmptyString = Data_String_NonEmpty_Internal.NonEmptyString;

// | Creates a `NonEmptyString` from a string by appending a character.
// |
// | ```purescript
// | snoc 'c' "ab" = NonEmptyString "abc"
// | snoc 'a' "" = NonEmptyString "a"
// | ```
var snoc = function (c) {
    return function (s) {
        return toNonEmptyString(s + Data_String_CodeUnits.singleton(c));
    };
};

// | Creates a `NonEmptyString` from a character.
var singleton = function ($16) {
    return toNonEmptyString(Data_String_CodeUnits.singleton($16));
};

// For internal use only. Do not export.
var liftS = function (f) {
    return function (v) {
        return f(v);
    };
};

// | Returns the longest prefix of characters that satisfy the predicate.
// | `Nothing` is returned if there is no matching prefix.
// |
// | ```purescript
// | takeWhile (_ /= ':') (NonEmptyString "http://purescript.org") == Just (NonEmptyString "http")
// | takeWhile (_ == 'a') (NonEmptyString "xyz") == Nothing
// | ```
var takeWhile = function (f) {
    var $17 = liftS(Data_String_CodeUnits.takeWhile(f));
    return function ($18) {
        return Data_String_NonEmpty_Internal.fromString($17($18));
    };
};

// | Returns the index of the last occurrence of the pattern in the
// | given string, starting at the specified index and searching
// | backwards towards the beginning of the string.
// |
// | Starting at a negative index is equivalent to starting at 0 and
// | starting at an index greater than the string length is equivalent
// | to searching in the whole string.
// |
// | Returns `Nothing` if there is no match.
// |
// | ```purescript
// | lastIndexOf' (Pattern "a") (-1) (NonEmptyString "ababa") == Just 0
// | lastIndexOf' (Pattern "a") 1 (NonEmptyString "ababa") == Just 0
// | lastIndexOf' (Pattern "a") 3 (NonEmptyString "ababa") == Just 2
// | lastIndexOf' (Pattern "a") 4 (NonEmptyString "ababa") == Just 4
// | lastIndexOf' (Pattern "a") 5 (NonEmptyString "ababa") == Just 4
// | ```
var lastIndexOf$prime = function (pat) {
    var $19 = Data_String_CodeUnits["lastIndexOf$prime"](pat);
    return function ($20) {
        return liftS($19($20));
    };
};

// | Returns the index of the last occurrence of the pattern in the
// | given string. Returns `Nothing` if there is no match.
// |
// | ```purescript
// | lastIndexOf (Pattern "c") (NonEmptyString "abcdc") == Just 4
// | lastIndexOf (Pattern "c") (NonEmptyString "aaa") == Nothing
// | ```
var lastIndexOf = function ($21) {
    return liftS(Data_String_CodeUnits.lastIndexOf($21));
};

// | Returns the index of the first occurrence of the pattern in the
// | given string, starting at the specified index. Returns `Nothing` if there is
// | no match.
// |
// | ```purescript
// | indexOf' (Pattern "a") 2 (NonEmptyString "ababa") == Just 2
// | indexOf' (Pattern "a") 3 (NonEmptyString "ababa") == Just 4
// | ```
var indexOf$prime = function (pat) {
    var $22 = Data_String_CodeUnits["indexOf$prime"](pat);
    return function ($23) {
        return liftS($22($23));
    };
};

// | Returns the index of the first occurrence of the pattern in the
// | given string. Returns `Nothing` if there is no match.
// |
// | ```purescript
// | indexOf (Pattern "c") (NonEmptyString "abcdc") == Just 2
// | indexOf (Pattern "c") (NonEmptyString "aaa") == Nothing
// | ```
var indexOf = function ($24) {
    return liftS(Data_String_CodeUnits.indexOf($24));
};

// For internal use only. Do not export.
var fromNonEmptyString = function (v) {
    return v;
};

// | Returns the number of characters the string is composed of.
// |
// | ```purescript
// | length (NonEmptyString "Hello World") == 11
// | ```
var length = function ($25) {
    return Data_String_CodeUnits.length(fromNonEmptyString($25));
};

// | Returns the substrings of a split at the given index, if the index is
// | within bounds.
// |
// | ```purescript
// | splitAt 2 (NonEmptyString "Hello World") == Just { before: Just (NonEmptyString "He"), after: Just (NonEmptyString "llo World") }
// | splitAt 10 (NonEmptyString "Hi") == Nothing
// | ```
var splitAt = function (i) {
    return function (nes) {
        var v = Data_String_CodeUnits.splitAt(i)(fromNonEmptyString(nes));
        return {
            before: Data_String_NonEmpty_Internal.fromString(v.before),
            after: Data_String_NonEmpty_Internal.fromString(v.after)
        };
    };
};

// | Returns the first `n` characters of the string. Returns `Nothing` if `n` is
// | less than 1.
// |
// | ```purescript
// | take 5 (NonEmptyString "Hello World") == Just (NonEmptyString "Hello")
// | take 0 (NonEmptyString "Hello World") == Nothing
// | ```
var take = function (i) {
    return function (nes) {
        var s = fromNonEmptyString(nes);
        var $11 = i < 1;
        if ($11) {
            return Data_Maybe.Nothing.value;
        };
        return new Data_Maybe.Just(toNonEmptyString(Data_String_CodeUnits.take(i)(s)));
    };
};

// | Returns the last `n` characters of the string. Returns `Nothing` if `n` is
// | less than 1.
// |
// | ```purescript
// | take 5 (NonEmptyString "Hello World") == Just (NonEmptyString "World")
// | take 0 (NonEmptyString "Hello World") == Nothing
// | ```
var takeRight = function (i) {
    return function (nes) {
        var s = fromNonEmptyString(nes);
        var $12 = i < 1;
        if ($12) {
            return Data_Maybe.Nothing.value;
        };
        return new Data_Maybe.Just(toNonEmptyString(Data_String_CodeUnits.takeRight(i)(s)));
    };
};

// | Converts the `NonEmptyString` to a character, if the length of the string
// | is exactly `1`.
// |
// | ```purescript
// | toChar "H" == Just 'H'
// | toChar "Hi" == Nothing
// | ```
var toChar = function ($26) {
    return Data_String_CodeUnits.toChar(fromNonEmptyString($26));
};

// | Converts the `NonEmptyString` into an array of characters.
// |
// | ```purescript
// | toCharArray (NonEmptyString "Hello☺\n") == ['H','e','l','l','o','☺','\n']
// | ```
var toCharArray = function ($27) {
    return Data_String_CodeUnits.toCharArray(fromNonEmptyString($27));
};

// | Converts the `NonEmptyString` into a non-empty array of characters.
var toNonEmptyCharArray = /* #__PURE__ */ (function () {
    var $28 = Data_Maybe.fromJust();
    return function ($29) {
        return $28(Data_Array_NonEmpty.fromArray(toCharArray($29)));
    };
})();

// | Returns the first character and the rest of the string.
// |
// | ```purescript
// | uncons "a" == { head: 'a', tail: Nothing }
// | uncons "Hello World" == { head: 'H', tail: Just (NonEmptyString "ello World") }
// | ```
var uncons = function (nes) {
    var s = fromNonEmptyString(nes);
    return {
        head: Data_String_Unsafe.charAt(0)(s),
        tail: Data_String_NonEmpty_Internal.fromString(Data_String_CodeUnits.drop(1)(s))
    };
};

// | Creates a `NonEmptyString` from a `Foldable1` container carrying
// | characters.
var fromFoldable1 = function (dictFoldable1) {
    var $30 = Data_Semigroup_Foldable.fold1(dictFoldable1)(Data_String_NonEmpty_Internal.semigroupNonEmptyString);
    return function ($31) {
        return $30($31);
    };
};

// | Creates a `NonEmptyString` from a character array `String`, returning
// | `Nothing` if the input is empty.
// |
// | ```purescript
// | fromCharArray [] = Nothing
// | fromCharArray ['a', 'b', 'c'] = Just (NonEmptyString "abc")
// | ```
var fromCharArray = function (v) {
    if (v.length === 0) {
        return Data_Maybe.Nothing.value;
    };
    return new Data_Maybe.Just(toNonEmptyString(Data_String_CodeUnits.fromCharArray(v)));
};
var fromNonEmptyCharArray = /* #__PURE__ */ (function () {
    var $32 = Data_Maybe.fromJust();
    return function ($33) {
        return $32(fromCharArray(Data_Array_NonEmpty.toArray($33)));
    };
})();

// | Returns the suffix remaining after `takeWhile`.
// |
// | ```purescript
// | dropWhile (_ /= '.') (NonEmptyString "Test.purs") == Just (NonEmptyString ".purs")
// | ```
var dropWhile = function (f) {
    var $34 = liftS(Data_String_CodeUnits.dropWhile(f));
    return function ($35) {
        return Data_String_NonEmpty_Internal.fromString($34($35));
    };
};

// | Returns the string without the last `n` characters. Returns `Nothing` if
// | more characters are dropped than the string is long.
// |
// | ```purescript
// | dropRight 6 (NonEmptyString "Hello World") == Just (NonEmptyString "Hello")
// | dropRight 20 (NonEmptyString "Hello World") == Nothing
// | ```
var dropRight = function (i) {
    return function (nes) {
        var s = fromNonEmptyString(nes);
        var $14 = i >= Data_String_CodeUnits.length(s);
        if ($14) {
            return Data_Maybe.Nothing.value;
        };
        return new Data_Maybe.Just(toNonEmptyString(Data_String_CodeUnits.dropRight(i)(s)));
    };
};

// | Returns the string without the first `n` characters. Returns `Nothing` if
// | more characters are dropped than the string is long.
// |
// | ```purescript
// | drop 6 (NonEmptyString "Hello World") == Just (NonEmptyString "World")
// | drop 20 (NonEmptyString "Hello World") == Nothing
// | ```
var drop = function (i) {
    return function (nes) {
        var s = fromNonEmptyString(nes);
        var $15 = i >= Data_String_CodeUnits.length(s);
        if ($15) {
            return Data_Maybe.Nothing.value;
        };
        return new Data_Maybe.Just(toNonEmptyString(Data_String_CodeUnits.drop(i)(s)));
    };
};

// | Returns the number of contiguous characters at the beginning of the string
// | for which the predicate holds.
// |
// | ```purescript
// | countPrefix (_ /= 'o') (NonEmptyString "Hello World") == 4
// | ```
var countPrefix = function ($36) {
    return liftS(Data_String_CodeUnits.countPrefix($36));
};

// | Creates a `NonEmptyString` from a string by prepending a character.
// |
// | ```purescript
// | cons 'a' "bc" = NonEmptyString "abc"
// | cons 'a' "" = NonEmptyString "a"
// | ```
var cons = function (c) {
    return function (s) {
        return toNonEmptyString(Data_String_CodeUnits.singleton(c) + s);
    };
};

// | Returns the character at the given index, if the index is within bounds.
// |
// | ```purescript
// | charAt 2 (NonEmptyString "Hello") == Just 'l'
// | charAt 10 (NonEmptyString "Hello") == Nothing
// | ```
var charAt = function ($37) {
    return liftS(Data_String_CodeUnits.charAt($37));
};
export {
    fromCharArray,
    fromNonEmptyCharArray,
    singleton,
    cons,
    snoc,
    fromFoldable1,
    toCharArray,
    toNonEmptyCharArray,
    charAt,
    toChar,
    indexOf,
    indexOf$prime,
    lastIndexOf,
    lastIndexOf$prime,
    uncons,
    length,
    take,
    takeRight,
    takeWhile,
    drop,
    dropRight,
    dropWhile,
    countPrefix,
    splitAt
};
