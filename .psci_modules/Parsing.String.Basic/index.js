// | Basic `String` parsers derived from primitive `String` parsers.
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_CodePoint_Unicode from "../Data.CodePoint.Unicode/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Number from "../Data.Number/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_String_CodePoints from "../Data.String.CodePoints/index.js";
import * as Data_String_CodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data_String_Regex_Flags from "../Data.String.Regex.Flags/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as Partial_Unsafe from "../Partial.Unsafe/index.js";

// | Skip whitespace characters and throw them away. Always succeeds.
var skipSpaces = /* #__PURE__ */ Parsing_String.consumeWith(function (input) {
    var consumed = Data_String_CodePoints.takeWhile(Data_CodePoint_Unicode.isSpace)(input);
    var remainder = Data_String_CodeUnits.drop(Data_String_CodeUnits.length(consumed))(input);
    return new Data_Either.Right({
        value: Data_Unit.unit,
        consumed: consumed,
        remainder: remainder
    });
});

// | Match zero or more whitespace characters satisfying
// | `Data.CodePoint.Unicode.isSpace`. Always succeeds.
var whiteSpace = /* #__PURE__ */ Data_Functor.map(Parsing.functorParserT)(Data_Tuple.fst)(/* #__PURE__ */ Parsing_String.match(skipSpaces));

// | Helper function
var satisfyCP = function (p) {
    return Parsing_String.satisfy(function ($10) {
        return p(Data_String_CodePoints.codePointFromChar($10));
    });
};

// | Parse a space character.  Matches any char that satisfies `Data.CodePoint.Unicode.isSpace`.
var space = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isSpace))("space");

// | Parse an uppercase letter.  Matches any char that satisfies `Data.CodePoint.Unicode.isUpper`.
var upper = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isUpper))("uppercase letter");

// | Match one of the Unicode characters in the array.
var oneOfCodePoints = function (ss) {
    return Parsing_Combinators.withLazyErrorMessage(Parsing_String.satisfyCodePoint(Data_Function.flip(Data_Array.elem(Data_String_CodePoints.eqCodePoint))(ss)))(function (v) {
        return "one of " + Data_Show.show(Data_Show.showArray(Data_Show.showString))(Data_Functor.map(Data_Functor.functorArray)(Data_String_CodePoints.singleton)(ss));
    });
};

// | Match one of the BMP `Char`s in the array.
var oneOf = function (ss) {
    return Parsing_Combinators.withLazyErrorMessage(Parsing_String.satisfy(Data_Function.flip(Data_Array.elem(Data_Eq.eqChar))(ss)))(function (v) {
        return "one of " + Data_Show.show(Data_Show.showArray(Data_Show.showChar))(ss);
    });
};

// | Parse an octal digit.  Matches any char that satisfies `Data.CodePoint.Unicode.isOctDigit`.
var octDigit = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isOctDigit))("oct digit");
var numberRegex = /* #__PURE__ */ Data_Either.either(Partial_Unsafe.unsafeCrashWith)(/* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn))(/* #__PURE__ */ Parsing_String.regex("[+-]?[0-9]*(\\.[0-9]*)?([eE][+-]?[0-9]*(\\.[0-9]*))?")(/* #__PURE__ */ Data_Monoid.mempty(Data_String_Regex_Flags.monoidRegexFlags)));

// | Parser based on the __Data.Number.fromString__ function.
// |
// | This should be the inverse of `show :: String -> Number`.
// |
// | Examples of strings which can be parsed by this parser:
// | * `"3"`
// | * `"3.0"`
// | * `"0.3"`
// | * `"-0.3"`
// | * `"+0.3"`
// | * `"-3e-1"`
// | * `"-3.0E-1.0"`
// | * `"NaN"`
// | * `"-Infinity"`
var number = /* #__PURE__ */ (function () {
    return Control_Alt.alt(Parsing.altParserT)(Parsing_Combinators.choice(Data_Foldable.foldableArray)([ Control_Apply.applySecond(Parsing.applyParserT)(Parsing_String.string("Infinity"))(Control_Applicative.pure(Parsing.applicativeParserT)(Data_Number.infinity)), Control_Apply.applySecond(Parsing.applyParserT)(Parsing_String.string("+Infinity"))(Control_Applicative.pure(Parsing.applicativeParserT)(Data_Number.infinity)), Control_Apply.applySecond(Parsing.applyParserT)(Parsing_String.string("-Infinity"))(Control_Applicative.pure(Parsing.applicativeParserT)(-Data_Number.infinity)), Control_Apply.applySecond(Parsing.applyParserT)(Parsing_String.string("NaN"))(Control_Applicative.pure(Parsing.applicativeParserT)(Data_Number.nan)), Parsing_Combinators.tryRethrow(Control_Bind.bind(Parsing.bindParserT)(numberRegex)(function (section) {
        var v = Data_Number.fromString(section);
        if (v instanceof Data_Maybe.Nothing) {
            return Parsing.fail("Number.fromString failed");
        };
        if (v instanceof Data_Maybe.Just) {
            return Control_Applicative.pure(Parsing.applicativeParserT)(v.value0);
        };
        throw new Error("Failed pattern match at Parsing.String.Basic (line 96, column 9 - line 101, column 27): " + [ v.constructor.name ]);
    })) ]))(Parsing.fail("Expected Number"));
})();

// | Match any Unicode character not in the array.
var noneOfCodePoints = function (ss) {
    return Parsing_Combinators.withLazyErrorMessage(Parsing_String.satisfyCodePoint(Data_Function.flip(Data_Array.notElem(Data_String_CodePoints.eqCodePoint))(ss)))(function (v) {
        return "none of " + Data_Show.show(Data_Show.showArray(Data_Show.showString))(Data_Functor.map(Data_Functor.functorArray)(Data_String_CodePoints.singleton)(ss));
    });
};

// | Match any BMP `Char` not in the array.
var noneOf = function (ss) {
    return Parsing_Combinators.withLazyErrorMessage(Parsing_String.satisfy(Data_Function.flip(Data_Array.notElem(Data_Eq.eqChar))(ss)))(function (v) {
        return "none of " + Data_Show.show(Data_Show.showArray(Data_Show.showChar))(ss);
    });
};

// | Parse a lowercase letter.  Matches any char that satisfies `Data.CodePoint.Unicode.isLower`.
var lower = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isLower))("lowercase letter");

// | Parse an alphabetical character.  Matches any char that satisfies `Data.CodePoint.Unicode.isAlpha`.
var letter = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isAlpha))("letter");
var intDecimalRegex = /* #__PURE__ */ Data_Either.either(Partial_Unsafe.unsafeCrashWith)(/* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn))(/* #__PURE__ */ Parsing_String.regex("[+-]?[0-9]*")(/* #__PURE__ */ Data_Monoid.mempty(Data_String_Regex_Flags.monoidRegexFlags)));

// | Parser based on the __Data.Int.fromString__ function.
// |
// | This should be the inverse of `show :: String -> Int`.
// |
// | Examples of strings which can be parsed by this parser:
// | * `"3"`
// | * `"-3"`
// | * `"+300"`
var intDecimal = /* #__PURE__ */ Parsing_Combinators.tryRethrow(/* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Control_Alt.alt(Parsing.altParserT)(intDecimalRegex)(/* #__PURE__ */ Parsing.fail("Expected Int")))(function (section) {
    var v = Data_Int.fromString(section);
    if (v instanceof Data_Maybe.Nothing) {
        return Parsing.fail("Int.fromString failed");
    };
    if (v instanceof Data_Maybe.Just) {
        return Control_Applicative.pure(Parsing.applicativeParserT)(v.value0);
    };
    throw new Error("Failed pattern match at Parsing.String.Basic (line 120, column 3 - line 122, column 21): " + [ v.constructor.name ]);
}));

// | Parse a hex digit.  Matches any char that satisfies `Data.CodePoint.Unicode.isHexDigit`.
var hexDigit = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isHexDigit))("hex digit");

// | Parse a digit.  Matches any char that satisfies `Data.CodePoint.Unicode.isDecDigit`.
var digit = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isDecDigit))("digit");

// | Parse an alphabetical or numerical character.
// | Matches any char that satisfies `Data.CodePoint.Unicode.isAlphaNum`.
var alphaNum = /* #__PURE__ */ Parsing_Combinators.withErrorMessage(/* #__PURE__ */ satisfyCP(Data_CodePoint_Unicode.isAlphaNum))("letter or digit");
export {
    digit,
    hexDigit,
    octDigit,
    letter,
    space,
    lower,
    upper,
    alphaNum,
    intDecimal,
    number,
    whiteSpace,
    skipSpaces,
    oneOf,
    oneOfCodePoints,
    noneOf,
    noneOfCodePoints
};
