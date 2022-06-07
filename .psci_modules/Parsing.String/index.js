// | Primitive parsers for working with an input stream of type `String`.
// |
// | All of these primitive parsers will consume their input when they succeed.
// |
// | All of these primitive parsers will consume no input when they
// | fail.
// |
// | The behavior of these primitive parsers is based on the behavior of the
// | `Data.String` module in the __strings__ package.
// | In most JavaScript runtime environments, the `String`
// | is little-endian [UTF-16](https://en.wikipedia.org/wiki/UTF-16).
// |
// | The primitive parsers which return `Char` will only succeed when the character
// | being parsed is a code point in the
// | [Basic Multilingual Plane](https://en.wikipedia.org/wiki/Plane_(Unicode)#Basic_Multilingual_Plane)
// | (the “BMP”). These parsers can be convenient because of the good support
// | that PureScript has for writing `Char` literals like `'あ'`, `'β'`, `'C'`.
// |
// | The other primitive parsers, which return `CodePoint` and `String` types,
// | can parse the full Unicode character set. All of the primitive parsers
// | in this module can be used together.
// |
// | ### Position
// |
// | In a `String` parser, the `Position {index}` counts the number of
// | unicode `CodePoint`s since the beginning of the input string.
// |
// | Each tab character (`0x09`) encountered in a `String` parser will advance
// | the `Position {column}` by 8.
// |
// | These patterns will advance the `Position {line}` by 1 and reset
// | the `Position {column}` to 1:
// | - newline (`0x0A`)
// | - carriage-return (`0x0D`)
// | - carriage-return-newline (`0x0D 0x0A`)
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Data_Array_NonEmpty from "../Data.Array.NonEmpty/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Function_Uncurried from "../Data.Function.Uncurried/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_String_CodePoints from "../Data.String.CodePoints/index.js";
import * as Data_String_CodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data_String_Common from "../Data.String.Common/index.js";
import * as Data_String_Regex from "../Data.String.Regex/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";

// | Updates a `Position` by adding the columns and lines in a
// | single `CodePoint`.
var updatePosSingle = function (v) {
    return function (cp) {
        return function (after) {
            var v1 = Data_Enum.fromEnum(Data_String_CodePoints.boundedEnumCodePoint)(cp);
            if (v1 === 10) {
                return {
                    index: v.index + 1 | 0,
                    line: v.line + 1 | 0,
                    column: 1
                };
            };
            if (v1 === 13) {
                var v2 = Data_String_CodePoints.codePointAt(0)(after);
                if (v2 instanceof Data_Maybe.Just && Data_Enum.fromEnum(Data_String_CodePoints.boundedEnumCodePoint)(v2.value0) === 10) {
                    return {
                        index: v.index + 1 | 0,
                        line: v.line,
                        column: v.column
                    };
                };
                return {
                    index: v.index + 1 | 0,
                    line: v.line + 1 | 0,
                    column: 1
                };
            };
            if (v1 === 9) {
                return {
                    index: v.index + 1 | 0,
                    line: v.line,
                    column: (v.column + 8 | 0) - Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(v.column - 1 | 0)(8) | 0
                };
            };
            return {
                index: v.index + 1 | 0,
                line: v.line,
                column: v.column + 1 | 0
            };
        };
    };
};

// | Updates a `Position` by adding the columns and lines in `String`.
var updatePosString = function ($copy_pos) {
    return function ($copy_before) {
        return function ($copy_after) {
            var $tco_var_pos = $copy_pos;
            var $tco_var_before = $copy_before;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(pos, before, after) {
                var v = Data_String_CodePoints.uncons(before);
                if (v instanceof Data_Maybe.Nothing) {
                    $tco_done = true;
                    return pos;
                };
                if (v instanceof Data_Maybe.Just) {
                    var newPos = (function () {
                        if (Data_String_Common["null"](v.value0.tail)) {
                            return updatePosSingle(pos)(v.value0.head)(after);
                        };
                        if (Data_Boolean.otherwise) {
                            return updatePosSingle(pos)(v.value0.head)(v.value0.tail);
                        };
                        throw new Error("Failed pattern match at Parsing.String (line 160, column 7 - line 162, column 52): " + [  ]);
                    })();
                    $tco_var_pos = newPos;
                    $tco_var_before = v.value0.tail;
                    $copy_after = after;
                    return;
                };
                throw new Error("Failed pattern match at Parsing.String (line 156, column 36 - line 163, column 38): " + [ v.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_pos, $tco_var_before, $copy_after);
            };
            return $tco_result;
        };
    };
};

// | Match a Unicode character satisfying the predicate.
var satisfyCodePoint = function (f) {
    return Data_Function_Uncurried.mkFn5(function (v) {
        return function (v1) {
            return function (v2) {
                return function ($$throw) {
                    return function (done) {
                        var v3 = Data_String_CodePoints.uncons(v.value0);
                        if (v3 instanceof Data_Maybe.Nothing) {
                            return $$throw(v, new Parsing.ParseError("Unexpected EOF", v.value1));
                        };
                        if (v3 instanceof Data_Maybe.Just) {
                            var $44 = f(v3.value0.head);
                            if ($44) {
                                return done(new Parsing.ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), v3.value0.head);
                            };
                            return $$throw(v, new Parsing.ParseError("Predicate unsatisfied", v.value1));
                        };
                        throw new Error("Failed pattern match at Parsing.String (line 131, column 7 - line 138, column 73): " + [ v3.constructor.name ]);
                    };
                };
            };
        };
    });
};

// | Match a BMP `Char` satisfying the predicate.
var satisfy = function (f) {
    return Data_Function_Uncurried.mkFn5(function (v) {
        return function (v1) {
            return function (v2) {
                return function ($$throw) {
                    return function (done) {
                        var v3 = Data_String_CodePoints.uncons(v.value0);
                        if (v3 instanceof Data_Maybe.Nothing) {
                            return $$throw(v, new Parsing.ParseError("Unexpected EOF", v.value1));
                        };
                        if (v3 instanceof Data_Maybe.Just) {
                            var cp = Data_Enum.fromEnum(Data_String_CodePoints.boundedEnumCodePoint)(v3.value0.head);
                            var $53 = cp < 0 || cp > 65535;
                            if ($53) {
                                return $$throw(v, new Parsing.ParseError("Expected Char", v.value1));
                            };
                            var ch = Data_Maybe.fromJust()(Data_Enum.toEnum(Data_Enum.boundedEnumChar)(cp));
                            var $54 = f(ch);
                            if ($54) {
                                return done(new Parsing.ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), ch);
                            };
                            return $$throw(v, new Parsing.ParseError("Predicate unsatisfied", v.value1));
                        };
                        throw new Error("Failed pattern match at Parsing.String (line 109, column 7 - line 124, column 75): " + [ v3.constructor.name ]);
                    };
                };
            };
        };
    });
};

// | Combinator which returns both the result of a parse and the slice of
// | the input that was consumed while it was being parsed.
// |
// | Because `String`s are not `Char` arrays in PureScript, `many` and `some`
// | on `Char` parsers need to
// | be used with `Data.String.CodeUnits.fromCharArray` to
// | construct a `String`.
// |
// | ```
// | fromCharArray <$> Data.Array.many (char 'x')
// | ```
// |
// | It’s more efficient to achieve the same result by using this `match` combinator
// | instead of `fromCharArray`.
// |
// | ```
// | fst <$> match (Combinators.skipMany (char 'x'))
// | ```
var match = function (p) {
    return Control_Bind.bind(Parsing.bindParserT)(Parsing.getParserT)(function (v) {
        return Control_Bind.bind(Parsing.bindParserT)(p)(function (x) {
            return Control_Bind.bind(Parsing.bindParserT)(Parsing.getParserT)(function (v1) {
                return Control_Applicative.pure(Parsing.applicativeParserT)(new Data_Tuple.Tuple(Data_String_CodeUnits.take(Data_String_CodeUnits.length(v.value0) - Data_String_CodeUnits.length(v1.value0) | 0)(v.value0), x));
            });
        });
    });
};

// | Match “end-of-file,” the end of the input stream.
var eof = /* #__PURE__ */ Data_Function_Uncurried.mkFn5(function (v) {
    return function (v1) {
        return function (v2) {
            return function ($$throw) {
                return function (done) {
                    var $70 = Data_String_Common["null"](v.value0);
                    if ($70) {
                        return done(new Parsing.ParseState(v.value0, v.value1, true), Data_Unit.unit);
                    };
                    return $$throw(v, new Parsing.ParseError("Expected EOF", v.value1));
                };
            };
        };
    };
});

// | Consume a portion of the input string while yielding a value.
// |
// | Takes a consumption function which takes the remaining input `String`
// | as its argument and returns either an error message, or three fields:
// |
// | * `value` is the value to return.
// | * `consumed` is the input `String` that was consumed. It is used to update the parser position.
// | * `remainder` is the new remaining input `String`.
// |
// | This function is used internally to construct primitive `String` parsers.
var consumeWith = function (f) {
    return Data_Function_Uncurried.mkFn5(function (v) {
        return function (v1) {
            return function (v2) {
                return function ($$throw) {
                    return function (done) {
                        var v3 = f(v.value0);
                        if (v3 instanceof Data_Either.Left) {
                            return $$throw(v, new Parsing.ParseError(v3.value0, v.value1));
                        };
                        if (v3 instanceof Data_Either.Right) {
                            return done(new Parsing.ParseState(v3.value0.remainder, updatePosString(v.value1)(v3.value0.consumed)(v3.value0.remainder), true), v3.value0.value);
                        };
                        throw new Error("Failed pattern match at Parsing.String (line 280, column 7 - line 284, column 97): " + [ v3.constructor.name ]);
                    };
                };
            };
        };
    });
};

// | Compile a regular expression string into a regular expression parser.
// |
// | This function will use the `Data.String.Regex.regex` function to compile and return a parser which can be used
// | in a `ParserT String m` monad.
// |
// | This parser will try to match the regular expression pattern starting
// | at the current parser position. On success, it will return the matched
// | substring.
// |
// | [*MDN Regular Expressions Cheatsheet*](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions/Cheatsheet)
// |
// | This function should be called outside the context of a `ParserT String m` monad, because this function might
// | fail with a `Left` RegExp compilation error message.
// | If you call this function inside of the `ParserT String m` monad and then `fail` the parse when the compilation fails,
// | then that could be confusing because a parser failure is supposed to indicate an invalid input string.
// | If the compilation failure occurs in an `alt` then the compilation failure might not be reported at all and instead
// | the input string would be parsed incorrectly.
// |
// | This parser may be useful for quickly consuming a large section of the
// | input `String`, because in a JavaScript runtime environment the RegExp
// | runtime is a lot faster than primitive parsers.
// |
// | #### Example
// |
// | This example shows how to compile and run the `xMany` parser which will
// | capture the regular expression pattern `x*`.
// |
// | ```purescript
// | case regex "x*" noFlags of
// |   Left compileError -> unsafeCrashWith $ "xMany failed to compile: " <> compileError
// |   Right xMany -> runParser "xxxZ" do
// |     xMany
// | ```
// |
// | #### Flags
// |
// | Set `RegexFlags` with the `Semigroup` instance like this.
// |
// | ```purescript
// | regex "x*" (dotAll <> ignoreCase)
// | ```
// |
// | The `dotAll`, `unicode`, and `ignoreCase` flags might make sense for a `regex` parser. The other flags will
// | probably cause surprising behavior and you should avoid them.
// |
// | [*MDN Advanced searching with flags*](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#advanced_searching_with_flags)
var regex = function (pattern) {
    return function (flags) {
        return Data_Functor.mapFlipped(Data_Either.functorEither)(Data_String_Regex.regex("^(" + (pattern + ")"))(flags))(function (regexobj) {
            return consumeWith(function (input) {
                var v = Data_Functor.map(Data_Maybe.functorMaybe)(Data_Array_NonEmpty.head)(Data_String_Regex.match(regexobj)(input));
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Data_Maybe.Just) {
                    var remainder = Data_String_CodeUnits.drop(Data_String_CodeUnits.length(v.value0.value0))(input);
                    return new Data_Either.Right({
                        value: v.value0.value0,
                        consumed: v.value0.value0,
                        remainder: remainder
                    });
                };
                return new Data_Either.Left("No Regex pattern match");
            });
        });
    };
};

// | Match the entire rest of the input stream. Always succeeds.
var rest = /* #__PURE__ */ consumeWith(function (consumed) {
    return new Data_Either.Right({
        value: consumed,
        consumed: consumed,
        remainder: ""
    });
});

// | Match the specified string.
var string = function (str) {
    return consumeWith(function (input) {
        var v = Data_String_CodeUnits.stripPrefix(str)(input);
        if (v instanceof Data_Maybe.Just) {
            return new Data_Either.Right({
                value: str,
                consumed: str,
                remainder: v.value0
            });
        };
        return new Data_Either.Left("Expected " + Data_Show.show(Data_Show.showString)(str));
    });
};

// | Match a `String` exactly *N* characters long.
var takeN = function (n) {
    return consumeWith(function (input) {
        var v = Data_String_CodePoints.splitAt(n)(input);
        var $90 = Data_String_CodePoints.length(v.before) === n;
        if ($90) {
            return new Data_Either.Right({
                value: v.before,
                consumed: v.before,
                remainder: v.after
            });
        };
        return new Data_Either.Left("Could not take " + (Data_Show.show(Data_Show.showInt)(n) + " characters"));
    });
};

// | Match the specified BMP `Char`.
var $$char = function (c) {
    return Parsing_Combinators.withErrorMessage(satisfy(function (v) {
        return v === c;
    }))(Data_Show.show(Data_Show.showChar)(c));
};

// | Match any Unicode character.
// | Always succeeds when any input remains.
var anyCodePoint = /* #__PURE__ */ satisfyCodePoint(/* #__PURE__ */ Data_Function["const"](true));

// | Combinator which finds the first position in the input `String` where the
// | phrase can parse. Returns both the
// | parsed result and the unparsable input section searched before the parse.
// | Will fail if no section of the input is parseable. To backtrack the input
// | stream on failure, combine with `tryRethrow`.
// |
// | This combinator works like
// | [Data.String.takeWhile](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String#v:takeWhile)
// | or
// | [Data.String.Regex.search](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex#v:search)
// | and it allows using a parser for the pattern search.
// |
// | This combinator is equivalent to `manyTill_ anyCodePoint`, but it will be
// | faster because it returns a slice of the input `String` for the
// | section preceding the parse instead of a `List CodePoint`.
// |
// | Be careful not to look too far
// | ahead; if the phrase parser looks to the end of the input then `anyTill`
// | could be *O(n²)*.
var anyTill = function (dictMonad) {
    return function (p) {
        var go = function (unit) {
            return Control_Alt.alt(Parsing.altParserT)(Control_Bind.bind(Parsing.bindParserT)(Parsing.getParserT)(function (v) {
                return Control_Bind.bind(Parsing.bindParserT)(Parsing_Combinators["try"](p))(function (t) {
                    return Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(v.value0, t)));
                });
            }))(Control_Bind.bind(Parsing.bindParserT)(anyCodePoint)(function () {
                return Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Loop(unit));
            }));
        };
        return Control_Bind.bind(Parsing.bindParserT)(Parsing.getParserT)(function (v) {
            return Control_Bind.bind(Parsing.bindParserT)(Control_Monad_Rec_Class.tailRecM(Parsing.monadRecParserT)(go)(Data_Unit.unit))(function (v1) {
                return Control_Applicative.pure(Parsing.applicativeParserT)(new Data_Tuple.Tuple(Data_String_CodeUnits.take(Data_String_CodeUnits.length(v.value0) - Data_String_CodeUnits.length(v1.value0) | 0)(v.value0), v1.value1));
            });
        });
    };
};

// | Match any BMP `Char`.
// | Parser will fail if the character is not in the Basic Multilingual Plane.
var anyChar = /* #__PURE__ */ satisfy(/* #__PURE__ */ Data_Function["const"](true));
export {
    $$char as char,
    string,
    anyChar,
    anyCodePoint,
    satisfy,
    satisfyCodePoint,
    takeN,
    rest,
    eof,
    match,
    regex,
    anyTill,
    consumeWith
};
