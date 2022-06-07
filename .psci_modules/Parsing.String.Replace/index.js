// | This module is for finding patterns in a `String`, and also
// | replacing or splitting on the found patterns.
// | This activity is traditionally done with
// | [__Regex__](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex),
// | but this module uses parsers instead for the pattern matching.
// |
// | Functions in this module are *ways to run a parser* on an input `String`,
// | like `runParser` or `runParserT`.
// |
// | #### Why would we want to do pattern matching and substitution with parsers instead of regular expressions?
// |
// | * Monadic parsers have a nicer syntax than
// |   [regular expressions](https://en.wikipedia.org/wiki/Regular_expression),
// |   which are notoriously
// |   [difficult to read](https://en.wikipedia.org/wiki/Write-only_language).
// |   With monadic parsers we can perform textual pattern-matching in plain
// |   PureScript rather than using a special regex domain-specific
// |   programming language.
// |
// | * Regular expressions can do “group capture” on sections of the matched
// |   pattern, but they can only return stringy lists of the capture groups. Parsers
// |   can construct typed data structures based on the capture groups, guaranteeing
// |   no disagreement between the pattern rules and the rules that we're using
// |   to build data structures based on the pattern matches.
// |
// |   For example, consider
// |   scanning a string for numbers. A lot of different things can look like a number,
// |   and can have leading plus or minus signs, or be in scientific notation, or
// |   have commas, or whatever. If we try to parse all of the numbers out of a string
// |   using regular expressions, then we have to make sure that the regular expression
// |   and the string-to-number conversion function agree about exactly what is
// |   and what isn't a numeric string. We can get into an awkward situation in which
// |   the regular expression says it has found a numeric string but the
// |   string-to-number conversion function fails. A typed parser will perform both
// |   the pattern match and the conversion, so it will never be in that situation.
// |   [Parse, don't validate.](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
// |
// | * Regular expressions are only able to pattern-match
// |   [regular grammars](https://en.wikipedia.org/wiki/Chomsky_hierarchy#The_hierarchy).
// |   Monadic parsers are able pattern-match context-free (by recursion)
// |   or context-sensitive (by monad transformer) grammars.
// |
// | * The replacement expression for a traditional regular expression-based
// |   substitution command is usually just a string template in which
// |   the *Nth* “capture group” can be inserted with the syntax `\N`. With
// |   this library, instead of a template, we get
// |   an `editor` function which can perform any computation, including `Effect`s.
// |
// | #### Implementation Notes
// |
// | All of the functions in this module work by calling `runParserT`
// | with the `anyTill` combinator.
// | We can expect the speed of parser-based pattern matching to be
// | about 10× worse than regex-based pattern matching in a JavaScript
// | runtime environment.
// | This module is based on the Haskell packages
// | [__replace-megaparsec__](https://hackage.haskell.org/package/replace-megaparsec)
// | and
// | [__replace-attoparsec__](https://hackage.haskell.org/package/replace-attoparsec).
// |
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Control_Monad_ST_Global from "../Control.Monad.ST.Global/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Array_ST from "../Data.Array.ST/index.js";
import * as Data_Array_ST_Partial from "../Data.Array.ST.Partial/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_NonEmpty from "../Data.List.NonEmpty/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Nullable from "../Data.Nullable/index.js";
import * as Data_String_CodePoints from "../Data.String.CodePoints/index.js";
import * as Data_String_Common from "../Data.String.Common/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Effect_Unsafe from "../Effect.Unsafe/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as Unsafe_Coerce from "../Unsafe.Coerce/index.js";

// | Internal splitCap helper combinator. Returns
// | - The reversed `many anyTill` List.
// | - The length of the nonempty tuple elements in the List.
// | - One CodePoint carried over from the last parser match, in case the
// |   parser succeeded without consuming any input. This is where most
// |   of the complexity comes from.
// | We have to make sure we reasonably handle two cases:
// | 1. The sep parser fails but consumes input
// | 2. The sep parser succeeds but does not consume any input
var splitCapCombinator = function (dictMonad) {
    return function (sep) {
        var posSep = Control_Bind.bind(Parsing.bindParserT)(Parsing.position)(function (v) {
            return Control_Bind.bind(Parsing.bindParserT)(sep)(function (x) {
                return Control_Bind.bind(Parsing.bindParserT)(Parsing.position)(function (v1) {
                    return Control_Applicative.pure(Parsing.applicativeParserT)(new Data_Tuple.Tuple(v.index, new Data_Tuple.Tuple(x, v1.index)));
                });
            });
        });
        var accum = function (v) {
            return Control_Bind.bind(Parsing.bindParserT)(Parsing_Combinators.optionMaybe(Parsing_Combinators["try"](Parsing_String.anyTill(dictMonad)(posSep))))(function (v1) {
                if (v1 instanceof Data_Maybe.Just) {
                    var carry_unmatched = (function () {
                        if (v.carry instanceof Data_Maybe.Nothing) {
                            return v1.value0.value0;
                        };
                        if (v.carry instanceof Data_Maybe.Just) {
                            return Data_String_CodePoints.singleton(v.carry.value0) + v1.value0.value0;
                        };
                        throw new Error("Failed pattern match at Parsing.String.Replace (line 289, column 29 - line 291, column 59): " + [ v.carry.constructor.name ]);
                    })();
                    var $27 = v1.value0.value1.value1.value1 === v1.value0.value1.value0;
                    if ($27) {
                        return Control_Bind.bind(Parsing.bindParserT)(Parsing_Combinators.optionMaybe(Parsing_String.anyCodePoint))(function (carryNext) {
                            var $28 = Data_Maybe.isJust(carryNext);
                            if ($28) {
                                return Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Loop({
                                    carry: carryNext,
                                    rlist: new Data_List_Types.Cons(new Data_Tuple.Tuple(carry_unmatched, v1.value0.value1.value1.value0), v.rlist),
                                    arraySize: (function () {
                                        var $29 = Data_String_Common["null"](carry_unmatched);
                                        if ($29) {
                                            return v.arraySize + 1 | 0;
                                        };
                                        return v.arraySize + 2 | 0;
                                    })()
                                }));
                            };
                            return Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Done({
                                carry: carryNext,
                                rlist: new Data_List_Types.Cons(new Data_Tuple.Tuple(carry_unmatched, v1.value0.value1.value1.value0), v.rlist),
                                arraySize: (function () {
                                    var $30 = Data_String_Common["null"](carry_unmatched);
                                    if ($30) {
                                        return v.arraySize + 1 | 0;
                                    };
                                    return v.arraySize + 2 | 0;
                                })()
                            }));
                        });
                    };
                    return Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Loop({
                        carry: Data_Maybe.Nothing.value,
                        rlist: new Data_List_Types.Cons(new Data_Tuple.Tuple(carry_unmatched, v1.value0.value1.value1.value0), v.rlist),
                        arraySize: (function () {
                            var $31 = Data_String_Common["null"](carry_unmatched);
                            if ($31) {
                                return v.arraySize + 1 | 0;
                            };
                            return v.arraySize + 2 | 0;
                        })()
                    }));
                };
                if (v1 instanceof Data_Maybe.Nothing) {
                    return Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Done({
                        carry: v.carry,
                        rlist: v.rlist,
                        arraySize: v.arraySize
                    }));
                };
                throw new Error("Failed pattern match at Parsing.String.Replace (line 286, column 44 - line 316, column 48): " + [ v1.constructor.name ]);
            });
        };
        return Control_Monad_Rec_Class.tailRecM(Parsing.monadRecParserT)(accum)({
            carry: Data_Maybe.Nothing.value,
            rlist: Data_List_Types.Nil.value,
            arraySize: 0
        });
    };
};

// | Monad transformer version of `splitCap`. The `sep` parser will run in the
// | monad context.
// |
// | #### Example
// |
// | Count the pattern matches.
// |
// | Parse in a `State` monad to remember state in the parser. This
// | stateful `letterCount` parser counts
// | the number of pattern matches which occur in the input, and also
// | tags each match with its index.
// |
// |
// | ```purescript
// | letterCount :: ParserT String (State Int) (Tuple Char Int)
// | letterCount = do
// |   x <- letter
// |   i <- modify (_+1)
// |   pure (x /\ i)
// |
// | flip runState 0 $ splitCapT "A B" letterCount
// | ```
// |
// | Result:
// |
// | ```purescript
// | [Right ('A' /\ 1), Left " ", Right ('B' /\ 2)] /\ 2
// | ```
var splitCapT = function (dictMonad) {
    return function (dictMonadRec) {
        return function (input) {
            return function (sep) {
                return Control_Bind.bind(dictMonad.Bind1())(Parsing.runParserT(dictMonadRec)(input)(Control_Apply.apply(Parsing.applyParserT)(Data_Functor.map(Parsing.functorParserT)(Data_Tuple.Tuple.create)(splitCapCombinator(dictMonad)(sep)))(Parsing_String.rest)))(function (v) {
                    if (v instanceof Data_Either.Left) {
                        return Control_Applicative.pure(dictMonad.Applicative0())(Data_List_NonEmpty.singleton(new Data_Either.Left(input)));
                    };
                    if (v instanceof Data_Either.Right) {
                        var term = Data_Maybe.maybe(v.value0.value1)(function (cp) {
                            return Data_String_CodePoints.singleton(cp) + v.value0.value1;
                        })(v.value0.value0.carry);
                        var $44 = Data_List["null"](v.value0.value0.rlist);
                        if ($44) {
                            var $45 = Data_String_Common["null"](term);
                            if ($45) {
                                return Control_Applicative.pure(dictMonad.Applicative0())(Data_List_NonEmpty.singleton(new Data_Either.Left(input)));
                            };
                            return Control_Applicative.pure(dictMonad.Applicative0())(Data_List_NonEmpty.singleton(new Data_Either.Left(term)));
                        };
                        return Control_Applicative.pure(dictMonad.Applicative0())(Data_Maybe.fromJust()(Data_List_NonEmpty.fromList(Data_Foldable.foldl(Data_List_Types.foldableList)(function (ls) {
                            return function (v1) {
                                var $47 = Data_String_Common["null"](v1.value0);
                                if ($47) {
                                    return new Data_List_Types.Cons(new Data_Either.Right(v1.value1), ls);
                                };
                                return new Data_List_Types.Cons(new Data_Either.Left(v1.value0), new Data_List_Types.Cons(new Data_Either.Right(v1.value1), ls));
                            };
                        })((function () {
                            var $50 = Data_String_Common["null"](term);
                            if ($50) {
                                return Data_List_Types.Nil.value;
                            };
                            return new Data_List_Types.Cons(new Data_Either.Left(term), Data_List_Types.Nil.value);
                        })())(v.value0.value0.rlist))));
                    };
                    throw new Error("Failed pattern match at Parsing.String.Replace (line 229, column 70 - line 253, column 16): " + [ v.constructor.name ]);
                });
            };
        };
    };
};

// | #### Split on and capture all patterns
// |
// | Find all occurences of the pattern parser `sep`, split the input string,
// | capture all the patterns and the splits.
// |
// | This function can be used instead of
// | [Data.String.Common.split](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Common#v:split)
// | or
// | [Data.String.Regex.split](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex#v:split)
// | or
// | [Data.String.Regex.match](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex#v:match)
// | or
// | [Data.String.Regex.search](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex#v:search).
// |
// | The input string will be split on every leftmost non-overlapping occurence
// | of the pattern `sep`. The output list will contain
// | the parsed result of input string sections which match the `sep` pattern
// | in `Right`, and non-matching sections in `Left`.
// |
// | #### Access the matched section of text
// |
// | If you want to capture the matched strings, then combine the pattern
// | parser `sep` with the `match` combinator.
// |
// | With the matched strings, we can reconstruct the input string.
// | For all `input`, `sep`, if
// |
// | ```purescript
// | let output = splitCap input (match sep)
// | ```
// |
// | then
// |
// | ```purescript
// | input == fold (either identity fst <$> output)
// | ```
// |
// | #### Example
// |
// | Split the input string on all `Int` pattern matches.
// |
// | ```purescript
// | splitCap "hay 1 straw 2 hay" intDecimal
// | ```
// |
// | Result:
// |
// | ```
// | [Left "hay ", Right 1, Left " straw ", Right 2, Left " hay"]
// | ```
// |
// | #### Example
// |
// | Find the beginning positions of all pattern matches in the input.
// |
// | ```purescript
// | catMaybes $ hush <$> splitCap ".𝝺...\n...𝝺." (position <* string "𝝺")
// | ```
// |
// | Result:
// |
// | ```purescript
// | [ Position {index: 1, line: 1, column: 2 }
// | , Position { index: 9, line: 2, column: 4 }
// | ]
// | ```
// |
// | #### Example
// |
// | Find groups of balanced nested parentheses. This pattern is an example of
// | a “context-free” grammar, a pattern that
// | [can't be expressed by a regular expression](https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags/1732454#1732454).
// | We can express the pattern with a recursive parser.
// |
// | ```purescript
// | balancedParens :: Parser String Unit
// | balancedParens = do
// |   void $ char '('
// |   void $ manyTill (balancedParens <|> void anyCodePoint) (char ')')
// |
// | rmap fst <$> splitCap "((🌼)) (()())" (match balancedParens)
// | ```
// |
// | Result:
// |
// | ```purescript
// | [Right "((🌼))", Left " ", Right "(()())"]
// | ```
var splitCap = function (input) {
    return function (sep) {
        return Data_Newtype.unwrap()(splitCapT(Data_Identity.monadIdentity)(Control_Monad_Rec_Class.monadRecIdentity)(input)(sep));
    };
};

// | Monad transformer version of `replace`.
// |
// | #### Example
// |
// | Find an environment variable in curly braces and replace it with its value
// | from the environment.
// | We can read from the environment with `lookupEnv` because `replaceT` is
// | running the `sep` parser in `Effect`.
// |
// | ```purescript
// | replaceT "◀ {HOME} ▶" do
// |   _ <- string "{"
// |   Tuple home _ <- anyTill (string "}")
// |   lift (lookupEnv home) >>= maybe empty pure
// | ```
// |
// | Result:
// |
// | ```purescript
// | "◀ /home/jbrock ▶"
// | ```
// |
// | [![Perl Problems](https://imgs.xkcd.com/comics/perl_problems.png)](https://xkcd.com/1171/)
var replaceT = function (dictMonad) {
    return function (dictMonadRec) {
        return function (input) {
            return function (sep) {
                
                // Sequence ST operations in some other monad. What could possibly go wrong?
var doST = (function () {
                    var $83 = Control_Applicative.pure(dictMonad.Applicative0());
                    return function ($84) {
                        return $83(Effect_Unsafe.unsafePerformEffect(Control_Monad_ST_Global.toEffect($84)));
                    };
                })();
                return Control_Bind.bind(dictMonad.Bind1())(Parsing.runParserT(dictMonadRec)(input)(Control_Apply.apply(Parsing.applyParserT)(Data_Functor.map(Parsing.functorParserT)(Data_Tuple.Tuple.create)(splitCapCombinator(dictMonad)(sep)))(Parsing_String.rest)))(function (v) {
                    if (v instanceof Data_Either.Left) {
                        return Control_Applicative.pure(dictMonad.Applicative0())(input);
                    };
                    if (v instanceof Data_Either.Right) {
                        var term = Data_Maybe.maybe(v.value0.value1)(function (cp) {
                            return Data_String_CodePoints.singleton(cp) + v.value0.value1;
                        })(v.value0.value0.carry);
                        return Control_Bind.bind(dictMonad.Bind1())(doST((function () {
                            var $58 = Data_String_Common["null"](term);
                            if ($58) {
                                return Data_Array_ST.unsafeThaw(Data_Array.replicate(v.value0.value0.arraySize)(Data_Nullable["null"]));
                            };
                            return function __do() {
                                var arr = Data_Array_ST.unsafeThaw(Data_Array.replicate(v.value0.value0.arraySize + 1 | 0)(Data_Nullable["null"]))();
                                Data_Array_ST_Partial.poke()(v.value0.value0.arraySize)(Data_Nullable.notNull(term))(arr)();
                                return arr;
                            };
                        })()))(function (v1) {
                            var accum = function (v2) {
                                var v3 = Data_List.uncons(v2["rlist'"]);
                                if (v3 instanceof Data_Maybe.Nothing) {
                                    return Control_Applicative.pure(dictMonad.Applicative0())(new Control_Monad_Rec_Class.Done(Data_Unit.unit));
                                };
                                if (v3 instanceof Data_Maybe.Just) {
                                    return Control_Bind.discard(Control_Bind.discardUnit)(dictMonad.Bind1())(doST(Data_Array_ST_Partial.poke()(v2.index)(Data_Nullable.notNull(v3.value0.head.value1))(v1)))(function () {
                                        var $62 = Data_String_Common["null"](v3.value0.head.value0);
                                        if ($62) {
                                            return Control_Applicative.pure(dictMonad.Applicative0())(new Control_Monad_Rec_Class.Loop({
                                                index: v2.index - 1 | 0,
                                                "rlist'": v3.value0.tail
                                            }));
                                        };
                                        return Control_Bind.discard(Control_Bind.discardUnit)(dictMonad.Bind1())(doST(Data_Array_ST_Partial.poke()(v2.index - 1 | 0)(Data_Nullable.notNull(v3.value0.head.value0))(v1)))(function () {
                                            return Control_Applicative.pure(dictMonad.Applicative0())(new Control_Monad_Rec_Class.Loop({
                                                index: v2.index - 2 | 0,
                                                "rlist'": v3.value0.tail
                                            }));
                                        });
                                    });
                                };
                                throw new Error("Failed pattern match at Parsing.String.Replace (line 463, column 35 - line 473, column 63): " + [ v3.constructor.name ]);
                            };
                            return Control_Bind.discard(Control_Bind.discardUnit)(dictMonad.Bind1())(Control_Monad_Rec_Class.tailRecM(dictMonadRec)(accum)({
                                index: v.value0.value0.arraySize - 1 | 0,
                                "rlist'": v.value0.value0.rlist
                            }))(function () {
                                return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(Data_Functor.map(Data_Functor.functorFn)(Data_String_Common.joinWith(""))(Unsafe_Coerce.unsafeCoerce))(doST(Data_Array_ST.unsafeFreeze(v1)));
                            });
                        });
                    };
                    throw new Error("Failed pattern match at Parsing.String.Replace (line 444, column 70 - line 481, column 72): " + [ v.constructor.name ]);
                });
            };
        };
    };
};

// |
// | #### Find-and-replace
// |
// | Also called “match-and-substitute”. Find all
// | of the leftmost non-overlapping sections of the input string which match
// | the pattern parser `sep`, and
// | replace them with the result of the parser.
// | The `sep` parser must return a result of type `String`.
// |
// | This function can be used instead of
// | [Data.String.replaceAll](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String#v:replaceAll)
// | or
// | [Data.String.Regex.replace'](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex#v:replace').
// |
// | #### Access the matched section of text in the `editor`
// |
// | To get access to the matched string for the replacement
// | combine the pattern parser `sep`
// | with `match`.
// | This allows us to write a `sep` parser which can choose to not
// | edit the match and just leave it as it is.
// |
// | So, for all `sep`:
// |
// | ```purescript
// | replace input (fst <$> match sep) == input
// | ```
// |
// | #### Example
// |
// | Find and uppercase the `"needle"` pattern.
// |
// | ```purescript
// | replace "hay needle hay" (toUpper <$> string "needle")
// | ```
// |
// | Result:
// |
// | ```purescript
// | "hay NEEDLE hay"
// | ```
// |
// | #### Example
// |
// | Find integers and double them.
// |
// | ```purescript
// | replace "1 6 21 107" (show <$> (_*2) <$> intDecimal)
// | ```
// |
// | Result:
// |
// | ```purescript
// | "2 12 42 214"
// | ```
var replace = function (input) {
    return function (sep) {
        return Data_Newtype.unwrap()(replaceT(Data_Identity.monadIdentity)(Control_Monad_Rec_Class.monadRecIdentity)(input)(sep));
    };
};

// | Monad transformer version of `breakCap`. The `sep` parser will run
// | in the monad context.
var breakCapT = function (dictMonad) {
    return function (dictMonadRec) {
        return function (input) {
            return function (sep) {
                var go = Control_Bind.bind(Parsing.bindParserT)(Parsing_String.anyTill(dictMonad)(sep))(function (v) {
                    return Control_Bind.bind(Parsing.bindParserT)(Parsing.getParserT)(function (v1) {
                        return Control_Applicative.pure(Parsing.applicativeParserT)(new Data_Tuple.Tuple(v.value0, new Data_Tuple.Tuple(v.value1, v1.value0)));
                    });
                });
                return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(Data_Either.hush)(Parsing.runParserT(dictMonadRec)(input)(go));
            };
        };
    };
};

// | #### Break on and capture one pattern
// |
// | Find the first occurence of a pattern in a text stream, capture the found
// | pattern, and break the input text stream on the found pattern.
// |
// | This function can be used instead of
// | [Data.String.indexOf](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String#v:indexOf)
// | or
// | [Data.String.Regex.search](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex#v:search)
// | or
// | [Data.String.Regex.replace](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String.Regex#v:replace)
// | and it allows using a parser for the pattern search.
// |
// | This function can be used instead of
// | [Data.String.takeWhile](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String#v:takeWhile)
// | or
// | [Data.String.dropWhile](https://pursuit.purescript.org/packages/purescript-strings/docs/Data.String#v:dropWhile)
// | and it is predicated beyond more than just the next single `CodePoint`.
// |
// | #### Output
// |
// | - `Nothing` when no pattern match was found.
// | - `Just (prefix /\ parse_result /\ suffix)` for the result of parsing the
// |   pattern match, and the `prefix` string before and the `suffix` string
// |   after the pattern match. `prefix` and `suffix` may be zero-length strings.
// |
// | #### Access the matched section of text
// |
// | If you want to capture the matched string, then combine the pattern
// | parser `sep` with `match`.
// |
// | With the matched string, we can reconstruct the input string.
// | For all `input`, `sep`, if
// |
// | ```purescript
// | let (Just (prefix /\ (infix /\ _) /\ suffix)) =
// |       breakCap input (match sep)
// | ```
// |
// | then
// |
// | ```purescript
// | input == prefix <> infix <> suffix
// | ```
// | #### Example
// |
// | Find the first pattern match and break the input string on the pattern.
// |
// | ```purescript
// | breakCap "hay needle hay" (string "needle")
// | ```
// |
// | Result:
// |
// | ```purescript
// | Just ("hay " /\ "needle" /\ " hay")
// | ```
// |
// | #### Example
// |
// | Find the first pattern match, capture the matched text and the parsed result.
// |
// | ```purescript
// | breakCap "abc 123 def" (match intDecimal)
// | ```
// |
// | Result:
// |
// | ```purescript
// | Just ("abc " /\ ("123" /\ 123) /\ " def")
// | ```
var breakCap = function (input) {
    return function (sep) {
        return Data_Newtype.unwrap()(breakCapT(Data_Identity.monadIdentity)(Control_Monad_Rec_Class.monadRecIdentity)(input)(sep));
    };
};
export {
    breakCap,
    breakCapT,
    splitCap,
    splitCapT,
    replace,
    replaceT
};
