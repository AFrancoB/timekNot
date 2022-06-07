// | A “parser combinator” is a function which takes some
// | parsers as arguments and returns a new parser.
// |
// | ## Combinators in other packages
// |
// | Many variations of well-known monadic and applicative combinators used for parsing are
// | defined in other PureScript packages. We list some of them here.
// |
// | If you use a combinator from some other package for parsing, keep in mind
// | this surprising truth about the __parsing__ package:
// | All other combinators used with this package will be stack-safe,
// | but usually the combinators with a `MonadRec` constraint will run faster.
// | So you should prefer `MonadRec` versions of combinators, but for reasons
// | of speed, not stack-safety.
// |
// | ### Data.Array
// |
// | Expect better parsing speed from the `List`-based combinators in this
// | module than from `Array`-based combinators.
// |
// | * [Data.Array.many](https://pursuit.purescript.org/packages/purescript-arrays/docs/Data.Array#v:many)
// | * [Data.Array.some](https://pursuit.purescript.org/packages/purescript-arrays/docs/Data.Array#v:some)
// | * [Data.Array.NonEmpty.some](https://pursuit.purescript.org/packages/purescript-arrays/docs/Data.Array.NonEmpty#v:some)
// |
// | ### Data.List
// |
// | The `many` and `many1` combinators in this package
// | are redeclarations of
// | the `manyRec` and `someRec` combinators in __Data.List__.
// |
// | ### Data.List.Lazy
// |
// | * [Data.List.Lazy.many](https://pursuit.purescript.org/packages/purescript-lists/docs/Data.List.Lazy#v:many)
// | * [Data.List.Lazy.some](https://pursuit.purescript.org/packages/purescript-lists/docs/Data.List.Lazy#v:some)
// |
// | ## Combinators in this package
// |
// | the __replicateA__ and __replicateM__ combinators are re-exported from
// | this module. `replicateA n p` or `replicateM n p`
// | will repeat parser `p` exactly `n` times. The `replicateA` combinator can
// | produce either an `Array` or a `List`.
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Lazy from "../Control.Lazy/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Control_Plus from "../Control.Plus/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Lazy from "../Data.List.Lazy/index.js";
import * as Data_List_NonEmpty from "../Data.List.NonEmpty/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";
import * as Data_Unfoldable1 from "../Data.Unfoldable1/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Parsing from "../Parsing/index.js";

// | Provide an error message in the case of failure, but lazily. This is handy
// | in cases where constructing the error message is expensive, so it's
// | preferable to defer it until an error actually happens.
// |
// |```purescript
// |parseBang :: Parser Char
// |parseBang = char '!' <~?> \_ -> "Expected a bang"
// |```
var withLazyErrorMessage = function (p) {
    return function (msg) {
        return Control_Alt.alt(Parsing.altParserT)(p)(Control_Lazy.defer(Parsing.lazyParserT)(function (v) {
            return Parsing.fail("Expected " + msg(Data_Unit.unit));
        }));
    };
};

// | Provide an error message in the case of failure.
var withErrorMessage = function (p) {
    return function (msg) {
        return Control_Alt.alt(Parsing.altParserT)(p)(Parsing.fail("Expected " + msg));
    };
};

// | If the parser fails then backtrack the input stream to the unconsumed state.
// |
// | Like `try`, but will reposition the error to the `try` point.
// |
// | ```
// | >>> runParser "ac" (try (char 'a' *> char 'b'))
// | Left (ParseError "Expected 'b'" (Position { index: 1, line: 1, column: 2 }))
// | ```
// |
// | ```
// | >>> runParser "ac" (tryRethrow (char 'a' *> char 'b'))
// | Left (ParseError "Expected 'b'" (Position { index: 0, line: 1, column: 1 }))
// | ```
var tryRethrow = function (v) {
    return function (v1, more, lift, $$throw, done) {
        return v(v1, more, lift, function (v2, v3) {
            return $$throw(new Parsing.ParseState(v2.value0, v2.value1, v1.value2), new Parsing.ParseError(v3.value0, v1.value1));
        }, done);
    };
};

// | If the parser fails then backtrack the input stream to the unconsumed state.
// |
// | One use for this combinator is to ensure that the right parser of an
// | alternative will always be tried when the left parser fails.
// | ```
// | >>> runParser "ac" ((char 'a' *> char 'b') <|> (char 'a' *> char 'c'))
// | Left (ParseError "Expected 'b'" (Position { line: 1, column: 2 }))
// | ```
// |
// | ```
// | >>> runParser "ac" (try (char 'a' *> char 'b') <|> (char 'a' *> char 'c'))
// | Right 'c'
// | ```
var $$try = function (v) {
    return function (v1, more, lift, $$throw, done) {
        return v(v1, more, lift, function (v2, err) {
            return $$throw(new Parsing.ParseState(v2.value0, v2.value1, v1.value2), err);
        }, done);
    };
};

// | Skip at least one instance of a phrase.
var skipMany1 = function (p) {
    var go = function (v) {
        return Control_Alt.alt(Parsing.altParserT)(Data_Functor.voidLeft(Parsing.functorParserT)(p)(new Control_Monad_Rec_Class.Loop(Data_Unit.unit)))(Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Done(Data_Unit.unit)));
    };
    return Control_Apply.applySecond(Parsing.applyParserT)(p)(Control_Monad_Rec_Class.tailRecM(Parsing.monadRecParserT)(go)(Data_Unit.unit));
};

// | Skip many instances of a phrase.
var skipMany = function (p) {
    return Control_Alt.alt(Parsing.altParserT)(skipMany1(p))(Control_Applicative.pure(Parsing.applicativeParserT)(Data_Unit.unit));
};

// | Parse phrases delimited and optionally terminated by a separator, requiring at least one match.
var sepEndBy1 = function (p) {
    return function (sep) {
        var go = function (acc) {
            var done = Control_Lazy.defer(Parsing.lazyParserT)(function (v) {
                return Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Done(Data_List.reverse(acc)));
            });
            var nextOne = Control_Bind.bind(Parsing.bindParserT)(sep)(function () {
                return Control_Alt.alt(Parsing.altParserT)(Data_Functor.mapFlipped(Parsing.functorParserT)(p)(function (a) {
                    return new Control_Monad_Rec_Class.Loop(new Data_List_Types.Cons(a, acc));
                }))(done);
            });
            return Control_Alt.alt(Parsing.altParserT)(nextOne)(done);
        };
        return Control_Bind.bind(Parsing.bindParserT)(p)(function (a) {
            return Control_Alt.alt(Parsing.altParserT)(Data_Functor.map(Parsing.functorParserT)(Data_List_NonEmpty["cons$prime"](a))(Control_Monad_Rec_Class.tailRecM(Parsing.monadRecParserT)(go)(Data_List_Types.Nil.value)))(Control_Applicative.pure(Parsing.applicativeParserT)(Data_List_NonEmpty.singleton(a)));
        });
    };
};

// | Parse phrases delimited and optionally terminated by a separator.
var sepEndBy = function (p) {
    return function (sep) {
        return Control_Alt.alt(Parsing.altParserT)(Data_Functor.map(Parsing.functorParserT)(Data_List_NonEmpty.toList)(sepEndBy1(p)(sep)))(Control_Applicative.pure(Parsing.applicativeParserT)(Data_List_Types.Nil.value));
    };
};

// | Parse phrases delimited by a separator, requiring at least one match.
var sepBy1 = function (p) {
    return function (sep) {
        return Control_Bind.bind(Parsing.bindParserT)(p)(function (a) {
            return Control_Bind.bind(Parsing.bindParserT)(Data_List.manyRec(Parsing.monadRecParserT)(Parsing.alternativeParserT)(Control_Apply.applySecond(Parsing.applyParserT)(sep)(p)))(function (as) {
                return Control_Applicative.pure(Parsing.applicativeParserT)(Data_List_NonEmpty["cons$prime"](a)(as));
            });
        });
    };
};

// | Parse phrases delimited by a separator.
// |
// | For example:
// |
// | ```purescript
// | digit `sepBy` string ","
// | ```
var sepBy = function (p) {
    return function (sep) {
        return Control_Alt.alt(Parsing.altParserT)(Data_Functor.map(Parsing.functorParserT)(Data_List_NonEmpty.toList)(sepBy1(p)(sep)))(Control_Applicative.pure(Parsing.applicativeParserT)(Data_List_Types.Nil.value));
    };
};

// | Optionally parse something, failing quietly.
// |
// | To optionally parse `p` and never fail: `optional (try p)`.
var optional = function (p) {
    return Control_Alt.alt(Parsing.altParserT)(Data_Functor["void"](Parsing.functorParserT)(p))(Control_Applicative.pure(Parsing.applicativeParserT)(Data_Unit.unit));
};

// | Provide a default result in the case where a parser fails without consuming input.
var option = function (a) {
    return function (p) {
        return Control_Alt.alt(Parsing.altParserT)(p)(Control_Applicative.pure(Parsing.applicativeParserT)(a));
    };
};

// | pure `Nothing` in the case where a parser fails without consuming input.
var optionMaybe = function (p) {
    return option(Data_Maybe.Nothing.value)(Data_Functor.map(Parsing.functorParserT)(Data_Maybe.Just.create)(p));
};

// | Fail if the parser succeeds.
// |
// | Will never consume input.
var notFollowedBy = function (p) {
    return $$try(Control_Alt.alt(Parsing.altParserT)(Control_Apply.applySecond(Parsing.applyParserT)($$try(p))(Parsing.fail("Negated parser succeeded")))(Control_Applicative.pure(Parsing.applicativeParserT)(Data_Unit.unit)));
};

// | Parse many phrases until the terminator phrase matches.
// | Returns the list of phrases and the terminator phrase.
// |
// | #### Non-greedy repetition
// |
// | Use the __manyTill_ __ combinator
// | to do non-greedy repetition of a pattern `p`, like we would in Regex
// | by writing `p*?`.
// | To repeat pattern `p` non-greedily, write
// | `manyTill_ p q` where `q` is the entire rest of the parser.
// |
// | For example, this parse fails because `many` repeats the pattern `letter`
// | greedily.
// |
// | ```
// | runParser "aab" do
// |   a <- many letter
// |   b <- char 'b'
// |   pure (Tuple a b)
// | ```
// | ```
// | (ParseError "Expected 'b'" (Position { line: 1, column: 4 }))
// | ```
// |
// | To repeat pattern `letter` non-greedily, use `manyTill_`.
// |
// | ```
// | runParser "aab" do
// |   Tuple a b <- manyTill_ letter do
// |     char 'b'
// |   pure (Tuple a b)
// | ```
// | ```
// | (Tuple ('a' : 'a' : Nil) 'b')
// | ```
var manyTill_ = function (p) {
    return function (end) {
        var go = function (xs) {
            return Control_Alt.alt(Parsing.altParserT)(Control_Bind.bind(Parsing.bindParserT)(end)(function (t) {
                return Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(Data_List.reverse(xs), t)));
            }))(Control_Bind.bind(Parsing.bindParserT)(p)(function (x) {
                return Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Loop(new Data_List_Types.Cons(x, xs)));
            }));
        };
        return Control_Monad_Rec_Class.tailRecM(Parsing.monadRecParserT)(go)(Data_List_Types.Nil.value);
    };
};

// | Parse many phrases until the terminator phrase matches.
var manyTill = function (p) {
    return function (end) {
        var go = function (acc) {
            return Control_Alt.alt(Parsing.altParserT)(Data_Functor.mapFlipped(Parsing.functorParserT)(end)(function (v) {
                return new Control_Monad_Rec_Class.Done(Data_List.reverse(acc));
            }))(Data_Functor.mapFlipped(Parsing.functorParserT)(p)(function (x) {
                return new Control_Monad_Rec_Class.Loop(new Data_List_Types.Cons(x, acc));
            }));
        };
        return Control_Monad_Rec_Class.tailRecM(Parsing.monadRecParserT)(go)(Data_List_Types.Nil.value);
    };
};

// | Parse the phrase as many times as possible, at least *N* times, but no
// | more than *M* times.
// | If the phrase can’t parse as least *N* times then the whole
// | parser fails. If the phrase parses successfully *M* times then stop.
// | The current phrase index, starting at *0*, is passed to the phrase.
// |
// | Returns the list of parse results and the number of results.
// |
// | `manyIndex n n (\_ -> p)` is equivalent to `replicateA n p`.
var manyIndex = function (from) {
    return function (to) {
        return function (p) {
            var go = function (v) {
                var $45 = v.value0 >= to;
                if ($45) {
                    return Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(v.value0, Data_List.reverse(v.value1))));
                };
                return Control_Alt.alt(Parsing.altParserT)(Control_Bind.bind(Parsing.bindParserT)(p(v.value0))(function (x) {
                    return Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Loop(new Data_Tuple.Tuple(v.value0 + 1 | 0, new Data_List_Types.Cons(x, v.value1))));
                }))((function () {
                    var $46 = v.value0 >= from;
                    if ($46) {
                        return Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(v.value0, Data_List.reverse(v.value1))));
                    };
                    return Parsing.fail("Expected more phrases");
                })());
            };
            var $49 = from > to || from < 0;
            if ($49) {
                return Control_Applicative.pure(Parsing.applicativeParserT)(new Data_Tuple.Tuple(0, Data_List_Types.Nil.value));
            };
            return Control_Monad_Rec_Class.tailRecM(Parsing.monadRecParserT)(go)(new Data_Tuple.Tuple(0, Data_List_Types.Nil.value));
        };
    };
};

// | Parse many phrases until the terminator phrase matches, requiring at least one match.
// | Returns the list of phrases and the terminator phrase.
var many1Till_ = function (p) {
    return function (end) {
        return Control_Bind.bind(Parsing.bindParserT)(p)(function (x) {
            return Control_Bind.bind(Parsing.bindParserT)(manyTill_(p)(end))(function (v) {
                return Control_Applicative.pure(Parsing.applicativeParserT)(new Data_Tuple.Tuple(Data_List_NonEmpty["cons$prime"](x)(v.value0), v.value1));
            });
        });
    };
};

// | Parse at least one phrase until the terminator phrase matches.
var many1Till = function (p) {
    return function (end) {
        return Control_Apply.apply(Parsing.applyParserT)(Data_Functor.map(Parsing.functorParserT)(Data_List_NonEmpty["cons$prime"])(p))(manyTill(p)(end));
    };
};

// | Match one or more times.
var many1 = function (p) {
    return Control_Apply.apply(Parsing.applyParserT)(Data_Functor.map(Parsing.functorParserT)(Data_List_NonEmpty["cons$prime"])(p))(Data_List.manyRec(Parsing.monadRecParserT)(Parsing.alternativeParserT)(p));
};

// | Match the parser `p` as many times as possible.
// |
// | If `p` never consumes input when it
// | fails then `many p` will always succeed,
// | but may return an empty list.
var many = /* #__PURE__ */ Data_List.manyRec(Parsing.monadRecParserT)(Parsing.alternativeParserT);

// | Parse a phrase, without modifying the consumed state or stream position.
var lookAhead = function (v) {
    return function (state1, more, lift, $$throw, done) {
        return v(state1, more, lift, function (v1, err) {
            return $$throw(state1, err);
        }, function (v1, res) {
            return done(state1, res);
        });
    };
};

// | Parse phrases delimited and terminated by a separator, requiring at least one match.
var endBy1 = function (p) {
    return function (sep) {
        return many1(Control_Apply.applyFirst(Parsing.applyParserT)(p)(sep));
    };
};

// | Parse phrases delimited and terminated by a separator.
var endBy = function (p) {
    return function (sep) {
        return Data_List.manyRec(Parsing.monadRecParserT)(Parsing.alternativeParserT)(Control_Apply.applyFirst(Parsing.applyParserT)(p)(sep));
    };
};

// | Parse one of a set of alternatives.
var choice = function (dictFoldable) {
    var go = function (p1) {
        return function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return new Data_Maybe.Just(p1);
            };
            if (v instanceof Data_Maybe.Just) {
                return new Data_Maybe.Just(Control_Alt.alt(Parsing.altParserT)(p1)(v.value0));
            };
            throw new Error("Failed pattern match at Parsing.Combinators (line 357, column 11 - line 359, column 32): " + [ v.constructor.name ]);
        };
    };
    var $68 = Data_Maybe.fromMaybe(Control_Plus.empty(Parsing.plusParserT));
    var $69 = Data_Foldable.foldr(dictFoldable)(go)(Data_Maybe.Nothing.value);
    return function ($70) {
        return $68($69($70));
    };
};

// | `chainr` requiring at least one match.
var chainr1 = function (p) {
    return function (f) {
        var apply = function (y) {
            return function (v) {
                return v.value1(v.value0)(y);
            };
        };
        
        // This looks scary at first glance, so I'm leaving a comment in a vain
        // attempt to explain how it works.
        //
        // The loop state is a record {init, last}, where `last` is the last (i.e.
        // rightmost) `a` value that has been parsed so far, and `init` is a list of
        // (value + operator) pairs that have been parsed before that.
        //
        // The very first value is parsed at top level, and it becomes the initial
        // value of `last`, while the initial value of `init` is just `Nil`,
        // indicating that no pairs of (value + operator) have been parsed yet.
        //
        // At every step, we parse an operator and a value, and then the newly parsed
        // value becomes `last` (because, well, it's been parsed last), and the pair
        // of (previous last + operator) is prepended to `init`.
        //
        // After we can no longer parse a pair of (value + operation), we're done. At
        // that point, we have a list of (value + operation) pairs in reverse order
        // (since we prepend each pair as we go) and the very last value. All that's
        // left is combine them all via `foldl`.
var go = function (v) {
            return Control_Alt.alt(Parsing.altParserT)(Control_Bind.bind(Parsing.bindParserT)(f)(function (op) {
                return Control_Bind.bind(Parsing.bindParserT)(p)(function (a) {
                    return Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Loop({
                        last: a,
                        init: new Data_List_Types.Cons(new Data_Tuple.Tuple(v.last, op), v.init)
                    }));
                });
            }))(Control_Lazy.defer(Parsing.lazyParserT)(function (v1) {
                return Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Done(Data_Foldable.foldl(Data_List_Types.foldableList)(apply)(v.last)(v.init)));
            }));
        };
        return Control_Bind.bind(Parsing.bindParserT)(p)(function (a) {
            return Control_Monad_Rec_Class.tailRecM(Parsing.monadRecParserT)(go)({
                last: a,
                init: Data_List_Types.Nil.value
            });
        });
    };
};

// | `chainr p f` parses one or more occurrences of `p`, separated by operator `f`.
// |
// | Returns a value
// | obtained by a right-associative application of the functions returned by
// | `f` to the values returned by `p`. This combinator can be used to
// | eliminate right-recursion in expression grammars.
// |
// | For example:
// |
// | ```purescript
// | chainr digit (string "+" $> add) 0
// | ```
var chainr = function (p) {
    return function (f) {
        return function (a) {
            return Control_Alt.alt(Parsing.altParserT)(chainr1(p)(f))(Control_Applicative.pure(Parsing.applicativeParserT)(a));
        };
    };
};

// | `chainl` requiring at least one match.
var chainl1 = function (p) {
    return function (f) {
        var go = function (a) {
            return Control_Alt.alt(Parsing.altParserT)(Control_Bind.bind(Parsing.bindParserT)(f)(function (op) {
                return Control_Bind.bind(Parsing.bindParserT)(p)(function (a$prime) {
                    return Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Loop(op(a)(a$prime)));
                });
            }))(Control_Applicative.pure(Parsing.applicativeParserT)(new Control_Monad_Rec_Class.Done(a)));
        };
        return Control_Bind.bind(Parsing.bindParserT)(p)(function (a) {
            return Control_Monad_Rec_Class.tailRecM(Parsing.monadRecParserT)(go)(a);
        });
    };
};

// | `chainl p f` parses one or more occurrences of `p`, separated by operator `f`.
// |
// | Returns a value
// | obtained by a left-associative application of the functions returned by
// | `f` to the values returned by `p`. This combinator can be used to
// | eliminate left-recursion in expression grammars.
// |
// | For example:
// |
// | ```purescript
// | chainl digit (string "+" $> add) 0
// | ```
var chainl = function (p) {
    return function (f) {
        return function (a) {
            return Control_Alt.alt(Parsing.altParserT)(chainl1(p)(f))(Control_Applicative.pure(Parsing.applicativeParserT)(a));
        };
    };
};

// | Wrap a parser with opening and closing markers.
// |
// | For example:
// |
// | ```purescript
// | parens = between (string "(") (string ")")
// | ```
var between = function (open) {
    return function (close) {
        return function (p) {
            return Control_Apply.applyFirst(Parsing.applyParserT)(Control_Apply.applySecond(Parsing.applyParserT)(open)(p))(close);
        };
    };
};

// | Flipped `(<?>)`.
var asErrorMessage = /* #__PURE__ */ Data_Function.flip(withErrorMessage);

// | If the parser succeeds without advancing the input stream position,
// | then force the parser to fail.
// |
// | This combinator can be used to prevent infinite parser repetition.
// |
// | Does not depend on or effect the `consumed` flag which indicates whether
// | we are committed to this parsing branch.
var advance = function (p) {
    return Control_Bind.bind(Parsing.bindParserT)(Parsing.position)(function (v) {
        return Control_Bind.bind(Parsing.bindParserT)(p)(function (x) {
            return Control_Bind.bind(Parsing.bindParserT)(Parsing.position)(function (v1) {
                var $65 = v1.index > v.index;
                if ($65) {
                    return Control_Applicative.pure(Parsing.applicativeParserT)(x);
                };
                return Parsing.fail("Expected progress");
            });
        });
    });
};
export {
    $$try as try,
    tryRethrow,
    lookAhead,
    choice,
    between,
    notFollowedBy,
    option,
    optionMaybe,
    optional,
    many,
    many1,
    manyTill,
    manyTill_,
    many1Till,
    many1Till_,
    manyIndex,
    skipMany,
    skipMany1,
    sepBy,
    sepBy1,
    sepEndBy,
    sepEndBy1,
    endBy,
    endBy1,
    chainl,
    chainl1,
    chainr,
    chainr1,
    advance,
    withErrorMessage,
    withLazyErrorMessage,
    asErrorMessage
};
export {
    alt,
    empty
} from "../Control.Plus/index.js";
export {
    replicateM
} from "../Data.List.Lazy/index.js";
export {
    replicateA
} from "../Data.Unfoldable/index.js";
export {
    replicate1A
} from "../Data.Unfoldable1/index.js";
