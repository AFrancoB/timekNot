// | Types and operations for monadic parsing.
// |
// | Combinators are in the `Parsing.Combinators` module.
// |
// | Primitive parsers for `String` input streams are in the `Parsing.String`
// | module.
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Error_Class from "../Control.Monad.Error.Class/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Control_Monad_State_Class from "../Control.Monad.State.Class/index.js";
import * as Control_Monad_Trans_Class from "../Control.Monad.Trans.Class/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Lazy from "../Data.Lazy/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Show_Generic from "../Data.Show.Generic/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
var $runtime_lazy = function (name, moduleName, init) {
    var state = 0;
    var val;
    return function (lineNumber) {
        if (state === 2) return val;
        if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
        state = 1;
        val = init();
        state = 2;
        return val;
    };
};

// | `Position` represents the position of the parser in the input stream.
// |
// | - `index` is the position offset since the start of the input. Starts
// |   at *0*.
// | - `line` is the current line in the input. Starts at *1*.
// | - `column` is the column of the next character in the current line that
// |   will be parsed. Starts at *1*.
var Position = function (x) {
    return x;
};

// | The internal state of the `ParserT s m` monad.
// |
// | Contains the remaining input and current position and the consumed flag.
// |
// | The consumed flag is used to implement the rule for `alt` that
// | - If the left parser fails *without consuming any input*, then backtrack and try the right parser.
// | - If the left parser fails and consumes input, then fail immediately.
var ParseState = /* #__PURE__ */ (function () {
    function ParseState(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    ParseState.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new ParseState(value0, value1, value2);
            };
        };
    };
    return ParseState;
})();

// | A parsing error, consisting of an error message and
// | the position in the input stream at which the error occurred.
var ParseError = /* #__PURE__ */ (function () {
    function ParseError(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ParseError.create = function (value0) {
        return function (value1) {
            return new ParseError(value0, value1);
        };
    };
    return ParseError;
})();

// ParseState constructor has three parameters,
// s: the remaining input
// Position: the current position
// Boolean: the consumed flag.
//
// The consumed flag is used to implement the rule for `alt` that
// * If the left parser fails *without consuming any input*, then backtrack and try the right parser.
// * If the left parser fails and consumes input, then fail immediately.
//
// https://hackage.haskell.org/package/parsec/docs/Text-Parsec.html#v:try
//
// http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/
// | The `Parser s` monad with a monad transformer parameter `m`.
var ParserT = function (x) {
    return x;
};

// When we want to run a parser, continuations are reified as data
// constructors and processed in a tail-recursive loop.
var More = /* #__PURE__ */ (function () {
    function More(value0) {
        this.value0 = value0;
    };
    More.create = function (value0) {
        return new More(value0);
    };
    return More;
})();

// When we want to run a parser, continuations are reified as data
// constructors and processed in a tail-recursive loop.
var Lift = /* #__PURE__ */ (function () {
    function Lift(value0) {
        this.value0 = value0;
    };
    Lift.create = function (value0) {
        return new Lift(value0);
    };
    return Lift;
})();

// When we want to run a parser, continuations are reified as data
// constructors and processed in a tail-recursive loop.
var Stop = /* #__PURE__ */ (function () {
    function Stop(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Stop.create = function (value0) {
        return function (value1) {
            return new Stop(value0, value1);
        };
    };
    return Stop;
})();
var monadTransParserT = {
    lift: function (dictMonad) {
        return function (m) {
            return function (state1, v, lift$prime, v1, done) {
                return lift$prime(Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(function (a) {
                    return function (v2) {
                        return done(state1, a);
                    };
                })(m));
            };
        };
    }
};
var lazyParserT = {
    defer: function (f) {
        var m = Data_Lazy.defer(f);
        return function (state1, more, lift, $$throw, done) {
            var v = Data_Lazy.force(m);
            return v(state1, more, lift, $$throw, done);
        };
    }
};
var genericPosition_ = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showPosition = {
    show: function (x) {
        return Data_Show_Generic.genericShow(genericPosition_)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Data_Show.showRecord()()(Data_Show.showRecordFieldsCons({
            reflectSymbol: function () {
                return "column";
            }
        })(Data_Show.showRecordFieldsCons({
            reflectSymbol: function () {
                return "index";
            }
        })(Data_Show.showRecordFieldsCons({
            reflectSymbol: function () {
                return "line";
            }
        })(Data_Show.showRecordFieldsNil)(Data_Show.showInt))(Data_Show.showInt))(Data_Show.showInt))))({
            reflectSymbol: function () {
                return "Position";
            }
        }))(x);
    }
};
var functorParserT = {
    map: function (f) {
        return function (v) {
            return function (state1, more, lift, $$throw, done) {
                return more(function (v1) {
                    return v(state1, more, lift, $$throw, function (state2, a) {
                        return more(function (v2) {
                            return done(state2, f(a));
                        });
                    });
                });
            };
        };
    }
};
var eqPosition = {
    eq: function (v) {
        return function (v1) {
            return v.index === v1.index;
        };
    }
};
var ordPosition = {
    compare: function (v) {
        return function (v1) {
            return Data_Ord.compare(Data_Ord.ordInt)(v.index)(v1.index);
        };
    },
    Eq0: function () {
        return eqPosition;
    }
};
var applyParserT = {
    apply: function (v) {
        return function (v1) {
            return function (state1, more, lift, $$throw, done) {
                return more(function (v2) {
                    return v(state1, more, lift, $$throw, function (state2, f) {
                        return more(function (v3) {
                            return v1(state2, more, lift, $$throw, function (state3, a) {
                                return more(function (v4) {
                                    return done(state3, f(a));
                                });
                            });
                        });
                    });
                });
            };
        };
    },
    Functor0: function () {
        return functorParserT;
    }
};
var bindParserT = {
    bind: function (v) {
        return function (next) {
            return function (state1, more, lift, $$throw, done) {
                return more(function (v1) {
                    return v(state1, more, lift, $$throw, function (state2, a) {
                        return more(function (v2) {
                            var v3 = next(a);
                            return v3(state2, more, lift, $$throw, done);
                        });
                    });
                });
            };
        };
    },
    Apply0: function () {
        return applyParserT;
    }
};
var semigroupParserT = function (dictSemigroup) {
    return {
        append: Control_Apply.lift2(applyParserT)(Data_Semigroup.append(dictSemigroup))
    };
};
var applicativeParserT = {
    pure: function (a) {
        return function (state1, v, v1, v2, done) {
            return done(state1, a);
        };
    },
    Apply0: function () {
        return applyParserT;
    }
};
var monadParserT = {
    Applicative0: function () {
        return applicativeParserT;
    },
    Bind1: function () {
        return bindParserT;
    }
};
var monadRecParserT = {
    tailRecM: function (next) {
        return function (initArg) {
            return function (state1, more, lift, $$throw, done) {
                var $lazy_loop = $runtime_lazy("loop", "Parsing", function () {
                    return function (state2, arg, gas) {
                        var v = next(arg);
                        return v(state2, more, lift, $$throw, function (state3, step) {
                            if (step instanceof Control_Monad_Rec_Class.Loop) {
                                var $120 = gas === 0;
                                if ($120) {
                                    return more(function (v1) {
                                        return $lazy_loop(269)(state3, step.value0, 30);
                                    });
                                };
                                return $lazy_loop(271)(state3, step.value0, gas - 1 | 0);
                            };
                            if (step instanceof Control_Monad_Rec_Class.Done) {
                                return done(state3, step.value0);
                            };
                            throw new Error("Failed pattern match at Parsing (line 265, column 39 - line 273, column 43): " + [ step.constructor.name ]);
                        });
                    };
                });
                var loop = $lazy_loop(262);
                return loop(state1, initArg, 30);
            };
        };
    },
    Monad0: function () {
        return monadParserT;
    }
};
var monadStateParserT = function (dictMonadState) {
    return {
        state: function (k) {
            return Control_Monad_Trans_Class.lift(monadTransParserT)(dictMonadState.Monad0())(Control_Monad_State_Class.state(dictMonadState)(k));
        },
        Monad0: function () {
            return monadParserT;
        }
    };
};
var monadThrowParseErrorParse = {
    throwError: function (err) {
        return function (state1, v, v1, $$throw, v2) {
            return $$throw(state1, err);
        };
    },
    Monad0: function () {
        return monadParserT;
    }
};
var monadErrorParseErrorParse = {
    catchError: function (v) {
        return function (next) {
            return function (state1, more, lift, $$throw, done) {
                return more(function (v1) {
                    return v(state1, more, lift, function (state2, err) {
                        var v2 = next(err);
                        return v2(state2, more, lift, $$throw, done);
                    }, done);
                });
            };
        };
    },
    MonadThrow0: function () {
        return monadThrowParseErrorParse;
    }
};
var monoidParserT = function (dictMonoid) {
    return {
        mempty: Control_Applicative.pure(applicativeParserT)(Data_Monoid.mempty(dictMonoid)),
        Semigroup0: function () {
            return semigroupParserT(dictMonoid.Semigroup0());
        }
    };
};

// | The alternative `Alt` instance provides the `alt` combinator `<|>`.
// |
// | The expression `p_left <|> p_right` will first try the `p_left` parser and if that fails
// | __and consumes no input__ then it will try the `p_right` parser.
// |
// | While we are parsing down the `p_left` branch we may reach a point where
// | we know this is the correct branch, but we cannot parse further. At
// | that point we want to fail the entire parse instead of trying the `p_right`
// | branch.
// |
// | For example, consider this `fileParser` which can parse either an HTML
// | file that begins with `<html>` or a shell script file that begins with `#!`.
// |
// | ```
// | fileParser =
// |   string "<html>" *> parseTheRestOfTheHtml
// |   <|>
// |   string "#!" *> parseTheRestOfTheScript
// | ```
// |
// | If we read a file from disk and run this `fileParser` on it and the
// | `string "<html>"` parser succeeds, then we know that the first branch
// | is the correct branch, so we want to commit to the first branch.
// | Even if the `parseTheRestOfTheHtml` parser fails
// | we don’t want to try the second branch.
// |
// | To control the point at which we commit to the `p_left` branch
// | use the `try` combinator and the `lookAhead` combinator and
// | the `consume` function.
// |
// | The `alt` combinator works this way because it gives us good localized
// | error messages while also allowing an efficient implementation. See
// | [*Parsec: Direct Style Monadic Parser Combinators For The Real World*](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf)
// | section __2.3 Backtracking__.
var altParserT = {
    alt: function (v) {
        return function (v1) {
            return function (v2, more, lift, $$throw, done) {
                return more(function (v3) {
                    return v(new ParseState(v2.value0, v2.value1, false), more, lift, function (v4, err) {
                        return more(function (v5) {
                            if (v4.value2) {
                                return $$throw(v4, err);
                            };
                            return v1(v2, more, lift, $$throw, done);
                        });
                    }, done);
                });
            };
        };
    },
    Functor0: function () {
        return functorParserT;
    }
};

// | Query and modify the `ParserT` internal state.
// |
// | Like the `state` member of `MonadState`.
var stateParserT = function (k) {
    return function (state1, v, v1, v2, done) {
        var v3 = k(state1);
        return done(v3.value1, v3.value0);
    };
};
var showParseError = {
    show: function (v) {
        return "(ParseError " + (Data_Show.show(Data_Show.showString)(v.value0) + (" " + (Data_Show.show(showPosition)(v.value1) + ")")));
    }
};

// | Run a parser and produce either an error or the result of the parser
// | along with the internal state of the parser when it finishes.
var runParserT$prime = function (dictMonadRec) {
    return function (state1) {
        return function (v) {
            var go = function ($copy_step) {
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(step) {
                    var v1 = step(Data_Unit.unit);
                    if (v1 instanceof More) {
                        $copy_step = v1.value0;
                        return;
                    };
                    if (v1 instanceof Lift) {
                        $tco_done = true;
                        return Data_Functor.map((((dictMonadRec.Monad0()).Bind1()).Apply0()).Functor0())(Control_Monad_Rec_Class.Loop.create)(v1.value0);
                    };
                    if (v1 instanceof Stop) {
                        $tco_done = true;
                        return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(v1.value1, v1.value0)));
                    };
                    throw new Error("Failed pattern match at Parsing (line 144, column 13 - line 150, column 32): " + [ v1.constructor.name ]);
                };
                while (!$tco_done) {
                    $tco_result = $tco_loop($copy_step);
                };
                return $tco_result;
            };
            return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go)(function (v1) {
                return v(state1, More.create, Lift.create, function (state2, err) {
                    return new Stop(state2, new Data_Either.Left(err));
                }, function (state2, res) {
                    return new Stop(state2, new Data_Either.Right(res));
                });
            });
        };
    };
};

// | Contextualize parsing failures inside a region. If a parsing failure
// | occurs, then the `ParseError` will be transformed by each containing
// | `region` as the parser backs out the call stack.
var region = function (context) {
    return function (p) {
        return Control_Monad_Error_Class.catchError(monadErrorParseErrorParse)(p)(function (err) {
            return Control_Monad_Error_Class.throwError(monadThrowParseErrorParse)(context(err));
        });
    };
};

// | Returns the current position in the stream.
var position = /* #__PURE__ */ stateParserT(function (v) {
    return new Data_Tuple.Tuple(v.value1, v);
});
var parseErrorPosition = function (v) {
    return v.value1;
};
var parseErrorMessage = function (v) {
    return v.value0;
};

// | Change the underlying monad action `m` and result data type `a` in
// | a `ParserT s m` monad action.
var mapParserT = function (dictMonadRec) {
    return function (dictFunctor) {
        return function (f) {
            return function (p) {
                return function (state1, v, lift, $$throw, done) {
                    return lift(Data_Functor.map(dictFunctor)(function (v1) {
                        return function (v2) {
                            if (v1.value0 instanceof Data_Either.Left) {
                                return $$throw(v1.value1, v1.value0.value0);
                            };
                            if (v1.value0 instanceof Data_Either.Right) {
                                return done(v1.value1, v1.value0.value0);
                            };
                            throw new Error("Failed pattern match at Parsing (line 184, column 13 - line 188, column 37): " + [ v1.value0.constructor.name ]);
                        };
                    })(f(runParserT$prime(dictMonadRec)(state1)(p))));
                };
            };
        };
    };
};

// | The `Position` before any input has been parsed.
// |
// | `{ index: 0, line: 1, column: 1 }`
var initialPos = {
    index: 0,
    line: 1,
    column: 1
};

// | `runParser` with a monad transfomer parameter `m`.
var runParserT = function (dictMonadRec) {
    return function (s) {
        return function (p) {
            var initialState = new ParseState(s, initialPos, false);
            return Data_Functor.map((((dictMonadRec.Monad0()).Bind1()).Apply0()).Functor0())(Data_Tuple.fst)(runParserT$prime(dictMonadRec)(initialState)(p));
        };
    };
};

// | Run a parser on an input stream `s` and produce either an error or the
// | result `a` of the parser.
var runParser = function (s) {
    var $185 = Data_Newtype.unwrap();
    var $186 = runParserT(Control_Monad_Rec_Class.monadRecIdentity)(s);
    return function ($187) {
        return $185($186($187));
    };
};
var hoistParserT = function (f) {
    return function (v) {
        return function (state1, more, lift, $$throw, done) {
            return v(state1, more, function ($188) {
                return lift(f($188));
            }, $$throw, done);
        };
    };
};

// | Query the `ParserT` internal state.
// |
// | Like the `get` member of `MonadState`.
var getParserT = function (state1, v, v1, v2, done) {
    return done(state1, state1);
};

// | Fail with a message and a position.
var failWithPosition = function (message) {
    return function (pos) {
        return Control_Monad_Error_Class.throwError(monadThrowParseErrorParse)(new ParseError(message, pos));
    };
};

// | Fail with a message.
var fail = function (message) {
    return Control_Bind.bindFlipped(bindParserT)(failWithPosition(message))(position);
};
var plusParserT = {
    empty: /* #__PURE__ */ fail("No alternative"),
    Alt0: function () {
        return altParserT;
    }
};
var alternativeParserT = {
    Applicative0: function () {
        return applicativeParserT;
    },
    Plus1: function () {
        return plusParserT;
    }
};
var monadPlusParserT = {
    Monad0: function () {
        return monadParserT;
    },
    Alternative1: function () {
        return alternativeParserT;
    }
};
var eqParseError = {
    eq: function (x) {
        return function (y) {
            return x.value0 === y.value0 && Data_Eq.eq(eqPosition)(x.value1)(y.value1);
        };
    }
};
var ordParseError = {
    compare: function (x) {
        return function (y) {
            var v = Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
            if (v instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            return Data_Ord.compare(ordPosition)(x.value1)(y.value1);
        };
    },
    Eq0: function () {
        return eqParseError;
    }
};

// | Set the consumed flag.
// |
// | Setting the consumed flag means that we're committed to this parsing branch
// | of an alternative (`<|>`), so that if this branch fails then we want to
// | fail the entire parse instead of trying the other alternative.
var consume = /* #__PURE__ */ stateParserT(function (v) {
    return new Data_Tuple.Tuple(Data_Unit.unit, new ParseState(v.value0, v.value1, true));
});
export {
    runParser,
    ParserT,
    runParserT,
    runParserT$prime,
    ParseError,
    parseErrorMessage,
    parseErrorPosition,
    Position,
    initialPos,
    consume,
    position,
    fail,
    failWithPosition,
    region,
    ParseState,
    stateParserT,
    getParserT,
    hoistParserT,
    mapParserT,
    showParseError,
    eqParseError,
    ordParseError,
    lazyParserT,
    semigroupParserT,
    monoidParserT,
    functorParserT,
    applyParserT,
    applicativeParserT,
    bindParserT,
    monadParserT,
    monadRecParserT,
    monadStateParserT,
    monadThrowParseErrorParse,
    monadErrorParseErrorParse,
    altParserT,
    plusParserT,
    alternativeParserT,
    monadPlusParserT,
    monadTransParserT,
    genericPosition_,
    showPosition,
    eqPosition,
    ordPosition
};
