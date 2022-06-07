// | This module defines the `MaybeT` monad transformer.
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Monad from "../Control.Monad/index.js";
import * as Control_Monad_Cont_Class from "../Control.Monad.Cont.Class/index.js";
import * as Control_Monad_Error_Class from "../Control.Monad.Error.Class/index.js";
import * as Control_Monad_Reader_Class from "../Control.Monad.Reader.Class/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Control_Monad_State_Class from "../Control.Monad.State.Class/index.js";
import * as Control_Monad_Trans_Class from "../Control.Monad.Trans.Class/index.js";
import * as Control_Monad_Writer_Class from "../Control.Monad.Writer.Class/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Effect_Class from "../Effect.Class/index.js";

// | The `MaybeT` monad transformer.
// |
// | This monad transformer extends the base monad, supporting failure and alternation via
// | the `MonadPlus` type class.
var MaybeT = function (x) {
    return x;
};

// | Run a computation in the `MaybeT` monad.
var runMaybeT = function (v) {
    return v;
};
var newtypeMaybeT = {
    Coercible0: function () {
        return undefined;
    }
};
var monadTransMaybeT = {
    lift: function (dictMonad) {
        var $71 = Control_Monad.liftM1(dictMonad)(Data_Maybe.Just.create);
        return function ($72) {
            return MaybeT($71($72));
        };
    }
};

// | Change the result type of a `MaybeT` monad action.
var mapMaybeT = function (f) {
    return function (v) {
        return f(v);
    };
};
var functorMaybeT = function (dictFunctor) {
    return {
        map: function (f) {
            return function (v) {
                return Data_Functor.map(dictFunctor)(Data_Functor.map(Data_Maybe.functorMaybe)(f))(v);
            };
        }
    };
};
var monadMaybeT = function (dictMonad) {
    return {
        Applicative0: function () {
            return applicativeMaybeT(dictMonad);
        },
        Bind1: function () {
            return bindMaybeT(dictMonad);
        }
    };
};
var bindMaybeT = function (dictMonad) {
    return {
        bind: function (v) {
            return function (f) {
                return Control_Bind.bind(dictMonad.Bind1())(v)(function (v1) {
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return Control_Applicative.pure(dictMonad.Applicative0())(Data_Maybe.Nothing.value);
                    };
                    if (v1 instanceof Data_Maybe.Just) {
                        var v2 = f(v1.value0);
                        return v2;
                    };
                    throw new Error("Failed pattern match at Control.Monad.Maybe.Trans (line 54, column 11 - line 56, column 42): " + [ v1.constructor.name ]);
                });
            };
        },
        Apply0: function () {
            return applyMaybeT(dictMonad);
        }
    };
};
var applyMaybeT = function (dictMonad) {
    return {
        apply: Control_Monad.ap(monadMaybeT(dictMonad)),
        Functor0: function () {
            return functorMaybeT(((dictMonad.Bind1()).Apply0()).Functor0());
        }
    };
};
var applicativeMaybeT = function (dictMonad) {
    return {
        pure: (function () {
            var $73 = Control_Applicative.pure(dictMonad.Applicative0());
            return function ($74) {
                return MaybeT($73(Data_Maybe.Just.create($74)));
            };
        })(),
        Apply0: function () {
            return applyMaybeT(dictMonad);
        }
    };
};
var semigroupMaybeT = function (dictMonad) {
    return function (dictSemigroup) {
        return {
            append: Control_Apply.lift2(applyMaybeT(dictMonad))(Data_Semigroup.append(dictSemigroup))
        };
    };
};
var monadAskMaybeT = function (dictMonadAsk) {
    return {
        ask: Control_Monad_Trans_Class.lift(monadTransMaybeT)(dictMonadAsk.Monad0())(Control_Monad_Reader_Class.ask(dictMonadAsk)),
        Monad0: function () {
            return monadMaybeT(dictMonadAsk.Monad0());
        }
    };
};
var monadReaderMaybeT = function (dictMonadReader) {
    return {
        local: function (f) {
            return mapMaybeT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
        },
        MonadAsk0: function () {
            return monadAskMaybeT(dictMonadReader.MonadAsk0());
        }
    };
};
var monadContMaybeT = function (dictMonadCont) {
    return {
        callCC: function (f) {
            return Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
                var v = f(function (a) {
                    return c(new Data_Maybe.Just(a));
                });
                return v;
            });
        },
        Monad0: function () {
            return monadMaybeT(dictMonadCont.Monad0());
        }
    };
};
var monadEffectMaybe = function (dictMonadEffect) {
    return {
        liftEffect: (function () {
            var $75 = Control_Monad_Trans_Class.lift(monadTransMaybeT)(dictMonadEffect.Monad0());
            var $76 = Effect_Class.liftEffect(dictMonadEffect);
            return function ($77) {
                return $75($76($77));
            };
        })(),
        Monad0: function () {
            return monadMaybeT(dictMonadEffect.Monad0());
        }
    };
};
var monadRecMaybeT = function (dictMonadRec) {
    return {
        tailRecM: function (f) {
            var $78 = Control_Monad_Rec_Class.tailRecM(dictMonadRec)(function (a) {
                var v = f(a);
                return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())(v)(function (m$prime) {
                    return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())((function () {
                        if (m$prime instanceof Data_Maybe.Nothing) {
                            return new Control_Monad_Rec_Class.Done(Data_Maybe.Nothing.value);
                        };
                        if (m$prime instanceof Data_Maybe.Just && m$prime.value0 instanceof Control_Monad_Rec_Class.Loop) {
                            return new Control_Monad_Rec_Class.Loop(m$prime.value0.value0);
                        };
                        if (m$prime instanceof Data_Maybe.Just && m$prime.value0 instanceof Control_Monad_Rec_Class.Done) {
                            return new Control_Monad_Rec_Class.Done(new Data_Maybe.Just(m$prime.value0.value0));
                        };
                        throw new Error("Failed pattern match at Control.Monad.Maybe.Trans (line 82, column 16 - line 85, column 43): " + [ m$prime.constructor.name ]);
                    })());
                });
            });
            return function ($79) {
                return MaybeT($78($79));
            };
        },
        Monad0: function () {
            return monadMaybeT(dictMonadRec.Monad0());
        }
    };
};
var monadStateMaybeT = function (dictMonadState) {
    return {
        state: function (f) {
            return Control_Monad_Trans_Class.lift(monadTransMaybeT)(dictMonadState.Monad0())(Control_Monad_State_Class.state(dictMonadState)(f));
        },
        Monad0: function () {
            return monadMaybeT(dictMonadState.Monad0());
        }
    };
};
var monadTellMaybeT = function (dictMonadTell) {
    return {
        tell: (function () {
            var $80 = Control_Monad_Trans_Class.lift(monadTransMaybeT)(dictMonadTell.Monad1());
            var $81 = Control_Monad_Writer_Class.tell(dictMonadTell);
            return function ($82) {
                return $80($81($82));
            };
        })(),
        Semigroup0: dictMonadTell.Semigroup0,
        Monad1: function () {
            return monadMaybeT(dictMonadTell.Monad1());
        }
    };
};
var monadWriterMaybeT = function (dictMonadWriter) {
    return {
        listen: mapMaybeT(function (m) {
            return Control_Bind.bind(((dictMonadWriter.MonadTell1()).Monad1()).Bind1())(Control_Monad_Writer_Class.listen(dictMonadWriter)(m))(function (v) {
                return Control_Applicative.pure(((dictMonadWriter.MonadTell1()).Monad1()).Applicative0())(Data_Functor.map(Data_Maybe.functorMaybe)(function (r) {
                    return new Data_Tuple.Tuple(r, v.value1);
                })(v.value0));
            });
        }),
        pass: mapMaybeT(function (m) {
            return Control_Monad_Writer_Class.pass(dictMonadWriter)(Control_Bind.bind(((dictMonadWriter.MonadTell1()).Monad1()).Bind1())(m)(function (a) {
                return Control_Applicative.pure(((dictMonadWriter.MonadTell1()).Monad1()).Applicative0())((function () {
                    if (a instanceof Data_Maybe.Nothing) {
                        return new Data_Tuple.Tuple(Data_Maybe.Nothing.value, Control_Category.identity(Control_Category.categoryFn));
                    };
                    if (a instanceof Data_Maybe.Just) {
                        return new Data_Tuple.Tuple(new Data_Maybe.Just(a.value0.value0), a.value0.value1);
                    };
                    throw new Error("Failed pattern match at Control.Monad.Maybe.Trans (line 119, column 10 - line 121, column 43): " + [ a.constructor.name ]);
                })());
            }));
        }),
        Monoid0: dictMonadWriter.Monoid0,
        MonadTell1: function () {
            return monadTellMaybeT(dictMonadWriter.MonadTell1());
        }
    };
};
var monadThrowMaybeT = function (dictMonadThrow) {
    return {
        throwError: function (e) {
            return Control_Monad_Trans_Class.lift(monadTransMaybeT)(dictMonadThrow.Monad0())(Control_Monad_Error_Class.throwError(dictMonadThrow)(e));
        },
        Monad0: function () {
            return monadMaybeT(dictMonadThrow.Monad0());
        }
    };
};
var monadErrorMaybeT = function (dictMonadError) {
    return {
        catchError: function (v) {
            return function (h) {
                return Control_Monad_Error_Class.catchError(dictMonadError)(v)(function (a) {
                    var v1 = h(a);
                    return v1;
                });
            };
        },
        MonadThrow0: function () {
            return monadThrowMaybeT(dictMonadError.MonadThrow0());
        }
    };
};
var monoidMaybeT = function (dictMonad) {
    return function (dictMonoid) {
        return {
            mempty: Control_Applicative.pure(applicativeMaybeT(dictMonad))(Data_Monoid.mempty(dictMonoid)),
            Semigroup0: function () {
                return semigroupMaybeT(dictMonad)(dictMonoid.Semigroup0());
            }
        };
    };
};
var altMaybeT = function (dictMonad) {
    return {
        alt: function (v) {
            return function (v1) {
                return Control_Bind.bind(dictMonad.Bind1())(v)(function (m) {
                    if (m instanceof Data_Maybe.Nothing) {
                        return v1;
                    };
                    return Control_Applicative.pure(dictMonad.Applicative0())(m);
                });
            };
        },
        Functor0: function () {
            return functorMaybeT(((dictMonad.Bind1()).Apply0()).Functor0());
        }
    };
};
var plusMaybeT = function (dictMonad) {
    return {
        empty: Control_Applicative.pure(dictMonad.Applicative0())(Data_Maybe.Nothing.value),
        Alt0: function () {
            return altMaybeT(dictMonad);
        }
    };
};
var alternativeMaybeT = function (dictMonad) {
    return {
        Applicative0: function () {
            return applicativeMaybeT(dictMonad);
        },
        Plus1: function () {
            return plusMaybeT(dictMonad);
        }
    };
};
var monadPlusMaybeT = function (dictMonad) {
    return {
        Monad0: function () {
            return monadMaybeT(dictMonad);
        },
        Alternative1: function () {
            return alternativeMaybeT(dictMonad);
        }
    };
};
export {
    MaybeT,
    runMaybeT,
    mapMaybeT,
    newtypeMaybeT,
    functorMaybeT,
    applyMaybeT,
    applicativeMaybeT,
    bindMaybeT,
    monadMaybeT,
    monadTransMaybeT,
    altMaybeT,
    plusMaybeT,
    alternativeMaybeT,
    monadPlusMaybeT,
    monadRecMaybeT,
    monadEffectMaybe,
    monadContMaybeT,
    monadThrowMaybeT,
    monadErrorMaybeT,
    monadAskMaybeT,
    monadReaderMaybeT,
    monadStateMaybeT,
    monadTellMaybeT,
    monadWriterMaybeT,
    semigroupMaybeT,
    monoidMaybeT
};
export {
    lift
} from "../Control.Monad.Trans.Class/index.js";
