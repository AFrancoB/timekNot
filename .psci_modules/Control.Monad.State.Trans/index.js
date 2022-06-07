// | This module defines the state monad transformer, `StateT`.
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad from "../Control.Monad/index.js";
import * as Control_Monad_Cont_Class from "../Control.Monad.Cont.Class/index.js";
import * as Control_Monad_Error_Class from "../Control.Monad.Error.Class/index.js";
import * as Control_Monad_Reader_Class from "../Control.Monad.Reader.Class/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Control_Monad_State_Class from "../Control.Monad.State.Class/index.js";
import * as Control_Monad_Trans_Class from "../Control.Monad.Trans.Class/index.js";
import * as Control_Monad_Writer_Class from "../Control.Monad.Writer.Class/index.js";
import * as Control_Plus from "../Control.Plus/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Effect_Class from "../Effect.Class/index.js";

// | The state monad transformer.
// |
// | This monad transformer extends the base monad with the operations `get`
// | and `put` which can be used to model a single piece of mutable state.
// |
// | The `MonadState` type class describes the operations supported by this monad.
var StateT = function (x) {
    return x;
};

// | Modify the final state in a `StateT` monad action.
var withStateT = function (f) {
    return function (v) {
        return function ($105) {
            return v(f($105));
        };
    };
};

// | Run a computation in the `StateT` monad.
var runStateT = function (v) {
    return v;
};
var newtypeStateT = {
    Coercible0: function () {
        return undefined;
    }
};
var monadTransStateT = {
    lift: function (dictMonad) {
        return function (m) {
            return function (s) {
                return Control_Bind.bind(dictMonad.Bind1())(m)(function (x) {
                    return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(x, s));
                });
            };
        };
    }
};

// | Change the result type in a `StateT` monad action.
var mapStateT = function (f) {
    return function (v) {
        return function ($106) {
            return f(v($106));
        };
    };
};
var lazyStateT = {
    defer: function (f) {
        return function (s) {
            var v = f(Data_Unit.unit);
            return v(s);
        };
    }
};
var functorStateT = function (dictFunctor) {
    return {
        map: function (f) {
            return function (v) {
                return function (s) {
                    return Data_Functor.map(dictFunctor)(function (v1) {
                        return new Data_Tuple.Tuple(f(v1.value0), v1.value1);
                    })(v(s));
                };
            };
        }
    };
};

// | Run a computation in the `StateT` monad discarding the result.
var execStateT = function (dictFunctor) {
    return function (v) {
        return function (s) {
            return Data_Functor.map(dictFunctor)(Data_Tuple.snd)(v(s));
        };
    };
};

// | Run a computation in the `StateT` monad, discarding the final state.
var evalStateT = function (dictFunctor) {
    return function (v) {
        return function (s) {
            return Data_Functor.map(dictFunctor)(Data_Tuple.fst)(v(s));
        };
    };
};
var monadStateT = function (dictMonad) {
    return {
        Applicative0: function () {
            return applicativeStateT(dictMonad);
        },
        Bind1: function () {
            return bindStateT(dictMonad);
        }
    };
};
var bindStateT = function (dictMonad) {
    return {
        bind: function (v) {
            return function (f) {
                return function (s) {
                    return Control_Bind.bind(dictMonad.Bind1())(v(s))(function (v1) {
                        var v3 = f(v1.value0);
                        return v3(v1.value1);
                    });
                };
            };
        },
        Apply0: function () {
            return applyStateT(dictMonad);
        }
    };
};
var applyStateT = function (dictMonad) {
    return {
        apply: Control_Monad.ap(monadStateT(dictMonad)),
        Functor0: function () {
            return functorStateT(((dictMonad.Bind1()).Apply0()).Functor0());
        }
    };
};
var applicativeStateT = function (dictMonad) {
    return {
        pure: function (a) {
            return function (s) {
                return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(a, s));
            };
        },
        Apply0: function () {
            return applyStateT(dictMonad);
        }
    };
};
var semigroupStateT = function (dictMonad) {
    return function (dictSemigroup) {
        return {
            append: Control_Apply.lift2(applyStateT(dictMonad))(Data_Semigroup.append(dictSemigroup))
        };
    };
};
var monadAskStateT = function (dictMonadAsk) {
    return {
        ask: Control_Monad_Trans_Class.lift(monadTransStateT)(dictMonadAsk.Monad0())(Control_Monad_Reader_Class.ask(dictMonadAsk)),
        Monad0: function () {
            return monadStateT(dictMonadAsk.Monad0());
        }
    };
};
var monadReaderStateT = function (dictMonadReader) {
    return {
        local: (function () {
            var $107 = Control_Monad_Reader_Class.local(dictMonadReader);
            return function ($108) {
                return mapStateT($107($108));
            };
        })(),
        MonadAsk0: function () {
            return monadAskStateT(dictMonadReader.MonadAsk0());
        }
    };
};
var monadContStateT = function (dictMonadCont) {
    return {
        callCC: function (f) {
            return function (s) {
                return Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
                    var v = f(function (a) {
                        return function (s$prime) {
                            return c(new Data_Tuple.Tuple(a, s$prime));
                        };
                    });
                    return v(s);
                });
            };
        },
        Monad0: function () {
            return monadStateT(dictMonadCont.Monad0());
        }
    };
};
var monadEffectState = function (dictMonadEffect) {
    return {
        liftEffect: (function () {
            var $109 = Control_Monad_Trans_Class.lift(monadTransStateT)(dictMonadEffect.Monad0());
            var $110 = Effect_Class.liftEffect(dictMonadEffect);
            return function ($111) {
                return $109($110($111));
            };
        })(),
        Monad0: function () {
            return monadStateT(dictMonadEffect.Monad0());
        }
    };
};
var monadRecStateT = function (dictMonadRec) {
    return {
        tailRecM: function (f) {
            return function (a) {
                var f$prime = function (v) {
                    var v1 = f(v.value0);
                    return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())(v1(v.value1))(function (v2) {
                        return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())((function () {
                            if (v2.value0 instanceof Control_Monad_Rec_Class.Loop) {
                                return new Control_Monad_Rec_Class.Loop(new Data_Tuple.Tuple(v2.value0.value0, v2.value1));
                            };
                            if (v2.value0 instanceof Control_Monad_Rec_Class.Done) {
                                return new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(v2.value0.value0, v2.value1));
                            };
                            throw new Error("Failed pattern match at Control.Monad.State.Trans (line 87, column 16 - line 89, column 40): " + [ v2.value0.constructor.name ]);
                        })());
                    });
                };
                return function (s) {
                    return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(f$prime)(new Data_Tuple.Tuple(a, s));
                };
            };
        },
        Monad0: function () {
            return monadStateT(dictMonadRec.Monad0());
        }
    };
};
var monadStateStateT = function (dictMonad) {
    return {
        state: function (f) {
            var $112 = Control_Applicative.pure(dictMonad.Applicative0());
            return function ($113) {
                return $112(f($113));
            };
        },
        Monad0: function () {
            return monadStateT(dictMonad);
        }
    };
};
var monadTellStateT = function (dictMonadTell) {
    return {
        tell: (function () {
            var $114 = Control_Monad_Trans_Class.lift(monadTransStateT)(dictMonadTell.Monad1());
            var $115 = Control_Monad_Writer_Class.tell(dictMonadTell);
            return function ($116) {
                return $114($115($116));
            };
        })(),
        Semigroup0: dictMonadTell.Semigroup0,
        Monad1: function () {
            return monadStateT(dictMonadTell.Monad1());
        }
    };
};
var monadWriterStateT = function (dictMonadWriter) {
    return {
        listen: function (m) {
            return function (s) {
                return Control_Bind.bind(((dictMonadWriter.MonadTell1()).Monad1()).Bind1())(Control_Monad_Writer_Class.listen(dictMonadWriter)(m(s)))(function (v) {
                    return Control_Applicative.pure(((dictMonadWriter.MonadTell1()).Monad1()).Applicative0())(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value0, v.value1), v.value0.value1));
                });
            };
        },
        pass: function (m) {
            return function (s) {
                return Control_Monad_Writer_Class.pass(dictMonadWriter)(Control_Bind.bind(((dictMonadWriter.MonadTell1()).Monad1()).Bind1())(m(s))(function (v) {
                    return Control_Applicative.pure(((dictMonadWriter.MonadTell1()).Monad1()).Applicative0())(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value0, v.value1), v.value0.value1));
                }));
            };
        },
        Monoid0: dictMonadWriter.Monoid0,
        MonadTell1: function () {
            return monadTellStateT(dictMonadWriter.MonadTell1());
        }
    };
};
var monadThrowStateT = function (dictMonadThrow) {
    return {
        throwError: function (e) {
            return Control_Monad_Trans_Class.lift(monadTransStateT)(dictMonadThrow.Monad0())(Control_Monad_Error_Class.throwError(dictMonadThrow)(e));
        },
        Monad0: function () {
            return monadStateT(dictMonadThrow.Monad0());
        }
    };
};
var monadErrorStateT = function (dictMonadError) {
    return {
        catchError: function (v) {
            return function (h) {
                return function (s) {
                    return Control_Monad_Error_Class.catchError(dictMonadError)(v(s))(function (e) {
                        var v1 = h(e);
                        return v1(s);
                    });
                };
            };
        },
        MonadThrow0: function () {
            return monadThrowStateT(dictMonadError.MonadThrow0());
        }
    };
};
var monoidStateT = function (dictMonad) {
    return function (dictMonoid) {
        return {
            mempty: Control_Applicative.pure(applicativeStateT(dictMonad))(Data_Monoid.mempty(dictMonoid)),
            Semigroup0: function () {
                return semigroupStateT(dictMonad)(dictMonoid.Semigroup0());
            }
        };
    };
};
var altStateT = function (dictMonad) {
    return function (dictAlt) {
        return {
            alt: function (v) {
                return function (v1) {
                    return function (s) {
                        return Control_Alt.alt(dictAlt)(v(s))(v1(s));
                    };
                };
            },
            Functor0: function () {
                return functorStateT(dictAlt.Functor0());
            }
        };
    };
};
var plusStateT = function (dictMonad) {
    return function (dictPlus) {
        return {
            empty: function (v) {
                return Control_Plus.empty(dictPlus);
            },
            Alt0: function () {
                return altStateT(dictMonad)(dictPlus.Alt0());
            }
        };
    };
};
var alternativeStateT = function (dictMonad) {
    return function (dictAlternative) {
        return {
            Applicative0: function () {
                return applicativeStateT(dictMonad);
            },
            Plus1: function () {
                return plusStateT(dictMonad)(dictAlternative.Plus1());
            }
        };
    };
};
var monadPlusStateT = function (dictMonadPlus) {
    return {
        Monad0: function () {
            return monadStateT(dictMonadPlus.Monad0());
        },
        Alternative1: function () {
            return alternativeStateT(dictMonadPlus.Monad0())(dictMonadPlus.Alternative1());
        }
    };
};
export {
    StateT,
    runStateT,
    evalStateT,
    execStateT,
    mapStateT,
    withStateT,
    newtypeStateT,
    functorStateT,
    applyStateT,
    applicativeStateT,
    altStateT,
    plusStateT,
    alternativeStateT,
    bindStateT,
    monadStateT,
    monadRecStateT,
    monadPlusStateT,
    monadTransStateT,
    lazyStateT,
    monadEffectState,
    monadContStateT,
    monadThrowStateT,
    monadErrorStateT,
    monadAskStateT,
    monadReaderStateT,
    monadStateStateT,
    monadTellStateT,
    monadWriterStateT,
    semigroupStateT,
    monoidStateT
};
export {
    get,
    gets,
    modify,
    modify_,
    put,
    state
} from "../Control.Monad.State.Class/index.js";
export {
    lift
} from "../Control.Monad.Trans.Class/index.js";
