// | This module defines the writer monad transformer, `WriterT`.
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
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

// | The writer monad transformer.
// |
// | This monad transformer extends the base monad with a monoidal accumulator of
// | type `w`.
// |
// | The `MonadWriter` type class describes the operations supported by this monad.
var WriterT = function (x) {
    return x;
};

// | Run a computation in the `WriterT` monad.
var runWriterT = function (v) {
    return v;
};
var newtypeWriterT = {
    Coercible0: function () {
        return undefined;
    }
};
var monadTransWriterT = function (dictMonoid) {
    return {
        lift: function (dictMonad) {
            return function (m) {
                return Control_Bind.bind(dictMonad.Bind1())(m)(function (a) {
                    return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid)));
                });
            };
        }
    };
};

// | Change the accumulator and base monad types in a `WriterT` monad action.
var mapWriterT = function (f) {
    return function (v) {
        return f(v);
    };
};
var functorWriterT = function (dictFunctor) {
    return {
        map: function (f) {
            return mapWriterT(Data_Functor.map(dictFunctor)(function (v) {
                return new Data_Tuple.Tuple(f(v.value0), v.value1);
            }));
        }
    };
};

// | Run a computation in the `WriterT` monad, discarding the result.
var execWriterT = function (dictFunctor) {
    return function (v) {
        return Data_Functor.map(dictFunctor)(Data_Tuple.snd)(v);
    };
};
var applyWriterT = function (dictSemigroup) {
    return function (dictApply) {
        return {
            apply: function (v) {
                return function (v1) {
                    var k = function (v3) {
                        return function (v4) {
                            return new Data_Tuple.Tuple(v3.value0(v4.value0), Data_Semigroup.append(dictSemigroup)(v3.value1)(v4.value1));
                        };
                    };
                    return Control_Apply.apply(dictApply)(Data_Functor.map(dictApply.Functor0())(k)(v))(v1);
                };
            },
            Functor0: function () {
                return functorWriterT(dictApply.Functor0());
            }
        };
    };
};
var bindWriterT = function (dictSemigroup) {
    return function (dictBind) {
        return {
            bind: function (v) {
                return function (k) {
                    return Control_Bind.bind(dictBind)(v)(function (v1) {
                        var v2 = k(v1.value0);
                        return Data_Functor.map((dictBind.Apply0()).Functor0())(function (v3) {
                            return new Data_Tuple.Tuple(v3.value0, Data_Semigroup.append(dictSemigroup)(v1.value1)(v3.value1));
                        })(v2);
                    });
                };
            },
            Apply0: function () {
                return applyWriterT(dictSemigroup)(dictBind.Apply0());
            }
        };
    };
};
var semigroupWriterT = function (dictApply) {
    return function (dictSemigroup) {
        return function (dictSemigroup1) {
            return {
                append: Control_Apply.lift2(applyWriterT(dictSemigroup)(dictApply))(Data_Semigroup.append(dictSemigroup1))
            };
        };
    };
};
var applicativeWriterT = function (dictMonoid) {
    return function (dictApplicative) {
        return {
            pure: function (a) {
                return Control_Applicative.pure(dictApplicative)(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid)));
            },
            Apply0: function () {
                return applyWriterT(dictMonoid.Semigroup0())(dictApplicative.Apply0());
            }
        };
    };
};
var monadWriterT = function (dictMonoid) {
    return function (dictMonad) {
        return {
            Applicative0: function () {
                return applicativeWriterT(dictMonoid)(dictMonad.Applicative0());
            },
            Bind1: function () {
                return bindWriterT(dictMonoid.Semigroup0())(dictMonad.Bind1());
            }
        };
    };
};
var monadAskWriterT = function (dictMonoid) {
    return function (dictMonadAsk) {
        return {
            ask: Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid))(dictMonadAsk.Monad0())(Control_Monad_Reader_Class.ask(dictMonadAsk)),
            Monad0: function () {
                return monadWriterT(dictMonoid)(dictMonadAsk.Monad0());
            }
        };
    };
};
var monadReaderWriterT = function (dictMonoid) {
    return function (dictMonadReader) {
        return {
            local: function (f) {
                return mapWriterT(Control_Monad_Reader_Class.local(dictMonadReader)(f));
            },
            MonadAsk0: function () {
                return monadAskWriterT(dictMonoid)(dictMonadReader.MonadAsk0());
            }
        };
    };
};
var monadContWriterT = function (dictMonoid) {
    return function (dictMonadCont) {
        return {
            callCC: function (f) {
                return Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
                    var v = f(function (a) {
                        return c(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid)));
                    });
                    return v;
                });
            },
            Monad0: function () {
                return monadWriterT(dictMonoid)(dictMonadCont.Monad0());
            }
        };
    };
};
var monadEffectWriter = function (dictMonoid) {
    return function (dictMonadEffect) {
        return {
            liftEffect: (function () {
                var $122 = Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid))(dictMonadEffect.Monad0());
                var $123 = Effect_Class.liftEffect(dictMonadEffect);
                return function ($124) {
                    return $122($123($124));
                };
            })(),
            Monad0: function () {
                return monadWriterT(dictMonoid)(dictMonadEffect.Monad0());
            }
        };
    };
};
var monadRecWriterT = function (dictMonoid) {
    return function (dictMonadRec) {
        return {
            tailRecM: function (f) {
                return function (a) {
                    var f$prime = function (v) {
                        var v1 = f(v.value0);
                        return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())(v1)(function (v2) {
                            return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())((function () {
                                if (v2.value0 instanceof Control_Monad_Rec_Class.Loop) {
                                    return new Control_Monad_Rec_Class.Loop(new Data_Tuple.Tuple(v2.value0.value0, Data_Semigroup.append(dictMonoid.Semigroup0())(v.value1)(v2.value1)));
                                };
                                if (v2.value0 instanceof Control_Monad_Rec_Class.Done) {
                                    return new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(v2.value0.value0, Data_Semigroup.append(dictMonoid.Semigroup0())(v.value1)(v2.value1)));
                                };
                                throw new Error("Failed pattern match at Control.Monad.Writer.Trans (line 83, column 16 - line 85, column 47): " + [ v2.value0.constructor.name ]);
                            })());
                        });
                    };
                    return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(f$prime)(new Data_Tuple.Tuple(a, Data_Monoid.mempty(dictMonoid)));
                };
            },
            Monad0: function () {
                return monadWriterT(dictMonoid)(dictMonadRec.Monad0());
            }
        };
    };
};
var monadStateWriterT = function (dictMonoid) {
    return function (dictMonadState) {
        return {
            state: function (f) {
                return Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid))(dictMonadState.Monad0())(Control_Monad_State_Class.state(dictMonadState)(f));
            },
            Monad0: function () {
                return monadWriterT(dictMonoid)(dictMonadState.Monad0());
            }
        };
    };
};
var monadTellWriterT = function (dictMonoid) {
    return function (dictMonad) {
        return {
            tell: (function () {
                var $125 = Control_Applicative.pure(dictMonad.Applicative0());
                var $126 = Data_Tuple.Tuple.create(Data_Unit.unit);
                return function ($127) {
                    return WriterT($125($126($127)));
                };
            })(),
            Semigroup0: dictMonoid.Semigroup0,
            Monad1: function () {
                return monadWriterT(dictMonoid)(dictMonad);
            }
        };
    };
};
var monadWriterWriterT = function (dictMonoid) {
    return function (dictMonad) {
        return {
            listen: function (v) {
                return Control_Bind.bind(dictMonad.Bind1())(v)(function (v1) {
                    return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v1.value0, v1.value1), v1.value1));
                });
            },
            pass: function (v) {
                return Control_Bind.bind(dictMonad.Bind1())(v)(function (v1) {
                    return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(v1.value0.value0, v1.value0.value1(v1.value1)));
                });
            },
            Monoid0: function () {
                return dictMonoid;
            },
            MonadTell1: function () {
                return monadTellWriterT(dictMonoid)(dictMonad);
            }
        };
    };
};
var monadThrowWriterT = function (dictMonoid) {
    return function (dictMonadThrow) {
        return {
            throwError: function (e) {
                return Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid))(dictMonadThrow.Monad0())(Control_Monad_Error_Class.throwError(dictMonadThrow)(e));
            },
            Monad0: function () {
                return monadWriterT(dictMonoid)(dictMonadThrow.Monad0());
            }
        };
    };
};
var monadErrorWriterT = function (dictMonoid) {
    return function (dictMonadError) {
        return {
            catchError: function (v) {
                return function (h) {
                    return Control_Monad_Error_Class.catchError(dictMonadError)(v)(function (e) {
                        var v1 = h(e);
                        return v1;
                    });
                };
            },
            MonadThrow0: function () {
                return monadThrowWriterT(dictMonoid)(dictMonadError.MonadThrow0());
            }
        };
    };
};
var monoidWriterT = function (dictApplicative) {
    return function (dictMonoid) {
        return function (dictMonoid1) {
            return {
                mempty: Control_Applicative.pure(applicativeWriterT(dictMonoid)(dictApplicative))(Data_Monoid.mempty(dictMonoid1)),
                Semigroup0: function () {
                    return semigroupWriterT(dictApplicative.Apply0())(dictMonoid.Semigroup0())(dictMonoid1.Semigroup0());
                }
            };
        };
    };
};
var altWriterT = function (dictAlt) {
    return {
        alt: function (v) {
            return function (v1) {
                return Control_Alt.alt(dictAlt)(v)(v1);
            };
        },
        Functor0: function () {
            return functorWriterT(dictAlt.Functor0());
        }
    };
};
var plusWriterT = function (dictPlus) {
    return {
        empty: Control_Plus.empty(dictPlus),
        Alt0: function () {
            return altWriterT(dictPlus.Alt0());
        }
    };
};
var alternativeWriterT = function (dictMonoid) {
    return function (dictAlternative) {
        return {
            Applicative0: function () {
                return applicativeWriterT(dictMonoid)(dictAlternative.Applicative0());
            },
            Plus1: function () {
                return plusWriterT(dictAlternative.Plus1());
            }
        };
    };
};
var monadPlusWriterT = function (dictMonoid) {
    return function (dictMonadPlus) {
        return {
            Monad0: function () {
                return monadWriterT(dictMonoid)(dictMonadPlus.Monad0());
            },
            Alternative1: function () {
                return alternativeWriterT(dictMonoid)(dictMonadPlus.Alternative1());
            }
        };
    };
};
export {
    WriterT,
    runWriterT,
    execWriterT,
    mapWriterT,
    newtypeWriterT,
    functorWriterT,
    applyWriterT,
    applicativeWriterT,
    altWriterT,
    plusWriterT,
    alternativeWriterT,
    bindWriterT,
    monadWriterT,
    monadRecWriterT,
    monadPlusWriterT,
    monadTransWriterT,
    monadEffectWriter,
    monadContWriterT,
    monadThrowWriterT,
    monadErrorWriterT,
    monadAskWriterT,
    monadReaderWriterT,
    monadStateWriterT,
    monadTellWriterT,
    monadWriterWriterT,
    semigroupWriterT,
    monoidWriterT
};
export {
    lift
} from "../Control.Monad.Trans.Class/index.js";
export {
    censor,
    listen,
    listens,
    pass,
    tell
} from "../Control.Monad.Writer.Class/index.js";
