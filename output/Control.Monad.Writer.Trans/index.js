// Generated by purs version 0.15.15
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
var WriterT = function (x) {
    return x;
};
var runWriterT = function (v) {
    return v;
};
var newtypeWriterT = {
    Coercible0: function () {
        return undefined;
    }
};
var monadTransWriterT = function (dictMonoid) {
    var mempty = Data_Monoid.mempty(dictMonoid);
    return {
        lift: function (dictMonad) {
            var bind = Control_Bind.bind(dictMonad.Bind1());
            var pure = Control_Applicative.pure(dictMonad.Applicative0());
            return function (m) {
                return bind(m)(function (a) {
                    return pure(new Data_Tuple.Tuple(a, mempty));
                });
            };
        }
    };
};
var mapWriterT = function (f) {
    return function (v) {
        return f(v);
    };
};
var functorWriterT = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return {
        map: function (f) {
            return mapWriterT(map(function (v) {
                return new Data_Tuple.Tuple(f(v.value0), v.value1);
            }));
        }
    };
};
var execWriterT = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (v) {
        return map(Data_Tuple.snd)(v);
    };
};
var applyWriterT = function (dictSemigroup) {
    var append = Data_Semigroup.append(dictSemigroup);
    return function (dictApply) {
        var apply = Control_Apply.apply(dictApply);
        var Functor0 = dictApply.Functor0();
        var map = Data_Functor.map(Functor0);
        var functorWriterT1 = functorWriterT(Functor0);
        return {
            apply: function (v) {
                return function (v1) {
                    var k = function (v3) {
                        return function (v4) {
                            return new Data_Tuple.Tuple(v3.value0(v4.value0), append(v3.value1)(v4.value1));
                        };
                    };
                    return apply(map(k)(v))(v1);
                };
            },
            Functor0: function () {
                return functorWriterT1;
            }
        };
    };
};
var bindWriterT = function (dictSemigroup) {
    var append = Data_Semigroup.append(dictSemigroup);
    var applyWriterT1 = applyWriterT(dictSemigroup);
    return function (dictBind) {
        var bind = Control_Bind.bind(dictBind);
        var Apply0 = dictBind.Apply0();
        var map = Data_Functor.map(Apply0.Functor0());
        var applyWriterT2 = applyWriterT1(Apply0);
        return {
            bind: function (v) {
                return function (k) {
                    return bind(v)(function (v1) {
                        var v2 = k(v1.value0);
                        return map(function (v3) {
                            return new Data_Tuple.Tuple(v3.value0, append(v1.value1)(v3.value1));
                        })(v2);
                    });
                };
            },
            Apply0: function () {
                return applyWriterT2;
            }
        };
    };
};
var semigroupWriterT = function (dictApply) {
    return function (dictSemigroup) {
        var lift2 = Control_Apply.lift2(applyWriterT(dictSemigroup)(dictApply));
        return function (dictSemigroup1) {
            return {
                append: lift2(Data_Semigroup.append(dictSemigroup1))
            };
        };
    };
};
var applicativeWriterT = function (dictMonoid) {
    var mempty = Data_Monoid.mempty(dictMonoid);
    var applyWriterT1 = applyWriterT(dictMonoid.Semigroup0());
    return function (dictApplicative) {
        var pure = Control_Applicative.pure(dictApplicative);
        var applyWriterT2 = applyWriterT1(dictApplicative.Apply0());
        return {
            pure: function (a) {
                return pure(new Data_Tuple.Tuple(a, mempty));
            },
            Apply0: function () {
                return applyWriterT2;
            }
        };
    };
};
var monadWriterT = function (dictMonoid) {
    var applicativeWriterT1 = applicativeWriterT(dictMonoid);
    var bindWriterT1 = bindWriterT(dictMonoid.Semigroup0());
    return function (dictMonad) {
        var applicativeWriterT2 = applicativeWriterT1(dictMonad.Applicative0());
        var bindWriterT2 = bindWriterT1(dictMonad.Bind1());
        return {
            Applicative0: function () {
                return applicativeWriterT2;
            },
            Bind1: function () {
                return bindWriterT2;
            }
        };
    };
};
var monadAskWriterT = function (dictMonoid) {
    var lift = Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid));
    var monadWriterT1 = monadWriterT(dictMonoid);
    return function (dictMonadAsk) {
        var Monad0 = dictMonadAsk.Monad0();
        var monadWriterT2 = monadWriterT1(Monad0);
        return {
            ask: lift(Monad0)(Control_Monad_Reader_Class.ask(dictMonadAsk)),
            Monad0: function () {
                return monadWriterT2;
            }
        };
    };
};
var monadReaderWriterT = function (dictMonoid) {
    var monadAskWriterT1 = monadAskWriterT(dictMonoid);
    return function (dictMonadReader) {
        var local = Control_Monad_Reader_Class.local(dictMonadReader);
        var monadAskWriterT2 = monadAskWriterT1(dictMonadReader.MonadAsk0());
        return {
            local: function (f) {
                return mapWriterT(local(f));
            },
            MonadAsk0: function () {
                return monadAskWriterT2;
            }
        };
    };
};
var monadContWriterT = function (dictMonoid) {
    var mempty = Data_Monoid.mempty(dictMonoid);
    var monadWriterT1 = monadWriterT(dictMonoid);
    return function (dictMonadCont) {
        var callCC = Control_Monad_Cont_Class.callCC(dictMonadCont);
        var monadWriterT2 = monadWriterT1(dictMonadCont.Monad0());
        return {
            callCC: function (f) {
                return callCC(function (c) {
                    var v = f(function (a) {
                        return c(new Data_Tuple.Tuple(a, mempty));
                    });
                    return v;
                });
            },
            Monad0: function () {
                return monadWriterT2;
            }
        };
    };
};
var monadEffectWriter = function (dictMonoid) {
    var lift = Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid));
    var monadWriterT1 = monadWriterT(dictMonoid);
    return function (dictMonadEffect) {
        var Monad0 = dictMonadEffect.Monad0();
        var monadWriterT2 = monadWriterT1(Monad0);
        return {
            liftEffect: (function () {
                var $249 = lift(Monad0);
                var $250 = Effect_Class.liftEffect(dictMonadEffect);
                return function ($251) {
                    return $249($250($251));
                };
            })(),
            Monad0: function () {
                return monadWriterT2;
            }
        };
    };
};
var monadRecWriterT = function (dictMonoid) {
    var append = Data_Semigroup.append(dictMonoid.Semigroup0());
    var mempty = Data_Monoid.mempty(dictMonoid);
    var monadWriterT1 = monadWriterT(dictMonoid);
    return function (dictMonadRec) {
        var Monad0 = dictMonadRec.Monad0();
        var bind = Control_Bind.bind(Monad0.Bind1());
        var pure = Control_Applicative.pure(Monad0.Applicative0());
        var tailRecM = Control_Monad_Rec_Class.tailRecM(dictMonadRec);
        var monadWriterT2 = monadWriterT1(Monad0);
        return {
            tailRecM: function (f) {
                return function (a) {
                    var f$prime = function (v) {
                        var v1 = f(v.value0);
                        return bind(v1)(function (v2) {
                            return pure((function () {
                                if (v2.value0 instanceof Control_Monad_Rec_Class.Loop) {
                                    return new Control_Monad_Rec_Class.Loop(new Data_Tuple.Tuple(v2.value0.value0, append(v.value1)(v2.value1)));
                                };
                                if (v2.value0 instanceof Control_Monad_Rec_Class.Done) {
                                    return new Control_Monad_Rec_Class.Done(new Data_Tuple.Tuple(v2.value0.value0, append(v.value1)(v2.value1)));
                                };
                                throw new Error("Failed pattern match at Control.Monad.Writer.Trans (line 83, column 16 - line 85, column 47): " + [ v2.value0.constructor.name ]);
                            })());
                        });
                    };
                    return tailRecM(f$prime)(new Data_Tuple.Tuple(a, mempty));
                };
            },
            Monad0: function () {
                return monadWriterT2;
            }
        };
    };
};
var monadStateWriterT = function (dictMonoid) {
    var lift = Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid));
    var monadWriterT1 = monadWriterT(dictMonoid);
    return function (dictMonadState) {
        var Monad0 = dictMonadState.Monad0();
        var lift1 = lift(Monad0);
        var state = Control_Monad_State_Class.state(dictMonadState);
        var monadWriterT2 = monadWriterT1(Monad0);
        return {
            state: function (f) {
                return lift1(state(f));
            },
            Monad0: function () {
                return monadWriterT2;
            }
        };
    };
};
var monadTellWriterT = function (dictMonoid) {
    var Semigroup0 = dictMonoid.Semigroup0();
    var monadWriterT1 = monadWriterT(dictMonoid);
    return function (dictMonad) {
        var monadWriterT2 = monadWriterT1(dictMonad);
        return {
            tell: (function () {
                var $252 = Control_Applicative.pure(dictMonad.Applicative0());
                var $253 = Data_Tuple.Tuple.create(Data_Unit.unit);
                return function ($254) {
                    return WriterT($252($253($254)));
                };
            })(),
            Semigroup0: function () {
                return Semigroup0;
            },
            Monad1: function () {
                return monadWriterT2;
            }
        };
    };
};
var monadWriterWriterT = function (dictMonoid) {
    var monadTellWriterT1 = monadTellWriterT(dictMonoid);
    return function (dictMonad) {
        var bind = Control_Bind.bind(dictMonad.Bind1());
        var pure = Control_Applicative.pure(dictMonad.Applicative0());
        var monadTellWriterT2 = monadTellWriterT1(dictMonad);
        return {
            listen: function (v) {
                return bind(v)(function (v1) {
                    return pure(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v1.value0, v1.value1), v1.value1));
                });
            },
            pass: function (v) {
                return bind(v)(function (v1) {
                    return pure(new Data_Tuple.Tuple(v1.value0.value0, v1.value0.value1(v1.value1)));
                });
            },
            Monoid0: function () {
                return dictMonoid;
            },
            MonadTell1: function () {
                return monadTellWriterT2;
            }
        };
    };
};
var monadThrowWriterT = function (dictMonoid) {
    var lift = Control_Monad_Trans_Class.lift(monadTransWriterT(dictMonoid));
    var monadWriterT1 = monadWriterT(dictMonoid);
    return function (dictMonadThrow) {
        var Monad0 = dictMonadThrow.Monad0();
        var lift1 = lift(Monad0);
        var throwError = Control_Monad_Error_Class.throwError(dictMonadThrow);
        var monadWriterT2 = monadWriterT1(Monad0);
        return {
            throwError: function (e) {
                return lift1(throwError(e));
            },
            Monad0: function () {
                return monadWriterT2;
            }
        };
    };
};
var monadErrorWriterT = function (dictMonoid) {
    var monadThrowWriterT1 = monadThrowWriterT(dictMonoid);
    return function (dictMonadError) {
        var catchError = Control_Monad_Error_Class.catchError(dictMonadError);
        var monadThrowWriterT2 = monadThrowWriterT1(dictMonadError.MonadThrow0());
        return {
            catchError: function (v) {
                return function (h) {
                    return catchError(v)(function (e) {
                        var v1 = h(e);
                        return v1;
                    });
                };
            },
            MonadThrow0: function () {
                return monadThrowWriterT2;
            }
        };
    };
};
var monoidWriterT = function (dictApplicative) {
    var semigroupWriterT1 = semigroupWriterT(dictApplicative.Apply0());
    return function (dictMonoid) {
        var pure = Control_Applicative.pure(applicativeWriterT(dictMonoid)(dictApplicative));
        var semigroupWriterT2 = semigroupWriterT1(dictMonoid.Semigroup0());
        return function (dictMonoid1) {
            var semigroupWriterT3 = semigroupWriterT2(dictMonoid1.Semigroup0());
            return {
                mempty: pure(Data_Monoid.mempty(dictMonoid1)),
                Semigroup0: function () {
                    return semigroupWriterT3;
                }
            };
        };
    };
};
var altWriterT = function (dictAlt) {
    var alt = Control_Alt.alt(dictAlt);
    var functorWriterT1 = functorWriterT(dictAlt.Functor0());
    return {
        alt: function (v) {
            return function (v1) {
                return alt(v)(v1);
            };
        },
        Functor0: function () {
            return functorWriterT1;
        }
    };
};
var plusWriterT = function (dictPlus) {
    var altWriterT1 = altWriterT(dictPlus.Alt0());
    return {
        empty: Control_Plus.empty(dictPlus),
        Alt0: function () {
            return altWriterT1;
        }
    };
};
var alternativeWriterT = function (dictMonoid) {
    var applicativeWriterT1 = applicativeWriterT(dictMonoid);
    return function (dictAlternative) {
        var applicativeWriterT2 = applicativeWriterT1(dictAlternative.Applicative0());
        var plusWriterT1 = plusWriterT(dictAlternative.Plus1());
        return {
            Applicative0: function () {
                return applicativeWriterT2;
            },
            Plus1: function () {
                return plusWriterT1;
            }
        };
    };
};
var monadPlusWriterT = function (dictMonoid) {
    var monadWriterT1 = monadWriterT(dictMonoid);
    var alternativeWriterT1 = alternativeWriterT(dictMonoid);
    return function (dictMonadPlus) {
        var monadWriterT2 = monadWriterT1(dictMonadPlus.Monad0());
        var alternativeWriterT2 = alternativeWriterT1(dictMonadPlus.Alternative1());
        return {
            Monad0: function () {
                return monadWriterT2;
            },
            Alternative1: function () {
                return alternativeWriterT2;
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
