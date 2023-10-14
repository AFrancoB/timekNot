// Generated by purs version 0.15.10
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
import * as Data_Distributive from "../Data.Distributive/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Effect_Class from "../Effect.Class/index.js";
var ReaderT = function (x) {
    return x;
};
var withReaderT = function (f) {
    return function (v) {
        return function ($146) {
            return v(f($146));
        };
    };
};
var runReaderT = function (v) {
    return v;
};
var newtypeReaderT = {
    Coercible0: function () {
        return undefined;
    }
};
var monadTransReaderT = {
    lift: function (dictMonad) {
        return function ($147) {
            return ReaderT(Data_Function["const"]($147));
        };
    }
};
var lift = /* #__PURE__ */ Control_Monad_Trans_Class.lift(monadTransReaderT);
var mapReaderT = function (f) {
    return function (v) {
        return function ($148) {
            return f(v($148));
        };
    };
};
var functorReaderT = function (dictFunctor) {
    return {
        map: (function () {
            var $149 = Data_Functor.map(dictFunctor);
            return function ($150) {
                return mapReaderT($149($150));
            };
        })()
    };
};
var distributiveReaderT = function (dictDistributive) {
    var collect = Data_Distributive.collect(dictDistributive);
    var functorReaderT1 = functorReaderT(dictDistributive.Functor0());
    return {
        distribute: function (dictFunctor) {
            var collect1 = collect(dictFunctor);
            return function (a) {
                return function (e) {
                    return collect1(function (r) {
                        return r(e);
                    })(a);
                };
            };
        },
        collect: function (dictFunctor) {
            var map = Data_Functor.map(dictFunctor);
            return function (f) {
                var $151 = Data_Distributive.distribute(distributiveReaderT(dictDistributive))(dictFunctor);
                var $152 = map(f);
                return function ($153) {
                    return $151($152($153));
                };
            };
        },
        Functor0: function () {
            return functorReaderT1;
        }
    };
};
var applyReaderT = function (dictApply) {
    var apply = Control_Apply.apply(dictApply);
    var functorReaderT1 = functorReaderT(dictApply.Functor0());
    return {
        apply: function (v) {
            return function (v1) {
                return function (r) {
                    return apply(v(r))(v1(r));
                };
            };
        },
        Functor0: function () {
            return functorReaderT1;
        }
    };
};
var bindReaderT = function (dictBind) {
    var bind = Control_Bind.bind(dictBind);
    var applyReaderT1 = applyReaderT(dictBind.Apply0());
    return {
        bind: function (v) {
            return function (k) {
                return function (r) {
                    return bind(v(r))(function (a) {
                        var v1 = k(a);
                        return v1(r);
                    });
                };
            };
        },
        Apply0: function () {
            return applyReaderT1;
        }
    };
};
var semigroupReaderT = function (dictApply) {
    var lift2 = Control_Apply.lift2(applyReaderT(dictApply));
    return function (dictSemigroup) {
        return {
            append: lift2(Data_Semigroup.append(dictSemigroup))
        };
    };
};
var applicativeReaderT = function (dictApplicative) {
    var applyReaderT1 = applyReaderT(dictApplicative.Apply0());
    return {
        pure: (function () {
            var $154 = Control_Applicative.pure(dictApplicative);
            return function ($155) {
                return ReaderT(Data_Function["const"]($154($155)));
            };
        })(),
        Apply0: function () {
            return applyReaderT1;
        }
    };
};
var monadReaderT = function (dictMonad) {
    var applicativeReaderT1 = applicativeReaderT(dictMonad.Applicative0());
    var bindReaderT1 = bindReaderT(dictMonad.Bind1());
    return {
        Applicative0: function () {
            return applicativeReaderT1;
        },
        Bind1: function () {
            return bindReaderT1;
        }
    };
};
var monadAskReaderT = function (dictMonad) {
    var monadReaderT1 = monadReaderT(dictMonad);
    return {
        ask: Control_Applicative.pure(dictMonad.Applicative0()),
        Monad0: function () {
            return monadReaderT1;
        }
    };
};
var monadReaderReaderT = function (dictMonad) {
    var monadAskReaderT1 = monadAskReaderT(dictMonad);
    return {
        local: withReaderT,
        MonadAsk0: function () {
            return monadAskReaderT1;
        }
    };
};
var monadContReaderT = function (dictMonadCont) {
    var callCC = Control_Monad_Cont_Class.callCC(dictMonadCont);
    var monadReaderT1 = monadReaderT(dictMonadCont.Monad0());
    return {
        callCC: function (f) {
            return function (r) {
                return callCC(function (c) {
                    var v = f(function ($156) {
                        return ReaderT(Data_Function["const"](c($156)));
                    });
                    return v(r);
                });
            };
        },
        Monad0: function () {
            return monadReaderT1;
        }
    };
};
var monadEffectReader = function (dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var monadReaderT1 = monadReaderT(Monad0);
    return {
        liftEffect: (function () {
            var $157 = lift(Monad0);
            var $158 = Effect_Class.liftEffect(dictMonadEffect);
            return function ($159) {
                return $157($158($159));
            };
        })(),
        Monad0: function () {
            return monadReaderT1;
        }
    };
};
var monadRecReaderT = function (dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var bindFlipped = Control_Bind.bindFlipped(Monad0.Bind1());
    var pure = Control_Applicative.pure(Monad0.Applicative0());
    var tailRecM = Control_Monad_Rec_Class.tailRecM(dictMonadRec);
    var monadReaderT1 = monadReaderT(Monad0);
    return {
        tailRecM: function (k) {
            return function (a) {
                var k$prime = function (r) {
                    return function (a$prime) {
                        var v = k(a$prime);
                        return bindFlipped(pure)(v(r));
                    };
                };
                return function (r) {
                    return tailRecM(k$prime(r))(a);
                };
            };
        },
        Monad0: function () {
            return monadReaderT1;
        }
    };
};
var monadStateReaderT = function (dictMonadState) {
    var Monad0 = dictMonadState.Monad0();
    var monadReaderT1 = monadReaderT(Monad0);
    return {
        state: (function () {
            var $160 = lift(Monad0);
            var $161 = Control_Monad_State_Class.state(dictMonadState);
            return function ($162) {
                return $160($161($162));
            };
        })(),
        Monad0: function () {
            return monadReaderT1;
        }
    };
};
var monadTellReaderT = function (dictMonadTell) {
    var Monad1 = dictMonadTell.Monad1();
    var Semigroup0 = dictMonadTell.Semigroup0();
    var monadReaderT1 = monadReaderT(Monad1);
    return {
        tell: (function () {
            var $163 = lift(Monad1);
            var $164 = Control_Monad_Writer_Class.tell(dictMonadTell);
            return function ($165) {
                return $163($164($165));
            };
        })(),
        Semigroup0: function () {
            return Semigroup0;
        },
        Monad1: function () {
            return monadReaderT1;
        }
    };
};
var monadWriterReaderT = function (dictMonadWriter) {
    var Monoid0 = dictMonadWriter.Monoid0();
    var monadTellReaderT1 = monadTellReaderT(dictMonadWriter.MonadTell1());
    return {
        listen: mapReaderT(Control_Monad_Writer_Class.listen(dictMonadWriter)),
        pass: mapReaderT(Control_Monad_Writer_Class.pass(dictMonadWriter)),
        Monoid0: function () {
            return Monoid0;
        },
        MonadTell1: function () {
            return monadTellReaderT1;
        }
    };
};
var monadThrowReaderT = function (dictMonadThrow) {
    var Monad0 = dictMonadThrow.Monad0();
    var monadReaderT1 = monadReaderT(Monad0);
    return {
        throwError: (function () {
            var $166 = lift(Monad0);
            var $167 = Control_Monad_Error_Class.throwError(dictMonadThrow);
            return function ($168) {
                return $166($167($168));
            };
        })(),
        Monad0: function () {
            return monadReaderT1;
        }
    };
};
var monadErrorReaderT = function (dictMonadError) {
    var catchError = Control_Monad_Error_Class.catchError(dictMonadError);
    var monadThrowReaderT1 = monadThrowReaderT(dictMonadError.MonadThrow0());
    return {
        catchError: function (v) {
            return function (h) {
                return function (r) {
                    return catchError(v(r))(function (e) {
                        var v1 = h(e);
                        return v1(r);
                    });
                };
            };
        },
        MonadThrow0: function () {
            return monadThrowReaderT1;
        }
    };
};
var monoidReaderT = function (dictApplicative) {
    var pure = Control_Applicative.pure(applicativeReaderT(dictApplicative));
    var semigroupReaderT1 = semigroupReaderT(dictApplicative.Apply0());
    return function (dictMonoid) {
        var semigroupReaderT2 = semigroupReaderT1(dictMonoid.Semigroup0());
        return {
            mempty: pure(Data_Monoid.mempty(dictMonoid)),
            Semigroup0: function () {
                return semigroupReaderT2;
            }
        };
    };
};
var altReaderT = function (dictAlt) {
    var alt = Control_Alt.alt(dictAlt);
    var functorReaderT1 = functorReaderT(dictAlt.Functor0());
    return {
        alt: function (v) {
            return function (v1) {
                return function (r) {
                    return alt(v(r))(v1(r));
                };
            };
        },
        Functor0: function () {
            return functorReaderT1;
        }
    };
};
var plusReaderT = function (dictPlus) {
    var altReaderT1 = altReaderT(dictPlus.Alt0());
    return {
        empty: Data_Function["const"](Control_Plus.empty(dictPlus)),
        Alt0: function () {
            return altReaderT1;
        }
    };
};
var alternativeReaderT = function (dictAlternative) {
    var applicativeReaderT1 = applicativeReaderT(dictAlternative.Applicative0());
    var plusReaderT1 = plusReaderT(dictAlternative.Plus1());
    return {
        Applicative0: function () {
            return applicativeReaderT1;
        },
        Plus1: function () {
            return plusReaderT1;
        }
    };
};
var monadPlusReaderT = function (dictMonadPlus) {
    var monadReaderT1 = monadReaderT(dictMonadPlus.Monad0());
    var alternativeReaderT1 = alternativeReaderT(dictMonadPlus.Alternative1());
    return {
        Monad0: function () {
            return monadReaderT1;
        },
        Alternative1: function () {
            return alternativeReaderT1;
        }
    };
};
export {
    ReaderT,
    runReaderT,
    withReaderT,
    mapReaderT,
    newtypeReaderT,
    functorReaderT,
    applyReaderT,
    applicativeReaderT,
    altReaderT,
    plusReaderT,
    alternativeReaderT,
    bindReaderT,
    monadReaderT,
    semigroupReaderT,
    monoidReaderT,
    monadPlusReaderT,
    monadTransReaderT,
    monadEffectReader,
    monadContReaderT,
    monadThrowReaderT,
    monadErrorReaderT,
    monadAskReaderT,
    monadReaderReaderT,
    monadStateReaderT,
    monadTellReaderT,
    monadWriterReaderT,
    distributiveReaderT,
    monadRecReaderT
};
export {
    ask,
    asks,
    local
} from "../Control.Monad.Reader.Class/index.js";
export {
    lift
} from "../Control.Monad.Trans.Class/index.js";
