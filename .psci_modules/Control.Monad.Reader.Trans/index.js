// | This module defines the reader monad transformer, `ReaderT`.
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

// | Change the type of the context in a `ReaderT` monad action.
var withReaderT = function (f) {
    return function (v) {
        return function ($62) {
            return v(f($62));
        };
    };
};

// | Run a computation in the `ReaderT` monad.
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
        return function ($63) {
            return ReaderT(Data_Function["const"]($63));
        };
    }
};

// | Change the type of the result in a `ReaderT` monad action.
var mapReaderT = function (f) {
    return function (v) {
        return function ($64) {
            return f(v($64));
        };
    };
};
var functorReaderT = function (dictFunctor) {
    return {
        map: (function () {
            var $65 = Data_Functor.map(dictFunctor);
            return function ($66) {
                return mapReaderT($65($66));
            };
        })()
    };
};
var distributiveReaderT = function (dictDistributive) {
    return {
        distribute: function (dictFunctor) {
            return function (a) {
                return function (e) {
                    return Data_Distributive.collect(dictDistributive)(dictFunctor)(function (r) {
                        return r(e);
                    })(a);
                };
            };
        },
        collect: function (dictFunctor) {
            return function (f) {
                var $67 = Data_Distributive.distribute(distributiveReaderT(dictDistributive))(dictFunctor);
                var $68 = Data_Functor.map(dictFunctor)(f);
                return function ($69) {
                    return $67($68($69));
                };
            };
        },
        Functor0: function () {
            return functorReaderT(dictDistributive.Functor0());
        }
    };
};
var applyReaderT = function (dictApply) {
    return {
        apply: function (v) {
            return function (v1) {
                return function (r) {
                    return Control_Apply.apply(dictApply)(v(r))(v1(r));
                };
            };
        },
        Functor0: function () {
            return functorReaderT(dictApply.Functor0());
        }
    };
};
var bindReaderT = function (dictBind) {
    return {
        bind: function (v) {
            return function (k) {
                return function (r) {
                    return Control_Bind.bind(dictBind)(v(r))(function (a) {
                        var v1 = k(a);
                        return v1(r);
                    });
                };
            };
        },
        Apply0: function () {
            return applyReaderT(dictBind.Apply0());
        }
    };
};
var semigroupReaderT = function (dictApply) {
    return function (dictSemigroup) {
        return {
            append: Control_Apply.lift2(applyReaderT(dictApply))(Data_Semigroup.append(dictSemigroup))
        };
    };
};
var applicativeReaderT = function (dictApplicative) {
    return {
        pure: (function () {
            var $70 = Control_Applicative.pure(dictApplicative);
            return function ($71) {
                return ReaderT(Data_Function["const"]($70($71)));
            };
        })(),
        Apply0: function () {
            return applyReaderT(dictApplicative.Apply0());
        }
    };
};
var monadReaderT = function (dictMonad) {
    return {
        Applicative0: function () {
            return applicativeReaderT(dictMonad.Applicative0());
        },
        Bind1: function () {
            return bindReaderT(dictMonad.Bind1());
        }
    };
};
var monadAskReaderT = function (dictMonad) {
    return {
        ask: Control_Applicative.pure(dictMonad.Applicative0()),
        Monad0: function () {
            return monadReaderT(dictMonad);
        }
    };
};
var monadReaderReaderT = function (dictMonad) {
    return {
        local: withReaderT,
        MonadAsk0: function () {
            return monadAskReaderT(dictMonad);
        }
    };
};
var monadContReaderT = function (dictMonadCont) {
    return {
        callCC: function (f) {
            return function (r) {
                return Control_Monad_Cont_Class.callCC(dictMonadCont)(function (c) {
                    var v = f(function ($72) {
                        return ReaderT(Data_Function["const"](c($72)));
                    });
                    return v(r);
                });
            };
        },
        Monad0: function () {
            return monadReaderT(dictMonadCont.Monad0());
        }
    };
};
var monadEffectReader = function (dictMonadEffect) {
    return {
        liftEffect: (function () {
            var $73 = Control_Monad_Trans_Class.lift(monadTransReaderT)(dictMonadEffect.Monad0());
            var $74 = Effect_Class.liftEffect(dictMonadEffect);
            return function ($75) {
                return $73($74($75));
            };
        })(),
        Monad0: function () {
            return monadReaderT(dictMonadEffect.Monad0());
        }
    };
};
var monadRecReaderT = function (dictMonadRec) {
    return {
        tailRecM: function (k) {
            return function (a) {
                var k$prime = function (r) {
                    return function (a$prime) {
                        var v = k(a$prime);
                        return Control_Bind.bindFlipped((dictMonadRec.Monad0()).Bind1())(Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0()))(v(r));
                    };
                };
                return function (r) {
                    return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(k$prime(r))(a);
                };
            };
        },
        Monad0: function () {
            return monadReaderT(dictMonadRec.Monad0());
        }
    };
};
var monadStateReaderT = function (dictMonadState) {
    return {
        state: (function () {
            var $76 = Control_Monad_Trans_Class.lift(monadTransReaderT)(dictMonadState.Monad0());
            var $77 = Control_Monad_State_Class.state(dictMonadState);
            return function ($78) {
                return $76($77($78));
            };
        })(),
        Monad0: function () {
            return monadReaderT(dictMonadState.Monad0());
        }
    };
};
var monadTellReaderT = function (dictMonadTell) {
    return {
        tell: (function () {
            var $79 = Control_Monad_Trans_Class.lift(monadTransReaderT)(dictMonadTell.Monad1());
            var $80 = Control_Monad_Writer_Class.tell(dictMonadTell);
            return function ($81) {
                return $79($80($81));
            };
        })(),
        Semigroup0: dictMonadTell.Semigroup0,
        Monad1: function () {
            return monadReaderT(dictMonadTell.Monad1());
        }
    };
};
var monadWriterReaderT = function (dictMonadWriter) {
    return {
        listen: mapReaderT(Control_Monad_Writer_Class.listen(dictMonadWriter)),
        pass: mapReaderT(Control_Monad_Writer_Class.pass(dictMonadWriter)),
        Monoid0: dictMonadWriter.Monoid0,
        MonadTell1: function () {
            return monadTellReaderT(dictMonadWriter.MonadTell1());
        }
    };
};
var monadThrowReaderT = function (dictMonadThrow) {
    return {
        throwError: (function () {
            var $82 = Control_Monad_Trans_Class.lift(monadTransReaderT)(dictMonadThrow.Monad0());
            var $83 = Control_Monad_Error_Class.throwError(dictMonadThrow);
            return function ($84) {
                return $82($83($84));
            };
        })(),
        Monad0: function () {
            return monadReaderT(dictMonadThrow.Monad0());
        }
    };
};
var monadErrorReaderT = function (dictMonadError) {
    return {
        catchError: function (v) {
            return function (h) {
                return function (r) {
                    return Control_Monad_Error_Class.catchError(dictMonadError)(v(r))(function (e) {
                        var v1 = h(e);
                        return v1(r);
                    });
                };
            };
        },
        MonadThrow0: function () {
            return monadThrowReaderT(dictMonadError.MonadThrow0());
        }
    };
};
var monoidReaderT = function (dictApplicative) {
    return function (dictMonoid) {
        return {
            mempty: Control_Applicative.pure(applicativeReaderT(dictApplicative))(Data_Monoid.mempty(dictMonoid)),
            Semigroup0: function () {
                return semigroupReaderT(dictApplicative.Apply0())(dictMonoid.Semigroup0());
            }
        };
    };
};
var altReaderT = function (dictAlt) {
    return {
        alt: function (v) {
            return function (v1) {
                return function (r) {
                    return Control_Alt.alt(dictAlt)(v(r))(v1(r));
                };
            };
        },
        Functor0: function () {
            return functorReaderT(dictAlt.Functor0());
        }
    };
};
var plusReaderT = function (dictPlus) {
    return {
        empty: Data_Function["const"](Control_Plus.empty(dictPlus)),
        Alt0: function () {
            return altReaderT(dictPlus.Alt0());
        }
    };
};
var alternativeReaderT = function (dictAlternative) {
    return {
        Applicative0: function () {
            return applicativeReaderT(dictAlternative.Applicative0());
        },
        Plus1: function () {
            return plusReaderT(dictAlternative.Plus1());
        }
    };
};
var monadPlusReaderT = function (dictMonadPlus) {
    return {
        Monad0: function () {
            return monadReaderT(dictMonadPlus.Monad0());
        },
        Alternative1: function () {
            return alternativeReaderT(dictMonadPlus.Alternative1());
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
