// | This module defines the CPS monad transformer.
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Cont_Class from "../Control.Monad.Cont.Class/index.js";
import * as Control_Monad_Reader_Class from "../Control.Monad.Reader.Class/index.js";
import * as Control_Monad_State_Class from "../Control.Monad.State.Class/index.js";
import * as Control_Monad_Trans_Class from "../Control.Monad.Trans.Class/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Effect_Class from "../Effect.Class/index.js";
var ContT = function (x) {
    return x;
};

// | Modify the continuation in a `ContT` monad action
var withContT = function (f) {
    return function (v) {
        return function (k) {
            return v(f(k));
        };
    };
};

// | Run a computation in the `ContT` monad, by providing a continuation.
var runContT = function (v) {
    return function (k) {
        return v(k);
    };
};
var newtypeContT = {
    Coercible0: function () {
        return undefined;
    }
};
var monadTransContT = {
    lift: function (dictMonad) {
        return function (m) {
            return function (k) {
                return Control_Bind.bind(dictMonad.Bind1())(m)(k);
            };
        };
    }
};

// | Modify the underlying action in a `ContT` monad action.
var mapContT = function (f) {
    return function (v) {
        return function (k) {
            return f(v(k));
        };
    };
};
var functorContT = function (dictFunctor) {
    return {
        map: function (f) {
            return function (v) {
                return function (k) {
                    return v(function (a) {
                        return k(f(a));
                    });
                };
            };
        }
    };
};
var applyContT = function (dictApply) {
    return {
        apply: function (v) {
            return function (v1) {
                return function (k) {
                    return v(function (g) {
                        return v1(function (a) {
                            return k(g(a));
                        });
                    });
                };
            };
        },
        Functor0: function () {
            return functorContT(dictApply.Functor0());
        }
    };
};
var bindContT = function (dictBind) {
    return {
        bind: function (v) {
            return function (k) {
                return function (k$prime) {
                    return v(function (a) {
                        var v1 = k(a);
                        return v1(k$prime);
                    });
                };
            };
        },
        Apply0: function () {
            return applyContT(dictBind.Apply0());
        }
    };
};
var semigroupContT = function (dictApply) {
    return function (dictSemigroup) {
        return {
            append: Control_Apply.lift2(applyContT(dictApply))(Data_Semigroup.append(dictSemigroup))
        };
    };
};
var applicativeContT = function (dictApplicative) {
    return {
        pure: function (a) {
            return function (k) {
                return k(a);
            };
        },
        Apply0: function () {
            return applyContT(dictApplicative.Apply0());
        }
    };
};
var monadContT = function (dictMonad) {
    return {
        Applicative0: function () {
            return applicativeContT(dictMonad.Applicative0());
        },
        Bind1: function () {
            return bindContT(dictMonad.Bind1());
        }
    };
};
var monadAskContT = function (dictMonadAsk) {
    return {
        ask: Control_Monad_Trans_Class.lift(monadTransContT)(dictMonadAsk.Monad0())(Control_Monad_Reader_Class.ask(dictMonadAsk)),
        Monad0: function () {
            return monadContT(dictMonadAsk.Monad0());
        }
    };
};
var monadReaderContT = function (dictMonadReader) {
    return {
        local: function (f) {
            return function (v) {
                return function (k) {
                    return Control_Bind.bind(((dictMonadReader.MonadAsk0()).Monad0()).Bind1())(Control_Monad_Reader_Class.ask(dictMonadReader.MonadAsk0()))(function (r) {
                        return Control_Monad_Reader_Class.local(dictMonadReader)(f)(v((function () {
                            var $44 = Control_Monad_Reader_Class.local(dictMonadReader)(Data_Function["const"](r));
                            return function ($45) {
                                return $44(k($45));
                            };
                        })()));
                    });
                };
            };
        },
        MonadAsk0: function () {
            return monadAskContT(dictMonadReader.MonadAsk0());
        }
    };
};
var monadContContT = function (dictMonad) {
    return {
        callCC: function (f) {
            return function (k) {
                var v = f(function (a) {
                    return function (v1) {
                        return k(a);
                    };
                });
                return v(k);
            };
        },
        Monad0: function () {
            return monadContT(dictMonad);
        }
    };
};
var monadEffectContT = function (dictMonadEffect) {
    return {
        liftEffect: (function () {
            var $46 = Control_Monad_Trans_Class.lift(monadTransContT)(dictMonadEffect.Monad0());
            var $47 = Effect_Class.liftEffect(dictMonadEffect);
            return function ($48) {
                return $46($47($48));
            };
        })(),
        Monad0: function () {
            return monadContT(dictMonadEffect.Monad0());
        }
    };
};
var monadStateContT = function (dictMonadState) {
    return {
        state: (function () {
            var $49 = Control_Monad_Trans_Class.lift(monadTransContT)(dictMonadState.Monad0());
            var $50 = Control_Monad_State_Class.state(dictMonadState);
            return function ($51) {
                return $49($50($51));
            };
        })(),
        Monad0: function () {
            return monadContT(dictMonadState.Monad0());
        }
    };
};
var monoidContT = function (dictApplicative) {
    return function (dictMonoid) {
        return {
            mempty: Control_Applicative.pure(applicativeContT(dictApplicative))(Data_Monoid.mempty(dictMonoid)),
            Semigroup0: function () {
                return semigroupContT(dictApplicative.Apply0())(dictMonoid.Semigroup0());
            }
        };
    };
};
export {
    ContT,
    runContT,
    mapContT,
    withContT,
    newtypeContT,
    monadContContT,
    functorContT,
    applyContT,
    applicativeContT,
    bindContT,
    monadContT,
    monadTransContT,
    monadEffectContT,
    monadAskContT,
    monadReaderContT,
    monadStateContT,
    semigroupContT,
    monoidContT
};
export {
    callCC
} from "../Control.Monad.Cont.Class/index.js";
export {
    lift
} from "../Control.Monad.Trans.Class/index.js";
