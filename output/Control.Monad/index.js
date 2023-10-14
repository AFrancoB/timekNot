// Generated by purs version 0.15.10
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
var whenM = function (dictMonad) {
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var when = Control_Applicative.when(dictMonad.Applicative0());
    return function (mb) {
        return function (m) {
            return bind(mb)(function (b) {
                return when(b)(m);
            });
        };
    };
};
var unlessM = function (dictMonad) {
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var unless = Control_Applicative.unless(dictMonad.Applicative0());
    return function (mb) {
        return function (m) {
            return bind(mb)(function (b) {
                return unless(b)(m);
            });
        };
    };
};
var monadProxy = {
    Applicative0: function () {
        return Control_Applicative.applicativeProxy;
    },
    Bind1: function () {
        return Control_Bind.bindProxy;
    }
};
var monadFn = {
    Applicative0: function () {
        return Control_Applicative.applicativeFn;
    },
    Bind1: function () {
        return Control_Bind.bindFn;
    }
};
var monadArray = {
    Applicative0: function () {
        return Control_Applicative.applicativeArray;
    },
    Bind1: function () {
        return Control_Bind.bindArray;
    }
};
var liftM1 = function (dictMonad) {
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var pure = Control_Applicative.pure(dictMonad.Applicative0());
    return function (f) {
        return function (a) {
            return bind(a)(function (a$prime) {
                return pure(f(a$prime));
            });
        };
    };
};
var ap = function (dictMonad) {
    var bind = Control_Bind.bind(dictMonad.Bind1());
    var pure = Control_Applicative.pure(dictMonad.Applicative0());
    return function (f) {
        return function (a) {
            return bind(f)(function (f$prime) {
                return bind(a)(function (a$prime) {
                    return pure(f$prime(a$prime));
                });
            });
        };
    };
};
export {
    liftM1,
    whenM,
    unlessM,
    ap,
    monadFn,
    monadArray,
    monadProxy
};
export {
    liftA1,
    pure,
    unless,
    when
} from "../Control.Applicative/index.js";
export {
    apply
} from "../Control.Apply/index.js";
export {
    bind,
    ifM,
    join
} from "../Control.Bind/index.js";
export {
    map,
    void
} from "../Data.Functor/index.js";
