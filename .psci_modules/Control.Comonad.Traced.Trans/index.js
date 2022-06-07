// | This module defines the cowriter comonad transformer, `TracedT`.
import * as Control_Comonad from "../Control.Comonad/index.js";
import * as Control_Extend from "../Control.Extend/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";

// | The cowriter comonad transformer.
// |
// | This comonad transformer extends the context of a value in the base comonad so that the value
// | depends on a monoidal position of type `t`.
// |
// | The `ComonadTraced` type class describes the operations supported by this comonad.
var TracedT = function (x) {
    return x;
};

// | Unwrap a value in the `TracedT` comonad.
var runTracedT = function (v) {
    return v;
};
var newtypeTracedT = {
    Coercible0: function () {
        return undefined;
    }
};
var functorTracedT = function (dictFunctor) {
    return {
        map: function (f) {
            return function (v) {
                return Data_Functor.map(dictFunctor)(function (g) {
                    return function (t) {
                        return f(g(t));
                    };
                })(v);
            };
        }
    };
};
var extendTracedT = function (dictExtend) {
    return function (dictSemigroup) {
        return {
            extend: function (f) {
                return function (v) {
                    return Control_Extend.extend(dictExtend)(function (w$prime) {
                        return function (t) {
                            return f(Data_Functor.map(dictExtend.Functor0())(function (h) {
                                return function (t$prime) {
                                    return h(Data_Semigroup.append(dictSemigroup)(t)(t$prime));
                                };
                            })(w$prime));
                        };
                    })(v);
                };
            },
            Functor0: function () {
                return functorTracedT(dictExtend.Functor0());
            }
        };
    };
};
var comonadTransTracedT = function (dictMonoid) {
    return {
        lower: function (dictComonad) {
            return function (v) {
                return Data_Functor.map((dictComonad.Extend0()).Functor0())(function (f) {
                    return f(Data_Monoid.mempty(dictMonoid));
                })(v);
            };
        }
    };
};
var comonadTracedT = function (dictComonad) {
    return function (dictMonoid) {
        return {
            extract: function (v) {
                return Control_Comonad.extract(dictComonad)(v)(Data_Monoid.mempty(dictMonoid));
            },
            Extend0: function () {
                return extendTracedT(dictComonad.Extend0())(dictMonoid.Semigroup0());
            }
        };
    };
};
export {
    TracedT,
    runTracedT,
    newtypeTracedT,
    functorTracedT,
    extendTracedT,
    comonadTracedT,
    comonadTransTracedT
};
