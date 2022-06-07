// | This module defines the `ComonadTraced` type class and its instances.
import * as Control_Comonad from "../Control.Comonad/index.js";
import * as Control_Comonad_Traced_Trans from "../Control.Comonad.Traced.Trans/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var track = function (dict) {
    return dict.track;
};

// | Extracts a value at a relative position which depends on the current value.
var tracks = function (dictComonadTraced) {
    return function (f) {
        return function (w) {
            return track(dictComonadTraced)(f(Control_Comonad.extract(dictComonadTraced.Comonad0())(w)))(w);
        };
    };
};

// | Get a value which depends on the current position.
var listens = function (dictFunctor) {
    return function (f) {
        return function (v) {
            return Data_Functor.map(dictFunctor)(function (g) {
                return function (t) {
                    return new Data_Tuple.Tuple(g(t), f(t));
                };
            })(v);
        };
    };
};

// | Get the current position.
var listen = function (dictFunctor) {
    return function (v) {
        return Data_Functor.map(dictFunctor)(function (f) {
            return function (t) {
                return new Data_Tuple.Tuple(f(t), t);
            };
        })(v);
    };
};
var comonadTracedTracedT = function (dictComonad) {
    return function (dictMonoid) {
        return {
            track: function (t) {
                return function (v) {
                    return Control_Comonad.extract(dictComonad)(v)(t);
                };
            },
            Comonad0: function () {
                return Control_Comonad_Traced_Trans.comonadTracedT(dictComonad)(dictMonoid);
            }
        };
    };
};

// | Apply a function to the current position.
var censor = function (dictFunctor) {
    return function (f) {
        return function (v) {
            return Data_Functor.map(dictFunctor)(function (v1) {
                return function ($19) {
                    return v1(f($19));
                };
            })(v);
        };
    };
};
export {
    track,
    tracks,
    listen,
    listens,
    censor,
    comonadTracedTracedT
};
