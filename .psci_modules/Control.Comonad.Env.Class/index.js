// | This module defines the `ComonadEnv` type class and its instances.
import * as Control_Comonad_Env_Trans from "../Control.Comonad.Env.Trans/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var local = function (dict) {
    return dict.local;
};
var comonadAskTuple = {
    ask: Data_Tuple.fst,
    Comonad0: function () {
        return Data_Tuple.comonadTuple;
    }
};
var comonadEnvTuple = {
    local: function (f) {
        return function (v) {
            return new Data_Tuple.Tuple(f(v.value0), v.value1);
        };
    },
    ComonadAsk0: function () {
        return comonadAskTuple;
    }
};
var comonadAskEnvT = function (dictComonad) {
    return {
        ask: function (v) {
            return Data_Tuple.fst(v);
        },
        Comonad0: function () {
            return Control_Comonad_Env_Trans.comonadEnvT(dictComonad);
        }
    };
};
var comonadEnvEnvT = function (dictComonad) {
    return {
        local: function (f) {
            return function (v) {
                return new Data_Tuple.Tuple(f(v.value0), v.value1);
            };
        },
        ComonadAsk0: function () {
            return comonadAskEnvT(dictComonad);
        }
    };
};
var ask = function (dict) {
    return dict.ask;
};

// | Get a value which depends on the environment.
var asks = function (dictComonadAsk) {
    return function (f) {
        return function (x) {
            return f(ask(dictComonadAsk)(x));
        };
    };
};
export {
    ask,
    local,
    asks,
    comonadAskTuple,
    comonadEnvTuple,
    comonadAskEnvT,
    comonadEnvEnvT
};
