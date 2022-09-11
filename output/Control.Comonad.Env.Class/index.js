// Generated by purs version 0.15.4
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
    var comonadEnvT = Control_Comonad_Env_Trans.comonadEnvT(dictComonad);
    return {
        ask: function (v) {
            return Data_Tuple.fst(v);
        },
        Comonad0: function () {
            return comonadEnvT;
        }
    };
};
var comonadEnvEnvT = function (dictComonad) {
    var comonadAskEnvT1 = comonadAskEnvT(dictComonad);
    return {
        local: function (f) {
            return function (v) {
                return new Data_Tuple.Tuple(f(v.value0), v.value1);
            };
        },
        ComonadAsk0: function () {
            return comonadAskEnvT1;
        }
    };
};
var ask = function (dict) {
    return dict.ask;
};
var asks = function (dictComonadAsk) {
    var ask1 = ask(dictComonadAsk);
    return function (f) {
        return function (x) {
            return f(ask1(x));
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
