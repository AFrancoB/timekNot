// | This module defines the `ComonadStore` type class and its instances.
import * as Control_Comonad from "../Control.Comonad/index.js";
import * as Control_Comonad_Env_Trans from "../Control.Comonad.Env.Trans/index.js";
import * as Control_Comonad_Store_Trans from "../Control.Comonad.Store.Trans/index.js";
import * as Control_Comonad_Traced_Trans from "../Control.Comonad.Traced.Trans/index.js";
import * as Control_Comonad_Trans_Class from "../Control.Comonad.Trans.Class/index.js";
import * as Control_Extend from "../Control.Extend/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
var pos = function (dict) {
    return dict.pos;
};
var peek = function (dict) {
    return dict.peek;
};

// | Extract a value from a position which depends on the current position.
var peeks = function (dictComonadStore) {
    return function (f) {
        return function (x) {
            return peek(dictComonadStore)(f(pos(dictComonadStore)(x)))(x);
        };
    };
};

// | Reposition the focus at the specified position, which depends on the current position.
var seeks = function (dictComonadStore) {
    return function (f) {
        var $20 = peeks(dictComonadStore)(f);
        var $21 = Control_Extend.duplicate((dictComonadStore.Comonad0()).Extend0());
        return function ($22) {
            return $20($21($22));
        };
    };
};

// | Reposition the focus at the specified position.
var seek = function (dictComonadStore) {
    return function (s) {
        var $23 = peek(dictComonadStore)(s);
        var $24 = Control_Extend.duplicate((dictComonadStore.Comonad0()).Extend0());
        return function ($25) {
            return $23($24($25));
        };
    };
};

// | Extract a collection of values from positions which depend on the current position.
var experiment = function (dictComonadStore) {
    return function (dictFunctor) {
        return function (f) {
            return function (x) {
                return Data_Functor.map(dictFunctor)(Data_Function.flip(peek(dictComonadStore))(x))(f(pos(dictComonadStore)(x)));
            };
        };
    };
};
var comonadStoreTracedT = function (dictComonadStore) {
    return function (dictMonoid) {
        return {
            pos: (function () {
                var $26 = pos(dictComonadStore);
                var $27 = Control_Comonad_Trans_Class.lower(Control_Comonad_Traced_Trans.comonadTransTracedT(dictMonoid))(dictComonadStore.Comonad0());
                return function ($28) {
                    return $26($27($28));
                };
            })(),
            peek: function (s) {
                var $29 = peek(dictComonadStore)(s);
                var $30 = Control_Comonad_Trans_Class.lower(Control_Comonad_Traced_Trans.comonadTransTracedT(dictMonoid))(dictComonadStore.Comonad0());
                return function ($31) {
                    return $29($30($31));
                };
            },
            Comonad0: function () {
                return Control_Comonad_Traced_Trans.comonadTracedT(dictComonadStore.Comonad0())(dictMonoid);
            }
        };
    };
};
var comonadStoreStoreT = function (dictComonad) {
    return {
        pos: function (v) {
            return v.value1;
        },
        peek: function (s) {
            return function (v) {
                return Control_Comonad.extract(dictComonad)(v.value0)(s);
            };
        },
        Comonad0: function () {
            return Control_Comonad_Store_Trans.comonadStoreT(dictComonad);
        }
    };
};
var comonadStoreEnvT = function (dictComonadStore) {
    return {
        pos: (function () {
            var $32 = pos(dictComonadStore);
            var $33 = Control_Comonad_Trans_Class.lower(Control_Comonad_Env_Trans.comonadTransEnvT)(dictComonadStore.Comonad0());
            return function ($34) {
                return $32($33($34));
            };
        })(),
        peek: function (s) {
            var $35 = peek(dictComonadStore)(s);
            var $36 = Control_Comonad_Trans_Class.lower(Control_Comonad_Env_Trans.comonadTransEnvT)(dictComonadStore.Comonad0());
            return function ($37) {
                return $35($36($37));
            };
        },
        Comonad0: function () {
            return Control_Comonad_Env_Trans.comonadEnvT(dictComonadStore.Comonad0());
        }
    };
};
export {
    peek,
    pos,
    experiment,
    peeks,
    seek,
    seeks,
    comonadStoreStoreT,
    comonadStoreEnvT,
    comonadStoreTracedT
};
