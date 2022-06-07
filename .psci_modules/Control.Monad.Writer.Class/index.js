// | This module defines the `MonadWriter` type class and its instances.
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var tell = function (dict) {
    return dict.tell;
};
var pass = function (dict) {
    return dict.pass;
};
var listen = function (dict) {
    return dict.listen;
};

// | Projects a value from modifications made to the accumulator during an
// | action.
var listens = function (dictMonadWriter) {
    return function (f) {
        return function (m) {
            return Control_Bind.bind(((dictMonadWriter.MonadTell1()).Monad1()).Bind1())(listen(dictMonadWriter)(m))(function (v) {
                return Control_Applicative.pure(((dictMonadWriter.MonadTell1()).Monad1()).Applicative0())(new Data_Tuple.Tuple(v.value0, f(v.value1)));
            });
        };
    };
};

// | Modify the final accumulator value by applying a function.
var censor = function (dictMonadWriter) {
    return function (f) {
        return function (m) {
            return pass(dictMonadWriter)(Control_Bind.bind(((dictMonadWriter.MonadTell1()).Monad1()).Bind1())(m)(function (a) {
                return Control_Applicative.pure(((dictMonadWriter.MonadTell1()).Monad1()).Applicative0())(new Data_Tuple.Tuple(a, f));
            }));
        };
    };
};
export {
    listen,
    pass,
    tell,
    listens,
    censor
};
