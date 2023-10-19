// Generated by purs version 0.15.10
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Monad from "../Control.Monad/index.js";
import * as Control_Semigroupoid from "../Control.Semigroupoid/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
var monadAskFun = {
    ask: /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn),
    Monad0: function () {
        return Control_Monad.monadFn;
    }
};
var monadReaderFun = {
    local: /* #__PURE__ */ Control_Semigroupoid.composeFlipped(Control_Semigroupoid.semigroupoidFn),
    MonadAsk0: function () {
        return monadAskFun;
    }
};
var local = function (dict) {
    return dict.local;
};
var ask = function (dict) {
    return dict.ask;
};
var asks = function (dictMonadAsk) {
    var map = Data_Functor.map((((dictMonadAsk.Monad0()).Bind1()).Apply0()).Functor0());
    var ask1 = ask(dictMonadAsk);
    return function (f) {
        return map(f)(ask1);
    };
};
export {
    ask,
    local,
    asks,
    monadAskFun,
    monadReaderFun
};
