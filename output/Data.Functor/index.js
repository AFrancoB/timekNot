// Generated by purs version 0.15.2
import * as $foreign from "./foreign.js";
import * as Control_Semigroupoid from "../Control.Semigroupoid/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var map = function (dict) {
    return dict.map;
};
var mapFlipped = function (dictFunctor) {
    return function (fa) {
        return function (f) {
            return map(dictFunctor)(f)(fa);
        };
    };
};
var $$void = function (dictFunctor) {
    return map(dictFunctor)(Data_Function["const"](Data_Unit.unit));
};
var voidLeft = function (dictFunctor) {
    return function (f) {
        return function (x) {
            return map(dictFunctor)(Data_Function["const"](x))(f);
        };
    };
};
var voidRight = function (dictFunctor) {
    return function (x) {
        return map(dictFunctor)(Data_Function["const"](x));
    };
};
var functorProxy = {
    map: function (v) {
        return function (v1) {
            return Type_Proxy["Proxy"].value;
        };
    }
};
var functorFn = {
    map: /* #__PURE__ */ Control_Semigroupoid.compose(Control_Semigroupoid.semigroupoidFn)
};
var functorArray = {
    map: $foreign.arrayMap
};
var flap = function (dictFunctor) {
    return function (ff) {
        return function (x) {
            return map(dictFunctor)(function (f) {
                return f(x);
            })(ff);
        };
    };
};
export {
    map,
    mapFlipped,
    $$void as void,
    voidRight,
    voidLeft,
    flap,
    functorFn,
    functorArray,
    functorProxy
};
