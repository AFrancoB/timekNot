// Generated by purs version 0.15.10
import * as $foreign from "./foreign.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_String_Unsafe from "../Data.String.Unsafe/index.js";
var uncons = function (v) {
    if (v === "") {
        return Data_Maybe.Nothing.value;
    };
    return new Data_Maybe.Just({
        head: Data_String_Unsafe.charAt(0)(v),
        tail: $foreign.drop(1)(v)
    });
};
var toChar = /* #__PURE__ */ (function () {
    return $foreign["_toChar"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();
var takeWhile = function (p) {
    return function (s) {
        return $foreign.take($foreign.countPrefix(p)(s))(s);
    };
};
var takeRight = function (i) {
    return function (s) {
        return $foreign.drop($foreign.length(s) - i | 0)(s);
    };
};
var stripSuffix = function (v) {
    return function (str) {
        var v1 = $foreign.splitAt($foreign.length(str) - $foreign.length(v) | 0)(str);
        var $14 = v1.after === v;
        if ($14) {
            return new Data_Maybe.Just(v1.before);
        };
        return Data_Maybe.Nothing.value;
    };
};
var stripPrefix = function (v) {
    return function (str) {
        var v1 = $foreign.splitAt($foreign.length(v))(str);
        var $20 = v1.before === v;
        if ($20) {
            return new Data_Maybe.Just(v1.after);
        };
        return Data_Maybe.Nothing.value;
    };
};
var lastIndexOf$prime = /* #__PURE__ */ (function () {
    return $foreign["_lastIndexOfStartingAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();
var lastIndexOf = /* #__PURE__ */ (function () {
    return $foreign["_lastIndexOf"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();
var indexOf$prime = /* #__PURE__ */ (function () {
    return $foreign["_indexOfStartingAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();
var indexOf = /* #__PURE__ */ (function () {
    return $foreign["_indexOf"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();
var dropWhile = function (p) {
    return function (s) {
        return $foreign.drop($foreign.countPrefix(p)(s))(s);
    };
};
var dropRight = function (i) {
    return function (s) {
        return $foreign.take($foreign.length(s) - i | 0)(s);
    };
};
var contains = function (pat) {
    var $23 = indexOf(pat);
    return function ($24) {
        return Data_Maybe.isJust($23($24));
    };
};
var charAt = /* #__PURE__ */ (function () {
    return $foreign["_charAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();
export {
    singleton,
    fromCharArray,
    toCharArray,
    length,
    countPrefix,
    take,
    drop,
    slice,
    splitAt
} from "./foreign.js";
export {
    stripPrefix,
    stripSuffix,
    contains,
    charAt,
    toChar,
    uncons,
    indexOf,
    indexOf$prime,
    lastIndexOf,
    lastIndexOf$prime,
    takeRight,
    takeWhile,
    dropRight,
    dropWhile
};
