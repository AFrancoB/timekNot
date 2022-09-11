// Generated by purs version 0.15.4
import * as $foreign from "./foreign.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Bounded from "../Data.Bounded/index.js";
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_String_CodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data_String_Common from "../Data.String.Common/index.js";
import * as Data_String_Unsafe from "../Data.String.Unsafe/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";
var $runtime_lazy = function (name, moduleName, init) {
    var state = 0;
    var val;
    return function (lineNumber) {
        if (state === 2) return val;
        if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
        state = 1;
        val = init();
        state = 2;
        return val;
    };
};
var fromEnum = /* #__PURE__ */ Data_Enum.fromEnum(Data_Enum.boundedEnumChar);
var map = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var unfoldr = /* #__PURE__ */ Data_Unfoldable.unfoldr(Data_Unfoldable.unfoldableArray);
var div = /* #__PURE__ */ Data_EuclideanRing.div(Data_EuclideanRing.euclideanRingInt);
var mod = /* #__PURE__ */ Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt);
var compare = /* #__PURE__ */ Data_Ord.compare(Data_Ord.ordInt);
var CodePoint = function (x) {
    return x;
};
var unsurrogate = function (lead) {
    return function (trail) {
        return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
};
var showCodePoint = {
    show: function (v) {
        return "(CodePoint 0x" + (Data_String_Common.toUpper(Data_Int.toStringAs(Data_Int.hexadecimal)(v)) + ")");
    }
};
var isTrail = function (cu) {
    return 56320 <= cu && cu <= 57343;
};
var isLead = function (cu) {
    return 55296 <= cu && cu <= 56319;
};
var uncons = function (s) {
    var v = Data_String_CodeUnits.length(s);
    if (v === 0) {
        return Data_Maybe.Nothing.value;
    };
    if (v === 1) {
        return new Data_Maybe.Just({
            head: fromEnum(Data_String_Unsafe.charAt(0)(s)),
            tail: ""
        });
    };
    var cu1 = fromEnum(Data_String_Unsafe.charAt(1)(s));
    var cu0 = fromEnum(Data_String_Unsafe.charAt(0)(s));
    var $42 = isLead(cu0) && isTrail(cu1);
    if ($42) {
        return new Data_Maybe.Just({
            head: unsurrogate(cu0)(cu1),
            tail: Data_String_CodeUnits.drop(2)(s)
        });
    };
    return new Data_Maybe.Just({
        head: cu0,
        tail: Data_String_CodeUnits.drop(1)(s)
    });
};
var unconsButWithTuple = function (s) {
    return map(function (v) {
        return new Data_Tuple.Tuple(v.head, v.tail);
    })(uncons(s));
};
var toCodePointArrayFallback = function (s) {
    return unfoldr(unconsButWithTuple)(s);
};
var unsafeCodePointAt0Fallback = function (s) {
    var cu0 = fromEnum(Data_String_Unsafe.charAt(0)(s));
    var $46 = isLead(cu0) && Data_String_CodeUnits.length(s) > 1;
    if ($46) {
        var cu1 = fromEnum(Data_String_Unsafe.charAt(1)(s));
        var $47 = isTrail(cu1);
        if ($47) {
            return unsurrogate(cu0)(cu1);
        };
        return cu0;
    };
    return cu0;
};
var unsafeCodePointAt0 = /* #__PURE__ */ $foreign["_unsafeCodePointAt0"](unsafeCodePointAt0Fallback);
var toCodePointArray = /* #__PURE__ */ $foreign["_toCodePointArray"](toCodePointArrayFallback)(unsafeCodePointAt0);
var length = function ($73) {
    return Data_Array.length(toCodePointArray($73));
};
var lastIndexOf = function (p) {
    return function (s) {
        return map(function (i) {
            return length(Data_String_CodeUnits.take(i)(s));
        })(Data_String_CodeUnits.lastIndexOf(p)(s));
    };
};
var indexOf = function (p) {
    return function (s) {
        return map(function (i) {
            return length(Data_String_CodeUnits.take(i)(s));
        })(Data_String_CodeUnits.indexOf(p)(s));
    };
};
var fromCharCode = /* #__PURE__ */ (function () {
    var $74 = Data_Enum.toEnumWithDefaults(Data_Enum.boundedEnumChar)(Data_Bounded.bottom(Data_Bounded.boundedChar))(Data_Bounded.top(Data_Bounded.boundedChar));
    return function ($75) {
        return Data_String_CodeUnits.singleton($74($75));
    };
})();
var singletonFallback = function (v) {
    if (v <= 65535) {
        return fromCharCode(v);
    };
    var lead = div(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = mod(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode(lead) + fromCharCode(trail);
};
var fromCodePointArray = /* #__PURE__ */ $foreign["_fromCodePointArray"](singletonFallback);
var singleton = /* #__PURE__ */ $foreign["_singleton"](singletonFallback);
var takeFallback = function (n) {
    return function (v) {
        if (n < 1) {
            return "";
        };
        var v1 = uncons(v);
        if (v1 instanceof Data_Maybe.Just) {
            return singleton(v1.value0.head) + takeFallback(n - 1 | 0)(v1.value0.tail);
        };
        return v;
    };
};
var take = /* #__PURE__ */ $foreign["_take"](takeFallback);
var lastIndexOf$prime = function (p) {
    return function (i) {
        return function (s) {
            var i$prime = Data_String_CodeUnits.length(take(i)(s));
            return map(function (k) {
                return length(Data_String_CodeUnits.take(k)(s));
            })(Data_String_CodeUnits["lastIndexOf$prime"](p)(i$prime)(s));
        };
    };
};
var splitAt = function (i) {
    return function (s) {
        var before = take(i)(s);
        return {
            before: before,
            after: Data_String_CodeUnits.drop(Data_String_CodeUnits.length(before))(s)
        };
    };
};
var eqCodePoint = {
    eq: function (x) {
        return function (y) {
            return x === y;
        };
    }
};
var ordCodePoint = {
    compare: function (x) {
        return function (y) {
            return compare(x)(y);
        };
    },
    Eq0: function () {
        return eqCodePoint;
    }
};
var drop = function (n) {
    return function (s) {
        return Data_String_CodeUnits.drop(Data_String_CodeUnits.length(take(n)(s)))(s);
    };
};
var indexOf$prime = function (p) {
    return function (i) {
        return function (s) {
            var s$prime = drop(i)(s);
            return map(function (k) {
                return i + length(Data_String_CodeUnits.take(k)(s$prime)) | 0;
            })(Data_String_CodeUnits.indexOf(p)(s$prime));
        };
    };
};
var countTail = function ($copy_p) {
    return function ($copy_s) {
        return function ($copy_accum) {
            var $tco_var_p = $copy_p;
            var $tco_var_s = $copy_s;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(p, s, accum) {
                var v = uncons(s);
                if (v instanceof Data_Maybe.Just) {
                    var $60 = p(v.value0.head);
                    if ($60) {
                        $tco_var_p = p;
                        $tco_var_s = v.value0.tail;
                        $copy_accum = accum + 1 | 0;
                        return;
                    };
                    $tco_done = true;
                    return accum;
                };
                $tco_done = true;
                return accum;
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_p, $tco_var_s, $copy_accum);
            };
            return $tco_result;
        };
    };
};
var countFallback = function (p) {
    return function (s) {
        return countTail(p)(s)(0);
    };
};
var countPrefix = /* #__PURE__ */ $foreign["_countPrefix"](countFallback)(unsafeCodePointAt0);
var dropWhile = function (p) {
    return function (s) {
        return drop(countPrefix(p)(s))(s);
    };
};
var takeWhile = function (p) {
    return function (s) {
        return take(countPrefix(p)(s))(s);
    };
};
var codePointFromChar = function ($76) {
    return CodePoint(fromEnum($76));
};
var codePointAtFallback = function ($copy_n) {
    return function ($copy_s) {
        var $tco_var_n = $copy_n;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(n, s) {
            var v = uncons(s);
            if (v instanceof Data_Maybe.Just) {
                var $65 = n === 0;
                if ($65) {
                    $tco_done = true;
                    return new Data_Maybe.Just(v.value0.head);
                };
                $tco_var_n = n - 1 | 0;
                $copy_s = v.value0.tail;
                return;
            };
            $tco_done = true;
            return Data_Maybe.Nothing.value;
        };
        while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_n, $copy_s);
        };
        return $tco_result;
    };
};
var codePointAt = function (v) {
    return function (v1) {
        if (v < 0) {
            return Data_Maybe.Nothing.value;
        };
        if (v === 0 && v1 === "") {
            return Data_Maybe.Nothing.value;
        };
        if (v === 0) {
            return new Data_Maybe.Just(unsafeCodePointAt0(v1));
        };
        return $foreign["_codePointAt"](codePointAtFallback)(Data_Maybe.Just.create)(Data_Maybe.Nothing.value)(unsafeCodePointAt0)(v)(v1);
    };
};
var boundedCodePoint = {
    bottom: 0,
    top: 1114111,
    Ord0: function () {
        return ordCodePoint;
    }
};
var boundedEnumCodePoint = /* #__PURE__ */ (function () {
    return {
        cardinality: 1114111 + 1 | 0,
        fromEnum: function (v) {
            return v;
        },
        toEnum: function (n) {
            if (n >= 0 && n <= 1114111) {
                return new Data_Maybe.Just(n);
            };
            if (Data_Boolean.otherwise) {
                return Data_Maybe.Nothing.value;
            };
            throw new Error("Failed pattern match at Data.String.CodePoints (line 63, column 1 - line 68, column 26): " + [ n.constructor.name ]);
        },
        Bounded0: function () {
            return boundedCodePoint;
        },
        Enum1: function () {
            return $lazy_enumCodePoint(0);
        }
    };
})();
var $lazy_enumCodePoint = /* #__PURE__ */ $runtime_lazy("enumCodePoint", "Data.String.CodePoints", function () {
    return {
        succ: Data_Enum.defaultSucc(Data_Enum.toEnum(boundedEnumCodePoint))(Data_Enum.fromEnum(boundedEnumCodePoint)),
        pred: Data_Enum.defaultPred(Data_Enum.toEnum(boundedEnumCodePoint))(Data_Enum.fromEnum(boundedEnumCodePoint)),
        Ord0: function () {
            return ordCodePoint;
        }
    };
});
var enumCodePoint = /* #__PURE__ */ $lazy_enumCodePoint(59);
export {
    codePointFromChar,
    singleton,
    fromCodePointArray,
    toCodePointArray,
    codePointAt,
    uncons,
    length,
    countPrefix,
    indexOf,
    indexOf$prime,
    lastIndexOf,
    lastIndexOf$prime,
    take,
    takeWhile,
    drop,
    dropWhile,
    splitAt,
    eqCodePoint,
    ordCodePoint,
    showCodePoint,
    boundedCodePoint,
    enumCodePoint,
    boundedEnumCodePoint
};
export {
    contains,
    stripPrefix,
    stripSuffix
} from "../Data.String.CodeUnits/index.js";
