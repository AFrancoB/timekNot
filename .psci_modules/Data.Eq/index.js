import * as $foreign from "./foreign.js";
import * as Data_Symbol from "../Data.Symbol/index.js";
import * as Record_Unsafe from "../Record.Unsafe/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var eqVoid = {
    eq: function (v) {
        return function (v1) {
            return true;
        };
    }
};
var eqUnit = {
    eq: function (v) {
        return function (v1) {
            return true;
        };
    }
};
var eqString = {
    eq: $foreign.eqStringImpl
};
var eqRowNil = {
    eqRecord: function (v) {
        return function (v1) {
            return function (v2) {
                return true;
            };
        };
    }
};
var eqRecord = function (dict) {
    return dict.eqRecord;
};
var eqRec = function () {
    return function (dictEqRecord) {
        return {
            eq: eqRecord(dictEqRecord)(Type_Proxy["Proxy"].value)
        };
    };
};
var eqProxy = {
    eq: function (v) {
        return function (v1) {
            return true;
        };
    }
};
var eqNumber = {
    eq: $foreign.eqNumberImpl
};
var eqInt = {
    eq: $foreign.eqIntImpl
};
var eqChar = {
    eq: $foreign.eqCharImpl
};
var eqBoolean = {
    eq: $foreign.eqBooleanImpl
};
var eq1 = function (dict) {
    return dict.eq1;
};
var eq = function (dict) {
    return dict.eq;
};
var eqArray = function (dictEq) {
    return {
        eq: $foreign.eqArrayImpl(eq(dictEq))
    };
};
var eq1Array = {
    eq1: function (dictEq) {
        return eq(eqArray(dictEq));
    }
};
var eqRowCons = function (dictEqRecord) {
    return function () {
        return function (dictIsSymbol) {
            return function (dictEq) {
                return {
                    eqRecord: function (v) {
                        return function (ra) {
                            return function (rb) {
                                var tail = eqRecord(dictEqRecord)(Type_Proxy["Proxy"].value)(ra)(rb);
                                var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Type_Proxy["Proxy"].value);
                                var get = Record_Unsafe.unsafeGet(key);
                                return eq(dictEq)(get(ra))(get(rb)) && tail;
                            };
                        };
                    }
                };
            };
        };
    };
};

// | `notEq` tests whether one value is _not equal_ to another. Shorthand for
// | `not (eq x y)`.
var notEq = function (dictEq) {
    return function (x) {
        return function (y) {
            return eq(eqBoolean)(eq(dictEq)(x)(y))(false);
        };
    };
};
var notEq1 = function (dictEq1) {
    return function (dictEq) {
        return function (x) {
            return function (y) {
                return eq(eqBoolean)(eq1(dictEq1)(dictEq)(x)(y))(false);
            };
        };
    };
};
export {
    eq,
    notEq,
    eq1,
    notEq1,
    eqRecord,
    eqBoolean,
    eqInt,
    eqNumber,
    eqChar,
    eqString,
    eqUnit,
    eqVoid,
    eqArray,
    eqRec,
    eqProxy,
    eq1Array,
    eqRowNil,
    eqRowCons
};
