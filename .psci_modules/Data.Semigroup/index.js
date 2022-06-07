import * as $foreign from "./foreign.js";
import * as Data_Symbol from "../Data.Symbol/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Data_Void from "../Data.Void/index.js";
import * as Record_Unsafe from "../Record.Unsafe/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var semigroupVoid = {
    append: function (v) {
        return Data_Void.absurd;
    }
};
var semigroupUnit = {
    append: function (v) {
        return function (v1) {
            return Data_Unit.unit;
        };
    }
};
var semigroupString = {
    append: $foreign.concatString
};
var semigroupRecordNil = {
    appendRecord: function (v) {
        return function (v1) {
            return function (v2) {
                return {};
            };
        };
    }
};
var semigroupProxy = {
    append: function (v) {
        return function (v1) {
            return Type_Proxy["Proxy"].value;
        };
    }
};
var semigroupArray = {
    append: $foreign.concatArray
};
var appendRecord = function (dict) {
    return dict.appendRecord;
};
var semigroupRecord = function () {
    return function (dictSemigroupRecord) {
        return {
            append: appendRecord(dictSemigroupRecord)(Type_Proxy["Proxy"].value)
        };
    };
};
var append = function (dict) {
    return dict.append;
};
var semigroupFn = function (dictSemigroup) {
    return {
        append: function (f) {
            return function (g) {
                return function (x) {
                    return append(dictSemigroup)(f(x))(g(x));
                };
            };
        }
    };
};
var semigroupRecordCons = function (dictIsSymbol) {
    return function () {
        return function (dictSemigroupRecord) {
            return function (dictSemigroup) {
                return {
                    appendRecord: function (v) {
                        return function (ra) {
                            return function (rb) {
                                var tail = appendRecord(dictSemigroupRecord)(Type_Proxy["Proxy"].value)(ra)(rb);
                                var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Type_Proxy["Proxy"].value);
                                var insert = Record_Unsafe.unsafeSet(key);
                                var get = Record_Unsafe.unsafeGet(key);
                                return insert(append(dictSemigroup)(get(ra))(get(rb)))(tail);
                            };
                        };
                    }
                };
            };
        };
    };
};
export {
    append,
    appendRecord,
    semigroupString,
    semigroupUnit,
    semigroupVoid,
    semigroupFn,
    semigroupArray,
    semigroupProxy,
    semigroupRecord,
    semigroupRecordNil,
    semigroupRecordCons
};
