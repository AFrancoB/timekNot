import * as $foreign from "./foreign.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Symbol from "../Data.Symbol/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Record_Unsafe from "../Record.Unsafe/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var topRecord = function (dict) {
    return dict.topRecord;
};
var top = function (dict) {
    return dict.top;
};
var boundedUnit = {
    top: Data_Unit.unit,
    bottom: Data_Unit.unit,
    Ord0: function () {
        return Data_Ord.ordUnit;
    }
};
var boundedRecordNil = {
    topRecord: function (v) {
        return function (v1) {
            return {};
        };
    },
    bottomRecord: function (v) {
        return function (v1) {
            return {};
        };
    },
    OrdRecord0: function () {
        return Data_Ord.ordRecordNil;
    }
};
var boundedProxy = /* #__PURE__ */ (function () {
    return {
        bottom: Type_Proxy["Proxy"].value,
        top: Type_Proxy["Proxy"].value,
        Ord0: function () {
            return Data_Ord.ordProxy;
        }
    };
})();
var boundedOrdering = /* #__PURE__ */ (function () {
    return {
        top: Data_Ordering.GT.value,
        bottom: Data_Ordering.LT.value,
        Ord0: function () {
            return Data_Ord.ordOrdering;
        }
    };
})();
var boundedNumber = {
    top: $foreign.topNumber,
    bottom: $foreign.bottomNumber,
    Ord0: function () {
        return Data_Ord.ordNumber;
    }
};

// | The `Bounded` `Int` instance has `top :: Int` equal to 2^31 - 1,
// | and `bottom :: Int` equal to -2^31, since these are the largest and smallest
// | integers representable by twos-complement 32-bit integers, respectively.
var boundedInt = {
    top: $foreign.topInt,
    bottom: $foreign.bottomInt,
    Ord0: function () {
        return Data_Ord.ordInt;
    }
};

// | Characters fall within the Unicode range.
var boundedChar = {
    top: $foreign.topChar,
    bottom: $foreign.bottomChar,
    Ord0: function () {
        return Data_Ord.ordChar;
    }
};
var boundedBoolean = {
    top: true,
    bottom: false,
    Ord0: function () {
        return Data_Ord.ordBoolean;
    }
};
var bottomRecord = function (dict) {
    return dict.bottomRecord;
};
var boundedRecord = function () {
    return function (dictBoundedRecord) {
        return {
            top: topRecord(dictBoundedRecord)(Type_Proxy["Proxy"].value)(Type_Proxy["Proxy"].value),
            bottom: bottomRecord(dictBoundedRecord)(Type_Proxy["Proxy"].value)(Type_Proxy["Proxy"].value),
            Ord0: function () {
                return Data_Ord.ordRecord()(dictBoundedRecord.OrdRecord0());
            }
        };
    };
};
var bottom = function (dict) {
    return dict.bottom;
};
var boundedRecordCons = function (dictIsSymbol) {
    return function (dictBounded) {
        return function () {
            return function () {
                return function (dictBoundedRecord) {
                    return {
                        topRecord: function (v) {
                            return function (rowProxy) {
                                var tail = topRecord(dictBoundedRecord)(Type_Proxy["Proxy"].value)(rowProxy);
                                var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Type_Proxy["Proxy"].value);
                                var insert = Record_Unsafe.unsafeSet(key);
                                return insert(top(dictBounded))(tail);
                            };
                        },
                        bottomRecord: function (v) {
                            return function (rowProxy) {
                                var tail = bottomRecord(dictBoundedRecord)(Type_Proxy["Proxy"].value)(rowProxy);
                                var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Type_Proxy["Proxy"].value);
                                var insert = Record_Unsafe.unsafeSet(key);
                                return insert(bottom(dictBounded))(tail);
                            };
                        },
                        OrdRecord0: function () {
                            return Data_Ord.ordRecordCons(dictBoundedRecord.OrdRecord0())()(dictIsSymbol)(dictBounded.Ord0());
                        }
                    };
                };
            };
        };
    };
};
export {
    bottom,
    top,
    bottomRecord,
    topRecord,
    boundedBoolean,
    boundedInt,
    boundedChar,
    boundedOrdering,
    boundedUnit,
    boundedNumber,
    boundedProxy,
    boundedRecordNil,
    boundedRecordCons,
    boundedRecord
};
export {
    EQ,
    GT,
    LT,
    compare
} from "../Data.Ord/index.js";
