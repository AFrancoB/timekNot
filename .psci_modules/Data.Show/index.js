import * as $foreign from "./foreign.js";
import * as Data_Symbol from "../Data.Symbol/index.js";
import * as Record_Unsafe from "../Record.Unsafe/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var showString = {
    show: $foreign.showStringImpl
};
var showRecordFieldsNil = {
    showRecordFields: function (v) {
        return function (v1) {
            return [  ];
        };
    }
};
var showRecordFields = function (dict) {
    return dict.showRecordFields;
};
var showRecord = function () {
    return function () {
        return function (dictShowRecordFields) {
            return {
                show: function (record) {
                    var v = showRecordFields(dictShowRecordFields)(Type_Proxy["Proxy"].value)(record);
                    if (v.length === 0) {
                        return "{}";
                    };
                    return $foreign.intercalate(" ")([ "{", $foreign.intercalate(", ")(v), "}" ]);
                }
            };
        };
    };
};
var showProxy = {
    show: function (v) {
        return "Proxy";
    }
};
var showNumber = {
    show: $foreign.showNumberImpl
};
var showInt = {
    show: $foreign.showIntImpl
};
var showChar = {
    show: $foreign.showCharImpl
};
var showBoolean = {
    show: function (v) {
        if (v) {
            return "true";
        };
        if (!v) {
            return "false";
        };
        throw new Error("Failed pattern match at Data.Show (line 23, column 1 - line 25, column 23): " + [ v.constructor.name ]);
    }
};
var show = function (dict) {
    return dict.show;
};
var showArray = function (dictShow) {
    return {
        show: $foreign.showArrayImpl(show(dictShow))
    };
};
var showRecordFieldsCons = function (dictIsSymbol) {
    return function (dictShowRecordFields) {
        return function (dictShow) {
            return {
                showRecordFields: function (v) {
                    return function (record) {
                        var tail = showRecordFields(dictShowRecordFields)(Type_Proxy["Proxy"].value)(record);
                        var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Type_Proxy["Proxy"].value);
                        var focus = Record_Unsafe.unsafeGet(key)(record);
                        return $foreign.cons($foreign.intercalate(": ")([ key, show(dictShow)(focus) ]))(tail);
                    };
                }
            };
        };
    };
};
export {
    show,
    showRecordFields,
    showBoolean,
    showInt,
    showNumber,
    showChar,
    showString,
    showArray,
    showProxy,
    showRecord,
    showRecordFieldsNil,
    showRecordFieldsCons
};
