import * as Data_Show from "../Data.Show/index.js";
import * as Data_Typelevel_Undefined from "../Data.Typelevel.Undefined/index.js";
var NumCons = /* #__PURE__ */ (function () {
    function NumCons(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    NumCons.create = function (value0) {
        return function (value1) {
            return new NumCons(value0, value1);
        };
    };
    return NumCons;
})();
var showDaDb = function (dictShow) {
    return function (dictShow1) {
        return {
            show: function (v) {
                return Data_Show.show(dictShow)(Data_Typelevel_Undefined["undefined"]) + Data_Show.show(dictShow1)(Data_Typelevel_Undefined["undefined"]);
            }
        };
    };
};
var showD9 = {
    show: function (v) {
        return "9";
    }
};
var showD8 = {
    show: function (v) {
        return "8";
    }
};
var showD7 = {
    show: function (v) {
        return "7";
    }
};
var showD6 = {
    show: function (v) {
        return "6";
    }
};
var showD5 = {
    show: function (v) {
        return "5";
    }
};
var showD4 = {
    show: function (v) {
        return "4";
    }
};
var showD3 = {
    show: function (v) {
        return "3";
    }
};
var showD2 = {
    show: function (v) {
        return "2";
    }
};
var showD1 = {
    show: function (v) {
        return "1";
    }
};
var showD0 = {
    show: function (v) {
        return "0";
    }
};
var d9 = Data_Typelevel_Undefined["undefined"];
var d8 = Data_Typelevel_Undefined["undefined"];
var d7 = Data_Typelevel_Undefined["undefined"];
var d6 = Data_Typelevel_Undefined["undefined"];
var d5 = Data_Typelevel_Undefined["undefined"];
var d4 = Data_Typelevel_Undefined["undefined"];
var d3 = Data_Typelevel_Undefined["undefined"];
var d2 = Data_Typelevel_Undefined["undefined"];
var d1 = Data_Typelevel_Undefined["undefined"];
var d0 = Data_Typelevel_Undefined["undefined"];
export {
    d0,
    d1,
    d2,
    d3,
    d4,
    d5,
    d6,
    d7,
    d8,
    d9,
    NumCons,
    showD0,
    showD1,
    showD2,
    showD3,
    showD4,
    showD5,
    showD6,
    showD7,
    showD8,
    showD9,
    showDaDb
};
