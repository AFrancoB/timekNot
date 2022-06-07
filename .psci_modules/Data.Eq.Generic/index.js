import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Generic_Rep from "../Data.Generic.Rep/index.js";
var genericEqNoConstructors = {
    "genericEq'": function (v) {
        return function (v1) {
            return true;
        };
    }
};
var genericEqNoArguments = {
    "genericEq'": function (v) {
        return function (v1) {
            return true;
        };
    }
};
var genericEqArgument = function (dictEq) {
    return {
        "genericEq'": function (v) {
            return function (v1) {
                return Data_Eq.eq(dictEq)(v)(v1);
            };
        }
    };
};
var genericEq$prime = function (dict) {
    return dict["genericEq'"];
};
var genericEqConstructor = function (dictGenericEq) {
    return {
        "genericEq'": function (v) {
            return function (v1) {
                return genericEq$prime(dictGenericEq)(v)(v1);
            };
        }
    };
};
var genericEqProduct = function (dictGenericEq) {
    return function (dictGenericEq1) {
        return {
            "genericEq'": function (v) {
                return function (v1) {
                    return genericEq$prime(dictGenericEq)(v.value0)(v1.value0) && genericEq$prime(dictGenericEq1)(v.value1)(v1.value1);
                };
            }
        };
    };
};
var genericEqSum = function (dictGenericEq) {
    return function (dictGenericEq1) {
        return {
            "genericEq'": function (v) {
                return function (v1) {
                    if (v instanceof Data_Generic_Rep.Inl && v1 instanceof Data_Generic_Rep.Inl) {
                        return genericEq$prime(dictGenericEq)(v.value0)(v1.value0);
                    };
                    if (v instanceof Data_Generic_Rep.Inr && v1 instanceof Data_Generic_Rep.Inr) {
                        return genericEq$prime(dictGenericEq1)(v.value0)(v1.value0);
                    };
                    return false;
                };
            }
        };
    };
};

// | A `Generic` implementation of the `eq` member from the `Eq` type class.
var genericEq = function (dictGeneric) {
    return function (dictGenericEq) {
        return function (x) {
            return function (y) {
                return genericEq$prime(dictGenericEq)(Data_Generic_Rep.from(dictGeneric)(x))(Data_Generic_Rep.from(dictGeneric)(y));
            };
        };
    };
};
export {
    genericEq$prime,
    genericEq,
    genericEqNoConstructors,
    genericEqNoArguments,
    genericEqSum,
    genericEqProduct,
    genericEqConstructor,
    genericEqArgument
};
