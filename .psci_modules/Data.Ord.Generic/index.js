import * as Data_Generic_Rep from "../Data.Generic.Rep/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
var genericOrdNoConstructors = {
    "genericCompare'": function (v) {
        return function (v1) {
            return Data_Ordering.EQ.value;
        };
    }
};
var genericOrdNoArguments = {
    "genericCompare'": function (v) {
        return function (v1) {
            return Data_Ordering.EQ.value;
        };
    }
};
var genericOrdArgument = function (dictOrd) {
    return {
        "genericCompare'": function (v) {
            return function (v1) {
                return Data_Ord.compare(dictOrd)(v)(v1);
            };
        }
    };
};
var genericCompare$prime = function (dict) {
    return dict["genericCompare'"];
};
var genericOrdConstructor = function (dictGenericOrd) {
    return {
        "genericCompare'": function (v) {
            return function (v1) {
                return genericCompare$prime(dictGenericOrd)(v)(v1);
            };
        }
    };
};
var genericOrdProduct = function (dictGenericOrd) {
    return function (dictGenericOrd1) {
        return {
            "genericCompare'": function (v) {
                return function (v1) {
                    var v2 = genericCompare$prime(dictGenericOrd)(v.value0)(v1.value0);
                    if (v2 instanceof Data_Ordering.EQ) {
                        return genericCompare$prime(dictGenericOrd1)(v.value1)(v1.value1);
                    };
                    return v2;
                };
            }
        };
    };
};
var genericOrdSum = function (dictGenericOrd) {
    return function (dictGenericOrd1) {
        return {
            "genericCompare'": function (v) {
                return function (v1) {
                    if (v instanceof Data_Generic_Rep.Inl && v1 instanceof Data_Generic_Rep.Inl) {
                        return genericCompare$prime(dictGenericOrd)(v.value0)(v1.value0);
                    };
                    if (v instanceof Data_Generic_Rep.Inr && v1 instanceof Data_Generic_Rep.Inr) {
                        return genericCompare$prime(dictGenericOrd1)(v.value0)(v1.value0);
                    };
                    if (v instanceof Data_Generic_Rep.Inl && v1 instanceof Data_Generic_Rep.Inr) {
                        return Data_Ordering.LT.value;
                    };
                    if (v instanceof Data_Generic_Rep.Inr && v1 instanceof Data_Generic_Rep.Inl) {
                        return Data_Ordering.GT.value;
                    };
                    throw new Error("Failed pattern match at Data.Ord.Generic (line 19, column 1 - line 23, column 39): " + [ v.constructor.name, v1.constructor.name ]);
                };
            }
        };
    };
};

// | A `Generic` implementation of the `compare` member from the `Ord` type class.
var genericCompare = function (dictGeneric) {
    return function (dictGenericOrd) {
        return function (x) {
            return function (y) {
                return genericCompare$prime(dictGenericOrd)(Data_Generic_Rep.from(dictGeneric)(x))(Data_Generic_Rep.from(dictGeneric)(y));
            };
        };
    };
};
export {
    genericCompare$prime,
    genericCompare,
    genericOrdNoConstructors,
    genericOrdNoArguments,
    genericOrdSum,
    genericOrdProduct,
    genericOrdConstructor,
    genericOrdArgument
};
