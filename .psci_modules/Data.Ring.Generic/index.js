import * as Data_Generic_Rep from "../Data.Generic.Rep/index.js";
import * as Data_Ring from "../Data.Ring/index.js";
var genericSub$prime = function (dict) {
    return dict["genericSub'"];
};

// | A `Generic` implementation of the `sub` member from the `Ring` type class.
var genericSub = function (dictGeneric) {
    return function (dictGenericRing) {
        return function (x) {
            return function (y) {
                return Data_Generic_Rep.to(dictGeneric)(genericSub$prime(dictGenericRing)(Data_Generic_Rep.from(dictGeneric)(x))(Data_Generic_Rep.from(dictGeneric)(y)));
            };
        };
    };
};
var genericRingProduct = function (dictGenericRing) {
    return function (dictGenericRing1) {
        return {
            "genericSub'": function (v) {
                return function (v1) {
                    return new Data_Generic_Rep.Product(genericSub$prime(dictGenericRing)(v.value0)(v1.value0), genericSub$prime(dictGenericRing1)(v.value1)(v1.value1));
                };
            }
        };
    };
};
var genericRingNoArguments = {
    "genericSub'": function (v) {
        return function (v1) {
            return Data_Generic_Rep.NoArguments.value;
        };
    }
};
var genericRingConstructor = function (dictGenericRing) {
    return {
        "genericSub'": function (v) {
            return function (v1) {
                return genericSub$prime(dictGenericRing)(v)(v1);
            };
        }
    };
};
var genericRingArgument = function (dictRing) {
    return {
        "genericSub'": function (v) {
            return function (v1) {
                return Data_Ring.sub(dictRing)(v)(v1);
            };
        }
    };
};
export {
    genericSub$prime,
    genericSub,
    genericRingNoArguments,
    genericRingArgument,
    genericRingProduct,
    genericRingConstructor
};
