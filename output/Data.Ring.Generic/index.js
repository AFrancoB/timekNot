// Generated by purs version 0.15.10
import * as Data_Generic_Rep from "../Data.Generic.Rep/index.js";
import * as Data_Ring from "../Data.Ring/index.js";
var genericSub$prime = function (dict) {
    return dict["genericSub'"];
};
var genericSub = function (dictGeneric) {
    var to = Data_Generic_Rep.to(dictGeneric);
    var from = Data_Generic_Rep.from(dictGeneric);
    return function (dictGenericRing) {
        var genericSub$prime1 = genericSub$prime(dictGenericRing);
        return function (x) {
            return function (y) {
                return to(genericSub$prime1(from(x))(from(y)));
            };
        };
    };
};
var genericRingProduct = function (dictGenericRing) {
    var genericSub$prime1 = genericSub$prime(dictGenericRing);
    return function (dictGenericRing1) {
        var genericSub$prime2 = genericSub$prime(dictGenericRing1);
        return {
            "genericSub'": function (v) {
                return function (v1) {
                    return new Data_Generic_Rep.Product(genericSub$prime1(v.value0)(v1.value0), genericSub$prime2(v.value1)(v1.value1));
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
    var genericSub$prime1 = genericSub$prime(dictGenericRing);
    return {
        "genericSub'": function (v) {
            return function (v1) {
                return genericSub$prime1(v)(v1);
            };
        }
    };
};
var genericRingArgument = function (dictRing) {
    var sub = Data_Ring.sub(dictRing);
    return {
        "genericSub'": function (v) {
            return function (v1) {
                return sub(v)(v1);
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
