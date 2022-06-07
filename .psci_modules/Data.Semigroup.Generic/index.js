import * as Data_Generic_Rep from "../Data.Generic.Rep/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
var genericSemigroupNoConstructors = {
    "genericAppend'": function (a) {
        return function (v) {
            return a;
        };
    }
};
var genericSemigroupNoArguments = {
    "genericAppend'": function (a) {
        return function (v) {
            return a;
        };
    }
};
var genericSemigroupArgument = function (dictSemigroup) {
    return {
        "genericAppend'": function (v) {
            return function (v1) {
                return Data_Semigroup.append(dictSemigroup)(v)(v1);
            };
        }
    };
};
var genericAppend$prime = function (dict) {
    return dict["genericAppend'"];
};
var genericSemigroupConstructor = function (dictGenericSemigroup) {
    return {
        "genericAppend'": function (v) {
            return function (v1) {
                return genericAppend$prime(dictGenericSemigroup)(v)(v1);
            };
        }
    };
};
var genericSemigroupProduct = function (dictGenericSemigroup) {
    return function (dictGenericSemigroup1) {
        return {
            "genericAppend'": function (v) {
                return function (v1) {
                    return new Data_Generic_Rep.Product(genericAppend$prime(dictGenericSemigroup)(v.value0)(v1.value0), genericAppend$prime(dictGenericSemigroup1)(v.value1)(v1.value1));
                };
            }
        };
    };
};

// | A `Generic` implementation of the `append` member from the `Semigroup` type class.
var genericAppend = function (dictGeneric) {
    return function (dictGenericSemigroup) {
        return function (x) {
            return function (y) {
                return Data_Generic_Rep.to(dictGeneric)(genericAppend$prime(dictGenericSemigroup)(Data_Generic_Rep.from(dictGeneric)(x))(Data_Generic_Rep.from(dictGeneric)(y)));
            };
        };
    };
};
export {
    genericAppend$prime,
    genericAppend,
    genericSemigroupNoConstructors,
    genericSemigroupNoArguments,
    genericSemigroupProduct,
    genericSemigroupConstructor,
    genericSemigroupArgument
};
