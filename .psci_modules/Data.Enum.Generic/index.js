import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Bounded_Generic from "../Data.Bounded.Generic/index.js";
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Generic_Rep from "../Data.Generic.Rep/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
var genericToEnum$prime = function (dict) {
    return dict["genericToEnum'"];
};

// | A `Generic` implementation of the `toEnum` member from the `BoundedEnum`
// | type class.
var genericToEnum = function (dictGeneric) {
    return function (dictGenericBoundedEnum) {
        var $95 = Data_Functor.map(Data_Maybe.functorMaybe)(Data_Generic_Rep.to(dictGeneric));
        var $96 = genericToEnum$prime(dictGenericBoundedEnum);
        return function ($97) {
            return $95($96($97));
        };
    };
};
var genericSucc$prime = function (dict) {
    return dict["genericSucc'"];
};

// | A `Generic` implementation of the `succ` member from the `Enum` type class.
var genericSucc = function (dictGeneric) {
    return function (dictGenericEnum) {
        var $98 = Data_Functor.map(Data_Maybe.functorMaybe)(Data_Generic_Rep.to(dictGeneric));
        var $99 = genericSucc$prime(dictGenericEnum);
        var $100 = Data_Generic_Rep.from(dictGeneric);
        return function ($101) {
            return $98($99($100($101)));
        };
    };
};
var genericPred$prime = function (dict) {
    return dict["genericPred'"];
};

// | A `Generic` implementation of the `pred` member from the `Enum` type class.
var genericPred = function (dictGeneric) {
    return function (dictGenericEnum) {
        var $102 = Data_Functor.map(Data_Maybe.functorMaybe)(Data_Generic_Rep.to(dictGeneric));
        var $103 = genericPred$prime(dictGenericEnum);
        var $104 = Data_Generic_Rep.from(dictGeneric);
        return function ($105) {
            return $102($103($104($105)));
        };
    };
};
var genericFromEnum$prime = function (dict) {
    return dict["genericFromEnum'"];
};

// | A `Generic` implementation of the `fromEnum` member from the `BoundedEnum`
// | type class.
var genericFromEnum = function (dictGeneric) {
    return function (dictGenericBoundedEnum) {
        var $106 = genericFromEnum$prime(dictGenericBoundedEnum);
        var $107 = Data_Generic_Rep.from(dictGeneric);
        return function ($108) {
            return $106($107($108));
        };
    };
};
var genericEnumSum = function (dictGenericEnum) {
    return function (dictGenericTop) {
        return function (dictGenericEnum1) {
            return function (dictGenericBottom) {
                return {
                    "genericPred'": function (v) {
                        if (v instanceof Data_Generic_Rep.Inl) {
                            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Generic_Rep.Inl.create)(genericPred$prime(dictGenericEnum)(v.value0));
                        };
                        if (v instanceof Data_Generic_Rep.Inr) {
                            var v1 = genericPred$prime(dictGenericEnum1)(v.value0);
                            if (v1 instanceof Data_Maybe.Nothing) {
                                return new Data_Maybe.Just(new Data_Generic_Rep.Inl(Data_Bounded_Generic["genericTop$prime"](dictGenericTop)));
                            };
                            if (v1 instanceof Data_Maybe.Just) {
                                return new Data_Maybe.Just(new Data_Generic_Rep.Inr(v1.value0));
                            };
                            throw new Error("Failed pattern match at Data.Enum.Generic (line 30, column 14 - line 32, column 31): " + [ v1.constructor.name ]);
                        };
                        throw new Error("Failed pattern match at Data.Enum.Generic (line 28, column 18 - line 32, column 31): " + [ v.constructor.name ]);
                    },
                    "genericSucc'": function (v) {
                        if (v instanceof Data_Generic_Rep.Inl) {
                            var v1 = genericSucc$prime(dictGenericEnum)(v.value0);
                            if (v1 instanceof Data_Maybe.Nothing) {
                                return new Data_Maybe.Just(new Data_Generic_Rep.Inr(Data_Bounded_Generic["genericBottom$prime"](dictGenericBottom)));
                            };
                            if (v1 instanceof Data_Maybe.Just) {
                                return new Data_Maybe.Just(new Data_Generic_Rep.Inl(v1.value0));
                            };
                            throw new Error("Failed pattern match at Data.Enum.Generic (line 34, column 14 - line 36, column 31): " + [ v1.constructor.name ]);
                        };
                        if (v instanceof Data_Generic_Rep.Inr) {
                            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Generic_Rep.Inr.create)(genericSucc$prime(dictGenericEnum1)(v.value0));
                        };
                        throw new Error("Failed pattern match at Data.Enum.Generic (line 33, column 18 - line 37, column 36): " + [ v.constructor.name ]);
                    }
                };
            };
        };
    };
};
var genericEnumProduct = function (dictGenericEnum) {
    return function (dictGenericTop) {
        return function (dictGenericBottom) {
            return function (dictGenericEnum1) {
                return function (dictGenericTop1) {
                    return function (dictGenericBottom1) {
                        return {
                            "genericPred'": function (v) {
                                var v1 = genericPred$prime(dictGenericEnum1)(v.value1);
                                if (v1 instanceof Data_Maybe.Just) {
                                    return new Data_Maybe.Just(new Data_Generic_Rep.Product(v.value0, v1.value0));
                                };
                                if (v1 instanceof Data_Maybe.Nothing) {
                                    return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Function.flip(Data_Generic_Rep.Product.create)(Data_Bounded_Generic["genericTop$prime"](dictGenericTop1)))(genericPred$prime(dictGenericEnum)(v.value0));
                                };
                                throw new Error("Failed pattern match at Data.Enum.Generic (line 40, column 32 - line 42, column 59): " + [ v1.constructor.name ]);
                            },
                            "genericSucc'": function (v) {
                                var v1 = genericSucc$prime(dictGenericEnum1)(v.value1);
                                if (v1 instanceof Data_Maybe.Just) {
                                    return new Data_Maybe.Just(new Data_Generic_Rep.Product(v.value0, v1.value0));
                                };
                                if (v1 instanceof Data_Maybe.Nothing) {
                                    return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Function.flip(Data_Generic_Rep.Product.create)(Data_Bounded_Generic["genericBottom$prime"](dictGenericBottom1)))(genericSucc$prime(dictGenericEnum)(v.value0));
                                };
                                throw new Error("Failed pattern match at Data.Enum.Generic (line 43, column 32 - line 45, column 62): " + [ v1.constructor.name ]);
                            }
                        };
                    };
                };
            };
        };
    };
};
var genericEnumNoArguments = {
    "genericPred'": function (v) {
        return Data_Maybe.Nothing.value;
    },
    "genericSucc'": function (v) {
        return Data_Maybe.Nothing.value;
    }
};
var genericEnumConstructor = function (dictGenericEnum) {
    return {
        "genericPred'": function (v) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Generic_Rep.Constructor)(genericPred$prime(dictGenericEnum)(v));
        },
        "genericSucc'": function (v) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Generic_Rep.Constructor)(genericSucc$prime(dictGenericEnum)(v));
        }
    };
};
var genericEnumArgument = function (dictEnum) {
    return {
        "genericPred'": function (v) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Generic_Rep.Argument)(Data_Enum.pred(dictEnum)(v));
        },
        "genericSucc'": function (v) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Generic_Rep.Argument)(Data_Enum.succ(dictEnum)(v));
        }
    };
};
var genericCardinality$prime = function (dict) {
    return dict["genericCardinality'"];
};

// | A `Generic` implementation of the `cardinality` member from the
// | `BoundedEnum` type class.
var genericCardinality = function (dictGeneric) {
    return function (dictGenericBoundedEnum) {
        return Data_Newtype.unwrap()(genericCardinality$prime(dictGenericBoundedEnum));
    };
};
var genericBoundedEnumSum = function (dictGenericBoundedEnum) {
    return function (dictGenericBoundedEnum1) {
        return {
            "genericCardinality'": Data_Newtype.unwrap()(genericCardinality$prime(dictGenericBoundedEnum)) + Data_Newtype.unwrap()(genericCardinality$prime(dictGenericBoundedEnum1)) | 0,
            "genericToEnum'": function (n) {
                var to = function (v) {
                    if (n >= 0 && n < v) {
                        return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Generic_Rep.Inl.create)(genericToEnum$prime(dictGenericBoundedEnum)(n));
                    };
                    if (Data_Boolean.otherwise) {
                        return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Generic_Rep.Inr.create)(genericToEnum$prime(dictGenericBoundedEnum1)(n - v | 0));
                    };
                    throw new Error("Failed pattern match at Data.Enum.Generic (line 83, column 5 - line 83, column 43): " + [ v.constructor.name ]);
                };
                return to(genericCardinality$prime(dictGenericBoundedEnum));
            },
            "genericFromEnum'": function (v) {
                if (v instanceof Data_Generic_Rep.Inl) {
                    return genericFromEnum$prime(dictGenericBoundedEnum)(v.value0);
                };
                if (v instanceof Data_Generic_Rep.Inr) {
                    return genericFromEnum$prime(dictGenericBoundedEnum1)(v.value0) + Data_Newtype.unwrap()(genericCardinality$prime(dictGenericBoundedEnum)) | 0;
                };
                throw new Error("Failed pattern match at Data.Enum.Generic (line 87, column 22 - line 89, column 80): " + [ v.constructor.name ]);
            }
        };
    };
};
var genericBoundedEnumProduct = function (dictGenericBoundedEnum) {
    return function (dictGenericBoundedEnum1) {
        return {
            "genericCardinality'": Data_Newtype.unwrap()(genericCardinality$prime(dictGenericBoundedEnum)) * Data_Newtype.unwrap()(genericCardinality$prime(dictGenericBoundedEnum1)) | 0,
            "genericToEnum'": function (n) {
                var to = function (v) {
                    return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Generic_Rep.Product.create)(genericToEnum$prime(dictGenericBoundedEnum)(Data_EuclideanRing.div(Data_EuclideanRing.euclideanRingInt)(n)(v))))(genericToEnum$prime(dictGenericBoundedEnum1)(Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(n)(v)));
                };
                return to(genericCardinality$prime(dictGenericBoundedEnum1));
            },
            "genericFromEnum'": (function () {
                var from = function (v) {
                    return function (v1) {
                        return (genericFromEnum$prime(dictGenericBoundedEnum)(v1.value0) * v | 0) + genericFromEnum$prime(dictGenericBoundedEnum1)(v1.value1) | 0;
                    };
                };
                return from(genericCardinality$prime(dictGenericBoundedEnum1));
            })()
        };
    };
};
var genericBoundedEnumNoArguments = {
    "genericCardinality'": 1,
    "genericToEnum'": function (i) {
        var $92 = i === 0;
        if ($92) {
            return new Data_Maybe.Just(Data_Generic_Rep.NoArguments.value);
        };
        return Data_Maybe.Nothing.value;
    },
    "genericFromEnum'": function (v) {
        return 0;
    }
};
var genericBoundedEnumConstructor = function (dictGenericBoundedEnum) {
    return {
        "genericCardinality'": Data_Newtype.unwrap()(genericCardinality$prime(dictGenericBoundedEnum)),
        "genericToEnum'": function (i) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Generic_Rep.Constructor)(genericToEnum$prime(dictGenericBoundedEnum)(i));
        },
        "genericFromEnum'": function (v) {
            return genericFromEnum$prime(dictGenericBoundedEnum)(v);
        }
    };
};
var genericBoundedEnumArgument = function (dictBoundedEnum) {
    return {
        "genericCardinality'": Data_Newtype.unwrap()(Data_Enum.cardinality(dictBoundedEnum)),
        "genericToEnum'": function (i) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Generic_Rep.Argument)(Data_Enum.toEnum(dictBoundedEnum)(i));
        },
        "genericFromEnum'": function (v) {
            return Data_Enum.fromEnum(dictBoundedEnum)(v);
        }
    };
};
export {
    genericCardinality$prime,
    genericFromEnum$prime,
    genericPred$prime,
    genericSucc$prime,
    genericToEnum$prime,
    genericPred,
    genericSucc,
    genericCardinality,
    genericToEnum,
    genericFromEnum,
    genericEnumNoArguments,
    genericEnumArgument,
    genericEnumConstructor,
    genericEnumSum,
    genericEnumProduct,
    genericBoundedEnumNoArguments,
    genericBoundedEnumArgument,
    genericBoundedEnumConstructor,
    genericBoundedEnumSum,
    genericBoundedEnumProduct
};
