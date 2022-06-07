import * as $foreign from "./foreign.js";
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Plus from "../Control.Plus/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor_Coproduct from "../Data.Functor.Coproduct/index.js";
import * as Data_HeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Maybe_First from "../Data.Maybe.First/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Monoid_Conj from "../Data.Monoid.Conj/index.js";
import * as Data_Monoid_Disj from "../Data.Monoid.Disj/index.js";
import * as Data_Monoid_Dual from "../Data.Monoid.Dual/index.js";
import * as Data_Monoid_Endo from "../Data.Monoid.Endo/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
var foldr = function (dict) {
    return dict.foldr;
};

// | Try to get nth element from the right in a data structure
var indexr = function (dictFoldable) {
    return function (idx) {
        var go = function (a) {
            return function (cursor) {
                if (cursor.elem instanceof Data_Maybe.Just) {
                    return cursor;
                };
                var $153 = cursor.pos === idx;
                if ($153) {
                    return {
                        elem: new Data_Maybe.Just(a),
                        pos: cursor.pos
                    };
                };
                return {
                    pos: cursor.pos + 1 | 0,
                    elem: cursor.elem
                };
            };
        };
        var $312 = foldr(dictFoldable)(go)({
            elem: Data_Maybe.Nothing.value,
            pos: 0
        });
        return function ($313) {
            return (function (v) {
                return v.elem;
            })($312($313));
        };
    };
};

// | Test whether the structure is empty.
// | Optimized for structures that are similar to cons-lists, because there
// | is no general way to do better.
var $$null = function (dictFoldable) {
    return foldr(dictFoldable)(function (v) {
        return function (v1) {
            return false;
        };
    })(true);
};

// | Combines a collection of elements using the `Alt` operation.
var oneOf = function (dictFoldable) {
    return function (dictPlus) {
        return foldr(dictFoldable)(Control_Alt.alt(dictPlus.Alt0()))(Control_Plus.empty(dictPlus));
    };
};

// | Folds a structure into some `Plus`.
var oneOfMap = function (dictFoldable) {
    return function (dictPlus) {
        return function (f) {
            return foldr(dictFoldable)((function () {
                var $314 = Control_Alt.alt(dictPlus.Alt0());
                return function ($315) {
                    return $314(f($315));
                };
            })())(Control_Plus.empty(dictPlus));
        };
    };
};

// | Traverse a data structure, performing some effects encoded by an
// | `Applicative` functor at each value, ignoring the final result.
// |
// | For example:
// |
// | ```purescript
// | traverse_ print [1, 2, 3]
// | ```
var traverse_ = function (dictApplicative) {
    return function (dictFoldable) {
        return function (f) {
            return foldr(dictFoldable)((function () {
                var $316 = Control_Apply.applySecond(dictApplicative.Apply0());
                return function ($317) {
                    return $316(f($317));
                };
            })())(Control_Applicative.pure(dictApplicative)(Data_Unit.unit));
        };
    };
};

// | A version of `traverse_` with its arguments flipped.
// |
// | This can be useful when running an action written using do notation
// | for every element in a data structure:
// |
// | For example:
// |
// | ```purescript
// | for_ [1, 2, 3] \n -> do
// |   print n
// |   trace "squared is"
// |   print (n * n)
// | ```
var for_ = function (dictApplicative) {
    return function (dictFoldable) {
        return Data_Function.flip(traverse_(dictApplicative)(dictFoldable));
    };
};

// | Perform all of the effects in some data structure in the order
// | given by the `Foldable` instance, ignoring the final result.
// |
// | For example:
// |
// | ```purescript
// | sequence_ [ trace "Hello, ", trace " world!" ]
// | ```
var sequence_ = function (dictApplicative) {
    return function (dictFoldable) {
        return traverse_(dictApplicative)(dictFoldable)(Control_Category.identity(Control_Category.categoryFn));
    };
};
var foldl = function (dict) {
    return dict.foldl;
};

// | Try to get nth element from the left in a data structure
var indexl = function (dictFoldable) {
    return function (idx) {
        var go = function (cursor) {
            return function (a) {
                if (cursor.elem instanceof Data_Maybe.Just) {
                    return cursor;
                };
                var $157 = cursor.pos === idx;
                if ($157) {
                    return {
                        elem: new Data_Maybe.Just(a),
                        pos: cursor.pos
                    };
                };
                return {
                    pos: cursor.pos + 1 | 0,
                    elem: cursor.elem
                };
            };
        };
        var $318 = foldl(dictFoldable)(go)({
            elem: Data_Maybe.Nothing.value,
            pos: 0
        });
        return function ($319) {
            return (function (v) {
                return v.elem;
            })($318($319));
        };
    };
};

// | Fold a data structure, accumulating values in some `Monoid`,
// | combining adjacent elements using the specified separator.
// |
// | For example:
// |
// | ```purescript
// | > intercalate ", " ["Lorem", "ipsum", "dolor"]
// | = "Lorem, ipsum, dolor"
// |
// | > intercalate "*" ["a", "b", "c"]
// | = "a*b*c"
// |
// | > intercalate [1] [[2, 3], [4, 5], [6, 7]]
// | = [2, 3, 1, 4, 5, 1, 6, 7]
// | ```
var intercalate = function (dictFoldable) {
    return function (dictMonoid) {
        return function (sep) {
            return function (xs) {
                var go = function (v) {
                    return function (x) {
                        if (v.init) {
                            return {
                                init: false,
                                acc: x
                            };
                        };
                        return {
                            init: false,
                            acc: Data_Semigroup.append(dictMonoid.Semigroup0())(v.acc)(Data_Semigroup.append(dictMonoid.Semigroup0())(sep)(x))
                        };
                    };
                };
                return (foldl(dictFoldable)(go)({
                    init: true,
                    acc: Data_Monoid.mempty(dictMonoid)
                })(xs)).acc;
            };
        };
    };
};

// | Returns the size/length of a finite structure.
// | Optimized for structures that are similar to cons-lists, because there
// | is no general way to do better.
var length = function (dictFoldable) {
    return function (dictSemiring) {
        return foldl(dictFoldable)(function (c) {
            return function (v) {
                return Data_Semiring.add(dictSemiring)(Data_Semiring.one(dictSemiring))(c);
            };
        })(Data_Semiring.zero(dictSemiring));
    };
};

// | Find the largest element of a structure, according to a given comparison
// | function. The comparison function should represent a total ordering (see
// | the `Ord` type class laws); if it does not, the behaviour is undefined.
var maximumBy = function (dictFoldable) {
    return function (cmp) {
        var max$prime = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing) {
                    return new Data_Maybe.Just(v1);
                };
                if (v instanceof Data_Maybe.Just) {
                    return new Data_Maybe.Just((function () {
                        var $164 = Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v1))(Data_Ordering.GT.value);
                        if ($164) {
                            return v.value0;
                        };
                        return v1;
                    })());
                };
                throw new Error("Failed pattern match at Data.Foldable (line 441, column 3 - line 441, column 27): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        return foldl(dictFoldable)(max$prime)(Data_Maybe.Nothing.value);
    };
};

// | Find the largest element of a structure, according to its `Ord` instance.
var maximum = function (dictOrd) {
    return function (dictFoldable) {
        return maximumBy(dictFoldable)(Data_Ord.compare(dictOrd));
    };
};

// | Find the smallest element of a structure, according to a given comparison
// | function. The comparison function should represent a total ordering (see
// | the `Ord` type class laws); if it does not, the behaviour is undefined.
var minimumBy = function (dictFoldable) {
    return function (cmp) {
        var min$prime = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing) {
                    return new Data_Maybe.Just(v1);
                };
                if (v instanceof Data_Maybe.Just) {
                    return new Data_Maybe.Just((function () {
                        var $168 = Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(v.value0)(v1))(Data_Ordering.LT.value);
                        if ($168) {
                            return v.value0;
                        };
                        return v1;
                    })());
                };
                throw new Error("Failed pattern match at Data.Foldable (line 454, column 3 - line 454, column 27): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        return foldl(dictFoldable)(min$prime)(Data_Maybe.Nothing.value);
    };
};

// | Find the smallest element of a structure, according to its `Ord` instance.
var minimum = function (dictOrd) {
    return function (dictFoldable) {
        return minimumBy(dictFoldable)(Data_Ord.compare(dictOrd));
    };
};

// | Find the product of the numeric values in a data structure.
var product = function (dictFoldable) {
    return function (dictSemiring) {
        return foldl(dictFoldable)(Data_Semiring.mul(dictSemiring))(Data_Semiring.one(dictSemiring));
    };
};

// | Find the sum of the numeric values in a data structure.
var sum = function (dictFoldable) {
    return function (dictSemiring) {
        return foldl(dictFoldable)(Data_Semiring.add(dictSemiring))(Data_Semiring.zero(dictSemiring));
    };
};
var foldableTuple = {
    foldr: function (f) {
        return function (z) {
            return function (v) {
                return f(v.value1)(z);
            };
        };
    },
    foldl: function (f) {
        return function (z) {
            return function (v) {
                return f(z)(v.value1);
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return f(v.value1);
            };
        };
    }
};
var foldableMultiplicative = {
    foldr: function (f) {
        return function (z) {
            return function (v) {
                return f(v)(z);
            };
        };
    },
    foldl: function (f) {
        return function (z) {
            return function (v) {
                return f(z)(v);
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return f(v);
            };
        };
    }
};
var foldableMaybe = {
    foldr: function (v) {
        return function (z) {
            return function (v1) {
                if (v1 instanceof Data_Maybe.Nothing) {
                    return z;
                };
                if (v1 instanceof Data_Maybe.Just) {
                    return v(v1.value0)(z);
                };
                throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
            };
        };
    },
    foldl: function (v) {
        return function (z) {
            return function (v1) {
                if (v1 instanceof Data_Maybe.Nothing) {
                    return z;
                };
                if (v1 instanceof Data_Maybe.Just) {
                    return v(z)(v1.value0);
                };
                throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (v) {
            return function (v1) {
                if (v1 instanceof Data_Maybe.Nothing) {
                    return Data_Monoid.mempty(dictMonoid);
                };
                if (v1 instanceof Data_Maybe.Just) {
                    return v(v1.value0);
                };
                throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    }
};
var foldableIdentity = {
    foldr: function (f) {
        return function (z) {
            return function (v) {
                return f(v)(z);
            };
        };
    },
    foldl: function (f) {
        return function (z) {
            return function (v) {
                return f(z)(v);
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return f(v);
            };
        };
    }
};
var foldableEither = {
    foldr: function (v) {
        return function (z) {
            return function (v1) {
                if (v1 instanceof Data_Either.Left) {
                    return z;
                };
                if (v1 instanceof Data_Either.Right) {
                    return v(v1.value0)(z);
                };
                throw new Error("Failed pattern match at Data.Foldable (line 181, column 1 - line 187, column 28): " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
            };
        };
    },
    foldl: function (v) {
        return function (z) {
            return function (v1) {
                if (v1 instanceof Data_Either.Left) {
                    return z;
                };
                if (v1 instanceof Data_Either.Right) {
                    return v(z)(v1.value0);
                };
                throw new Error("Failed pattern match at Data.Foldable (line 181, column 1 - line 187, column 28): " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (v) {
            return function (v1) {
                if (v1 instanceof Data_Either.Left) {
                    return Data_Monoid.mempty(dictMonoid);
                };
                if (v1 instanceof Data_Either.Right) {
                    return v(v1.value0);
                };
                throw new Error("Failed pattern match at Data.Foldable (line 181, column 1 - line 187, column 28): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    }
};
var foldableDual = {
    foldr: function (f) {
        return function (z) {
            return function (v) {
                return f(v)(z);
            };
        };
    },
    foldl: function (f) {
        return function (z) {
            return function (v) {
                return f(z)(v);
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return f(v);
            };
        };
    }
};
var foldableDisj = {
    foldr: function (f) {
        return function (z) {
            return function (v) {
                return f(v)(z);
            };
        };
    },
    foldl: function (f) {
        return function (z) {
            return function (v) {
                return f(z)(v);
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return f(v);
            };
        };
    }
};
var foldableConst = {
    foldr: function (v) {
        return function (z) {
            return function (v1) {
                return z;
            };
        };
    },
    foldl: function (v) {
        return function (z) {
            return function (v1) {
                return z;
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (v) {
            return function (v1) {
                return Data_Monoid.mempty(dictMonoid);
            };
        };
    }
};
var foldableConj = {
    foldr: function (f) {
        return function (z) {
            return function (v) {
                return f(v)(z);
            };
        };
    },
    foldl: function (f) {
        return function (z) {
            return function (v) {
                return f(z)(v);
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return f(v);
            };
        };
    }
};
var foldableAdditive = {
    foldr: function (f) {
        return function (z) {
            return function (v) {
                return f(v)(z);
            };
        };
    },
    foldl: function (f) {
        return function (z) {
            return function (v) {
                return f(z)(v);
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return f(v);
            };
        };
    }
};

// | A default implementation of `foldMap` using `foldr`.
// |
// | Note: when defining a `Foldable` instance, this function is unsafe to use
// | in combination with `foldrDefault`.
var foldMapDefaultR = function (dictFoldable) {
    return function (dictMonoid) {
        return function (f) {
            return foldr(dictFoldable)(function (x) {
                return function (acc) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(f(x))(acc);
                };
            })(Data_Monoid.mempty(dictMonoid));
        };
    };
};
var foldableArray = {
    foldr: $foreign.foldrArray,
    foldl: $foreign.foldlArray,
    foldMap: function (dictMonoid) {
        return foldMapDefaultR(foldableArray)(dictMonoid);
    }
};

// | A default implementation of `foldMap` using `foldl`.
// |
// | Note: when defining a `Foldable` instance, this function is unsafe to use
// | in combination with `foldlDefault`.
var foldMapDefaultL = function (dictFoldable) {
    return function (dictMonoid) {
        return function (f) {
            return foldl(dictFoldable)(function (acc) {
                return function (x) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(acc)(f(x));
                };
            })(Data_Monoid.mempty(dictMonoid));
        };
    };
};
var foldMap = function (dict) {
    return dict.foldMap;
};
var foldableApp = function (dictFoldable) {
    return {
        foldr: function (f) {
            return function (i) {
                return function (v) {
                    return foldr(dictFoldable)(f)(i)(v);
                };
            };
        },
        foldl: function (f) {
            return function (i) {
                return function (v) {
                    return foldl(dictFoldable)(f)(i)(v);
                };
            };
        },
        foldMap: function (dictMonoid) {
            return function (f) {
                return function (v) {
                    return foldMap(dictFoldable)(dictMonoid)(f)(v);
                };
            };
        }
    };
};
var foldableCompose = function (dictFoldable) {
    return function (dictFoldable1) {
        return {
            foldr: function (f) {
                return function (i) {
                    return function (v) {
                        return foldr(dictFoldable)(Data_Function.flip(foldr(dictFoldable1)(f)))(i)(v);
                    };
                };
            },
            foldl: function (f) {
                return function (i) {
                    return function (v) {
                        return foldl(dictFoldable)(foldl(dictFoldable1)(f))(i)(v);
                    };
                };
            },
            foldMap: function (dictMonoid) {
                return function (f) {
                    return function (v) {
                        return foldMap(dictFoldable)(dictMonoid)(foldMap(dictFoldable1)(dictMonoid)(f))(v);
                    };
                };
            }
        };
    };
};
var foldableCoproduct = function (dictFoldable) {
    return function (dictFoldable1) {
        return {
            foldr: function (f) {
                return function (z) {
                    return Data_Functor_Coproduct.coproduct(foldr(dictFoldable)(f)(z))(foldr(dictFoldable1)(f)(z));
                };
            },
            foldl: function (f) {
                return function (z) {
                    return Data_Functor_Coproduct.coproduct(foldl(dictFoldable)(f)(z))(foldl(dictFoldable1)(f)(z));
                };
            },
            foldMap: function (dictMonoid) {
                return function (f) {
                    return Data_Functor_Coproduct.coproduct(foldMap(dictFoldable)(dictMonoid)(f))(foldMap(dictFoldable1)(dictMonoid)(f));
                };
            }
        };
    };
};
var foldableFirst = {
    foldr: function (f) {
        return function (z) {
            return function (v) {
                return foldr(foldableMaybe)(f)(z)(v);
            };
        };
    },
    foldl: function (f) {
        return function (z) {
            return function (v) {
                return foldl(foldableMaybe)(f)(z)(v);
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return foldMap(foldableMaybe)(dictMonoid)(f)(v);
            };
        };
    }
};
var foldableLast = {
    foldr: function (f) {
        return function (z) {
            return function (v) {
                return foldr(foldableMaybe)(f)(z)(v);
            };
        };
    },
    foldl: function (f) {
        return function (z) {
            return function (v) {
                return foldl(foldableMaybe)(f)(z)(v);
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return foldMap(foldableMaybe)(dictMonoid)(f)(v);
            };
        };
    }
};
var foldableProduct = function (dictFoldable) {
    return function (dictFoldable1) {
        return {
            foldr: function (f) {
                return function (z) {
                    return function (v) {
                        return foldr(dictFoldable)(f)(foldr(dictFoldable1)(f)(z)(v.value1))(v.value0);
                    };
                };
            },
            foldl: function (f) {
                return function (z) {
                    return function (v) {
                        return foldl(dictFoldable1)(f)(foldl(dictFoldable)(f)(z)(v.value0))(v.value1);
                    };
                };
            },
            foldMap: function (dictMonoid) {
                return function (f) {
                    return function (v) {
                        return Data_Semigroup.append(dictMonoid.Semigroup0())(foldMap(dictFoldable)(dictMonoid)(f)(v.value0))(foldMap(dictFoldable1)(dictMonoid)(f)(v.value1));
                    };
                };
            }
        };
    };
};

// | A default implementation of `foldl` using `foldMap`.
// |
// | Note: when defining a `Foldable` instance, this function is unsafe to use
// | in combination with `foldMapDefaultL`.
var foldlDefault = function (dictFoldable) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Newtype.unwrap()(Data_Newtype.unwrap()(foldMap(dictFoldable)(Data_Monoid_Dual.monoidDual(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn)))((function () {
                    var $320 = Data_Function.flip(c);
                    return function ($321) {
                        return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo($320($321)));
                    };
                })())(xs)))(u);
            };
        };
    };
};

// | A default implementation of `foldr` using `foldMap`.
// |
// | Note: when defining a `Foldable` instance, this function is unsafe to use
// | in combination with `foldMapDefaultR`.
var foldrDefault = function (dictFoldable) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Newtype.unwrap()(foldMap(dictFoldable)(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn))(function ($322) {
                    return Data_Monoid_Endo.Endo(c($322));
                })(xs))(u);
            };
        };
    };
};

// | Lookup a value in a data structure of `Tuple`s, generalizing association lists.
var lookup = function (dictFoldable) {
    return function (dictEq) {
        return function (a) {
            var $323 = Data_Newtype.unwrap();
            var $324 = foldMap(dictFoldable)(Data_Maybe_First.monoidFirst)(function (v) {
                var $305 = Data_Eq.eq(dictEq)(a)(v.value0);
                if ($305) {
                    return new Data_Maybe.Just(v.value1);
                };
                return Data_Maybe.Nothing.value;
            });
            return function ($325) {
                return $323($324($325));
            };
        };
    };
};

// | `foldMap` but with each element surrounded by some fixed value.
// |
// | For example:
// |
// | ```purescript
// | > surroundMap "*" show []
// | = "*"
// |
// | > surroundMap "*" show [1]
// | = "*1*"
// |
// | > surroundMap "*" show [1, 2]
// | = "*1*2*"
// |
// | > surroundMap "*" show [1, 2, 3]
// | = "*1*2*3*"
// | ```
var surroundMap = function (dictFoldable) {
    return function (dictSemigroup) {
        return function (d) {
            return function (t) {
                return function (f) {
                    var joined = function (a) {
                        return function (m) {
                            return Data_Semigroup.append(dictSemigroup)(d)(Data_Semigroup.append(dictSemigroup)(t(a))(m));
                        };
                    };
                    return Data_Newtype.unwrap()(foldMap(dictFoldable)(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn))(joined)(f))(d);
                };
            };
        };
    };
};

// | `fold` but with each element surrounded by some fixed value.
// |
// | For example:
// |
// | ```purescript
// | > surround "*" []
// | = "*"
// |
// | > surround "*" ["1"]
// | = "*1*"
// |
// | > surround "*" ["1", "2"]
// | = "*1*2*"
// |
// | > surround "*" ["1", "2", "3"]
// | = "*1*2*3*"
// | ```
var surround = function (dictFoldable) {
    return function (dictSemigroup) {
        return function (d) {
            return surroundMap(dictFoldable)(dictSemigroup)(d)(Control_Category.identity(Control_Category.categoryFn));
        };
    };
};

// | Similar to 'foldl', but the result is encapsulated in a monad.
// |
// | Note: this function is not generally stack-safe, e.g., for monads which
// | build up thunks a la `Eff`.
var foldM = function (dictFoldable) {
    return function (dictMonad) {
        return function (f) {
            return function (b0) {
                return foldl(dictFoldable)(function (b) {
                    return function (a) {
                        return Control_Bind.bind(dictMonad.Bind1())(b)(Data_Function.flip(f)(a));
                    };
                })(Control_Applicative.pure(dictMonad.Applicative0())(b0));
            };
        };
    };
};

// | Fold a data structure, accumulating values in some `Monoid`.
var fold = function (dictFoldable) {
    return function (dictMonoid) {
        return foldMap(dictFoldable)(dictMonoid)(Control_Category.identity(Control_Category.categoryFn));
    };
};

// | Try to find an element in a data structure which satisfies a predicate mapping.
var findMap = function (dictFoldable) {
    return function (p) {
        var go = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing) {
                    return p(v1);
                };
                return v;
            };
        };
        return foldl(dictFoldable)(go)(Data_Maybe.Nothing.value);
    };
};

// | Try to find an element in a data structure which satisfies a predicate.
var find = function (dictFoldable) {
    return function (p) {
        var go = function (v) {
            return function (v1) {
                if (v instanceof Data_Maybe.Nothing && p(v1)) {
                    return new Data_Maybe.Just(v1);
                };
                return v;
            };
        };
        return foldl(dictFoldable)(go)(Data_Maybe.Nothing.value);
    };
};

// | `any f` is the same as `or <<< map f`; map a function over the structure,
// | and then get the disjunction of the results.
var any = function (dictFoldable) {
    return function (dictHeytingAlgebra) {
        return Data_Newtype.alaF()()()()(Data_Monoid_Disj.Disj)(foldMap(dictFoldable)(Data_Monoid_Disj.monoidDisj(dictHeytingAlgebra)));
    };
};

// | Test whether a value is an element of a data structure.
var elem = function (dictFoldable) {
    return function (dictEq) {
        var $326 = any(dictFoldable)(Data_HeytingAlgebra.heytingAlgebraBoolean);
        var $327 = Data_Eq.eq(dictEq);
        return function ($328) {
            return $326($327($328));
        };
    };
};

// | Test whether a value is not an element of a data structure.
var notElem = function (dictFoldable) {
    return function (dictEq) {
        return function (x) {
            var $329 = Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean);
            var $330 = elem(dictFoldable)(dictEq)(x);
            return function ($331) {
                return $329($330($331));
            };
        };
    };
};

// | The disjunction of all the values in a data structure. When specialized
// | to `Boolean`, this function will test whether any of the values in a data
// | structure is `true`.
var or = function (dictFoldable) {
    return function (dictHeytingAlgebra) {
        return any(dictFoldable)(dictHeytingAlgebra)(Control_Category.identity(Control_Category.categoryFn));
    };
};

// | `all f` is the same as `and <<< map f`; map a function over the structure,
// | and then get the conjunction of the results.
var all = function (dictFoldable) {
    return function (dictHeytingAlgebra) {
        return Data_Newtype.alaF()()()()(Data_Monoid_Conj.Conj)(foldMap(dictFoldable)(Data_Monoid_Conj.monoidConj(dictHeytingAlgebra)));
    };
};

// | The conjunction of all the values in a data structure. When specialized
// | to `Boolean`, this function will test whether all of the values in a data
// | structure are `true`.
var and = function (dictFoldable) {
    return function (dictHeytingAlgebra) {
        return all(dictFoldable)(dictHeytingAlgebra)(Control_Category.identity(Control_Category.categoryFn));
    };
};
export {
    foldr,
    foldl,
    foldMap,
    foldrDefault,
    foldlDefault,
    foldMapDefaultL,
    foldMapDefaultR,
    fold,
    foldM,
    traverse_,
    for_,
    sequence_,
    oneOf,
    oneOfMap,
    intercalate,
    surroundMap,
    surround,
    and,
    or,
    all,
    any,
    sum,
    product,
    elem,
    notElem,
    indexl,
    indexr,
    find,
    findMap,
    maximum,
    maximumBy,
    minimum,
    minimumBy,
    $$null as null,
    length,
    lookup,
    foldableArray,
    foldableMaybe,
    foldableFirst,
    foldableLast,
    foldableAdditive,
    foldableDual,
    foldableDisj,
    foldableConj,
    foldableMultiplicative,
    foldableEither,
    foldableTuple,
    foldableIdentity,
    foldableConst,
    foldableProduct,
    foldableCoproduct,
    foldableCompose,
    foldableApp
};
