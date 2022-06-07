import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor_Coproduct from "../Data.Functor.Coproduct/index.js";
import * as Data_FunctorWithIndex from "../Data.FunctorWithIndex/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Monoid_Conj from "../Data.Monoid.Conj/index.js";
import * as Data_Monoid_Disj from "../Data.Monoid.Disj/index.js";
import * as Data_Monoid_Dual from "../Data.Monoid.Dual/index.js";
import * as Data_Monoid_Endo from "../Data.Monoid.Endo/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
var foldrWithIndex = function (dict) {
    return dict.foldrWithIndex;
};

// | Traverse a data structure with access to the index, performing some
// | effects encoded by an `Applicative` functor at each value, ignoring the
// | final result.
// |
// | For example:
// |
// | ```purescript
// | > traverseWithIndex_ (curry logShow) ["a", "b", "c"]
// | (Tuple 0 "a")
// | (Tuple 1 "b")
// | (Tuple 2 "c")
// | ```
var traverseWithIndex_ = function (dictApplicative) {
    return function (dictFoldableWithIndex) {
        return function (f) {
            return foldrWithIndex(dictFoldableWithIndex)(function (i) {
                var $164 = Control_Apply.applySecond(dictApplicative.Apply0());
                var $165 = f(i);
                return function ($166) {
                    return $164($165($166));
                };
            })(Control_Applicative.pure(dictApplicative)(Data_Unit.unit));
        };
    };
};

// | A version of `traverseWithIndex_` with its arguments flipped.
// |
// | This can be useful when running an action written using do notation
// | for every element in a data structure:
// |
// | For example:
// |
// | ```purescript
// | forWithIndex_ ["a", "b", "c"] \i x -> do
// |   logShow i
// |   log x
// | ```
var forWithIndex_ = function (dictApplicative) {
    return function (dictFoldableWithIndex) {
        return Data_Function.flip(traverseWithIndex_(dictApplicative)(dictFoldableWithIndex));
    };
};

// | A default implementation of `foldr` using `foldrWithIndex`
var foldrDefault = function (dictFoldableWithIndex) {
    return function (f) {
        return foldrWithIndex(dictFoldableWithIndex)(Data_Function["const"](f));
    };
};
var foldlWithIndex = function (dict) {
    return dict.foldlWithIndex;
};

// | A default implementation of `foldl` using `foldlWithIndex`
var foldlDefault = function (dictFoldableWithIndex) {
    return function (f) {
        return foldlWithIndex(dictFoldableWithIndex)(Data_Function["const"](f));
    };
};
var foldableWithIndexTuple = {
    foldrWithIndex: function (f) {
        return function (z) {
            return function (v) {
                return f(Data_Unit.unit)(v.value1)(z);
            };
        };
    },
    foldlWithIndex: function (f) {
        return function (z) {
            return function (v) {
                return f(Data_Unit.unit)(z)(v.value1);
            };
        };
    },
    foldMapWithIndex: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return f(Data_Unit.unit)(v.value1);
            };
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableTuple;
    }
};
var foldableWithIndexMultiplicative = {
    foldrWithIndex: function (f) {
        return Data_Foldable.foldr(Data_Foldable.foldableMultiplicative)(f(Data_Unit.unit));
    },
    foldlWithIndex: function (f) {
        return Data_Foldable.foldl(Data_Foldable.foldableMultiplicative)(f(Data_Unit.unit));
    },
    foldMapWithIndex: function (dictMonoid) {
        return function (f) {
            return Data_Foldable.foldMap(Data_Foldable.foldableMultiplicative)(dictMonoid)(f(Data_Unit.unit));
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableMultiplicative;
    }
};
var foldableWithIndexMaybe = {
    foldrWithIndex: function (f) {
        return Data_Foldable.foldr(Data_Foldable.foldableMaybe)(f(Data_Unit.unit));
    },
    foldlWithIndex: function (f) {
        return Data_Foldable.foldl(Data_Foldable.foldableMaybe)(f(Data_Unit.unit));
    },
    foldMapWithIndex: function (dictMonoid) {
        return function (f) {
            return Data_Foldable.foldMap(Data_Foldable.foldableMaybe)(dictMonoid)(f(Data_Unit.unit));
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableMaybe;
    }
};
var foldableWithIndexLast = {
    foldrWithIndex: function (f) {
        return Data_Foldable.foldr(Data_Foldable.foldableLast)(f(Data_Unit.unit));
    },
    foldlWithIndex: function (f) {
        return Data_Foldable.foldl(Data_Foldable.foldableLast)(f(Data_Unit.unit));
    },
    foldMapWithIndex: function (dictMonoid) {
        return function (f) {
            return Data_Foldable.foldMap(Data_Foldable.foldableLast)(dictMonoid)(f(Data_Unit.unit));
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableLast;
    }
};
var foldableWithIndexIdentity = {
    foldrWithIndex: function (f) {
        return function (z) {
            return function (v) {
                return f(Data_Unit.unit)(v)(z);
            };
        };
    },
    foldlWithIndex: function (f) {
        return function (z) {
            return function (v) {
                return f(Data_Unit.unit)(z)(v);
            };
        };
    },
    foldMapWithIndex: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return f(Data_Unit.unit)(v);
            };
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableIdentity;
    }
};
var foldableWithIndexFirst = {
    foldrWithIndex: function (f) {
        return Data_Foldable.foldr(Data_Foldable.foldableFirst)(f(Data_Unit.unit));
    },
    foldlWithIndex: function (f) {
        return Data_Foldable.foldl(Data_Foldable.foldableFirst)(f(Data_Unit.unit));
    },
    foldMapWithIndex: function (dictMonoid) {
        return function (f) {
            return Data_Foldable.foldMap(Data_Foldable.foldableFirst)(dictMonoid)(f(Data_Unit.unit));
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableFirst;
    }
};
var foldableWithIndexEither = {
    foldrWithIndex: function (v) {
        return function (z) {
            return function (v1) {
                if (v1 instanceof Data_Either.Left) {
                    return z;
                };
                if (v1 instanceof Data_Either.Right) {
                    return v(Data_Unit.unit)(v1.value0)(z);
                };
                throw new Error("Failed pattern match at Data.FoldableWithIndex (line 164, column 1 - line 170, column 42): " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
            };
        };
    },
    foldlWithIndex: function (v) {
        return function (z) {
            return function (v1) {
                if (v1 instanceof Data_Either.Left) {
                    return z;
                };
                if (v1 instanceof Data_Either.Right) {
                    return v(Data_Unit.unit)(z)(v1.value0);
                };
                throw new Error("Failed pattern match at Data.FoldableWithIndex (line 164, column 1 - line 170, column 42): " + [ v.constructor.name, z.constructor.name, v1.constructor.name ]);
            };
        };
    },
    foldMapWithIndex: function (dictMonoid) {
        return function (v) {
            return function (v1) {
                if (v1 instanceof Data_Either.Left) {
                    return Data_Monoid.mempty(dictMonoid);
                };
                if (v1 instanceof Data_Either.Right) {
                    return v(Data_Unit.unit)(v1.value0);
                };
                throw new Error("Failed pattern match at Data.FoldableWithIndex (line 164, column 1 - line 170, column 42): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableEither;
    }
};
var foldableWithIndexDual = {
    foldrWithIndex: function (f) {
        return Data_Foldable.foldr(Data_Foldable.foldableDual)(f(Data_Unit.unit));
    },
    foldlWithIndex: function (f) {
        return Data_Foldable.foldl(Data_Foldable.foldableDual)(f(Data_Unit.unit));
    },
    foldMapWithIndex: function (dictMonoid) {
        return function (f) {
            return Data_Foldable.foldMap(Data_Foldable.foldableDual)(dictMonoid)(f(Data_Unit.unit));
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableDual;
    }
};
var foldableWithIndexDisj = {
    foldrWithIndex: function (f) {
        return Data_Foldable.foldr(Data_Foldable.foldableDisj)(f(Data_Unit.unit));
    },
    foldlWithIndex: function (f) {
        return Data_Foldable.foldl(Data_Foldable.foldableDisj)(f(Data_Unit.unit));
    },
    foldMapWithIndex: function (dictMonoid) {
        return function (f) {
            return Data_Foldable.foldMap(Data_Foldable.foldableDisj)(dictMonoid)(f(Data_Unit.unit));
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableDisj;
    }
};
var foldableWithIndexConst = {
    foldrWithIndex: function (v) {
        return function (z) {
            return function (v1) {
                return z;
            };
        };
    },
    foldlWithIndex: function (v) {
        return function (z) {
            return function (v1) {
                return z;
            };
        };
    },
    foldMapWithIndex: function (dictMonoid) {
        return function (v) {
            return function (v1) {
                return Data_Monoid.mempty(dictMonoid);
            };
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableConst;
    }
};
var foldableWithIndexConj = {
    foldrWithIndex: function (f) {
        return Data_Foldable.foldr(Data_Foldable.foldableConj)(f(Data_Unit.unit));
    },
    foldlWithIndex: function (f) {
        return Data_Foldable.foldl(Data_Foldable.foldableConj)(f(Data_Unit.unit));
    },
    foldMapWithIndex: function (dictMonoid) {
        return function (f) {
            return Data_Foldable.foldMap(Data_Foldable.foldableConj)(dictMonoid)(f(Data_Unit.unit));
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableConj;
    }
};
var foldableWithIndexAdditive = {
    foldrWithIndex: function (f) {
        return Data_Foldable.foldr(Data_Foldable.foldableAdditive)(f(Data_Unit.unit));
    },
    foldlWithIndex: function (f) {
        return Data_Foldable.foldl(Data_Foldable.foldableAdditive)(f(Data_Unit.unit));
    },
    foldMapWithIndex: function (dictMonoid) {
        return function (f) {
            return Data_Foldable.foldMap(Data_Foldable.foldableAdditive)(dictMonoid)(f(Data_Unit.unit));
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableAdditive;
    }
};

// | Similar to 'foldlWithIndex', but the result is encapsulated in a monad.
// |
// | Note: this function is not generally stack-safe, e.g., for monads which
// | build up thunks a la `Eff`.
var foldWithIndexM = function (dictFoldableWithIndex) {
    return function (dictMonad) {
        return function (f) {
            return function (a0) {
                return foldlWithIndex(dictFoldableWithIndex)(function (i) {
                    return function (ma) {
                        return function (b) {
                            return Control_Bind.bind(dictMonad.Bind1())(ma)(Data_Function.flip(f(i))(b));
                        };
                    };
                })(Control_Applicative.pure(dictMonad.Applicative0())(a0));
            };
        };
    };
};

// | A default implementation of `foldMapWithIndex` using `foldrWithIndex`.
// |
// | Note: when defining a `FoldableWithIndex` instance, this function is
// | unsafe to use in combination with `foldrWithIndexDefault`.
var foldMapWithIndexDefaultR = function (dictFoldableWithIndex) {
    return function (dictMonoid) {
        return function (f) {
            return foldrWithIndex(dictFoldableWithIndex)(function (i) {
                return function (x) {
                    return function (acc) {
                        return Data_Semigroup.append(dictMonoid.Semigroup0())(f(i)(x))(acc);
                    };
                };
            })(Data_Monoid.mempty(dictMonoid));
        };
    };
};
var foldableWithIndexArray = {
    foldrWithIndex: function (f) {
        return function (z) {
            var $167 = Data_Foldable.foldr(Data_Foldable.foldableArray)(function (v) {
                return function (y) {
                    return f(v.value0)(v.value1)(y);
                };
            })(z);
            var $168 = Data_FunctorWithIndex.mapWithIndex(Data_FunctorWithIndex.functorWithIndexArray)(Data_Tuple.Tuple.create);
            return function ($169) {
                return $167($168($169));
            };
        };
    },
    foldlWithIndex: function (f) {
        return function (z) {
            var $170 = Data_Foldable.foldl(Data_Foldable.foldableArray)(function (y) {
                return function (v) {
                    return f(v.value0)(y)(v.value1);
                };
            })(z);
            var $171 = Data_FunctorWithIndex.mapWithIndex(Data_FunctorWithIndex.functorWithIndexArray)(Data_Tuple.Tuple.create);
            return function ($172) {
                return $170($171($172));
            };
        };
    },
    foldMapWithIndex: function (dictMonoid) {
        return foldMapWithIndexDefaultR(foldableWithIndexArray)(dictMonoid);
    },
    Foldable0: function () {
        return Data_Foldable.foldableArray;
    }
};

// | A default implementation of `foldMapWithIndex` using `foldlWithIndex`.
// |
// | Note: when defining a `FoldableWithIndex` instance, this function is
// | unsafe to use in combination with `foldlWithIndexDefault`.
var foldMapWithIndexDefaultL = function (dictFoldableWithIndex) {
    return function (dictMonoid) {
        return function (f) {
            return foldlWithIndex(dictFoldableWithIndex)(function (i) {
                return function (acc) {
                    return function (x) {
                        return Data_Semigroup.append(dictMonoid.Semigroup0())(acc)(f(i)(x));
                    };
                };
            })(Data_Monoid.mempty(dictMonoid));
        };
    };
};
var foldMapWithIndex = function (dict) {
    return dict.foldMapWithIndex;
};
var foldableWithIndexApp = function (dictFoldableWithIndex) {
    return {
        foldrWithIndex: function (f) {
            return function (z) {
                return function (v) {
                    return foldrWithIndex(dictFoldableWithIndex)(f)(z)(v);
                };
            };
        },
        foldlWithIndex: function (f) {
            return function (z) {
                return function (v) {
                    return foldlWithIndex(dictFoldableWithIndex)(f)(z)(v);
                };
            };
        },
        foldMapWithIndex: function (dictMonoid) {
            return function (f) {
                return function (v) {
                    return foldMapWithIndex(dictFoldableWithIndex)(dictMonoid)(f)(v);
                };
            };
        },
        Foldable0: function () {
            return Data_Foldable.foldableApp(dictFoldableWithIndex.Foldable0());
        }
    };
};
var foldableWithIndexCompose = function (dictFoldableWithIndex) {
    return function (dictFoldableWithIndex1) {
        return {
            foldrWithIndex: function (f) {
                return function (i) {
                    return function (v) {
                        return foldrWithIndex(dictFoldableWithIndex)(function (a) {
                            return Data_Function.flip(foldrWithIndex(dictFoldableWithIndex1)(Data_Tuple.curry(f)(a)));
                        })(i)(v);
                    };
                };
            },
            foldlWithIndex: function (f) {
                return function (i) {
                    return function (v) {
                        return foldlWithIndex(dictFoldableWithIndex)((function () {
                            var $173 = foldlWithIndex(dictFoldableWithIndex1);
                            var $174 = Data_Tuple.curry(f);
                            return function ($175) {
                                return $173($174($175));
                            };
                        })())(i)(v);
                    };
                };
            },
            foldMapWithIndex: function (dictMonoid) {
                return function (f) {
                    return function (v) {
                        return foldMapWithIndex(dictFoldableWithIndex)(dictMonoid)((function () {
                            var $176 = foldMapWithIndex(dictFoldableWithIndex1)(dictMonoid);
                            var $177 = Data_Tuple.curry(f);
                            return function ($178) {
                                return $176($177($178));
                            };
                        })())(v);
                    };
                };
            },
            Foldable0: function () {
                return Data_Foldable.foldableCompose(dictFoldableWithIndex.Foldable0())(dictFoldableWithIndex1.Foldable0());
            }
        };
    };
};
var foldableWithIndexCoproduct = function (dictFoldableWithIndex) {
    return function (dictFoldableWithIndex1) {
        return {
            foldrWithIndex: function (f) {
                return function (z) {
                    return Data_Functor_Coproduct.coproduct(foldrWithIndex(dictFoldableWithIndex)(function ($179) {
                        return f(Data_Either.Left.create($179));
                    })(z))(foldrWithIndex(dictFoldableWithIndex1)(function ($180) {
                        return f(Data_Either.Right.create($180));
                    })(z));
                };
            },
            foldlWithIndex: function (f) {
                return function (z) {
                    return Data_Functor_Coproduct.coproduct(foldlWithIndex(dictFoldableWithIndex)(function ($181) {
                        return f(Data_Either.Left.create($181));
                    })(z))(foldlWithIndex(dictFoldableWithIndex1)(function ($182) {
                        return f(Data_Either.Right.create($182));
                    })(z));
                };
            },
            foldMapWithIndex: function (dictMonoid) {
                return function (f) {
                    return Data_Functor_Coproduct.coproduct(foldMapWithIndex(dictFoldableWithIndex)(dictMonoid)(function ($183) {
                        return f(Data_Either.Left.create($183));
                    }))(foldMapWithIndex(dictFoldableWithIndex1)(dictMonoid)(function ($184) {
                        return f(Data_Either.Right.create($184));
                    }));
                };
            },
            Foldable0: function () {
                return Data_Foldable.foldableCoproduct(dictFoldableWithIndex.Foldable0())(dictFoldableWithIndex1.Foldable0());
            }
        };
    };
};
var foldableWithIndexProduct = function (dictFoldableWithIndex) {
    return function (dictFoldableWithIndex1) {
        return {
            foldrWithIndex: function (f) {
                return function (z) {
                    return function (v) {
                        return foldrWithIndex(dictFoldableWithIndex)(function ($185) {
                            return f(Data_Either.Left.create($185));
                        })(foldrWithIndex(dictFoldableWithIndex1)(function ($186) {
                            return f(Data_Either.Right.create($186));
                        })(z)(v.value1))(v.value0);
                    };
                };
            },
            foldlWithIndex: function (f) {
                return function (z) {
                    return function (v) {
                        return foldlWithIndex(dictFoldableWithIndex1)(function ($187) {
                            return f(Data_Either.Right.create($187));
                        })(foldlWithIndex(dictFoldableWithIndex)(function ($188) {
                            return f(Data_Either.Left.create($188));
                        })(z)(v.value0))(v.value1);
                    };
                };
            },
            foldMapWithIndex: function (dictMonoid) {
                return function (f) {
                    return function (v) {
                        return Data_Semigroup.append(dictMonoid.Semigroup0())(foldMapWithIndex(dictFoldableWithIndex)(dictMonoid)(function ($189) {
                            return f(Data_Either.Left.create($189));
                        })(v.value0))(foldMapWithIndex(dictFoldableWithIndex1)(dictMonoid)(function ($190) {
                            return f(Data_Either.Right.create($190));
                        })(v.value1));
                    };
                };
            },
            Foldable0: function () {
                return Data_Foldable.foldableProduct(dictFoldableWithIndex.Foldable0())(dictFoldableWithIndex1.Foldable0());
            }
        };
    };
};

// | A default implementation of `foldlWithIndex` using `foldMapWithIndex`.
// |
// | Note: when defining a `FoldableWithIndex` instance, this function is
// | unsafe to use in combination with `foldMapWithIndexDefaultL`.
var foldlWithIndexDefault = function (dictFoldableWithIndex) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Newtype.unwrap()(Data_Newtype.unwrap()(foldMapWithIndex(dictFoldableWithIndex)(Data_Monoid_Dual.monoidDual(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn)))(function (i) {
                    var $191 = Data_Function.flip(c(i));
                    return function ($192) {
                        return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo($191($192)));
                    };
                })(xs)))(u);
            };
        };
    };
};

// | A default implementation of `foldrWithIndex` using `foldMapWithIndex`.
// |
// | Note: when defining a `FoldableWithIndex` instance, this function is
// | unsafe to use in combination with `foldMapWithIndexDefaultR`.
var foldrWithIndexDefault = function (dictFoldableWithIndex) {
    return function (c) {
        return function (u) {
            return function (xs) {
                return Data_Newtype.unwrap()(foldMapWithIndex(dictFoldableWithIndex)(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn))(function (i) {
                    var $193 = c(i);
                    return function ($194) {
                        return Data_Monoid_Endo.Endo($193($194));
                    };
                })(xs))(u);
            };
        };
    };
};

// | `foldMapWithIndex` but with each element surrounded by some fixed value.
// |
// | For example:
// |
// | ```purescript
// | > surroundMapWithIndex "*" (\i x -> show i <> x) []
// | = "*"
// |
// | > surroundMapWithIndex "*" (\i x -> show i <> x) ["a"]
// | = "*0a*"
// |
// | > surroundMapWithIndex "*" (\i x -> show i <> x) ["a", "b"]
// | = "*0a*1b*"
// |
// | > surroundMapWithIndex "*" (\i x -> show i <> x) ["a", "b", "c"]
// | = "*0a*1b*2c*"
// | ```
var surroundMapWithIndex = function (dictFoldableWithIndex) {
    return function (dictSemigroup) {
        return function (d) {
            return function (t) {
                return function (f) {
                    var joined = function (i) {
                        return function (a) {
                            return function (m) {
                                return Data_Semigroup.append(dictSemigroup)(d)(Data_Semigroup.append(dictSemigroup)(t(i)(a))(m));
                            };
                        };
                    };
                    return Data_Newtype.unwrap()(foldMapWithIndex(dictFoldableWithIndex)(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn))(joined)(f))(d);
                };
            };
        };
    };
};

// | A default implementation of `foldMap` using `foldMapWithIndex`
var foldMapDefault = function (dictFoldableWithIndex) {
    return function (dictMonoid) {
        return function (f) {
            return foldMapWithIndex(dictFoldableWithIndex)(dictMonoid)(Data_Function["const"](f));
        };
    };
};

// | Try to find an element in a data structure which satisfies a predicate
// | with access to the index.
var findWithIndex = function (dictFoldableWithIndex) {
    return function (p) {
        var go = function (v) {
            return function (v1) {
                return function (v2) {
                    if (v1 instanceof Data_Maybe.Nothing && p(v)(v2)) {
                        return new Data_Maybe.Just({
                            index: v,
                            value: v2
                        });
                    };
                    return v1;
                };
            };
        };
        return foldlWithIndex(dictFoldableWithIndex)(go)(Data_Maybe.Nothing.value);
    };
};

// | Try to find an element in a data structure which satisfies a predicate mapping
// | with access to the index.
var findMapWithIndex = function (dictFoldableWithIndex) {
    return function (f) {
        var go = function (v) {
            return function (v1) {
                return function (v2) {
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return f(v)(v2);
                    };
                    return v1;
                };
            };
        };
        return foldlWithIndex(dictFoldableWithIndex)(go)(Data_Maybe.Nothing.value);
    };
};

// | `anyWithIndex f` is the same as `or <<< mapWithIndex f`; map a function over the
// | structure, and then get the disjunction of the results.
var anyWithIndex = function (dictFoldableWithIndex) {
    return function (dictHeytingAlgebra) {
        return function (t) {
            var $195 = Data_Newtype.unwrap();
            var $196 = foldMapWithIndex(dictFoldableWithIndex)(Data_Monoid_Disj.monoidDisj(dictHeytingAlgebra))(function (i) {
                var $198 = t(i);
                return function ($199) {
                    return Data_Monoid_Disj.Disj($198($199));
                };
            });
            return function ($197) {
                return $195($196($197));
            };
        };
    };
};

// | `allWithIndex f` is the same as `and <<< mapWithIndex f`; map a function over the
// | structure, and then get the conjunction of the results.
var allWithIndex = function (dictFoldableWithIndex) {
    return function (dictHeytingAlgebra) {
        return function (t) {
            var $200 = Data_Newtype.unwrap();
            var $201 = foldMapWithIndex(dictFoldableWithIndex)(Data_Monoid_Conj.monoidConj(dictHeytingAlgebra))(function (i) {
                var $203 = t(i);
                return function ($204) {
                    return Data_Monoid_Conj.Conj($203($204));
                };
            });
            return function ($202) {
                return $200($201($202));
            };
        };
    };
};
export {
    foldrWithIndex,
    foldlWithIndex,
    foldMapWithIndex,
    foldrWithIndexDefault,
    foldlWithIndexDefault,
    foldMapWithIndexDefaultR,
    foldMapWithIndexDefaultL,
    foldWithIndexM,
    traverseWithIndex_,
    forWithIndex_,
    surroundMapWithIndex,
    allWithIndex,
    anyWithIndex,
    findWithIndex,
    findMapWithIndex,
    foldrDefault,
    foldlDefault,
    foldMapDefault,
    foldableWithIndexArray,
    foldableWithIndexMaybe,
    foldableWithIndexFirst,
    foldableWithIndexLast,
    foldableWithIndexAdditive,
    foldableWithIndexDual,
    foldableWithIndexDisj,
    foldableWithIndexConj,
    foldableWithIndexMultiplicative,
    foldableWithIndexEither,
    foldableWithIndexTuple,
    foldableWithIndexIdentity,
    foldableWithIndexConst,
    foldableWithIndexProduct,
    foldableWithIndexCoproduct,
    foldableWithIndexCompose,
    foldableWithIndexApp
};
