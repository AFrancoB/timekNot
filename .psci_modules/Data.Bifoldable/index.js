import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Monoid_Conj from "../Data.Monoid.Conj/index.js";
import * as Data_Monoid_Disj from "../Data.Monoid.Disj/index.js";
import * as Data_Monoid_Dual from "../Data.Monoid.Dual/index.js";
import * as Data_Monoid_Endo from "../Data.Monoid.Endo/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
var bifoldr = function (dict) {
    return dict.bifoldr;
};

// | Traverse a data structure, accumulating effects using an `Applicative` functor,
// | ignoring the final result.
var bitraverse_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return function (f) {
            return function (g) {
                return bifoldr(dictBifoldable)((function () {
                    var $150 = Control_Apply.applySecond(dictApplicative.Apply0());
                    return function ($151) {
                        return $150(f($151));
                    };
                })())((function () {
                    var $152 = Control_Apply.applySecond(dictApplicative.Apply0());
                    return function ($153) {
                        return $152(g($153));
                    };
                })())(Control_Applicative.pure(dictApplicative)(Data_Unit.unit));
            };
        };
    };
};

// | A version of `bitraverse_` with the data structure as the first argument.
var bifor_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return function (g) {
                    return bitraverse_(dictBifoldable)(dictApplicative)(f)(g)(t);
                };
            };
        };
    };
};

// | Collapse a data structure, collecting effects using an `Applicative` functor,
// | ignoring the final result.
var bisequence_ = function (dictBifoldable) {
    return function (dictApplicative) {
        return bitraverse_(dictBifoldable)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn))(Control_Category.identity(Control_Category.categoryFn));
    };
};
var bifoldl = function (dict) {
    return dict.bifoldl;
};
var bifoldableTuple = {
    bifoldMap: function (dictMonoid) {
        return function (f) {
            return function (g) {
                return function (v) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(f(v.value0))(g(v.value1));
                };
            };
        };
    },
    bifoldr: function (f) {
        return function (g) {
            return function (z) {
                return function (v) {
                    return f(v.value0)(g(v.value1)(z));
                };
            };
        };
    },
    bifoldl: function (f) {
        return function (g) {
            return function (z) {
                return function (v) {
                    return g(f(z)(v.value0))(v.value1);
                };
            };
        };
    }
};
var bifoldableJoker = function (dictFoldable) {
    return {
        bifoldr: function (v) {
            return function (r) {
                return function (u) {
                    return function (v1) {
                        return Data_Foldable.foldr(dictFoldable)(r)(u)(v1);
                    };
                };
            };
        },
        bifoldl: function (v) {
            return function (r) {
                return function (u) {
                    return function (v1) {
                        return Data_Foldable.foldl(dictFoldable)(r)(u)(v1);
                    };
                };
            };
        },
        bifoldMap: function (dictMonoid) {
            return function (v) {
                return function (r) {
                    return function (v1) {
                        return Data_Foldable.foldMap(dictFoldable)(dictMonoid)(r)(v1);
                    };
                };
            };
        }
    };
};
var bifoldableEither = {
    bifoldr: function (v) {
        return function (v1) {
            return function (z) {
                return function (v2) {
                    if (v2 instanceof Data_Either.Left) {
                        return v(v2.value0)(z);
                    };
                    if (v2 instanceof Data_Either.Right) {
                        return v1(v2.value0)(z);
                    };
                    throw new Error("Failed pattern match at Data.Bifoldable (line 62, column 1 - line 68, column 32): " + [ v.constructor.name, v1.constructor.name, z.constructor.name, v2.constructor.name ]);
                };
            };
        };
    },
    bifoldl: function (v) {
        return function (v1) {
            return function (z) {
                return function (v2) {
                    if (v2 instanceof Data_Either.Left) {
                        return v(z)(v2.value0);
                    };
                    if (v2 instanceof Data_Either.Right) {
                        return v1(z)(v2.value0);
                    };
                    throw new Error("Failed pattern match at Data.Bifoldable (line 62, column 1 - line 68, column 32): " + [ v.constructor.name, v1.constructor.name, z.constructor.name, v2.constructor.name ]);
                };
            };
        };
    },
    bifoldMap: function (dictMonoid) {
        return function (v) {
            return function (v1) {
                return function (v2) {
                    if (v2 instanceof Data_Either.Left) {
                        return v(v2.value0);
                    };
                    if (v2 instanceof Data_Either.Right) {
                        return v1(v2.value0);
                    };
                    throw new Error("Failed pattern match at Data.Bifoldable (line 62, column 1 - line 68, column 32): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
                };
            };
        };
    }
};
var bifoldableConst = {
    bifoldr: function (f) {
        return function (v) {
            return function (z) {
                return function (v1) {
                    return f(v1)(z);
                };
            };
        };
    },
    bifoldl: function (f) {
        return function (v) {
            return function (z) {
                return function (v1) {
                    return f(z)(v1);
                };
            };
        };
    },
    bifoldMap: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return function (v1) {
                    return f(v1);
                };
            };
        };
    }
};
var bifoldableClown = function (dictFoldable) {
    return {
        bifoldr: function (l) {
            return function (v) {
                return function (u) {
                    return function (v1) {
                        return Data_Foldable.foldr(dictFoldable)(l)(u)(v1);
                    };
                };
            };
        },
        bifoldl: function (l) {
            return function (v) {
                return function (u) {
                    return function (v1) {
                        return Data_Foldable.foldl(dictFoldable)(l)(u)(v1);
                    };
                };
            };
        },
        bifoldMap: function (dictMonoid) {
            return function (l) {
                return function (v) {
                    return function (v1) {
                        return Data_Foldable.foldMap(dictFoldable)(dictMonoid)(l)(v1);
                    };
                };
            };
        }
    };
};

// | A default implementation of `bifoldMap` using `bifoldr`.
// |
// | Note: when defining a `Bifoldable` instance, this function is unsafe to
// | use in combination with `bifoldrDefault`.
var bifoldMapDefaultR = function (dictBifoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (g) {
                return bifoldr(dictBifoldable)((function () {
                    var $154 = Data_Semigroup.append(dictMonoid.Semigroup0());
                    return function ($155) {
                        return $154(f($155));
                    };
                })())((function () {
                    var $156 = Data_Semigroup.append(dictMonoid.Semigroup0());
                    return function ($157) {
                        return $156(g($157));
                    };
                })())(Data_Monoid.mempty(dictMonoid));
            };
        };
    };
};

// | A default implementation of `bifoldMap` using `bifoldl`.
// |
// | Note: when defining a `Bifoldable` instance, this function is unsafe to
// | use in combination with `bifoldlDefault`.
var bifoldMapDefaultL = function (dictBifoldable) {
    return function (dictMonoid) {
        return function (f) {
            return function (g) {
                return bifoldl(dictBifoldable)(function (m) {
                    return function (a) {
                        return Data_Semigroup.append(dictMonoid.Semigroup0())(m)(f(a));
                    };
                })(function (m) {
                    return function (b) {
                        return Data_Semigroup.append(dictMonoid.Semigroup0())(m)(g(b));
                    };
                })(Data_Monoid.mempty(dictMonoid));
            };
        };
    };
};
var bifoldMap = function (dict) {
    return dict.bifoldMap;
};
var bifoldableFlip = function (dictBifoldable) {
    return {
        bifoldr: function (r) {
            return function (l) {
                return function (u) {
                    return function (v) {
                        return bifoldr(dictBifoldable)(l)(r)(u)(v);
                    };
                };
            };
        },
        bifoldl: function (r) {
            return function (l) {
                return function (u) {
                    return function (v) {
                        return bifoldl(dictBifoldable)(l)(r)(u)(v);
                    };
                };
            };
        },
        bifoldMap: function (dictMonoid) {
            return function (r) {
                return function (l) {
                    return function (v) {
                        return bifoldMap(dictBifoldable)(dictMonoid)(l)(r)(v);
                    };
                };
            };
        }
    };
};

// | A default implementation of `bifoldl` using `bifoldMap`.
// |
// | Note: when defining a `Bifoldable` instance, this function is unsafe to
// | use in combination with `bifoldMapDefaultL`.
var bifoldlDefault = function (dictBifoldable) {
    return function (f) {
        return function (g) {
            return function (z) {
                return function (p) {
                    return Data_Newtype.unwrap()(Data_Newtype.unwrap()(bifoldMap(dictBifoldable)(Data_Monoid_Dual.monoidDual(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn)))((function () {
                        var $158 = Data_Function.flip(f);
                        return function ($159) {
                            return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo($158($159)));
                        };
                    })())((function () {
                        var $160 = Data_Function.flip(g);
                        return function ($161) {
                            return Data_Monoid_Dual.Dual(Data_Monoid_Endo.Endo($160($161)));
                        };
                    })())(p)))(z);
                };
            };
        };
    };
};

// | A default implementation of `bifoldr` using `bifoldMap`.
// |
// | Note: when defining a `Bifoldable` instance, this function is unsafe to
// | use in combination with `bifoldMapDefaultR`.
var bifoldrDefault = function (dictBifoldable) {
    return function (f) {
        return function (g) {
            return function (z) {
                return function (p) {
                    return Data_Newtype.unwrap()(bifoldMap(dictBifoldable)(Data_Monoid_Endo.monoidEndo(Control_Category.categoryFn))(function ($162) {
                        return Data_Monoid_Endo.Endo(f($162));
                    })(function ($163) {
                        return Data_Monoid_Endo.Endo(g($163));
                    })(p))(z);
                };
            };
        };
    };
};
var bifoldableProduct2 = function (dictBifoldable) {
    return function (dictBifoldable1) {
        return {
            bifoldr: function (l) {
                return function (r) {
                    return function (u) {
                        return function (m) {
                            return bifoldrDefault(bifoldableProduct2(dictBifoldable)(dictBifoldable1))(l)(r)(u)(m);
                        };
                    };
                };
            },
            bifoldl: function (l) {
                return function (r) {
                    return function (u) {
                        return function (m) {
                            return bifoldlDefault(bifoldableProduct2(dictBifoldable)(dictBifoldable1))(l)(r)(u)(m);
                        };
                    };
                };
            },
            bifoldMap: function (dictMonoid) {
                return function (l) {
                    return function (r) {
                        return function (v) {
                            return Data_Semigroup.append(dictMonoid.Semigroup0())(bifoldMap(dictBifoldable)(dictMonoid)(l)(r)(v.value0))(bifoldMap(dictBifoldable1)(dictMonoid)(l)(r)(v.value1));
                        };
                    };
                };
            }
        };
    };
};

// | Fold a data structure, accumulating values in a monoidal type.
var bifold = function (dictBifoldable) {
    return function (dictMonoid) {
        return bifoldMap(dictBifoldable)(dictMonoid)(Control_Category.identity(Control_Category.categoryFn))(Control_Category.identity(Control_Category.categoryFn));
    };
};

// | Test whether a predicate holds at any position in a data structure.
var biany = function (dictBifoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function (q) {
                var $164 = Data_Newtype.unwrap();
                var $165 = bifoldMap(dictBifoldable)(Data_Monoid_Disj.monoidDisj(dictBooleanAlgebra.HeytingAlgebra0()))(function ($167) {
                    return Data_Monoid_Disj.Disj(p($167));
                })(function ($168) {
                    return Data_Monoid_Disj.Disj(q($168));
                });
                return function ($166) {
                    return $164($165($166));
                };
            };
        };
    };
};

// | Test whether a predicate holds at all positions in a data structure.
var biall = function (dictBifoldable) {
    return function (dictBooleanAlgebra) {
        return function (p) {
            return function (q) {
                var $169 = Data_Newtype.unwrap();
                var $170 = bifoldMap(dictBifoldable)(Data_Monoid_Conj.monoidConj(dictBooleanAlgebra.HeytingAlgebra0()))(function ($172) {
                    return Data_Monoid_Conj.Conj(p($172));
                })(function ($173) {
                    return Data_Monoid_Conj.Conj(q($173));
                });
                return function ($171) {
                    return $169($170($171));
                };
            };
        };
    };
};
export {
    bifoldMap,
    bifoldl,
    bifoldr,
    bifoldrDefault,
    bifoldlDefault,
    bifoldMapDefaultR,
    bifoldMapDefaultL,
    bifold,
    bitraverse_,
    bifor_,
    bisequence_,
    biany,
    biall,
    bifoldableClown,
    bifoldableJoker,
    bifoldableFlip,
    bifoldableProduct2,
    bifoldableEither,
    bifoldableTuple,
    bifoldableConst
};
