import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Monoid_Dual from "../Data.Monoid.Dual/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Ord_Max from "../Data.Ord.Max/index.js";
import * as Data_Ord_Min from "../Data.Ord.Min/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Unit from "../Data.Unit/index.js";

// | Internal. Used by intercalation functions.
var JoinWith = function (x) {
    return x;
};

// | Internal. Used by foldr1Default and foldl1Default.
var FoldRight1 = /* #__PURE__ */ (function () {
    function FoldRight1(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    FoldRight1.create = function (value0) {
        return function (value1) {
            return new FoldRight1(value0, value1);
        };
    };
    return FoldRight1;
})();
var Act = function (x) {
    return x;
};
var semigroupJoinWith = function (dictSemigroup) {
    return {
        append: function (v) {
            return function (v1) {
                return function (j) {
                    return Data_Semigroup.append(dictSemigroup)(v(j))(Data_Semigroup.append(dictSemigroup)(j)(v1(j)));
                };
            };
        }
    };
};
var semigroupAct = function (dictApply) {
    return {
        append: function (v) {
            return function (v1) {
                return Control_Apply.applySecond(dictApply)(v)(v1);
            };
        }
    };
};
var runFoldRight1 = function (v) {
    return v.value0(v.value1);
};
var mkFoldRight1 = /* #__PURE__ */ (function () {
    return FoldRight1.create(Data_Function["const"]);
})();
var joinee = function (v) {
    return v;
};
var getAct = function (v) {
    return v;
};
var foldr1 = function (dict) {
    return dict.foldr1;
};
var foldl1 = function (dict) {
    return dict.foldl1;
};
var maximumBy = function (dictFoldable1) {
    return function (cmp) {
        return foldl1(dictFoldable1)(function (x) {
            return function (y) {
                var $72 = Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(x)(y))(Data_Ordering.GT.value);
                if ($72) {
                    return x;
                };
                return y;
            };
        });
    };
};
var minimumBy = function (dictFoldable1) {
    return function (cmp) {
        return foldl1(dictFoldable1)(function (x) {
            return function (y) {
                var $73 = Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(x)(y))(Data_Ordering.LT.value);
                if ($73) {
                    return x;
                };
                return y;
            };
        });
    };
};
var foldableTuple = {
    foldMap1: function (dictSemigroup) {
        return function (f) {
            return function (v) {
                return f(v.value1);
            };
        };
    },
    foldr1: function (v) {
        return function (v1) {
            return v1.value1;
        };
    },
    foldl1: function (v) {
        return function (v1) {
            return v1.value1;
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableTuple;
    }
};
var foldableMultiplicative = {
    foldr1: function (v) {
        return function (v1) {
            return v1;
        };
    },
    foldl1: function (v) {
        return function (v1) {
            return v1;
        };
    },
    foldMap1: function (dictSemigroup) {
        return function (f) {
            return function (v) {
                return f(v);
            };
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableMultiplicative;
    }
};
var foldableIdentity = {
    foldMap1: function (dictSemigroup) {
        return function (f) {
            return function (v) {
                return f(v);
            };
        };
    },
    foldl1: function (v) {
        return function (v1) {
            return v1;
        };
    },
    foldr1: function (v) {
        return function (v1) {
            return v1;
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableIdentity;
    }
};
var foldableDual = {
    foldr1: function (v) {
        return function (v1) {
            return v1;
        };
    },
    foldl1: function (v) {
        return function (v1) {
            return v1;
        };
    },
    foldMap1: function (dictSemigroup) {
        return function (f) {
            return function (v) {
                return f(v);
            };
        };
    },
    Foldable0: function () {
        return Data_Foldable.foldableDual;
    }
};
var foldRight1Semigroup = {
    append: function (v) {
        return function (v1) {
            return new FoldRight1(function (a) {
                return function (f) {
                    return v.value0(f(v.value1)(v1.value0(a)(f)))(f);
                };
            }, v1.value1);
        };
    }
};

// | A default implementation of `foldMap1` using `foldr1`.
// |
// | Note: when defining a `Foldable1` instance, this function is unsafe to use
// | in combination with `foldr1Default`.
var foldMap1DefaultR = function (dictFoldable1) {
    return function (dictFunctor) {
        return function (dictSemigroup) {
            return function (f) {
                var $111 = foldr1(dictFoldable1)(Data_Semigroup.append(dictSemigroup));
                var $112 = Data_Functor.map(dictFunctor)(f);
                return function ($113) {
                    return $111($112($113));
                };
            };
        };
    };
};

// | A default implementation of `foldMap1` using `foldl1`.
// |
// | Note: when defining a `Foldable1` instance, this function is unsafe to use
// | in combination with `foldl1Default`.
var foldMap1DefaultL = function (dictFoldable1) {
    return function (dictFunctor) {
        return function (dictSemigroup) {
            return function (f) {
                var $114 = foldl1(dictFoldable1)(Data_Semigroup.append(dictSemigroup));
                var $115 = Data_Functor.map(dictFunctor)(f);
                return function ($116) {
                    return $114($115($116));
                };
            };
        };
    };
};
var foldMap1 = function (dict) {
    return dict.foldMap1;
};

// | A default implementation of `foldl1` using `foldMap1`.
// |
// | Note: when defining a `Foldable1` instance, this function is unsafe to use
// | in combination with `foldMap1DefaultL`.
var foldl1Default = function (dictFoldable1) {
    var $117 = Data_Function.flip((function () {
        var $119 = Data_Newtype.alaF()()()()(Data_Monoid_Dual.Dual)(foldMap1(dictFoldable1)(Data_Monoid_Dual.semigroupDual(foldRight1Semigroup)))(mkFoldRight1);
        return function ($120) {
            return runFoldRight1($119($120));
        };
    })());
    return function ($118) {
        return $117(Data_Function.flip($118));
    };
};

// | A default implementation of `foldr1` using `foldMap1`.
// |
// | Note: when defining a `Foldable1` instance, this function is unsafe to use
// | in combination with `foldMap1DefaultR`.
var foldr1Default = function (dictFoldable1) {
    return Data_Function.flip((function () {
        var $121 = foldMap1(dictFoldable1)(foldRight1Semigroup)(mkFoldRight1);
        return function ($122) {
            return runFoldRight1($121($122));
        };
    })());
};

// | Fold a data structure, accumulating values in some `Semigroup`,
// | combining adjacent elements using the specified separator.
var intercalateMap = function (dictFoldable1) {
    return function (dictSemigroup) {
        return function (j) {
            return function (f) {
                return function (foldable) {
                    return joinee(foldMap1(dictFoldable1)(semigroupJoinWith(dictSemigroup))(function ($123) {
                        return JoinWith(Data_Function["const"](f($123)));
                    })(foldable))(j);
                };
            };
        };
    };
};

// | Fold a data structure using a `Semigroup` instance,
// | combining adjacent elements using the specified separator.
var intercalate = function (dictFoldable1) {
    return function (dictSemigroup) {
        return Data_Function.flip(intercalateMap(dictFoldable1)(dictSemigroup))(Control_Category.identity(Control_Category.categoryFn));
    };
};
var maximum = function (dictOrd) {
    return function (dictFoldable1) {
        return Data_Newtype.ala()()()(Data_Ord_Max.Max)(foldMap1(dictFoldable1)(Data_Ord_Max.semigroupMax(dictOrd)));
    };
};
var minimum = function (dictOrd) {
    return function (dictFoldable1) {
        return Data_Newtype.ala()()()(Data_Ord_Min.Min)(foldMap1(dictFoldable1)(Data_Ord_Min.semigroupMin(dictOrd)));
    };
};

// | Traverse a data structure, performing some effects encoded by an
// | `Apply` instance at each value, ignoring the final result.
var traverse1_ = function (dictFoldable1) {
    return function (dictApply) {
        return function (f) {
            return function (t) {
                return Data_Functor.voidRight(dictApply.Functor0())(Data_Unit.unit)(getAct(foldMap1(dictFoldable1)(semigroupAct(dictApply))(function ($124) {
                    return Act(f($124));
                })(t)));
            };
        };
    };
};

// | A version of `traverse1_` with its arguments flipped.
// |
// | This can be useful when running an action written using do notation
// | for every element in a data structure:
var for1_ = function (dictFoldable1) {
    return function (dictApply) {
        return Data_Function.flip(traverse1_(dictFoldable1)(dictApply));
    };
};

// | Perform all of the effects in some data structure in the order
// | given by the `Foldable1` instance, ignoring the final result.
var sequence1_ = function (dictFoldable1) {
    return function (dictApply) {
        return traverse1_(dictFoldable1)(dictApply)(Control_Category.identity(Control_Category.categoryFn));
    };
};

// | Fold a data structure, accumulating values in some `Semigroup`.
var fold1 = function (dictFoldable1) {
    return function (dictSemigroup) {
        return foldMap1(dictFoldable1)(dictSemigroup)(Control_Category.identity(Control_Category.categoryFn));
    };
};
export {
    foldMap1,
    fold1,
    foldr1,
    foldl1,
    traverse1_,
    for1_,
    sequence1_,
    foldr1Default,
    foldl1Default,
    foldMap1DefaultR,
    foldMap1DefaultL,
    intercalate,
    intercalateMap,
    maximum,
    maximumBy,
    minimum,
    minimumBy,
    foldableDual,
    foldableMultiplicative,
    foldableTuple,
    foldableIdentity
};
