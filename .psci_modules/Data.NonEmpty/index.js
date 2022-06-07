// | This module defines a generic non-empty data structure, which adds an
// | additional element to any container type.
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Plus from "../Control.Plus/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_FoldableWithIndex from "../Data.FoldableWithIndex/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_FunctorWithIndex from "../Data.FunctorWithIndex/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Semigroup_Foldable from "../Data.Semigroup.Foldable/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Data_TraversableWithIndex from "../Data.TraversableWithIndex/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";

// | A non-empty container of elements of type a.
// |
// | ```purescript
// | import Data.NonEmpty
// |
// | nonEmptyArray :: NonEmpty Array Int
// | nonEmptyArray = NonEmpty 1 [2,3]
// |
// | import Data.List(List(..), (:))
// |
// | nonEmptyList :: NonEmpty List Int
// | nonEmptyList = NonEmpty 1 (2 : 3 : Nil)
// | ```
var NonEmpty = /* #__PURE__ */ (function () {
    function NonEmpty(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    NonEmpty.create = function (value0) {
        return function (value1) {
            return new NonEmpty(value0, value1);
        };
    };
    return NonEmpty;
})();
var unfoldable1NonEmpty = function (dictUnfoldable) {
    return {
        unfoldr1: function (f) {
            return function (b) {
                return Data_Tuple.uncurry(NonEmpty.create)(Data_Functor.map(Data_Tuple.functorTuple)(Data_Unfoldable.unfoldr(dictUnfoldable)(Data_Functor.map(Data_Maybe.functorMaybe)(f)))(f(b)));
            };
        }
    };
};

// | Get everything but the 'first' element of a non-empty container.
// |
// | ```purescript
// | tail (1 :| [2, 3]) == [2, 3]
// | ```
var tail = function (v) {
    return v.value1;
};

// | Create a non-empty structure with a single value.
// |
// | ```purescript
// | import Prelude
// |
// | singleton 1 == 1 :| []
// | singleton 1 == 1 :| Nil
// | ```
var singleton = function (dictPlus) {
    return function (a) {
        return new NonEmpty(a, Control_Plus.empty(dictPlus));
    };
};
var showNonEmpty = function (dictShow) {
    return function (dictShow1) {
        return {
            show: function (v) {
                return "(NonEmpty " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(dictShow1)(v.value1) + ")")));
            }
        };
    };
};

// | This is a lawful `Semigroup` instance that will behave sensibly for common nonempty
// | containers like lists and arrays. However, it's not guaranteed that `pure` will behave
// | sensibly alongside `<>` for all types, as we don't have any laws which govern their behavior.
var semigroupNonEmpty = function (dictApplicative) {
    return function (dictSemigroup) {
        return {
            append: function (v) {
                return function (v1) {
                    return new NonEmpty(v.value0, Data_Semigroup.append(dictSemigroup)(v.value1)(Data_Semigroup.append(dictSemigroup)(Control_Applicative.pure(dictApplicative)(v1.value0))(v1.value1)));
                };
            }
        };
    };
};

// | Returns the `alt` (`<|>`) result of:
// | - The first element lifted to the container of the remaining elements.
// | - The remaining elements.
// |
// | ```purescript
// | import Data.Maybe(Maybe(..))
// |
// | oneOf (1 :| Nothing) == Just 1
// | oneOf (1 :| Just 2) == Just 1
// |
// | oneOf (1 :| [2, 3]) == [1,2,3]
// | ```
var oneOf = function (dictAlternative) {
    return function (v) {
        return Control_Alt.alt((dictAlternative.Plus1()).Alt0())(Control_Applicative.pure(dictAlternative.Applicative0())(v.value0))(v.value1);
    };
};

// | Get the 'first' element of a non-empty container.
// |
// | ```purescript
// | head (1 :| [2, 3]) == 1
// | ```
var head = function (v) {
    return v.value0;
};
var functorNonEmpty = function (dictFunctor) {
    return {
        map: function (f) {
            return function (m) {
                return new NonEmpty(f(m.value0), Data_Functor.map(dictFunctor)(f)(m.value1));
            };
        }
    };
};
var functorWithIndex = function (dictFunctorWithIndex) {
    return {
        mapWithIndex: function (f) {
            return function (v) {
                return new NonEmpty(f(Data_Maybe.Nothing.value)(v.value0), Data_FunctorWithIndex.mapWithIndex(dictFunctorWithIndex)(function ($159) {
                    return f(Data_Maybe.Just.create($159));
                })(v.value1));
            };
        },
        Functor0: function () {
            return functorNonEmpty(dictFunctorWithIndex.Functor0());
        }
    };
};

// | Apply a function that takes the `first` element and remaining elements
// | as arguments to a non-empty container.
// |
// | For example, return the remaining elements multiplied by the first element:
// |
// | ```purescript
// | fromNonEmpty (\x xs -> map (_ * x) xs) (3 :| [2, 1]) == [6, 3]
// | ```
var fromNonEmpty = function (f) {
    return function (v) {
        return f(v.value0)(v.value1);
    };
};
var foldableNonEmpty = function (dictFoldable) {
    return {
        foldMap: function (dictMonoid) {
            return function (f) {
                return function (v) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(f(v.value0))(Data_Foldable.foldMap(dictFoldable)(dictMonoid)(f)(v.value1));
                };
            };
        },
        foldl: function (f) {
            return function (b) {
                return function (v) {
                    return Data_Foldable.foldl(dictFoldable)(f)(f(b)(v.value0))(v.value1);
                };
            };
        },
        foldr: function (f) {
            return function (b) {
                return function (v) {
                    return f(v.value0)(Data_Foldable.foldr(dictFoldable)(f)(b)(v.value1));
                };
            };
        }
    };
};
var foldableWithIndexNonEmpty = function (dictFoldableWithIndex) {
    return {
        foldMapWithIndex: function (dictMonoid) {
            return function (f) {
                return function (v) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(f(Data_Maybe.Nothing.value)(v.value0))(Data_FoldableWithIndex.foldMapWithIndex(dictFoldableWithIndex)(dictMonoid)(function ($160) {
                        return f(Data_Maybe.Just.create($160));
                    })(v.value1));
                };
            };
        },
        foldlWithIndex: function (f) {
            return function (b) {
                return function (v) {
                    return Data_FoldableWithIndex.foldlWithIndex(dictFoldableWithIndex)(function ($161) {
                        return f(Data_Maybe.Just.create($161));
                    })(f(Data_Maybe.Nothing.value)(b)(v.value0))(v.value1);
                };
            };
        },
        foldrWithIndex: function (f) {
            return function (b) {
                return function (v) {
                    return f(Data_Maybe.Nothing.value)(v.value0)(Data_FoldableWithIndex.foldrWithIndex(dictFoldableWithIndex)(function ($162) {
                        return f(Data_Maybe.Just.create($162));
                    })(b)(v.value1));
                };
            };
        },
        Foldable0: function () {
            return foldableNonEmpty(dictFoldableWithIndex.Foldable0());
        }
    };
};
var traversableNonEmpty = function (dictTraversable) {
    return {
        sequence: function (dictApplicative) {
            return function (v) {
                return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(NonEmpty.create)(v.value0))(Data_Traversable.sequence(dictTraversable)(dictApplicative)(v.value1));
            };
        },
        traverse: function (dictApplicative) {
            return function (f) {
                return function (v) {
                    return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(NonEmpty.create)(f(v.value0)))(Data_Traversable.traverse(dictTraversable)(dictApplicative)(f)(v.value1));
                };
            };
        },
        Functor0: function () {
            return functorNonEmpty(dictTraversable.Functor0());
        },
        Foldable1: function () {
            return foldableNonEmpty(dictTraversable.Foldable1());
        }
    };
};
var traversableWithIndexNonEmpty = function (dictTraversableWithIndex) {
    return {
        traverseWithIndex: function (dictApplicative) {
            return function (f) {
                return function (v) {
                    return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(NonEmpty.create)(f(Data_Maybe.Nothing.value)(v.value0)))(Data_TraversableWithIndex.traverseWithIndex(dictTraversableWithIndex)(dictApplicative)(function ($163) {
                        return f(Data_Maybe.Just.create($163));
                    })(v.value1));
                };
            };
        },
        FunctorWithIndex0: function () {
            return functorWithIndex(dictTraversableWithIndex.FunctorWithIndex0());
        },
        FoldableWithIndex1: function () {
            return foldableWithIndexNonEmpty(dictTraversableWithIndex.FoldableWithIndex1());
        },
        Traversable2: function () {
            return traversableNonEmpty(dictTraversableWithIndex.Traversable2());
        }
    };
};
var foldable1NonEmpty = function (dictFoldable) {
    return {
        foldMap1: function (dictSemigroup) {
            return function (f) {
                return function (v) {
                    return Data_Foldable.foldl(dictFoldable)(function (s) {
                        return function (a1) {
                            return Data_Semigroup.append(dictSemigroup)(s)(f(a1));
                        };
                    })(f(v.value0))(v.value1);
                };
            };
        },
        foldr1: function (f) {
            return function (v) {
                return Data_Maybe.maybe(v.value0)(f(v.value0))(Data_Foldable.foldr(dictFoldable)(function (a1) {
                    var $164 = Data_Maybe.maybe(a1)(f(a1));
                    return function ($165) {
                        return Data_Maybe.Just.create($164($165));
                    };
                })(Data_Maybe.Nothing.value)(v.value1));
            };
        },
        foldl1: function (f) {
            return function (v) {
                return Data_Foldable.foldl(dictFoldable)(f)(v.value0)(v.value1);
            };
        },
        Foldable0: function () {
            return foldableNonEmpty(dictFoldable);
        }
    };
};

// | Fold a non-empty structure, collecting results using a binary operation.
// |
// | ```purescript
// | foldl1 (+) (1 :| [2, 3]) == 6
// | ```
var foldl1 = function (dictFoldable) {
    return Data_Semigroup_Foldable.foldl1(foldable1NonEmpty(dictFoldable));
};
var eqNonEmpty = function (dictEq1) {
    return function (dictEq) {
        return {
            eq: function (x) {
                return function (y) {
                    return Data_Eq.eq(dictEq)(x.value0)(y.value0) && Data_Eq.eq1(dictEq1)(dictEq)(x.value1)(y.value1);
                };
            }
        };
    };
};
var ordNonEmpty = function (dictOrd1) {
    return function (dictOrd) {
        return {
            compare: function (x) {
                return function (y) {
                    var v = Data_Ord.compare(dictOrd)(x.value0)(y.value0);
                    if (v instanceof Data_Ordering.LT) {
                        return Data_Ordering.LT.value;
                    };
                    if (v instanceof Data_Ordering.GT) {
                        return Data_Ordering.GT.value;
                    };
                    return Data_Ord.compare1(dictOrd1)(dictOrd)(x.value1)(y.value1);
                };
            },
            Eq0: function () {
                return eqNonEmpty(dictOrd1.Eq10())(dictOrd.Eq0());
            }
        };
    };
};
var eq1NonEmpty = function (dictEq1) {
    return {
        eq1: function (dictEq) {
            return Data_Eq.eq(eqNonEmpty(dictEq1)(dictEq));
        }
    };
};
var ord1NonEmpty = function (dictOrd1) {
    return {
        compare1: function (dictOrd) {
            return Data_Ord.compare(ordNonEmpty(dictOrd1)(dictOrd));
        },
        Eq10: function () {
            return eq1NonEmpty(dictOrd1.Eq10());
        }
    };
};
export {
    NonEmpty,
    singleton,
    foldl1,
    fromNonEmpty,
    oneOf,
    head,
    tail,
    showNonEmpty,
    eqNonEmpty,
    eq1NonEmpty,
    ordNonEmpty,
    ord1NonEmpty,
    functorNonEmpty,
    functorWithIndex,
    foldableNonEmpty,
    foldableWithIndexNonEmpty,
    traversableNonEmpty,
    traversableWithIndexNonEmpty,
    foldable1NonEmpty,
    unfoldable1NonEmpty,
    semigroupNonEmpty
};
