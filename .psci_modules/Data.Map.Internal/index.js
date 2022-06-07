// | This module defines a type of maps as balanced 2-3 trees, based on
// | <http://www.cs.princeton.edu/~dpw/courses/cos326-12/ass/2-3-trees.pdf>
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_FoldableWithIndex from "../Data.FoldableWithIndex/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_FunctorWithIndex from "../Data.FunctorWithIndex/index.js";
import * as Data_HeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Lazy from "../Data.List.Lazy/index.js";
import * as Data_List_Lazy_Types from "../Data.List.Lazy.Types/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Data_TraversableWithIndex from "../Data.TraversableWithIndex/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";
import * as Partial_Unsafe from "../Partial.Unsafe/index.js";

// | `Map k v` represents maps from keys of type `k` to values of type `v`.
var Leaf = /* #__PURE__ */ (function () {
    function Leaf() {

    };
    Leaf.value = new Leaf();
    return Leaf;
})();

// | `Map k v` represents maps from keys of type `k` to values of type `v`.
var Two = /* #__PURE__ */ (function () {
    function Two(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    Two.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new Two(value0, value1, value2, value3);
                };
            };
        };
    };
    return Two;
})();

// | `Map k v` represents maps from keys of type `k` to values of type `v`.
var Three = /* #__PURE__ */ (function () {
    function Three(value0, value1, value2, value3, value4, value5, value6) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
        this.value6 = value6;
    };
    Three.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return function (value6) {
                                return new Three(value0, value1, value2, value3, value4, value5, value6);
                            };
                        };
                    };
                };
            };
        };
    };
    return Three;
})();
var TwoLeft = /* #__PURE__ */ (function () {
    function TwoLeft(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    TwoLeft.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new TwoLeft(value0, value1, value2);
            };
        };
    };
    return TwoLeft;
})();
var TwoRight = /* #__PURE__ */ (function () {
    function TwoRight(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    TwoRight.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new TwoRight(value0, value1, value2);
            };
        };
    };
    return TwoRight;
})();
var ThreeLeft = /* #__PURE__ */ (function () {
    function ThreeLeft(value0, value1, value2, value3, value4, value5) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
    };
    ThreeLeft.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return new ThreeLeft(value0, value1, value2, value3, value4, value5);
                        };
                    };
                };
            };
        };
    };
    return ThreeLeft;
})();
var ThreeMiddle = /* #__PURE__ */ (function () {
    function ThreeMiddle(value0, value1, value2, value3, value4, value5) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
    };
    ThreeMiddle.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return new ThreeMiddle(value0, value1, value2, value3, value4, value5);
                        };
                    };
                };
            };
        };
    };
    return ThreeMiddle;
})();
var ThreeRight = /* #__PURE__ */ (function () {
    function ThreeRight(value0, value1, value2, value3, value4, value5) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
    };
    ThreeRight.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return new ThreeRight(value0, value1, value2, value3, value4, value5);
                        };
                    };
                };
            };
        };
    };
    return ThreeRight;
})();
var KickUp = /* #__PURE__ */ (function () {
    function KickUp(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    KickUp.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new KickUp(value0, value1, value2, value3);
                };
            };
        };
    };
    return KickUp;
})();

// | Calculate the number of key/value pairs in a map
var size = function (v) {
    if (v instanceof Leaf) {
        return 0;
    };
    if (v instanceof Two) {
        return (1 + size(v.value0) | 0) + size(v.value3) | 0;
    };
    if (v instanceof Three) {
        return ((2 + size(v.value0) | 0) + size(v.value3) | 0) + size(v.value6) | 0;
    };
    throw new Error("Failed pattern match at Data.Map.Internal (line 705, column 1 - line 705, column 35): " + [ v.constructor.name ]);
};

// | Create a map with one key/value pair
var singleton = function (k) {
    return function (v) {
        return new Two(Leaf.value, k, v, Leaf.value);
    };
};

// | Convert a map to an unfoldable structure of key/value pairs where the keys are in ascending order
var toUnfoldable = function (dictUnfoldable) {
    return function (m) {
        var go = function ($copy_v) {
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v) {
                if (v instanceof Data_List_Types.Nil) {
                    $tco_done = true;
                    return Data_Maybe.Nothing.value;
                };
                if (v instanceof Data_List_Types.Cons) {
                    if (v.value0 instanceof Leaf) {
                        $copy_v = v.value1;
                        return;
                    };
                    if (v.value0 instanceof Two && (v.value0.value0 instanceof Leaf && v.value0.value3 instanceof Leaf)) {
                        $tco_done = true;
                        return new Data_Maybe.Just(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value1, v.value0.value2), v.value1));
                    };
                    if (v.value0 instanceof Two && v.value0.value0 instanceof Leaf) {
                        $tco_done = true;
                        return new Data_Maybe.Just(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value1, v.value0.value2), new Data_List_Types.Cons(v.value0.value3, v.value1)));
                    };
                    if (v.value0 instanceof Two) {
                        $copy_v = new Data_List_Types.Cons(v.value0.value0, new Data_List_Types.Cons(singleton(v.value0.value1)(v.value0.value2), new Data_List_Types.Cons(v.value0.value3, v.value1)));
                        return;
                    };
                    if (v.value0 instanceof Three) {
                        $copy_v = new Data_List_Types.Cons(v.value0.value0, new Data_List_Types.Cons(singleton(v.value0.value1)(v.value0.value2), new Data_List_Types.Cons(v.value0.value3, new Data_List_Types.Cons(singleton(v.value0.value4)(v.value0.value5), new Data_List_Types.Cons(v.value0.value6, v.value1)))));
                        return;
                    };
                    throw new Error("Failed pattern match at Data.Map.Internal (line 624, column 18 - line 633, column 71): " + [ v.value0.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 623, column 3 - line 623, column 19): " + [ v.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($copy_v);
            };
            return $tco_result;
        };
        return Data_Unfoldable.unfoldr(dictUnfoldable)(go)(new Data_List_Types.Cons(m, Data_List_Types.Nil.value));
    };
};

// Internal use
var toAscArray = /* #__PURE__ */ toUnfoldable(Data_Unfoldable.unfoldableArray);

// | Convert a map to an unfoldable structure of key/value pairs
// |
// | While this traversal is up to 10% faster in benchmarks than `toUnfoldable`,
// | it leaks the underlying map stucture, making it only suitable for applications
// | where order is irrelevant.
// |
// | If you are unsure, use `toUnfoldable`
var toUnfoldableUnordered = function (dictUnfoldable) {
    return function (m) {
        var go = function ($copy_v) {
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v) {
                if (v instanceof Data_List_Types.Nil) {
                    $tco_done = true;
                    return Data_Maybe.Nothing.value;
                };
                if (v instanceof Data_List_Types.Cons) {
                    if (v.value0 instanceof Leaf) {
                        $copy_v = v.value1;
                        return;
                    };
                    if (v.value0 instanceof Two) {
                        $tco_done = true;
                        return new Data_Maybe.Just(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value1, v.value0.value2), new Data_List_Types.Cons(v.value0.value0, new Data_List_Types.Cons(v.value0.value3, v.value1))));
                    };
                    if (v.value0 instanceof Three) {
                        $tco_done = true;
                        return new Data_Maybe.Just(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0.value1, v.value0.value2), new Data_List_Types.Cons(singleton(v.value0.value4)(v.value0.value5), new Data_List_Types.Cons(v.value0.value0, new Data_List_Types.Cons(v.value0.value3, new Data_List_Types.Cons(v.value0.value6, v.value1))))));
                    };
                    throw new Error("Failed pattern match at Data.Map.Internal (line 645, column 18 - line 650, column 77): " + [ v.value0.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 644, column 3 - line 644, column 19): " + [ v.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($copy_v);
            };
            return $tco_result;
        };
        return Data_Unfoldable.unfoldr(dictUnfoldable)(go)(new Data_List_Types.Cons(m, Data_List_Types.Nil.value));
    };
};

// | Render a `Map` as a `String`
var showTree = function (dictShow) {
    return function (dictShow1) {
        return function (v) {
            if (v instanceof Leaf) {
                return "Leaf";
            };
            if (v instanceof Two) {
                return "Two (" + (showTree(dictShow)(dictShow1)(v.value0) + (") (" + (Data_Show.show(dictShow)(v.value1) + (") (" + (Data_Show.show(dictShow1)(v.value2) + (") (" + (showTree(dictShow)(dictShow1)(v.value3) + ")")))))));
            };
            if (v instanceof Three) {
                return "Three (" + (showTree(dictShow)(dictShow1)(v.value0) + (") (" + (Data_Show.show(dictShow)(v.value1) + (") (" + (Data_Show.show(dictShow1)(v.value2) + (") (" + (showTree(dictShow)(dictShow1)(v.value3) + (") (" + (Data_Show.show(dictShow)(v.value4) + (") (" + (Data_Show.show(dictShow1)(v.value5) + (") (" + (showTree(dictShow)(dictShow1)(v.value6) + ")")))))))))))));
            };
            throw new Error("Failed pattern match at Data.Map.Internal (line 194, column 1 - line 194, column 62): " + [ v.constructor.name ]);
        };
    };
};
var showMap = function (dictShow) {
    return function (dictShow1) {
        return {
            show: function (m) {
                return "(fromFoldable " + (Data_Show.show(Data_Show.showArray(Data_Tuple.showTuple(dictShow)(dictShow1)))(toAscArray(m)) + ")");
            }
        };
    };
};

// | Look up a value for the specified key, or the greatest one less than it
var lookupLE = function (dictOrd) {
    return function (k) {
        var comp = Data_Ord.compare(dictOrd);
        var go = function (v) {
            if (v instanceof Leaf) {
                return Data_Maybe.Nothing.value;
            };
            if (v instanceof Two) {
                var v2 = comp(k)(v.value1);
                if (v2 instanceof Data_Ordering.EQ) {
                    return new Data_Maybe.Just({
                        key: v.value1,
                        value: v.value2
                    });
                };
                if (v2 instanceof Data_Ordering.GT) {
                    return new Data_Maybe.Just(Data_Maybe.fromMaybe({
                        key: v.value1,
                        value: v.value2
                    })(go(v.value3)));
                };
                if (v2 instanceof Data_Ordering.LT) {
                    return go(v.value0);
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 266, column 33 - line 269, column 20): " + [ v2.constructor.name ]);
            };
            if (v instanceof Three) {
                var v3 = comp(k)(v.value4);
                if (v3 instanceof Data_Ordering.EQ) {
                    return new Data_Maybe.Just({
                        key: v.value4,
                        value: v.value5
                    });
                };
                if (v3 instanceof Data_Ordering.GT) {
                    return new Data_Maybe.Just(Data_Maybe.fromMaybe({
                        key: v.value4,
                        value: v.value5
                    })(go(v.value6)));
                };
                if (v3 instanceof Data_Ordering.LT) {
                    return go(new Two(v.value0, v.value1, v.value2, v.value3));
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 270, column 45 - line 273, column 36): " + [ v3.constructor.name ]);
            };
            throw new Error("Failed pattern match at Data.Map.Internal (line 265, column 5 - line 265, column 22): " + [ v.constructor.name ]);
        };
        return go;
    };
};

// | Look up a value for the specified key, or the least one greater than it
var lookupGE = function (dictOrd) {
    return function (k) {
        var comp = Data_Ord.compare(dictOrd);
        var go = function (v) {
            if (v instanceof Leaf) {
                return Data_Maybe.Nothing.value;
            };
            if (v instanceof Two) {
                var v2 = comp(k)(v.value1);
                if (v2 instanceof Data_Ordering.EQ) {
                    return new Data_Maybe.Just({
                        key: v.value1,
                        value: v.value2
                    });
                };
                if (v2 instanceof Data_Ordering.LT) {
                    return new Data_Maybe.Just(Data_Maybe.fromMaybe({
                        key: v.value1,
                        value: v.value2
                    })(go(v.value0)));
                };
                if (v2 instanceof Data_Ordering.GT) {
                    return go(v.value3);
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 300, column 33 - line 303, column 21): " + [ v2.constructor.name ]);
            };
            if (v instanceof Three) {
                var v3 = comp(k)(v.value1);
                if (v3 instanceof Data_Ordering.EQ) {
                    return new Data_Maybe.Just({
                        key: v.value1,
                        value: v.value2
                    });
                };
                if (v3 instanceof Data_Ordering.LT) {
                    return new Data_Maybe.Just(Data_Maybe.fromMaybe({
                        key: v.value1,
                        value: v.value2
                    })(go(v.value0)));
                };
                if (v3 instanceof Data_Ordering.GT) {
                    return go(new Two(v.value3, v.value4, v.value5, v.value6));
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 304, column 45 - line 307, column 37): " + [ v3.constructor.name ]);
            };
            throw new Error("Failed pattern match at Data.Map.Internal (line 299, column 5 - line 299, column 22): " + [ v.constructor.name ]);
        };
        return go;
    };
};

// | Look up a value for the specified key
var lookup = function (dictOrd) {
    return function (k) {
        var comp = Data_Ord.compare(dictOrd);
        var go = function ($copy_v) {
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v) {
                if (v instanceof Leaf) {
                    $tco_done = true;
                    return Data_Maybe.Nothing.value;
                };
                if (v instanceof Two) {
                    var v2 = comp(k)(v.value1);
                    if (v2 instanceof Data_Ordering.EQ) {
                        $tco_done = true;
                        return new Data_Maybe.Just(v.value2);
                    };
                    if (v2 instanceof Data_Ordering.LT) {
                        $copy_v = v.value0;
                        return;
                    };
                    $copy_v = v.value3;
                    return;
                };
                if (v instanceof Three) {
                    var v3 = comp(k)(v.value1);
                    if (v3 instanceof Data_Ordering.EQ) {
                        $tco_done = true;
                        return new Data_Maybe.Just(v.value2);
                    };
                    var v4 = comp(k)(v.value4);
                    if (v4 instanceof Data_Ordering.EQ) {
                        $tco_done = true;
                        return new Data_Maybe.Just(v.value5);
                    };
                    if (v3 instanceof Data_Ordering.LT) {
                        $copy_v = v.value0;
                        return;
                    };
                    if (v4 instanceof Data_Ordering.GT) {
                        $copy_v = v.value6;
                        return;
                    };
                    $copy_v = v.value3;
                    return;
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): " + [ v.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($copy_v);
            };
            return $tco_result;
        };
        return go;
    };
};

// | Test if a key is a member of a map
var member = function (dictOrd) {
    return function (k) {
        return function (m) {
            return Data_Maybe.isJust(lookup(dictOrd)(k)(m));
        };
    };
};

// | Test whether one map contains all of the keys and values contained in another map
var isSubmap = function (dictOrd) {
    return function (dictEq) {
        return function (m1) {
            return function (m2) {
                var f = function (v) {
                    return Data_Eq.eq(Data_Maybe.eqMaybe(dictEq))(lookup(dictOrd)(v.value0)(m2))(new Data_Maybe.Just(v.value1));
                };
                return Data_Foldable.all(Data_List_Lazy_Types.foldableList)(Data_HeytingAlgebra.heytingAlgebraBoolean)(f)(toUnfoldable(Data_List_Lazy_Types.unfoldableList)(m1));
            };
        };
    };
};

// | Test if a map is empty
var isEmpty = function (v) {
    if (v instanceof Leaf) {
        return true;
    };
    return false;
};
var functorMap = {
    map: function (v) {
        return function (v1) {
            if (v1 instanceof Leaf) {
                return Leaf.value;
            };
            if (v1 instanceof Two) {
                return new Two(Data_Functor.map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), Data_Functor.map(functorMap)(v)(v1.value3));
            };
            if (v1 instanceof Three) {
                return new Three(Data_Functor.map(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), Data_Functor.map(functorMap)(v)(v1.value3), v1.value4, v(v1.value5), Data_Functor.map(functorMap)(v)(v1.value6));
            };
            throw new Error("Failed pattern match at Data.Map.Internal (line 116, column 1 - line 119, column 110): " + [ v.constructor.name, v1.constructor.name ]);
        };
    }
};
var functorWithIndexMap = {
    mapWithIndex: function (v) {
        return function (v1) {
            if (v1 instanceof Leaf) {
                return Leaf.value;
            };
            if (v1 instanceof Two) {
                return new Two(Data_FunctorWithIndex.mapWithIndex(functorWithIndexMap)(v)(v1.value0), v1.value1, v(v1.value1)(v1.value2), Data_FunctorWithIndex.mapWithIndex(functorWithIndexMap)(v)(v1.value3));
            };
            if (v1 instanceof Three) {
                return new Three(Data_FunctorWithIndex.mapWithIndex(functorWithIndexMap)(v)(v1.value0), v1.value1, v(v1.value1)(v1.value2), Data_FunctorWithIndex.mapWithIndex(functorWithIndexMap)(v)(v1.value3), v1.value4, v(v1.value4)(v1.value5), Data_FunctorWithIndex.mapWithIndex(functorWithIndexMap)(v)(v1.value6));
            };
            throw new Error("Failed pattern match at Data.Map.Internal (line 121, column 1 - line 124, column 152): " + [ v.constructor.name, v1.constructor.name ]);
        };
    },
    Functor0: function () {
        return functorMap;
    }
};
var fromZipper = function ($copy_dictOrd) {
    return function ($copy_v) {
        return function ($copy_tree) {
            var $tco_var_dictOrd = $copy_dictOrd;
            var $tco_var_v = $copy_v;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(dictOrd, v, tree) {
                if (v instanceof Data_List_Types.Nil) {
                    $tco_done = true;
                    return tree;
                };
                if (v instanceof Data_List_Types.Cons) {
                    if (v.value0 instanceof TwoLeft) {
                        $tco_var_dictOrd = dictOrd;
                        $tco_var_v = v.value1;
                        $copy_tree = new Two(tree, v.value0.value0, v.value0.value1, v.value0.value2);
                        return;
                    };
                    if (v.value0 instanceof TwoRight) {
                        $tco_var_dictOrd = dictOrd;
                        $tco_var_v = v.value1;
                        $copy_tree = new Two(v.value0.value0, v.value0.value1, v.value0.value2, tree);
                        return;
                    };
                    if (v.value0 instanceof ThreeLeft) {
                        $tco_var_dictOrd = dictOrd;
                        $tco_var_v = v.value1;
                        $copy_tree = new Three(tree, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
                        return;
                    };
                    if (v.value0 instanceof ThreeMiddle) {
                        $tco_var_dictOrd = dictOrd;
                        $tco_var_v = v.value1;
                        $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, tree, v.value0.value3, v.value0.value4, v.value0.value5);
                        return;
                    };
                    if (v.value0 instanceof ThreeRight) {
                        $tco_var_dictOrd = dictOrd;
                        $tco_var_v = v.value1;
                        $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, tree);
                        return;
                    };
                    throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): " + [ v.value0.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): " + [ v.constructor.name, tree.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_tree);
            };
            return $tco_result;
        };
    };
};

// | Insert or replace a key/value pair in a map
var insert = function (dictOrd) {
    return function (k) {
        return function (v) {
            var up = function ($copy_v1) {
                return function ($copy_v2) {
                    var $tco_var_v1 = $copy_v1;
                    var $tco_done = false;
                    var $tco_result;
                    function $tco_loop(v1, v2) {
                        if (v1 instanceof Data_List_Types.Nil) {
                            $tco_done = true;
                            return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
                        };
                        if (v1 instanceof Data_List_Types.Cons) {
                            if (v1.value0 instanceof TwoLeft) {
                                $tco_done = true;
                                return fromZipper(dictOrd)(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                            };
                            if (v1.value0 instanceof TwoRight) {
                                $tco_done = true;
                                return fromZipper(dictOrd)(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
                            };
                            if (v1.value0 instanceof ThreeLeft) {
                                $tco_var_v1 = v1.value1;
                                $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                                return;
                            };
                            if (v1.value0 instanceof ThreeMiddle) {
                                $tco_var_v1 = v1.value1;
                                $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                                return;
                            };
                            if (v1.value0 instanceof ThreeRight) {
                                $tco_var_v1 = v1.value1;
                                $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                                return;
                            };
                            throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): " + [ v1.value0.constructor.name, v2.constructor.name ]);
                        };
                        throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): " + [ v1.constructor.name, v2.constructor.name ]);
                    };
                    while (!$tco_done) {
                        $tco_result = $tco_loop($tco_var_v1, $copy_v2);
                    };
                    return $tco_result;
                };
            };
            var comp = Data_Ord.compare(dictOrd);
            var down = function ($copy_ctx) {
                return function ($copy_v1) {
                    var $tco_var_ctx = $copy_ctx;
                    var $tco_done1 = false;
                    var $tco_result;
                    function $tco_loop(ctx, v1) {
                        if (v1 instanceof Leaf) {
                            $tco_done1 = true;
                            return up(ctx)(new KickUp(Leaf.value, k, v, Leaf.value));
                        };
                        if (v1 instanceof Two) {
                            var v2 = comp(k)(v1.value1);
                            if (v2 instanceof Data_Ordering.EQ) {
                                $tco_done1 = true;
                                return fromZipper(dictOrd)(ctx)(new Two(v1.value0, k, v, v1.value3));
                            };
                            if (v2 instanceof Data_Ordering.LT) {
                                $tco_var_ctx = new Data_List_Types.Cons(new TwoLeft(v1.value1, v1.value2, v1.value3), ctx);
                                $copy_v1 = v1.value0;
                                return;
                            };
                            $tco_var_ctx = new Data_List_Types.Cons(new TwoRight(v1.value0, v1.value1, v1.value2), ctx);
                            $copy_v1 = v1.value3;
                            return;
                        };
                        if (v1 instanceof Three) {
                            var v3 = comp(k)(v1.value1);
                            if (v3 instanceof Data_Ordering.EQ) {
                                $tco_done1 = true;
                                return fromZipper(dictOrd)(ctx)(new Three(v1.value0, k, v, v1.value3, v1.value4, v1.value5, v1.value6));
                            };
                            var v4 = comp(k)(v1.value4);
                            if (v4 instanceof Data_Ordering.EQ) {
                                $tco_done1 = true;
                                return fromZipper(dictOrd)(ctx)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, k, v, v1.value6));
                            };
                            if (v3 instanceof Data_Ordering.LT) {
                                $tco_var_ctx = new Data_List_Types.Cons(new ThreeLeft(v1.value1, v1.value2, v1.value3, v1.value4, v1.value5, v1.value6), ctx);
                                $copy_v1 = v1.value0;
                                return;
                            };
                            if (v3 instanceof Data_Ordering.GT && v4 instanceof Data_Ordering.LT) {
                                $tco_var_ctx = new Data_List_Types.Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, v1.value4, v1.value5, v1.value6), ctx);
                                $copy_v1 = v1.value3;
                                return;
                            };
                            $tco_var_ctx = new Data_List_Types.Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
                            $copy_v1 = v1.value6;
                            return;
                        };
                        throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): " + [ ctx.constructor.name, v1.constructor.name ]);
                    };
                    while (!$tco_done1) {
                        $tco_result = $tco_loop($tco_var_ctx, $copy_v1);
                    };
                    return $tco_result;
                };
            };
            return down(Data_List_Types.Nil.value);
        };
    };
};

// | Delete a key and its corresponding value from a map, returning the value
// | as well as the subsequent map.
var pop = function (dictOrd) {
    return function (k) {
        var up = function ($copy_ctxs) {
            return function ($copy_tree) {
                var $tco_var_ctxs = $copy_ctxs;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(ctxs, tree) {
                    if (ctxs instanceof Data_List_Types.Nil) {
                        $tco_done = true;
                        return tree;
                    };
                    if (ctxs instanceof Data_List_Types.Cons) {
                        if (ctxs.value0 instanceof TwoLeft && (ctxs.value0.value2 instanceof Leaf && tree instanceof Leaf)) {
                            $tco_done = true;
                            return fromZipper(dictOrd)(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
                        };
                        if (ctxs.value0 instanceof TwoRight && (ctxs.value0.value0 instanceof Leaf && tree instanceof Leaf)) {
                            $tco_done = true;
                            return fromZipper(dictOrd)(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
                        };
                        if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Two) {
                            $tco_var_ctxs = ctxs.value1;
                            $copy_tree = new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3);
                            return;
                        };
                        if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Two) {
                            $tco_var_ctxs = ctxs.value1;
                            $copy_tree = new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree);
                            return;
                        };
                        if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Three) {
                            $tco_done = true;
                            return fromZipper(dictOrd)(ctxs.value1)(new Two(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
                        };
                        if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                            $tco_done = true;
                            return fromZipper(dictOrd)(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree)));
                        };
                        if (ctxs.value0 instanceof ThreeLeft && (ctxs.value0.value2 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                            $tco_done = true;
                            return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
                        };
                        if (ctxs.value0 instanceof ThreeMiddle && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                            $tco_done = true;
                            return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
                        };
                        if (ctxs.value0 instanceof ThreeRight && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value3 instanceof Leaf && tree instanceof Leaf))) {
                            $tco_done = true;
                            return fromZipper(dictOrd)(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
                        };
                        if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                            $tco_done = true;
                            return fromZipper(dictOrd)(ctxs.value1)(new Two(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                        };
                        if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                            $tco_done = true;
                            return fromZipper(dictOrd)(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                        };
                        if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                            $tco_done = true;
                            return fromZipper(dictOrd)(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
                        };
                        if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                            $tco_done = true;
                            return fromZipper(dictOrd)(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree)));
                        };
                        if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                            $tco_done = true;
                            return fromZipper(dictOrd)(ctxs.value1)(new Three(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                        };
                        if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                            $tco_done = true;
                            return fromZipper(dictOrd)(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
                        };
                        if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                            $tco_done = true;
                            return fromZipper(dictOrd)(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
                        };
                        if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                            $tco_done = true;
                            return fromZipper(dictOrd)(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree)));
                        };
                        $tco_done = true;
                        return Partial_Unsafe.unsafeCrashWith("The impossible happened in partial function `up`.");
                    };
                    throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): " + [ ctxs.constructor.name ]);
                };
                while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_ctxs, $copy_tree);
                };
                return $tco_result;
            };
        };
        var removeMaxNode = function ($copy_ctx) {
            return function ($copy_m) {
                var $tco_var_ctx = $copy_ctx;
                var $tco_done1 = false;
                var $tco_result;
                function $tco_loop(ctx, m) {
                    if (m instanceof Two && (m.value0 instanceof Leaf && m.value3 instanceof Leaf)) {
                        $tco_done1 = true;
                        return up(ctx)(Leaf.value);
                    };
                    if (m instanceof Two) {
                        $tco_var_ctx = new Data_List_Types.Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
                        $copy_m = m.value3;
                        return;
                    };
                    if (m instanceof Three && (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf))) {
                        $tco_done1 = true;
                        return up(new Data_List_Types.Cons(new TwoRight(Leaf.value, m.value1, m.value2), ctx))(Leaf.value);
                    };
                    if (m instanceof Three) {
                        $tco_var_ctx = new Data_List_Types.Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
                        $copy_m = m.value6;
                        return;
                    };
                    $tco_done1 = true;
                    return Partial_Unsafe.unsafeCrashWith("The impossible happened in partial function `removeMaxNode`.");
                };
                while (!$tco_done1) {
                    $tco_result = $tco_loop($tco_var_ctx, $copy_m);
                };
                return $tco_result;
            };
        };
        var maxNode = function ($copy_m) {
            var $tco_done2 = false;
            var $tco_result;
            function $tco_loop(m) {
                if (m instanceof Two && m.value3 instanceof Leaf) {
                    $tco_done2 = true;
                    return {
                        key: m.value1,
                        value: m.value2
                    };
                };
                if (m instanceof Two) {
                    $copy_m = m.value3;
                    return;
                };
                if (m instanceof Three && m.value6 instanceof Leaf) {
                    $tco_done2 = true;
                    return {
                        key: m.value4,
                        value: m.value5
                    };
                };
                if (m instanceof Three) {
                    $copy_m = m.value6;
                    return;
                };
                $tco_done2 = true;
                return Partial_Unsafe.unsafeCrashWith("The impossible happened in partial function `maxNode`.");
            };
            while (!$tco_done2) {
                $tco_result = $tco_loop($copy_m);
            };
            return $tco_result;
        };
        var comp = Data_Ord.compare(dictOrd);
        var down = function ($copy_ctx) {
            return function ($copy_m) {
                var $tco_var_ctx = $copy_ctx;
                var $tco_done3 = false;
                var $tco_result;
                function $tco_loop(ctx, m) {
                    if (m instanceof Leaf) {
                        $tco_done3 = true;
                        return Data_Maybe.Nothing.value;
                    };
                    if (m instanceof Two) {
                        var v = comp(k)(m.value1);
                        if (m.value3 instanceof Leaf && v instanceof Data_Ordering.EQ) {
                            $tco_done3 = true;
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, up(ctx)(Leaf.value)));
                        };
                        if (v instanceof Data_Ordering.EQ) {
                            var max = maxNode(m.value0);
                            $tco_done3 = true;
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, removeMaxNode(new Data_List_Types.Cons(new TwoLeft(max.key, max.value, m.value3), ctx))(m.value0)));
                        };
                        if (v instanceof Data_Ordering.LT) {
                            $tco_var_ctx = new Data_List_Types.Cons(new TwoLeft(m.value1, m.value2, m.value3), ctx);
                            $copy_m = m.value0;
                            return;
                        };
                        $tco_var_ctx = new Data_List_Types.Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
                        $copy_m = m.value3;
                        return;
                    };
                    if (m instanceof Three) {
                        var leaves = (function () {
                            if (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf)) {
                                return true;
                            };
                            return false;
                        })();
                        var v = comp(k)(m.value4);
                        var v3 = comp(k)(m.value1);
                        if (leaves && v3 instanceof Data_Ordering.EQ) {
                            $tco_done3 = true;
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, fromZipper(dictOrd)(ctx)(new Two(Leaf.value, m.value4, m.value5, Leaf.value))));
                        };
                        if (leaves && v instanceof Data_Ordering.EQ) {
                            $tco_done3 = true;
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value5, fromZipper(dictOrd)(ctx)(new Two(Leaf.value, m.value1, m.value2, Leaf.value))));
                        };
                        if (v3 instanceof Data_Ordering.EQ) {
                            var max = maxNode(m.value0);
                            $tco_done3 = true;
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value2, removeMaxNode(new Data_List_Types.Cons(new ThreeLeft(max.key, max.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
                        };
                        if (v instanceof Data_Ordering.EQ) {
                            var max = maxNode(m.value3);
                            $tco_done3 = true;
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(m.value5, removeMaxNode(new Data_List_Types.Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max.key, max.value, m.value6), ctx))(m.value3)));
                        };
                        if (v3 instanceof Data_Ordering.LT) {
                            $tco_var_ctx = new Data_List_Types.Cons(new ThreeLeft(m.value1, m.value2, m.value3, m.value4, m.value5, m.value6), ctx);
                            $copy_m = m.value0;
                            return;
                        };
                        if (v3 instanceof Data_Ordering.GT && v instanceof Data_Ordering.LT) {
                            $tco_var_ctx = new Data_List_Types.Cons(new ThreeMiddle(m.value0, m.value1, m.value2, m.value4, m.value5, m.value6), ctx);
                            $copy_m = m.value3;
                            return;
                        };
                        $tco_var_ctx = new Data_List_Types.Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
                        $copy_m = m.value6;
                        return;
                    };
                    throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): " + [ m.constructor.name ]);
                };
                while (!$tco_done3) {
                    $tco_result = $tco_loop($tco_var_ctx, $copy_m);
                };
                return $tco_result;
            };
        };
        return down(Data_List_Types.Nil.value);
    };
};
var foldableMap = {
    foldr: function (f) {
        return function (z) {
            return function (m) {
                if (m instanceof Leaf) {
                    return z;
                };
                if (m instanceof Two) {
                    return Data_Foldable.foldr(foldableMap)(f)(f(m.value2)(Data_Foldable.foldr(foldableMap)(f)(z)(m.value3)))(m.value0);
                };
                if (m instanceof Three) {
                    return Data_Foldable.foldr(foldableMap)(f)(f(m.value2)(Data_Foldable.foldr(foldableMap)(f)(f(m.value5)(Data_Foldable.foldr(foldableMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): " + [ m.constructor.name ]);
            };
        };
    },
    foldl: function (f) {
        return function (z) {
            return function (m) {
                if (m instanceof Leaf) {
                    return z;
                };
                if (m instanceof Two) {
                    return Data_Foldable.foldl(foldableMap)(f)(f(Data_Foldable.foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3);
                };
                if (m instanceof Three) {
                    return Data_Foldable.foldl(foldableMap)(f)(f(Data_Foldable.foldl(foldableMap)(f)(f(Data_Foldable.foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): " + [ m.constructor.name ]);
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (f) {
            return function (m) {
                if (m instanceof Leaf) {
                    return Data_Monoid.mempty(dictMonoid);
                };
                if (m instanceof Two) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(Data_Foldable.foldMap(foldableMap)(dictMonoid)(f)(m.value0))(Data_Semigroup.append(dictMonoid.Semigroup0())(f(m.value2))(Data_Foldable.foldMap(foldableMap)(dictMonoid)(f)(m.value3)));
                };
                if (m instanceof Three) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(Data_Foldable.foldMap(foldableMap)(dictMonoid)(f)(m.value0))(Data_Semigroup.append(dictMonoid.Semigroup0())(f(m.value2))(Data_Semigroup.append(dictMonoid.Semigroup0())(Data_Foldable.foldMap(foldableMap)(dictMonoid)(f)(m.value3))(Data_Semigroup.append(dictMonoid.Semigroup0())(f(m.value5))(Data_Foldable.foldMap(foldableMap)(dictMonoid)(f)(m.value6)))));
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): " + [ m.constructor.name ]);
            };
        };
    }
};
var foldableWithIndexMap = {
    foldrWithIndex: function (f) {
        return function (z) {
            return function (m) {
                if (m instanceof Leaf) {
                    return z;
                };
                if (m instanceof Two) {
                    return Data_FoldableWithIndex.foldrWithIndex(foldableWithIndexMap)(f)(f(m.value1)(m.value2)(Data_FoldableWithIndex.foldrWithIndex(foldableWithIndexMap)(f)(z)(m.value3)))(m.value0);
                };
                if (m instanceof Three) {
                    return Data_FoldableWithIndex.foldrWithIndex(foldableWithIndexMap)(f)(f(m.value1)(m.value2)(Data_FoldableWithIndex.foldrWithIndex(foldableWithIndexMap)(f)(f(m.value4)(m.value5)(Data_FoldableWithIndex.foldrWithIndex(foldableWithIndexMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): " + [ m.constructor.name ]);
            };
        };
    },
    foldlWithIndex: function (f) {
        return function (z) {
            return function (m) {
                if (m instanceof Leaf) {
                    return z;
                };
                if (m instanceof Two) {
                    return Data_FoldableWithIndex.foldlWithIndex(foldableWithIndexMap)(f)(f(m.value1)(Data_FoldableWithIndex.foldlWithIndex(foldableWithIndexMap)(f)(z)(m.value0))(m.value2))(m.value3);
                };
                if (m instanceof Three) {
                    return Data_FoldableWithIndex.foldlWithIndex(foldableWithIndexMap)(f)(f(m.value4)(Data_FoldableWithIndex.foldlWithIndex(foldableWithIndexMap)(f)(f(m.value1)(Data_FoldableWithIndex.foldlWithIndex(foldableWithIndexMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): " + [ m.constructor.name ]);
            };
        };
    },
    foldMapWithIndex: function (dictMonoid) {
        return function (f) {
            return function (m) {
                if (m instanceof Leaf) {
                    return Data_Monoid.mempty(dictMonoid);
                };
                if (m instanceof Two) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(Data_FoldableWithIndex.foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value0))(Data_Semigroup.append(dictMonoid.Semigroup0())(f(m.value1)(m.value2))(Data_FoldableWithIndex.foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value3)));
                };
                if (m instanceof Three) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(Data_FoldableWithIndex.foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value0))(Data_Semigroup.append(dictMonoid.Semigroup0())(f(m.value1)(m.value2))(Data_Semigroup.append(dictMonoid.Semigroup0())(Data_FoldableWithIndex.foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value3))(Data_Semigroup.append(dictMonoid.Semigroup0())(f(m.value4)(m.value5))(Data_FoldableWithIndex.foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value6)))));
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): " + [ m.constructor.name ]);
            };
        };
    },
    Foldable0: function () {
        return foldableMap;
    }
};

// | Get a list of the keys contained in a map
var keys = /* #__PURE__ */ (function () {
    return Data_FoldableWithIndex.foldrWithIndex(foldableWithIndexMap)(function (k) {
        return function (v) {
            return function (acc) {
                return new Data_List_Types.Cons(k, acc);
            };
        };
    })(Data_List_Types.Nil.value);
})();
var traversableMap = {
    traverse: function (dictApplicative) {
        return function (v) {
            return function (v1) {
                if (v1 instanceof Leaf) {
                    return Control_Applicative.pure(dictApplicative)(Leaf.value);
                };
                if (v1 instanceof Two) {
                    return Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Two.create)(Data_Traversable.traverse(traversableMap)(dictApplicative)(v)(v1.value0)))(Control_Applicative.pure(dictApplicative)(v1.value1)))(v(v1.value2)))(Data_Traversable.traverse(traversableMap)(dictApplicative)(v)(v1.value3));
                };
                if (v1 instanceof Three) {
                    return Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Three.create)(Data_Traversable.traverse(traversableMap)(dictApplicative)(v)(v1.value0)))(Control_Applicative.pure(dictApplicative)(v1.value1)))(v(v1.value2)))(Data_Traversable.traverse(traversableMap)(dictApplicative)(v)(v1.value3)))(Control_Applicative.pure(dictApplicative)(v1.value4)))(v(v1.value5)))(Data_Traversable.traverse(traversableMap)(dictApplicative)(v)(v1.value6));
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 160, column 1 - line 175, column 31): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    },
    sequence: function (dictApplicative) {
        return Data_Traversable.traverse(traversableMap)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
    },
    Functor0: function () {
        return functorMap;
    },
    Foldable1: function () {
        return foldableMap;
    }
};
var traversableWithIndexMap = {
    traverseWithIndex: function (dictApplicative) {
        return function (v) {
            return function (v1) {
                if (v1 instanceof Leaf) {
                    return Control_Applicative.pure(dictApplicative)(Leaf.value);
                };
                if (v1 instanceof Two) {
                    return Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Two.create)(Data_TraversableWithIndex.traverseWithIndex(traversableWithIndexMap)(dictApplicative)(v)(v1.value0)))(Control_Applicative.pure(dictApplicative)(v1.value1)))(v(v1.value1)(v1.value2)))(Data_TraversableWithIndex.traverseWithIndex(traversableWithIndexMap)(dictApplicative)(v)(v1.value3));
                };
                if (v1 instanceof Three) {
                    return Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Three.create)(Data_TraversableWithIndex.traverseWithIndex(traversableWithIndexMap)(dictApplicative)(v)(v1.value0)))(Control_Applicative.pure(dictApplicative)(v1.value1)))(v(v1.value1)(v1.value2)))(Data_TraversableWithIndex.traverseWithIndex(traversableWithIndexMap)(dictApplicative)(v)(v1.value3)))(Control_Applicative.pure(dictApplicative)(v1.value4)))(v(v1.value4)(v1.value5)))(Data_TraversableWithIndex.traverseWithIndex(traversableWithIndexMap)(dictApplicative)(v)(v1.value6));
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 177, column 1 - line 191, column 40): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    },
    FunctorWithIndex0: function () {
        return functorWithIndexMap;
    },
    FoldableWithIndex1: function () {
        return foldableWithIndexMap;
    },
    Traversable2: function () {
        return traversableMap;
    }
};

// | Get a list of the values contained in a map
var values = /* #__PURE__ */ (function () {
    return Data_Foldable.foldr(foldableMap)(Data_List_Types.Cons.create)(Data_List_Types.Nil.value);
})();
var foldSubmapBy = function (dictOrd) {
    return function (appendFn) {
        return function (memptyValue) {
            return function (kmin) {
                return function (kmax) {
                    return function (f) {
                        var tooSmall = (function () {
                            if (kmin instanceof Data_Maybe.Just) {
                                return function (k) {
                                    return Data_Ord.lessThan(dictOrd)(k)(kmin.value0);
                                };
                            };
                            if (kmin instanceof Data_Maybe.Nothing) {
                                return Data_Function["const"](false);
                            };
                            throw new Error("Failed pattern match at Data.Map.Internal (line 363, column 7 - line 367, column 22): " + [ kmin.constructor.name ]);
                        })();
                        var tooLarge = (function () {
                            if (kmax instanceof Data_Maybe.Just) {
                                return function (k) {
                                    return Data_Ord.greaterThan(dictOrd)(k)(kmax.value0);
                                };
                            };
                            if (kmax instanceof Data_Maybe.Nothing) {
                                return Data_Function["const"](false);
                            };
                            throw new Error("Failed pattern match at Data.Map.Internal (line 370, column 7 - line 374, column 22): " + [ kmax.constructor.name ]);
                        })();
                        var inBounds = (function () {
                            if (kmin instanceof Data_Maybe.Just && kmax instanceof Data_Maybe.Just) {
                                return function (k) {
                                    return Data_Ord.lessThanOrEq(dictOrd)(kmin.value0)(k) && Data_Ord.lessThanOrEq(dictOrd)(k)(kmax.value0);
                                };
                            };
                            if (kmin instanceof Data_Maybe.Just && kmax instanceof Data_Maybe.Nothing) {
                                return function (k) {
                                    return Data_Ord.lessThanOrEq(dictOrd)(kmin.value0)(k);
                                };
                            };
                            if (kmin instanceof Data_Maybe.Nothing && kmax instanceof Data_Maybe.Just) {
                                return function (k) {
                                    return Data_Ord.lessThanOrEq(dictOrd)(k)(kmax.value0);
                                };
                            };
                            if (kmin instanceof Data_Maybe.Nothing && kmax instanceof Data_Maybe.Nothing) {
                                return Data_Function["const"](true);
                            };
                            throw new Error("Failed pattern match at Data.Map.Internal (line 377, column 7 - line 385, column 21): " + [ kmin.constructor.name, kmax.constructor.name ]);
                        })();
                        
                        // We can take advantage of the invariants of the tree structure to reduce
                        // the amount of work we need to do. For example, in the following tree:
                        //
                        //      [2][4]
                        //      / |  \
                        //     /  |   \
                        //   [1] [3] [5]
                        //
                        // If we are given a lower bound of 3, we do not need to inspect the left
                        // subtree, because we know that every entry in it is less than or equal to
                        // 2. Similarly, if we are given a lower bound of 5, we do not need to
                        // inspect the central subtree, because we know that every entry in it must
                        // be less than or equal to 4.
                        //
                        // Unfortunately we cannot extract `if cond then x else mempty` into a
                        // function because of strictness.
var go = function (v) {
                            if (v instanceof Leaf) {
                                return memptyValue;
                            };
                            if (v instanceof Two) {
                                return appendFn(appendFn((function () {
                                    var $689 = tooSmall(v.value1);
                                    if ($689) {
                                        return memptyValue;
                                    };
                                    return go(v.value0);
                                })())((function () {
                                    var $690 = inBounds(v.value1);
                                    if ($690) {
                                        return f(v.value1)(v.value2);
                                    };
                                    return memptyValue;
                                })()))((function () {
                                    var $691 = tooLarge(v.value1);
                                    if ($691) {
                                        return memptyValue;
                                    };
                                    return go(v.value3);
                                })());
                            };
                            if (v instanceof Three) {
                                return appendFn(appendFn(appendFn(appendFn((function () {
                                    var $696 = tooSmall(v.value1);
                                    if ($696) {
                                        return memptyValue;
                                    };
                                    return go(v.value0);
                                })())((function () {
                                    var $697 = inBounds(v.value1);
                                    if ($697) {
                                        return f(v.value1)(v.value2);
                                    };
                                    return memptyValue;
                                })()))((function () {
                                    var $698 = tooSmall(v.value4) || tooLarge(v.value1);
                                    if ($698) {
                                        return memptyValue;
                                    };
                                    return go(v.value3);
                                })()))((function () {
                                    var $699 = inBounds(v.value4);
                                    if ($699) {
                                        return f(v.value4)(v.value5);
                                    };
                                    return memptyValue;
                                })()))((function () {
                                    var $700 = tooLarge(v.value4);
                                    if ($700) {
                                        return memptyValue;
                                    };
                                    return go(v.value6);
                                })());
                            };
                            throw new Error("Failed pattern match at Data.Map.Internal (line 403, column 10 - line 415, column 67): " + [ v.constructor.name ]);
                        };
                        return go;
                    };
                };
            };
        };
    };
};

// | Fold over the entries of a given map where the key is between a lower and
// | an upper bound. Passing `Nothing` as either the lower or upper bound
// | argument means that the fold has no lower or upper bound, i.e. the fold
// | starts from (or ends with) the smallest (or largest) key in the map.
// |
// | ```purescript
// | foldSubmap (Just 1) (Just 2) (\_ v -> [v])
// |  (fromFoldable [Tuple 0 "zero", Tuple 1 "one", Tuple 2 "two", Tuple 3 "three"])
// |  == ["one", "two"]
// |
// | foldSubmap Nothing (Just 2) (\_ v -> [v])
// |  (fromFoldable [Tuple 0 "zero", Tuple 1 "one", Tuple 2 "two", Tuple 3 "three"])
// |  == ["zero", "one", "two"]
// | ```
var foldSubmap = function (dictOrd) {
    return function (dictMonoid) {
        return foldSubmapBy(dictOrd)(Data_Semigroup.append(dictMonoid.Semigroup0()))(Data_Monoid.mempty(dictMonoid));
    };
};

// | Returns the pair with the least key
var findMin = /* #__PURE__ */ (function () {
    var go = function ($copy_v) {
        return function ($copy_v1) {
            var $tco_var_v = $copy_v;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v, v1) {
                if (v1 instanceof Leaf) {
                    $tco_done = true;
                    return v;
                };
                if (v1 instanceof Two) {
                    $tco_var_v = new Data_Maybe.Just({
                        key: v1.value1,
                        value: v1.value2
                    });
                    $copy_v1 = v1.value0;
                    return;
                };
                if (v1 instanceof Three) {
                    $tco_var_v = new Data_Maybe.Just({
                        key: v1.value1,
                        value: v1.value2
                    });
                    $copy_v1 = v1.value0;
                    return;
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 338, column 5 - line 338, column 22): " + [ v.constructor.name, v1.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
            };
            return $tco_result;
        };
    };
    return go(Data_Maybe.Nothing.value);
})();

// | Look up a value for the least key greater than the specified key
var lookupGT = function (dictOrd) {
    return function (k) {
        var comp = Data_Ord.compare(dictOrd);
        var go = function (v) {
            if (v instanceof Leaf) {
                return Data_Maybe.Nothing.value;
            };
            if (v instanceof Two) {
                var v2 = comp(k)(v.value1);
                if (v2 instanceof Data_Ordering.EQ) {
                    return findMin(v.value3);
                };
                if (v2 instanceof Data_Ordering.LT) {
                    return new Data_Maybe.Just(Data_Maybe.fromMaybe({
                        key: v.value1,
                        value: v.value2
                    })(go(v.value0)));
                };
                if (v2 instanceof Data_Ordering.GT) {
                    return go(v.value3);
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 317, column 33 - line 320, column 21): " + [ v2.constructor.name ]);
            };
            if (v instanceof Three) {
                var v3 = comp(k)(v.value1);
                if (v3 instanceof Data_Ordering.EQ) {
                    return findMin(new Two(v.value3, v.value4, v.value5, v.value6));
                };
                if (v3 instanceof Data_Ordering.LT) {
                    return new Data_Maybe.Just(Data_Maybe.fromMaybe({
                        key: v.value1,
                        value: v.value2
                    })(go(v.value0)));
                };
                if (v3 instanceof Data_Ordering.GT) {
                    return go(new Two(v.value3, v.value4, v.value5, v.value6));
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 321, column 45 - line 324, column 37): " + [ v3.constructor.name ]);
            };
            throw new Error("Failed pattern match at Data.Map.Internal (line 316, column 5 - line 316, column 22): " + [ v.constructor.name ]);
        };
        return go;
    };
};

// | Returns the pair with the greatest key
var findMax = /* #__PURE__ */ (function () {
    var go = function ($copy_v) {
        return function ($copy_v1) {
            var $tco_var_v = $copy_v;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v, v1) {
                if (v1 instanceof Leaf) {
                    $tco_done = true;
                    return v;
                };
                if (v1 instanceof Two) {
                    $tco_var_v = new Data_Maybe.Just({
                        key: v1.value1,
                        value: v1.value2
                    });
                    $copy_v1 = v1.value3;
                    return;
                };
                if (v1 instanceof Three) {
                    $tco_var_v = new Data_Maybe.Just({
                        key: v1.value4,
                        value: v1.value5
                    });
                    $copy_v1 = v1.value6;
                    return;
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 330, column 5 - line 330, column 22): " + [ v.constructor.name, v1.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
            };
            return $tco_result;
        };
    };
    return go(Data_Maybe.Nothing.value);
})();

// | Look up a value for the greatest key less than the specified key
var lookupLT = function (dictOrd) {
    return function (k) {
        var comp = Data_Ord.compare(dictOrd);
        var go = function (v) {
            if (v instanceof Leaf) {
                return Data_Maybe.Nothing.value;
            };
            if (v instanceof Two) {
                var v2 = comp(k)(v.value1);
                if (v2 instanceof Data_Ordering.EQ) {
                    return findMax(v.value0);
                };
                if (v2 instanceof Data_Ordering.GT) {
                    return new Data_Maybe.Just(Data_Maybe.fromMaybe({
                        key: v.value1,
                        value: v.value2
                    })(go(v.value3)));
                };
                if (v2 instanceof Data_Ordering.LT) {
                    return go(v.value0);
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 283, column 33 - line 286, column 20): " + [ v2.constructor.name ]);
            };
            if (v instanceof Three) {
                var v3 = comp(k)(v.value4);
                if (v3 instanceof Data_Ordering.EQ) {
                    return findMax(new Two(v.value0, v.value1, v.value2, v.value3));
                };
                if (v3 instanceof Data_Ordering.GT) {
                    return new Data_Maybe.Just(Data_Maybe.fromMaybe({
                        key: v.value4,
                        value: v.value5
                    })(go(v.value6)));
                };
                if (v3 instanceof Data_Ordering.LT) {
                    return go(new Two(v.value0, v.value1, v.value2, v.value3));
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 287, column 45 - line 290, column 36): " + [ v3.constructor.name ]);
            };
            throw new Error("Failed pattern match at Data.Map.Internal (line 282, column 5 - line 282, column 22): " + [ v.constructor.name ]);
        };
        return go;
    };
};
var eqMap = function (dictEq) {
    return function (dictEq1) {
        return {
            eq: function (m1) {
                return function (m2) {
                    return Data_Eq.eq(Data_Eq.eqArray(Data_Tuple.eqTuple(dictEq)(dictEq1)))(toAscArray(m1))(toAscArray(m2));
                };
            }
        };
    };
};
var ordMap = function (dictOrd) {
    return function (dictOrd1) {
        return {
            compare: function (m1) {
                return function (m2) {
                    return Data_Ord.compare(Data_Ord.ordArray(Data_Tuple.ordTuple(dictOrd)(dictOrd1)))(toAscArray(m1))(toAscArray(m2));
                };
            },
            Eq0: function () {
                return eqMap(dictOrd.Eq0())(dictOrd1.Eq0());
            }
        };
    };
};
var eq1Map = function (dictEq) {
    return {
        eq1: function (dictEq1) {
            return Data_Eq.eq(eqMap(dictEq)(dictEq1));
        }
    };
};
var ord1Map = function (dictOrd) {
    return {
        compare1: function (dictOrd1) {
            return Data_Ord.compare(ordMap(dictOrd)(dictOrd1));
        },
        Eq10: function () {
            return eq1Map(dictOrd.Eq0());
        }
    };
};

// | An empty map
var empty = /* #__PURE__ */ (function () {
    return Leaf.value;
})();

// | Convert any foldable collection of key/value pairs to a map.
// | On key collision, later values take precedence over earlier ones.
var fromFoldable = function (dictOrd) {
    return function (dictFoldable) {
        return Data_Foldable.foldl(dictFoldable)(function (m) {
            return function (v) {
                return insert(dictOrd)(v.value0)(v.value1)(m);
            };
        })(empty);
    };
};

// | Filter out those key/value pairs of a map for which a predicate
// | fails to hold.
var filterWithKey = function (dictOrd) {
    return function (predicate) {
        var $797 = fromFoldable(dictOrd)(Data_List_Lazy_Types.foldableList);
        var $798 = Data_List_Lazy.filter(Data_Tuple.uncurry(predicate));
        var $799 = toUnfoldable(Data_List_Lazy_Types.unfoldableList);
        return function ($800) {
            return $797($798($799($800)));
        };
    };
};

// | Filter out those key/value pairs of a map for which a predicate
// | on the value fails to hold.
var filter = function (dictOrd) {
    return function (predicate) {
        return filterWithKey(dictOrd)(Data_Function["const"](predicate));
    };
};

// | Filter out those key/value pairs of a map for which a predicate
// | on the key fails to hold.
var filterKeys = function (dictOrd) {
    return function (predicate) {
        return filterWithKey(dictOrd)(function ($801) {
            return Data_Function["const"](predicate($801));
        });
    };
};

// | Convert any indexed foldable collection into a map.
var fromFoldableWithIndex = function (dictOrd) {
    return function (dictFoldableWithIndex) {
        return Data_FoldableWithIndex.foldlWithIndex(dictFoldableWithIndex)(function (k) {
            return function (m) {
                return function (v) {
                    return insert(dictOrd)(k)(v)(m);
                };
            };
        })(empty);
    };
};

// | Compute the intersection of two maps, using the specified function
// | to combine values for duplicate keys.
var intersectionWith = function (dictOrd) {
    return function (f) {
        return function (m1) {
            return function (m2) {
                var go = function ($copy_v) {
                    return function ($copy_v1) {
                        return function ($copy_m) {
                            var $tco_var_v = $copy_v;
                            var $tco_var_v1 = $copy_v1;
                            var $tco_done = false;
                            var $tco_result;
                            function $tco_loop(v, v1, m) {
                                if (v instanceof Data_List_Types.Nil) {
                                    $tco_done = true;
                                    return m;
                                };
                                if (v1 instanceof Data_List_Types.Nil) {
                                    $tco_done = true;
                                    return m;
                                };
                                if (v instanceof Data_List_Types.Cons && v1 instanceof Data_List_Types.Cons) {
                                    var v2 = Data_Ord.compare(dictOrd)(v.value0.value0)(v1.value0.value0);
                                    if (v2 instanceof Data_Ordering.LT) {
                                        $tco_var_v = v.value1;
                                        $tco_var_v1 = v1;
                                        $copy_m = m;
                                        return;
                                    };
                                    if (v2 instanceof Data_Ordering.EQ) {
                                        $tco_var_v = v.value1;
                                        $tco_var_v1 = v1.value1;
                                        $copy_m = insert(dictOrd)(v.value0.value0)(f(v.value0.value1)(v1.value0.value1))(m);
                                        return;
                                    };
                                    if (v2 instanceof Data_Ordering.GT) {
                                        $tco_var_v = v;
                                        $tco_var_v1 = v1.value1;
                                        $copy_m = m;
                                        return;
                                    };
                                    throw new Error("Failed pattern match at Data.Map.Internal (line 684, column 5 - line 687, column 27): " + [ v2.constructor.name ]);
                                };
                                throw new Error("Failed pattern match at Data.Map.Internal (line 681, column 3 - line 681, column 17): " + [ v.constructor.name, v1.constructor.name, m.constructor.name ]);
                            };
                            while (!$tco_done) {
                                $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_m);
                            };
                            return $tco_result;
                        };
                    };
                };
                return go(toUnfoldable(Data_List_Types.unfoldableList)(m1))(toUnfoldable(Data_List_Types.unfoldableList)(m2))(empty);
            };
        };
    };
};

// | Compute the intersection of two maps, preferring values from the first map in the case
// | of duplicate keys.
var intersection = function (dictOrd) {
    return intersectionWith(dictOrd)(Data_Function["const"]);
};

// | Applies a function to each key/value pair in a map, discarding entries
// | where the function returns `Nothing`.
var mapMaybeWithKey = function (dictOrd) {
    return function (f) {
        return Data_FoldableWithIndex.foldrWithIndex(foldableWithIndexMap)(function (k) {
            return function (a) {
                return function (acc) {
                    return Data_Maybe.maybe(acc)(function (b) {
                        return insert(dictOrd)(k)(b)(acc);
                    })(f(k)(a));
                };
            };
        })(empty);
    };
};

// | Applies a function to each value in a map, discarding entries where the
// | function returns `Nothing`.
var mapMaybe = function (dictOrd) {
    var $802 = mapMaybeWithKey(dictOrd);
    return function ($803) {
        return $802(Data_Function["const"]($803));
    };
};

// | Delete a key and its corresponding value from a map.
var $$delete = function (dictOrd) {
    return function (k) {
        return function (m) {
            return Data_Maybe.maybe(m)(Data_Tuple.snd)(pop(dictOrd)(k)(m));
        };
    };
};

// | Difference of two maps. Return elements of the first map where
// | the keys do not exist in the second map.
var difference = function (dictOrd) {
    return function (m1) {
        return function (m2) {
            return Data_Foldable.foldl(Data_List_Types.foldableList)(Data_Function.flip($$delete(dictOrd)))(m1)(keys(m2));
        };
    };
};

// | Check whether the underlying tree satisfies the 2-3 invariant
// |
// | This function is provided for internal use.
var checkValid = function (tree) {
    var allHeights = function (v) {
        if (v instanceof Leaf) {
            return Control_Applicative.pure(Data_List_Types.applicativeList)(0);
        };
        if (v instanceof Two) {
            return Data_Functor.map(Data_List_Types.functorList)(function (n) {
                return n + 1 | 0;
            })(Data_Semigroup.append(Data_List_Types.semigroupList)(allHeights(v.value0))(allHeights(v.value3)));
        };
        if (v instanceof Three) {
            return Data_Functor.map(Data_List_Types.functorList)(function (n) {
                return n + 1 | 0;
            })(Data_Semigroup.append(Data_List_Types.semigroupList)(allHeights(v.value0))(Data_Semigroup.append(Data_List_Types.semigroupList)(allHeights(v.value3))(allHeights(v.value6))));
        };
        throw new Error("Failed pattern match at Data.Map.Internal (line 229, column 3 - line 229, column 36): " + [ v.constructor.name ]);
    };
    return Data_List.length(Data_List.nub(Data_Ord.ordInt)(allHeights(tree))) === 1;
};

// | Filter a map of optional values, keeping only the key/value pairs which
// | contain a value, creating a new map.
var catMaybes = function (dictOrd) {
    return mapMaybe(dictOrd)(Control_Category.identity(Control_Category.categoryFn));
};
var applyMap = function (dictOrd) {
    return {
        apply: intersectionWith(dictOrd)(Control_Category.identity(Control_Category.categoryFn)),
        Functor0: function () {
            return functorMap;
        }
    };
};
var bindMap = function (dictOrd) {
    return {
        bind: function (m) {
            return function (f) {
                return mapMaybeWithKey(dictOrd)(function (k) {
                    var $804 = lookup(dictOrd)(k);
                    return function ($805) {
                        return $804(f($805));
                    };
                })(m);
            };
        },
        Apply0: function () {
            return applyMap(dictOrd);
        }
    };
};

// | Insert the value, delete a value, or update a value for a key in a map
var alter = function (dictOrd) {
    return function (f) {
        return function (k) {
            return function (m) {
                var v = f(lookup(dictOrd)(k)(m));
                if (v instanceof Data_Maybe.Nothing) {
                    return $$delete(dictOrd)(k)(m);
                };
                if (v instanceof Data_Maybe.Just) {
                    return insert(dictOrd)(k)(v.value0)(m);
                };
                throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): " + [ v.constructor.name ]);
            };
        };
    };
};

// | Convert any foldable collection of key/value pairs to a map.
// | On key collision, the values are configurably combined.
var fromFoldableWith = function (dictOrd) {
    return function (dictFoldable) {
        return function (f) {
            var combine = function (v) {
                return function (v1) {
                    if (v1 instanceof Data_Maybe.Just) {
                        return new Data_Maybe.Just(f(v)(v1.value0));
                    };
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return new Data_Maybe.Just(v);
                    };
                    throw new Error("Failed pattern match at Data.Map.Internal (line 613, column 3 - line 613, column 38): " + [ v.constructor.name, v1.constructor.name ]);
                };
            };
            return Data_Foldable.foldl(dictFoldable)(function (m) {
                return function (v) {
                    return alter(dictOrd)(combine(v.value1))(v.value0)(m);
                };
            })(empty);
        };
    };
};

// | Inserts or updates a value with the given function.
// |
// | The combining function is called with the existing value as the first
// | argument and the new value as the second argument.
var insertWith = function (dictOrd) {
    return function (f) {
        return function (k) {
            return function (v) {
                return alter(dictOrd)((function () {
                    var $806 = Data_Maybe.maybe(v)(Data_Function.flip(f)(v));
                    return function ($807) {
                        return Data_Maybe.Just.create($806($807));
                    };
                })())(k);
            };
        };
    };
};

// | Compute the union of two maps, using the specified function
// | to combine values for duplicate keys.
var unionWith = function (dictOrd) {
    return function (f) {
        return function (m1) {
            return function (m2) {
                var go = function (k) {
                    return function (m) {
                        return function (v) {
                            return alter(dictOrd)((function () {
                                var $808 = Data_Maybe.maybe(v)(f(v));
                                return function ($809) {
                                    return Data_Maybe.Just.create($808($809));
                                };
                            })())(k)(m);
                        };
                    };
                };
                return Data_FoldableWithIndex.foldlWithIndex(foldableWithIndexMap)(go)(m2)(m1);
            };
        };
    };
};
var semigroupMap = function () {
    return function (dictOrd) {
        return function (dictSemigroup) {
            return {
                append: function (l) {
                    return function (r) {
                        return unionWith(dictOrd)(Data_Semigroup.append(dictSemigroup))(l)(r);
                    };
                }
            };
        };
    };
};
var monoidSemigroupMap = function () {
    return function (dictOrd) {
        return function (dictSemigroup) {
            return {
                mempty: empty,
                Semigroup0: function () {
                    return semigroupMap()(dictOrd)(dictSemigroup);
                }
            };
        };
    };
};

// | Compute the union of two maps, preferring values from the first map in the case
// | of duplicate keys
var union = function (dictOrd) {
    return unionWith(dictOrd)(Data_Function["const"]);
};

// | Returns a new map containing all entries of the given map which lie
// | between a given lower and upper bound, treating `Nothing` as no bound i.e.
// | including the smallest (or largest) key in the map, no matter how small
// | (or large) it is. For example:
// |
// | ```purescript
// | submap (Just 1) (Just 2)
// |   (fromFoldable [Tuple 0 "zero", Tuple 1 "one", Tuple 2 "two", Tuple 3 "three"])
// |   == fromFoldable [Tuple 1 "one", Tuple 2 "two"]
// |
// | submap Nothing (Just 2)
// |   (fromFoldable [Tuple 0 "zero", Tuple 1 "one", Tuple 2 "two", Tuple 3 "three"])
// |   == fromFoldable [Tuple 0 "zero", Tuple 1 "one", Tuple 2 "two"]
// | ```
// |
// | The function is entirely specified by the following
// | property:
// |
// | ```purescript
// | Given any m :: Map k v, mmin :: Maybe k, mmax :: Maybe k, key :: k,
// |   let m' = submap mmin mmax m in
// |     if (maybe true (\min -> min <= key) mmin &&
// |         maybe true (\max -> max >= key) mmax)
// |       then lookup key m == lookup key m'
// |       else not (member key m')
// | ```
var submap = function (dictOrd) {
    return function (kmin) {
        return function (kmax) {
            return foldSubmapBy(dictOrd)(union(dictOrd))(empty)(kmin)(kmax)(singleton);
        };
    };
};

// | Compute the union of a collection of maps
var unions = function (dictOrd) {
    return function (dictFoldable) {
        return Data_Foldable.foldl(dictFoldable)(union(dictOrd))(empty);
    };
};

// | Update or delete the value for a key in a map
var update = function (dictOrd) {
    return function (f) {
        return function (k) {
            return function (m) {
                return alter(dictOrd)(Data_Maybe.maybe(Data_Maybe.Nothing.value)(f))(k)(m);
            };
        };
    };
};
var altMap = function (dictOrd) {
    return {
        alt: union(dictOrd),
        Functor0: function () {
            return functorMap;
        }
    };
};
var plusMap = function (dictOrd) {
    return {
        empty: empty,
        Alt0: function () {
            return altMap(dictOrd);
        }
    };
};
export {
    Leaf,
    Two,
    Three,
    showTree,
    empty,
    isEmpty,
    singleton,
    checkValid,
    insert,
    insertWith,
    lookup,
    lookupLE,
    lookupLT,
    lookupGE,
    lookupGT,
    findMin,
    findMax,
    foldSubmap,
    submap,
    fromFoldable,
    fromFoldableWith,
    fromFoldableWithIndex,
    toUnfoldable,
    toUnfoldableUnordered,
    $$delete as delete,
    pop,
    member,
    alter,
    update,
    keys,
    values,
    union,
    unionWith,
    unions,
    intersection,
    intersectionWith,
    difference,
    isSubmap,
    size,
    filterWithKey,
    filterKeys,
    filter,
    mapMaybeWithKey,
    mapMaybe,
    catMaybes,
    eq1Map,
    eqMap,
    ord1Map,
    ordMap,
    showMap,
    semigroupMap,
    monoidSemigroupMap,
    altMap,
    plusMap,
    functorMap,
    functorWithIndexMap,
    applyMap,
    bindMap,
    foldableMap,
    foldableWithIndexMap,
    traversableMap,
    traversableWithIndexMap
};
