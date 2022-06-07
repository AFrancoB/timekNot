// | This module defines a type of sets as balanced 2-3 trees, based on
// | <http://www.cs.princeton.edu/~dpw/courses/cos326-12/ass/2-3-trees.pdf>
// |
// | Qualified import is encouraged, so as to avoid name clashes with other modules.
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Control_Monad_ST_Internal from "../Control.Monad.ST.Internal/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Array_ST from "../Data.Array.ST/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";
import * as Data_Unit from "../Data.Unit/index.js";

// | `Set a` represents a set of values of type `a`
var $$Set = function (x) {
    return x;
};

// | Form the union of two sets
// |
// | Running time: `O(n * log(m))`
var union = function (dictOrd) {
    return function (v) {
        return function (v1) {
            return Data_Map_Internal.union(dictOrd)(v)(v1);
        };
    };
};

// | Insert a value into a set if it is not already present, if it is present, delete it.
var toggle = function (dictOrd) {
    return function (a) {
        return function (v) {
            return Data_Map_Internal.alter(dictOrd)(Data_Maybe.maybe(new Data_Maybe.Just(Data_Unit.unit))(function (v1) {
                return Data_Maybe.Nothing.value;
            }))(a)(v);
        };
    };
};

// | A set is a map with no value attached to each key.
var toMap = function (v) {
    return v;
};
var toList = function (v) {
    return Data_Map_Internal.keys(v);
};

// | Convert a set to an unfoldable structure.
var toUnfoldable = function (dictUnfoldable) {
    var $70 = Data_List.toUnfoldable(dictUnfoldable);
    return function ($71) {
        return $70(toList($71));
    };
};

// | Find the size of a set
var size = function (v) {
    return Data_Map_Internal.size(v);
};

// | Create a set with one element
var singleton = function (a) {
    return Data_Map_Internal.singleton(a)(Data_Unit.unit);
};
var showSet = function (dictShow) {
    return {
        show: function (s) {
            return "(fromFoldable " + (Data_Show.show(Data_Show.showArray(dictShow))(toUnfoldable(Data_Unfoldable.unfoldableArray)(s)) + ")");
        }
    };
};
var semigroupSet = function (dictOrd) {
    return {
        append: union(dictOrd)
    };
};

// | Test if a value is a member of a set
var member = function (dictOrd) {
    return function (a) {
        return function (v) {
            return Data_Map_Internal.member(dictOrd)(a)(v);
        };
    };
};

// | Test if a set is empty
var isEmpty = function (v) {
    return Data_Map_Internal.isEmpty(v);
};

// | Insert a value into a set
var insert = function (dictOrd) {
    return function (a) {
        return function (v) {
            return Data_Map_Internal.insert(dictOrd)(a)(Data_Unit.unit)(v);
        };
    };
};

// | A map with no value attached to each key is a set.
// | See also `Data.Map.keys`.
var fromMap = $$Set;
var foldableSet = {
    foldMap: function (dictMonoid) {
        return function (f) {
            var $72 = Data_Foldable.foldMap(Data_List_Types.foldableList)(dictMonoid)(f);
            return function ($73) {
                return $72(toList($73));
            };
        };
    },
    foldl: function (f) {
        return function (x) {
            var $74 = Data_Foldable.foldl(Data_List_Types.foldableList)(f)(x);
            return function ($75) {
                return $74(toList($75));
            };
        };
    },
    foldr: function (f) {
        return function (x) {
            var $76 = Data_Foldable.foldr(Data_List_Types.foldableList)(f)(x);
            return function ($77) {
                return $76(toList($77));
            };
        };
    }
};
var findMin = function (v) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(function (v1) {
        return v1.key;
    })(Data_Map_Internal.findMin(v));
};
var findMax = function (v) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(function (v1) {
        return v1.key;
    })(Data_Map_Internal.findMax(v));
};

// | Filter out those values of a set for which a predicate on the value fails
// | to hold.
var filter = function (dictOrd) {
    return function (f) {
        return function (v) {
            return Data_Map_Internal.filterWithKey(dictOrd)(function (k) {
                return function (v1) {
                    return f(k);
                };
            })(v);
        };
    };
};
var eqSet = function (dictEq) {
    return {
        eq: function (v) {
            return function (v1) {
                return Data_Eq.eq(Data_Map_Internal.eqMap(dictEq)(Data_Eq.eqUnit))(v)(v1);
            };
        }
    };
};
var ordSet = function (dictOrd) {
    return {
        compare: function (s1) {
            return function (s2) {
                return Data_Ord.compare(Data_List_Types.ordList(dictOrd))(toList(s1))(toList(s2));
            };
        },
        Eq0: function () {
            return eqSet(dictOrd.Eq0());
        }
    };
};
var eq1Set = {
    eq1: function (dictEq) {
        return Data_Eq.eq(eqSet(dictEq));
    }
};
var ord1Set = {
    compare1: function (dictOrd) {
        return Data_Ord.compare(ordSet(dictOrd));
    },
    Eq10: function () {
        return eq1Set;
    }
};

// | An empty set
var empty = Data_Map_Internal.empty;

// | Create a set from a foldable structure.
var fromFoldable = function (dictFoldable) {
    return function (dictOrd) {
        return Data_Foldable.foldl(dictFoldable)(function (m) {
            return function (a) {
                return insert(dictOrd)(a)(m);
            };
        })(empty);
    };
};

// | The set of elements which are in both the first and second set
var intersection = function (dictOrd) {
    return function (s1) {
        return function (s2) {
            var toArray = (function () {
                var $78 = Data_Array.fromFoldable(Data_List_Types.foldableList);
                return function ($79) {
                    return $78(toList($79));
                };
            })();
            var rs = toArray(s2);
            var rl = Data_Array.length(rs);
            var ls = toArray(s1);
            var ll = Data_Array.length(ls);
            var intersect = function (acc) {
                var go = function (l) {
                    return function (r) {
                        var $65 = l < ll && r < rl;
                        if ($65) {
                            var v = Data_Ord.compare(dictOrd)(ls[l])(rs[r]);
                            if (v instanceof Data_Ordering.EQ) {
                                return function __do() {
                                    Data_Array_ST.push(ls[l])(acc)();
                                    return new Control_Monad_Rec_Class.Loop({
                                        a: l + 1 | 0,
                                        b: r + 1 | 0
                                    });
                                };
                            };
                            if (v instanceof Data_Ordering.LT) {
                                return Control_Applicative.pure(Control_Monad_ST_Internal.applicativeST)(new Control_Monad_Rec_Class.Loop({
                                    a: l + 1 | 0,
                                    b: r
                                }));
                            };
                            if (v instanceof Data_Ordering.GT) {
                                return Control_Applicative.pure(Control_Monad_ST_Internal.applicativeST)(new Control_Monad_Rec_Class.Loop({
                                    a: l,
                                    b: r + 1 | 0
                                }));
                            };
                            throw new Error("Failed pattern match at Data.Set (line 184, column 12 - line 189, column 43): " + [ v.constructor.name ]);
                        };
                        return Control_Applicative.pure(Control_Monad_ST_Internal.applicativeST)(new Control_Monad_Rec_Class.Done(acc));
                    };
                };
                return Control_Monad_Rec_Class.tailRecM2(Control_Monad_ST_Internal.monadRecST)(go)(0)(0);
            };
            return fromFoldable(Data_Foldable.foldableArray)(dictOrd)(Control_Bind.bind(Control_Monad_ST_Internal.bindST)(Control_Bind.bind(Control_Monad_ST_Internal.bindST)(Data_Array_ST["new"])(intersect))(Data_Array_ST.unsafeFreeze)());
        };
    };
};

// | Maps over the values in a set.
// |
// | This operation is not structure-preserving for sets, so is not a valid
// | `Functor`. An example case: mapping `const x` over a set with `n > 0`
// | elements will result in a set with one element.
var map = function (dictOrd) {
    return function (f) {
        return Data_Foldable.foldl(foldableSet)(function (m) {
            return function (a) {
                return insert(dictOrd)(f(a))(m);
            };
        })(empty);
    };
};

// | Applies a function to each value in a set, discarding entries where the
// | function returns `Nothing`.
var mapMaybe = function (dictOrd) {
    return function (f) {
        return Data_Foldable.foldr(foldableSet)(function (a) {
            return function (acc) {
                return Data_Maybe.maybe(acc)(function (b) {
                    return insert(dictOrd)(b)(acc);
                })(f(a));
            };
        })(empty);
    };
};
var monoidSet = function (dictOrd) {
    return {
        mempty: empty,
        Semigroup0: function () {
            return semigroupSet(dictOrd);
        }
    };
};

// | Form the union of a collection of sets
var unions = function (dictFoldable) {
    return function (dictOrd) {
        return Data_Foldable.foldl(dictFoldable)(union(dictOrd))(empty);
    };
};

// | Delete a value from a set
var $$delete = function (dictOrd) {
    return function (a) {
        return function (v) {
            return Data_Map_Internal["delete"](dictOrd)(a)(v);
        };
    };
};

// | Form the set difference
var difference = function (dictOrd) {
    return function (s1) {
        return function (s2) {
            return Data_Foldable.foldl(Data_List_Types.foldableList)(Data_Function.flip($$delete(dictOrd)))(s1)(toList(s2));
        };
    };
};

// | True if and only if every element in the first set
// | is an element of the second set
var subset = function (dictOrd) {
    return function (s1) {
        return function (s2) {
            return isEmpty(difference(dictOrd)(s1)(s2));
        };
    };
};

// | True if and only if the first set is a subset of the second set
// | and the sets are not equal
var properSubset = function (dictOrd) {
    return function (s1) {
        return function (s2) {
            return subset(dictOrd)(s1)(s2) && Data_Eq.notEq(eqSet(dictOrd.Eq0()))(s1)(s2);
        };
    };
};

// | Check whether the underlying tree satisfies the 2-3 invariant
// |
// | This function is provided for internal use.
var checkValid = function (v) {
    return Data_Map_Internal.checkValid(v);
};

// | Filter a set of optional values, discarding values that contain `Nothing`
var catMaybes = function (dictOrd) {
    return mapMaybe(dictOrd)(Control_Category.identity(Control_Category.categoryFn));
};
export {
    fromFoldable,
    toUnfoldable,
    empty,
    isEmpty,
    singleton,
    map,
    checkValid,
    insert,
    member,
    $$delete as delete,
    toggle,
    size,
    findMin,
    findMax,
    union,
    unions,
    difference,
    subset,
    properSubset,
    intersection,
    filter,
    mapMaybe,
    catMaybes,
    toMap,
    fromMap,
    eqSet,
    eq1Set,
    showSet,
    ordSet,
    ord1Set,
    monoidSet,
    semigroupSet,
    foldableSet
};
