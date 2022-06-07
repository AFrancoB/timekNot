import * as Data_Array_NonEmpty_Internal from "../Data.Array.NonEmpty.Internal/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Semigroup_Foldable from "../Data.Semigroup.Foldable/index.js";
import * as Data_Set from "../Data.Set/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable1 from "../Data.Unfoldable1/index.js";

// | `NonEmptySet a` represents a non-empty set of values of type `a`
var NonEmptySet = function (x) {
    return x;
};

// | Form the union of a set and the non-empty set.
var unionSet = function (dictOrd) {
    return function (s1) {
        return function (v) {
            return Data_Semigroup.append(Data_Set.semigroupSet(dictOrd))(s1)(v);
        };
    };
};

// | Convert a set to a non-empty unfoldable structure.
var toUnfoldable1 = function (dictUnfoldable1) {
    return function (v) {
        var go = function (v1) {
            if (v1 instanceof Data_List_Types.Cons && v1.value1 instanceof Data_List_Types.Nil) {
                return new Data_Tuple.Tuple(v1.value0, Data_Maybe.Nothing.value);
            };
            if (v1 instanceof Data_List_Types.Cons) {
                return new Data_Tuple.Tuple(v1.value0, new Data_Maybe.Just(v1.value1));
            };
            throw new Error("Failed pattern match at Data.Set.NonEmpty (line 95, column 24 - line 97, column 38): " + [ v1.constructor.name ]);
        };
        return Data_Unfoldable1.unfoldr1(dictUnfoldable1)(go)(Data_Set.toUnfoldable(Data_List_Types.unfoldableList)(v));
    };
};

// | Convert a set to an unfoldable structure.
var toUnfoldable = function (dictUnfoldable) {
    return function (v) {
        return Data_Set.toUnfoldable(dictUnfoldable)(v);
    };
};

// | Forgets the non-empty property of a set, giving a normal possibly-empty
// | set.
var toSet = function (v) {
    return v;
};

// | True if and only if every element in the first set is an element of the
// | second set.
var subset = function (dictOrd) {
    return function (v) {
        return function (v1) {
            return Data_Set.subset(dictOrd)(v)(v1);
        };
    };
};

// | Find the size of a set.
var size = function (v) {
    return Data_Set.size(v);
};

// | Create a set with one element.
var singleton = function (a) {
    return Data_Set.singleton(a);
};
var showNonEmptySet = function (dictShow) {
    return {
        show: function (s) {
            return "(fromFoldable1 " + (Data_Show.show(Data_Array_NonEmpty_Internal.showNonEmptyArray(dictShow))(toUnfoldable1(Data_Array_NonEmpty_Internal.unfoldable1NonEmptyArray)(s)) + ")");
        }
    };
};
var semigroupNonEmptySet = function (dictOrd) {
    return Data_Set.semigroupSet(dictOrd);
};

// | True if and only if the first set is a subset of the second set and the
// | sets are not equal.
var properSubset = function (dictOrd) {
    return function (v) {
        return function (v1) {
            return Data_Set.properSubset(dictOrd)(v)(v1);
        };
    };
};
var ordNonEmptySet = function (dictOrd) {
    return Data_Set.ordSet(dictOrd);
};
var ord1NonEmptySet = Data_Set.ord1Set;

// | The minimum value in the set.
var min = function (v) {
    return Data_Maybe.fromJust()(Data_Set.findMin(v));
};

// | Test if a value is a member of a set.
var member = function (dictOrd) {
    return function (a) {
        return function (v) {
            return Data_Set.member(dictOrd)(a)(v);
        };
    };
};

// | The maximum value in the set.
var max = function (v) {
    return Data_Maybe.fromJust()(Data_Set.findMax(v));
};

// | Applies a function to each value in a set, discarding entries where the
// | function returns `Nothing`.
var mapMaybe = function (dictOrd) {
    return function (f) {
        return function (v) {
            return Data_Set.mapMaybe(dictOrd)(f)(v);
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
        return function (v) {
            return Data_Set.map(dictOrd)(f)(v);
        };
    };
};

// | Insert a value into a set.
var insert = function (dictOrd) {
    return function (a) {
        return function (v) {
            return Data_Set.insert(dictOrd)(a)(v);
        };
    };
};

// | Attempts to create a non-empty set from a possibly-empty set.
var fromSet = function (s) {
    var $70 = Data_Set.isEmpty(s);
    if ($70) {
        return Data_Maybe.Nothing.value;
    };
    return new Data_Maybe.Just(s);
};

// | The set of elements which are in both the first and second set. `Nothing`
// | if the sets are disjoint.
var intersection = function (dictOrd) {
    return function (v) {
        return function (v1) {
            return fromSet(Data_Set.intersection(dictOrd)(v)(v1));
        };
    };
};

// | Create a set from a non-empty foldable structure.
var fromFoldable1 = function (dictFoldable1) {
    return function (dictOrd) {
        return Data_Semigroup_Foldable.foldMap1(dictFoldable1)(semigroupNonEmptySet(dictOrd))(singleton);
    };
};

// | Create a set from a foldable structure.
var fromFoldable = function (dictFoldable) {
    return function (dictOrd) {
        var $79 = Data_Set.fromFoldable(dictFoldable)(dictOrd);
        return function ($80) {
            return fromSet($79($80));
        };
    };
};
var foldableNonEmptySet = Data_Set.foldableSet;
var foldable1NonEmptySet = {
    foldMap1: function (dictSemigroup) {
        return function (f) {
            var $81 = Data_Semigroup_Foldable.foldMap1(Data_List_Types.foldable1NonEmptyList)(dictSemigroup)(f);
            var $82 = toUnfoldable1(Data_List_Types.unfoldable1NonEmptyList);
            return function ($83) {
                return $81($82($83));
            };
        };
    },
    foldr1: function (f) {
        var $84 = Data_Semigroup_Foldable.foldr1(Data_List_Types.foldable1NonEmptyList)(f);
        var $85 = toUnfoldable1(Data_List_Types.unfoldable1NonEmptyList);
        return function ($86) {
            return $84($85($86));
        };
    },
    foldl1: function (f) {
        var $87 = Data_Semigroup_Foldable.foldl1(Data_List_Types.foldable1NonEmptyList)(f);
        var $88 = toUnfoldable1(Data_List_Types.unfoldable1NonEmptyList);
        return function ($89) {
            return $87($88($89));
        };
    },
    Foldable0: function () {
        return foldableNonEmptySet;
    }
};

// | Filter out those values of a set for which a predicate on the value fails
// | to hold.
var filter = function (dictOrd) {
    return function (f) {
        return function (v) {
            return Data_Set.filter(dictOrd)(f)(v);
        };
    };
};
var eqNonEmptySet = function (dictEq) {
    return Data_Set.eqSet(dictEq);
};
var eq1NonEmptySet = Data_Set.eq1Set;

// | Form the set difference. `Nothing` if the first is a subset of the second.
var difference = function (dictOrd) {
    return function (v) {
        return function (v1) {
            return fromSet(Data_Set.difference(dictOrd)(v)(v1));
        };
    };
};

// | Delete a value from a non-empty set. If this would empty the set, the
// | result is `Nothing`.
var $$delete = function (dictOrd) {
    return function (a) {
        return function (v) {
            return fromSet(Data_Set["delete"](dictOrd)(a)(v));
        };
    };
};

// | Creates a `NonEmptySet` from an item and a `Set`.
var cons = function (dictOrd) {
    return function (a) {
        var $90 = Data_Set.insert(dictOrd)(a);
        return function ($91) {
            return NonEmptySet($90($91));
        };
    };
};
export {
    singleton,
    cons,
    fromSet,
    fromFoldable,
    fromFoldable1,
    toSet,
    toUnfoldable,
    toUnfoldable1,
    map,
    member,
    insert,
    $$delete as delete,
    size,
    min,
    max,
    unionSet,
    difference,
    subset,
    properSubset,
    intersection,
    filter,
    mapMaybe,
    eqNonEmptySet,
    eq1NonEmptySet,
    ordNonEmptySet,
    ord1NonEmptySet,
    semigroupNonEmptySet,
    foldableNonEmptySet,
    foldable1NonEmptySet,
    showNonEmptySet
};
