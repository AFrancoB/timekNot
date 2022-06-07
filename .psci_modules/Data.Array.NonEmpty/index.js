import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Array_NonEmpty_Internal from "../Data.Array.NonEmpty.Internal/index.js";
import * as Data_Bifunctor from "../Data.Bifunctor/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_NonEmpty from "../Data.NonEmpty/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Semigroup_Foldable from "../Data.Semigroup.Foldable/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable1 from "../Data.Unfoldable1/index.js";
import * as Unsafe_Coerce from "../Unsafe.Coerce/index.js";
var unsafeFromArrayF = Unsafe_Coerce.unsafeCoerce;

// | INTERNAL
var unsafeFromArray = Data_Array_NonEmpty_Internal.NonEmptyArray;
var toArray = function (v) {
    return v;
};
var unionBy$prime = function (eq) {
    return function (xs) {
        var $45 = Data_Array.unionBy(eq)(toArray(xs));
        return function ($46) {
            return unsafeFromArray($45($46));
        };
    };
};
var union$prime = function (dictEq) {
    return unionBy$prime(Data_Eq.eq(dictEq));
};
var unionBy = function (eq) {
    return function (xs) {
        var $47 = unionBy$prime(eq)(xs);
        return function ($48) {
            return $47(toArray($48));
        };
    };
};
var union = function (dictEq) {
    return unionBy(Data_Eq.eq(dictEq));
};
var unzip = /* #__PURE__ */ (function () {
    var $49 = Data_Bifunctor.bimap(Data_Bifunctor.bifunctorTuple)(unsafeFromArray)(unsafeFromArray);
    return function ($50) {
        return $49(Data_Array.unzip(toArray($50)));
    };
})();
var updateAt = function (i) {
    return function (x) {
        var $51 = Data_Array.updateAt(i)(x);
        return function ($52) {
            return unsafeFromArrayF($51(toArray($52)));
        };
    };
};
var zip = function (xs) {
    return function (ys) {
        return unsafeFromArray(Data_Array.zip(toArray(xs))(toArray(ys)));
    };
};
var zipWith = function (f) {
    return function (xs) {
        return function (ys) {
            return unsafeFromArray(Data_Array.zipWith(f)(toArray(xs))(toArray(ys)));
        };
    };
};
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return unsafeFromArrayF(Data_Array.zipWithA(dictApplicative)(f)(toArray(xs))(toArray(ys)));
            };
        };
    };
};
var splitAt = function (i) {
    return function (xs) {
        return Data_Array.splitAt(i)(toArray(xs));
    };
};
var some = function (dictAlternative) {
    return function (dictLazy) {
        var $53 = Data_Array.some(dictAlternative)(dictLazy);
        return function ($54) {
            return unsafeFromArrayF($53($54));
        };
    };
};
var snoc$prime = function (xs) {
    return function (x) {
        return unsafeFromArray(Data_Array.snoc(xs)(x));
    };
};
var snoc = function (xs) {
    return function (x) {
        return unsafeFromArray(Data_Array.snoc(toArray(xs))(x));
    };
};
var singleton = function ($55) {
    return unsafeFromArray(Data_Array.singleton($55));
};

// | Replicate an item at least once
var replicate = function (i) {
    return function (x) {
        return unsafeFromArray(Data_Array.replicate(Data_Ord.max(Data_Ord.ordInt)(1)(i))(x));
    };
};
var range = function (x) {
    return function (y) {
        return unsafeFromArray(Data_Array.range(x)(y));
    };
};
var modifyAt = function (i) {
    return function (f) {
        var $56 = Data_Array.modifyAt(i)(f);
        return function ($57) {
            return unsafeFromArrayF($56(toArray($57)));
        };
    };
};
var intersectBy$prime = function (eq) {
    return function (xs) {
        return Data_Array.intersectBy(eq)(toArray(xs));
    };
};
var intersectBy = function (eq) {
    return function (xs) {
        var $58 = intersectBy$prime(eq)(xs);
        return function ($59) {
            return $58(toArray($59));
        };
    };
};
var intersect$prime = function (dictEq) {
    return intersectBy$prime(Data_Eq.eq(dictEq));
};
var intersect = function (dictEq) {
    return intersectBy(Data_Eq.eq(dictEq));
};
var intercalate = function (dictSemigroup) {
    return Data_Semigroup_Foldable.intercalate(Data_Array_NonEmpty_Internal.foldable1NonEmptyArray)(dictSemigroup);
};
var insertAt = function (i) {
    return function (x) {
        var $60 = Data_Array.insertAt(i)(x);
        return function ($61) {
            return unsafeFromArrayF($60(toArray($61)));
        };
    };
};
var fromFoldable1 = function (dictFoldable1) {
    var $62 = Data_Array.fromFoldable(dictFoldable1.Foldable0());
    return function ($63) {
        return unsafeFromArray($62($63));
    };
};
var fromArray = function (xs) {
    if (Data_Array.length(xs) > 0) {
        return new Data_Maybe.Just(unsafeFromArray(xs));
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Array.NonEmpty (line 157, column 1 - line 157, column 58): " + [ xs.constructor.name ]);
};
var fromFoldable = function (dictFoldable) {
    var $64 = Data_Array.fromFoldable(dictFoldable);
    return function ($65) {
        return fromArray($64($65));
    };
};
var foldr1 = /* #__PURE__ */ Data_Semigroup_Foldable.foldr1(Data_Array_NonEmpty_Internal.foldable1NonEmptyArray);
var foldl1 = /* #__PURE__ */ Data_Semigroup_Foldable.foldl1(Data_Array_NonEmpty_Internal.foldable1NonEmptyArray);
var foldMap1 = function (dictSemigroup) {
    return Data_Semigroup_Foldable.foldMap1(Data_Array_NonEmpty_Internal.foldable1NonEmptyArray)(dictSemigroup);
};
var fold1 = function (dictSemigroup) {
    return Data_Semigroup_Foldable.fold1(Data_Array_NonEmpty_Internal.foldable1NonEmptyArray)(dictSemigroup);
};
var difference$prime = function (dictEq) {
    return function (xs) {
        return Data_Array.difference(dictEq)(toArray(xs));
    };
};
var cons$prime = function (x) {
    return function (xs) {
        return unsafeFromArray(Data_Array.cons(x)(xs));
    };
};
var fromNonEmpty = function (v) {
    return cons$prime(v.value0)(v.value1);
};
var concatMap = /* #__PURE__ */ Data_Function.flip(/* #__PURE__ */ Control_Bind.bind(Data_Array_NonEmpty_Internal.bindNonEmptyArray));
var concat = /* #__PURE__ */ (function () {
    var $66 = Data_Functor.map(Data_Array_NonEmpty_Internal.functorNonEmptyArray)(toArray);
    return function ($67) {
        return unsafeFromArray(Data_Array.concat(toArray($66($67))));
    };
})();
var appendArray = function (xs) {
    return function (ys) {
        return unsafeFromArray(Data_Semigroup.append(Data_Semigroup.semigroupArray)(toArray(xs))(ys));
    };
};
var alterAt = function (i) {
    return function (f) {
        var $68 = Data_Array.alterAt(i)(f);
        return function ($69) {
            return $68(toArray($69));
        };
    };
};

// | Internal - adapt Array functions returning Maybes to NonEmptyArray
var adaptMaybe = function (f) {
    var $70 = Data_Maybe.fromJust();
    return function ($71) {
        return $70(f(toArray($71)));
    };
};
var head = /* #__PURE__ */ adaptMaybe(Data_Array.head);
var init = /* #__PURE__ */ adaptMaybe(Data_Array.init);
var last = /* #__PURE__ */ adaptMaybe(Data_Array.last);
var tail = /* #__PURE__ */ adaptMaybe(Data_Array.tail);
var uncons = /* #__PURE__ */ adaptMaybe(Data_Array.uncons);
var toNonEmpty = function ($72) {
    return (function (v) {
        return new Data_NonEmpty.NonEmpty(v.head, v.tail);
    })(uncons($72));
};
var unsnoc = /* #__PURE__ */ adaptMaybe(Data_Array.unsnoc);

// | Internal - adapt an Array transform to NonEmptyArray,
//   with polymorphic result.
//
// Note that this is unsafe: if the transform returns an empty array, this can
// explode at runtime.
var adaptAny = function (f) {
    return function ($73) {
        return f(toArray($73));
    };
};
var all = function (p) {
    return adaptAny(Data_Array.all(p));
};
var any = function (p) {
    return adaptAny(Data_Array.any(p));
};
var catMaybes = /* #__PURE__ */ adaptAny(Data_Array.catMaybes);
var $$delete = function (dictEq) {
    return function (x) {
        return adaptAny(Data_Array["delete"](dictEq)(x));
    };
};
var deleteAt = function (i) {
    return adaptAny(Data_Array.deleteAt(i));
};
var deleteBy = function (f) {
    return function (x) {
        return adaptAny(Data_Array.deleteBy(f)(x));
    };
};
var difference = function (dictEq) {
    return function (xs) {
        return adaptAny(difference$prime(dictEq)(xs));
    };
};
var drop = function (i) {
    return adaptAny(Data_Array.drop(i));
};
var dropEnd = function (i) {
    return adaptAny(Data_Array.dropEnd(i));
};
var dropWhile = function (f) {
    return adaptAny(Data_Array.dropWhile(f));
};
var elem = function (dictEq) {
    return function (x) {
        return adaptAny(Data_Array.elem(dictEq)(x));
    };
};
var elemIndex = function (dictEq) {
    return function (x) {
        return adaptAny(Data_Array.elemIndex(dictEq)(x));
    };
};
var elemLastIndex = function (dictEq) {
    return function (x) {
        return adaptAny(Data_Array.elemLastIndex(dictEq)(x));
    };
};
var filter = function (f) {
    return adaptAny(Data_Array.filter(f));
};
var filterA = function (dictApplicative) {
    return function (f) {
        return adaptAny(Data_Array.filterA(dictApplicative)(f));
    };
};
var find = function (p) {
    return adaptAny(Data_Array.find(p));
};
var findIndex = function (p) {
    return adaptAny(Data_Array.findIndex(p));
};
var findLastIndex = function (x) {
    return adaptAny(Data_Array.findLastIndex(x));
};
var findMap = function (p) {
    return adaptAny(Data_Array.findMap(p));
};
var foldM = function (dictMonad) {
    return function (f) {
        return function (acc) {
            return adaptAny(Data_Array.foldM(dictMonad)(f)(acc));
        };
    };
};
var foldRecM = function (dictMonadRec) {
    return function (f) {
        return function (acc) {
            return adaptAny(Data_Array.foldRecM(dictMonadRec)(f)(acc));
        };
    };
};
var index = /* #__PURE__ */ adaptAny(Data_Array.index);
var length = /* #__PURE__ */ adaptAny(Data_Array.length);
var mapMaybe = function (f) {
    return adaptAny(Data_Array.mapMaybe(f));
};
var notElem = function (dictEq) {
    return function (x) {
        return adaptAny(Data_Array.notElem(dictEq)(x));
    };
};
var partition = function (f) {
    return adaptAny(Data_Array.partition(f));
};
var slice = function (start) {
    return function (end) {
        return adaptAny(Data_Array.slice(start)(end));
    };
};
var span = function (f) {
    return adaptAny(Data_Array.span(f));
};
var take = function (i) {
    return adaptAny(Data_Array.take(i));
};
var takeEnd = function (i) {
    return adaptAny(Data_Array.takeEnd(i));
};
var takeWhile = function (f) {
    return adaptAny(Data_Array.takeWhile(f));
};
var toUnfoldable = function (dictUnfoldable) {
    return adaptAny(Data_Array.toUnfoldable(dictUnfoldable));
};

// | Internal - adapt an Array transform to NonEmptyArray
//
// Note that this is unsafe: if the transform returns an empty array, this can
// explode at runtime.
var unsafeAdapt = function (f) {
    var $74 = adaptAny(f);
    return function ($75) {
        return unsafeFromArray($74($75));
    };
};
var cons = function (x) {
    return unsafeAdapt(Data_Array.cons(x));
};

// | Group equal, consecutive elements of an array into arrays.
// |
// | ```purescript
// | group (NonEmptyArray [1, 1, 2, 2, 1]) ==
// |   NonEmptyArray [NonEmptyArray [1, 1], NonEmptyArray [2, 2], NonEmptyArray [1]]
// | ```
var group = function (dictEq) {
    return unsafeAdapt(Data_Array.group(dictEq));
};

// | Group equal elements of an array into arrays, using the specified
// | comparison function to determine equality.
// |
// | ```purescript
// | groupAllBy (comparing Down) (NonEmptyArray [1, 3, 2, 4, 3, 3])
// |    = NonEmptyArray [NonEmptyArray [4], NonEmptyArray [3, 3, 3], NonEmptyArray [2], NonEmptyArray [1]]
// | ```
var groupAllBy = function (op) {
    return unsafeAdapt(Data_Array.groupAllBy(op));
};

// | Group equal elements of an array into arrays.
// |
// | ```purescript
// | groupAll (NonEmptyArray [1, 1, 2, 2, 1]) ==
// |   NonEmptyArray [NonEmptyArray [1, 1, 1], NonEmptyArray [2, 2]]
// | `
var groupAll = function (dictOrd) {
    return groupAllBy(Data_Ord.compare(dictOrd));
};

// | Group equal, consecutive elements of an array into arrays, using the
// | specified equivalence relation to determine equality.
// |
// | ```purescript
// | groupBy (\a b -> odd a && odd b) (NonEmptyArray [1, 3, 2, 4, 3, 3])
// |    = NonEmptyArray [NonEmptyArray [1, 3], NonEmptyArray [2], NonEmptyArray [4], NonEmptyArray [3, 3]]
// | ```
// |
var groupBy = function (op) {
    return unsafeAdapt(Data_Array.groupBy(op));
};
var insert = function (dictOrd) {
    return function (x) {
        return unsafeAdapt(Data_Array.insert(dictOrd)(x));
    };
};
var insertBy = function (f) {
    return function (x) {
        return unsafeAdapt(Data_Array.insertBy(f)(x));
    };
};
var intersperse = function (x) {
    return unsafeAdapt(Data_Array.intersperse(x));
};
var mapWithIndex = function (f) {
    return unsafeAdapt(Data_Array.mapWithIndex(f));
};
var modifyAtIndices = function (dictFoldable) {
    return function (is) {
        return function (f) {
            return unsafeAdapt(Data_Array.modifyAtIndices(dictFoldable)(is)(f));
        };
    };
};
var nub = function (dictOrd) {
    return unsafeAdapt(Data_Array.nub(dictOrd));
};
var nubBy = function (f) {
    return unsafeAdapt(Data_Array.nubBy(f));
};
var nubByEq = function (f) {
    return unsafeAdapt(Data_Array.nubByEq(f));
};
var nubEq = function (dictEq) {
    return unsafeAdapt(Data_Array.nubEq(dictEq));
};
var reverse = /* #__PURE__ */ unsafeAdapt(Data_Array.reverse);
var scanl = function (f) {
    return function (x) {
        return unsafeAdapt(Data_Array.scanl(f)(x));
    };
};
var scanr = function (f) {
    return function (x) {
        return unsafeAdapt(Data_Array.scanr(f)(x));
    };
};
var sort = function (dictOrd) {
    return unsafeAdapt(Data_Array.sort(dictOrd));
};
var sortBy = function (f) {
    return unsafeAdapt(Data_Array.sortBy(f));
};
var sortWith = function (dictOrd) {
    return function (f) {
        return unsafeAdapt(Data_Array.sortWith(dictOrd)(f));
    };
};
var updateAtIndices = function (dictFoldable) {
    return function (pairs) {
        return unsafeAdapt(Data_Array.updateAtIndices(dictFoldable)(pairs));
    };
};
var unsafeIndex = function () {
    return adaptAny(Data_Array.unsafeIndex());
};
var toUnfoldable1 = function (dictUnfoldable1) {
    return function (xs) {
        var len = length(xs);
        var f = function (i) {
            return new Data_Tuple.Tuple(unsafeIndex()(xs)(i), (function () {
                var $44 = i < (len - 1 | 0);
                if ($44) {
                    return new Data_Maybe.Just(i + 1 | 0);
                };
                return Data_Maybe.Nothing.value;
            })());
        };
        return Data_Unfoldable1.unfoldr1(dictUnfoldable1)(f)(0);
    };
};
export {
    fromArray,
    fromNonEmpty,
    toArray,
    toNonEmpty,
    fromFoldable,
    fromFoldable1,
    toUnfoldable,
    toUnfoldable1,
    singleton,
    range,
    replicate,
    some,
    length,
    cons,
    cons$prime,
    snoc,
    snoc$prime,
    appendArray,
    insert,
    insertBy,
    head,
    last,
    tail,
    init,
    uncons,
    unsnoc,
    index,
    elem,
    notElem,
    elemIndex,
    elemLastIndex,
    find,
    findMap,
    findIndex,
    findLastIndex,
    insertAt,
    deleteAt,
    updateAt,
    updateAtIndices,
    modifyAt,
    modifyAtIndices,
    alterAt,
    intersperse,
    reverse,
    concat,
    concatMap,
    filter,
    partition,
    splitAt,
    filterA,
    mapMaybe,
    catMaybes,
    mapWithIndex,
    foldl1,
    foldr1,
    foldMap1,
    fold1,
    intercalate,
    scanl,
    scanr,
    sort,
    sortBy,
    sortWith,
    slice,
    take,
    takeEnd,
    takeWhile,
    drop,
    dropEnd,
    dropWhile,
    span,
    group,
    groupAll,
    groupBy,
    groupAllBy,
    nub,
    nubBy,
    nubEq,
    nubByEq,
    union,
    union$prime,
    unionBy,
    unionBy$prime,
    $$delete as delete,
    deleteBy,
    difference,
    difference$prime,
    intersect,
    intersect$prime,
    intersectBy,
    intersectBy$prime,
    zipWith,
    zipWithA,
    zip,
    unzip,
    any,
    all,
    foldM,
    foldRecM,
    unsafeIndex
};
