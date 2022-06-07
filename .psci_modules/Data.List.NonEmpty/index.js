import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_NonEmpty from "../Data.NonEmpty/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Semigroup_Foldable from "../Data.Semigroup.Foldable/index.js";
import * as Data_Semigroup_Traversable from "../Data.Semigroup.Traversable/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";
import * as Partial_Unsafe from "../Partial.Unsafe/index.js";
var zipWith = function (f) {
    return function (v) {
        return function (v1) {
            return new Data_NonEmpty.NonEmpty(f(v.value0)(v1.value0), Data_List.zipWith(f)(v.value1)(v1.value1));
        };
    };
};
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Semigroup_Traversable.sequence1(Data_List_Types.traversable1NonEmptyList)(dictApplicative.Apply0())(zipWith(f)(xs)(ys));
            };
        };
    };
};
var zip = /* #__PURE__ */ (function () {
    return zipWith(Data_Tuple.Tuple.create);
})();

// | Like `wrappedOperation`, but for functions that operate on 2 lists.
var wrappedOperation2 = function (name) {
    return function (f) {
        return function (v) {
            return function (v1) {
                var v2 = f(new Data_List_Types.Cons(v.value0, v.value1))(new Data_List_Types.Cons(v1.value0, v1.value1));
                if (v2 instanceof Data_List_Types.Cons) {
                    return new Data_NonEmpty.NonEmpty(v2.value0, v2.value1);
                };
                if (v2 instanceof Data_List_Types.Nil) {
                    return Partial_Unsafe.unsafeCrashWith("Impossible: empty list in NonEmptyList " + name);
                };
                throw new Error("Failed pattern match at Data.List.NonEmpty (line 105, column 3 - line 107, column 81): " + [ v2.constructor.name ]);
            };
        };
    };
};

// | Internal function: any operation on a list that is guaranteed not to delete
// | all elements also applies to a NEL, this function is a helper for defining
// | those cases.
var wrappedOperation = function (name) {
    return function (f) {
        return function (v) {
            var v1 = f(new Data_List_Types.Cons(v.value0, v.value1));
            if (v1 instanceof Data_List_Types.Cons) {
                return new Data_NonEmpty.NonEmpty(v1.value0, v1.value1);
            };
            if (v1 instanceof Data_List_Types.Nil) {
                return Partial_Unsafe.unsafeCrashWith("Impossible: empty list in NonEmptyList " + name);
            };
            throw new Error("Failed pattern match at Data.List.NonEmpty (line 92, column 3 - line 94, column 81): " + [ v1.constructor.name ]);
        };
    };
};
var updateAt = function (i) {
    return function (a) {
        return function (v) {
            if (i === 0) {
                return new Data_Maybe.Just(new Data_NonEmpty.NonEmpty(a, v.value1));
            };
            if (Data_Boolean.otherwise) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(function ($162) {
                    return Data_List_Types.NonEmptyList((function (v1) {
                        return new Data_NonEmpty.NonEmpty(v.value0, v1);
                    })($162));
                })(Data_List.updateAt(i - 1 | 0)(a)(v.value1));
            };
            throw new Error("Failed pattern match at Data.List.NonEmpty (line 198, column 1 - line 198, column 75): " + [ i.constructor.name, a.constructor.name, v.constructor.name ]);
        };
    };
};
var unzip = function (ts) {
    return new Data_Tuple.Tuple(Data_Functor.map(Data_List_Types.functorNonEmptyList)(Data_Tuple.fst)(ts), Data_Functor.map(Data_List_Types.functorNonEmptyList)(Data_Tuple.snd)(ts));
};
var unsnoc = function (v) {
    var v1 = Data_List.unsnoc(v.value1);
    if (v1 instanceof Data_Maybe.Nothing) {
        return {
            init: Data_List_Types.Nil.value,
            last: v.value0
        };
    };
    if (v1 instanceof Data_Maybe.Just) {
        return {
            init: new Data_List_Types.Cons(v.value0, v1.value0.init),
            last: v1.value0.last
        };
    };
    throw new Error("Failed pattern match at Data.List.NonEmpty (line 160, column 35 - line 162, column 50): " + [ v1.constructor.name ]);
};
var unionBy = /* #__PURE__ */ (function () {
    var $163 = wrappedOperation2("unionBy");
    return function ($164) {
        return $163(Data_List.unionBy($164));
    };
})();
var union = function (dictEq) {
    return wrappedOperation2("union")(Data_List.union(dictEq));
};
var uncons = function (v) {
    return {
        head: v.value0,
        tail: v.value1
    };
};
var toList = function (v) {
    return new Data_List_Types.Cons(v.value0, v.value1);
};
var toUnfoldable = function (dictUnfoldable) {
    var $165 = Data_Unfoldable.unfoldr(dictUnfoldable)(function (xs) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(function (rec) {
            return new Data_Tuple.Tuple(rec.head, rec.tail);
        })(Data_List.uncons(xs));
    });
    return function ($166) {
        return $165(toList($166));
    };
};
var tail = function (v) {
    return v.value1;
};
var sortBy = /* #__PURE__ */ (function () {
    var $167 = wrappedOperation("sortBy");
    return function ($168) {
        return $167(Data_List.sortBy($168));
    };
})();
var sort = function (dictOrd) {
    return function (xs) {
        return sortBy(Data_Ord.compare(dictOrd))(xs);
    };
};
var snoc = function (v) {
    return function (y) {
        return new Data_NonEmpty.NonEmpty(v.value0, Data_List.snoc(v.value1)(y));
    };
};
var singleton = /* #__PURE__ */ (function () {
    var $169 = Data_NonEmpty.singleton(Data_List_Types.plusList);
    return function ($170) {
        return Data_List_Types.NonEmptyList($169($170));
    };
})();
var snoc$prime = function (v) {
    return function (y) {
        if (v instanceof Data_List_Types.Cons) {
            return new Data_NonEmpty.NonEmpty(v.value0, Data_List.snoc(v.value1)(y));
        };
        if (v instanceof Data_List_Types.Nil) {
            return singleton(y);
        };
        throw new Error("Failed pattern match at Data.List.NonEmpty (line 140, column 1 - line 140, column 51): " + [ v.constructor.name, y.constructor.name ]);
    };
};
var reverse = /* #__PURE__ */ wrappedOperation("reverse")(Data_List.reverse);
var nubEq = function (dictEq) {
    return wrappedOperation("nubEq")(Data_List.nubEq(dictEq));
};
var nubByEq = /* #__PURE__ */ (function () {
    var $171 = wrappedOperation("nubByEq");
    return function ($172) {
        return $171(Data_List.nubByEq($172));
    };
})();
var nubBy = /* #__PURE__ */ (function () {
    var $173 = wrappedOperation("nubBy");
    return function ($174) {
        return $173(Data_List.nubBy($174));
    };
})();
var nub = function (dictOrd) {
    return wrappedOperation("nub")(Data_List.nub(dictOrd));
};
var modifyAt = function (i) {
    return function (f) {
        return function (v) {
            if (i === 0) {
                return new Data_Maybe.Just(new Data_NonEmpty.NonEmpty(f(v.value0), v.value1));
            };
            if (Data_Boolean.otherwise) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(function ($175) {
                    return Data_List_Types.NonEmptyList((function (v1) {
                        return new Data_NonEmpty.NonEmpty(v.value0, v1);
                    })($175));
                })(Data_List.modifyAt(i - 1 | 0)(f)(v.value1));
            };
            throw new Error("Failed pattern match at Data.List.NonEmpty (line 203, column 1 - line 203, column 82): " + [ i.constructor.name, f.constructor.name, v.constructor.name ]);
        };
    };
};

// | Lifts a function that operates on a list to work on a NEL. This does not
// | preserve the non-empty status of the result.
var lift = function (f) {
    return function (v) {
        return f(new Data_List_Types.Cons(v.value0, v.value1));
    };
};
var mapMaybe = function ($176) {
    return lift(Data_List.mapMaybe($176));
};
var partition = function ($177) {
    return lift(Data_List.partition($177));
};
var span = function ($178) {
    return lift(Data_List.span($178));
};
var take = function ($179) {
    return lift(Data_List.take($179));
};
var takeWhile = function ($180) {
    return lift(Data_List.takeWhile($180));
};
var length = function (v) {
    return 1 + Data_List.length(v.value1) | 0;
};
var last = function (v) {
    return Data_Maybe.fromMaybe(v.value0)(Data_List.last(v.value1));
};
var intersectBy = /* #__PURE__ */ (function () {
    var $181 = wrappedOperation2("intersectBy");
    return function ($182) {
        return $181(Data_List.intersectBy($182));
    };
})();
var intersect = function (dictEq) {
    return wrappedOperation2("intersect")(Data_List.intersect(dictEq));
};
var insertAt = function (i) {
    return function (a) {
        return function (v) {
            if (i === 0) {
                return new Data_Maybe.Just(new Data_NonEmpty.NonEmpty(a, new Data_List_Types.Cons(v.value0, v.value1)));
            };
            if (Data_Boolean.otherwise) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(function ($183) {
                    return Data_List_Types.NonEmptyList((function (v1) {
                        return new Data_NonEmpty.NonEmpty(v.value0, v1);
                    })($183));
                })(Data_List.insertAt(i - 1 | 0)(a)(v.value1));
            };
            throw new Error("Failed pattern match at Data.List.NonEmpty (line 193, column 1 - line 193, column 75): " + [ i.constructor.name, a.constructor.name, v.constructor.name ]);
        };
    };
};
var init = function (v) {
    return Data_Maybe.maybe(Data_List_Types.Nil.value)(function (v1) {
        return new Data_List_Types.Cons(v.value0, v1);
    })(Data_List.init(v.value1));
};
var index = function (v) {
    return function (i) {
        if (i === 0) {
            return new Data_Maybe.Just(v.value0);
        };
        if (Data_Boolean.otherwise) {
            return Data_List.index(v.value1)(i - 1 | 0);
        };
        throw new Error("Failed pattern match at Data.List.NonEmpty (line 167, column 1 - line 167, column 52): " + [ v.constructor.name, i.constructor.name ]);
    };
};
var head = function (v) {
    return v.value0;
};
var groupBy = /* #__PURE__ */ (function () {
    var $184 = wrappedOperation("groupBy");
    return function ($185) {
        return $184(Data_List.groupBy($185));
    };
})();
var groupAllBy = /* #__PURE__ */ (function () {
    var $186 = wrappedOperation("groupAllBy");
    return function ($187) {
        return $186(Data_List.groupAllBy($187));
    };
})();
var groupAll = function (dictOrd) {
    return wrappedOperation("groupAll")(Data_List.groupAll(dictOrd));
};
var group = function (dictEq) {
    return wrappedOperation("group")(Data_List.group(dictEq));
};
var fromList = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Data_List_Types.Cons) {
        return new Data_Maybe.Just(new Data_NonEmpty.NonEmpty(v.value0, v.value1));
    };
    throw new Error("Failed pattern match at Data.List.NonEmpty (line 121, column 1 - line 121, column 57): " + [ v.constructor.name ]);
};
var fromFoldable = function (dictFoldable) {
    var $188 = Data_List.fromFoldable(dictFoldable);
    return function ($189) {
        return fromList($188($189));
    };
};
var foldM = function (dictMonad) {
    return function (f) {
        return function (b) {
            return function (v) {
                return Control_Bind.bind(dictMonad.Bind1())(f(b)(v.value0))(function (b$prime) {
                    return Data_List.foldM(dictMonad)(f)(b$prime)(v.value1);
                });
            };
        };
    };
};
var findLastIndex = function (f) {
    return function (v) {
        var v1 = Data_List.findLastIndex(f)(v.value1);
        if (v1 instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(v1.value0 + 1 | 0);
        };
        if (v1 instanceof Data_Maybe.Nothing) {
            if (f(v.value0)) {
                return new Data_Maybe.Just(0);
            };
            if (Data_Boolean.otherwise) {
                return Data_Maybe.Nothing.value;
            };
        };
        throw new Error("Failed pattern match at Data.List.NonEmpty (line 187, column 3 - line 191, column 29): " + [ v1.constructor.name ]);
    };
};
var findIndex = function (f) {
    return function (v) {
        if (f(v.value0)) {
            return new Data_Maybe.Just(0);
        };
        if (Data_Boolean.otherwise) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(function (v1) {
                return v1 + 1 | 0;
            })(Data_List.findIndex(f)(v.value1));
        };
        throw new Error("Failed pattern match at Data.List.NonEmpty (line 180, column 1 - line 180, column 69): " + [ f.constructor.name, v.constructor.name ]);
    };
};
var filterM = function (dictMonad) {
    var $190 = Data_List.filterM(dictMonad);
    return function ($191) {
        return lift($190($191));
    };
};
var filter = function ($192) {
    return lift(Data_List.filter($192));
};
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var dropWhile = function ($193) {
    return lift(Data_List.dropWhile($193));
};
var drop = function ($194) {
    return lift(Data_List.drop($194));
};
var cons$prime = function (x) {
    return function (xs) {
        return new Data_NonEmpty.NonEmpty(x, xs);
    };
};
var cons = function (y) {
    return function (v) {
        return new Data_NonEmpty.NonEmpty(y, new Data_List_Types.Cons(v.value0, v.value1));
    };
};
var concatMap = /* #__PURE__ */ Data_Function.flip(/* #__PURE__ */ Control_Bind.bind(Data_List_Types.bindNonEmptyList));
var concat = function (v) {
    return Control_Bind.bind(Data_List_Types.bindNonEmptyList)(v)(Control_Category.identity(Control_Category.categoryFn));
};
var catMaybes = /* #__PURE__ */ lift(Data_List.catMaybes);
var appendFoldable = function (dictFoldable) {
    return function (v) {
        return function (ys) {
            return new Data_NonEmpty.NonEmpty(v.value0, Data_Semigroup.append(Data_List_Types.semigroupList)(v.value1)(Data_List.fromFoldable(dictFoldable)(ys)));
        };
    };
};
export {
    toUnfoldable,
    fromFoldable,
    fromList,
    toList,
    singleton,
    length,
    cons,
    cons$prime,
    snoc,
    snoc$prime,
    head,
    last,
    tail,
    init,
    uncons,
    unsnoc,
    index,
    elemIndex,
    elemLastIndex,
    findIndex,
    findLastIndex,
    insertAt,
    updateAt,
    modifyAt,
    reverse,
    concat,
    concatMap,
    filter,
    filterM,
    mapMaybe,
    catMaybes,
    appendFoldable,
    sort,
    sortBy,
    take,
    takeWhile,
    drop,
    dropWhile,
    span,
    group,
    groupAll,
    groupBy,
    groupAllBy,
    partition,
    nub,
    nubBy,
    nubEq,
    nubByEq,
    union,
    unionBy,
    intersect,
    intersectBy,
    zipWith,
    zipWithA,
    zip,
    unzip,
    foldM
};
export {
    all,
    any,
    elem,
    find,
    findMap,
    fold,
    foldMap,
    foldl,
    foldr,
    intercalate,
    notElem
} from "../Data.Foldable/index.js";
export {
    NonEmptyList
} from "../Data.List.Types/index.js";
export {
    fold1,
    foldMap1,
    for1_,
    sequence1_,
    traverse1_
} from "../Data.Semigroup.Foldable/index.js";
export {
    sequence1,
    traverse1,
    traverse1Default
} from "../Data.Semigroup.Traversable/index.js";
export {
    scanl,
    scanr
} from "../Data.Traversable/index.js";
