import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Lazy from "../Data.Lazy/index.js";
import * as Data_List_Lazy from "../Data.List.Lazy/index.js";
import * as Data_List_Lazy_Types from "../Data.List.Lazy.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_NonEmpty from "../Data.NonEmpty/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";
var uncons = function (v) {
    var v1 = Data_Lazy.force(v);
    return {
        head: v1.value0,
        tail: v1.value1
    };
};
var toList = function (v) {
    var v1 = Data_Lazy.force(v);
    return Data_List_Lazy_Types.cons(v1.value0)(v1.value1);
};
var toUnfoldable = function (dictUnfoldable) {
    var $62 = Data_Unfoldable.unfoldr(dictUnfoldable)(function (xs) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(function (rec) {
            return new Data_Tuple.Tuple(rec.head, rec.tail);
        })(Data_List_Lazy.uncons(xs));
    });
    return function ($63) {
        return $62(toList($63));
    };
};
var tail = function (v) {
    var v1 = Data_Lazy.force(v);
    return v1.value1;
};
var singleton = /* #__PURE__ */ Control_Applicative.pure(Data_List_Lazy_Types.applicativeNonEmptyList);
var repeat = function (x) {
    return Data_Lazy.defer(function (v) {
        return new Data_NonEmpty.NonEmpty(x, Data_List_Lazy.repeat(x));
    });
};
var length = function (v) {
    var v1 = Data_Lazy.force(v);
    return 1 + Data_List_Lazy.length(v1.value1) | 0;
};
var last = function (v) {
    var v1 = Data_Lazy.force(v);
    return Data_Maybe.fromMaybe(v1.value0)(Data_List_Lazy.last(v1.value1));
};
var iterate = function (f) {
    return function (x) {
        return Data_Lazy.defer(function (v) {
            return new Data_NonEmpty.NonEmpty(x, Data_List_Lazy.iterate(f)(f(x)));
        });
    };
};
var init = function (v) {
    var v1 = Data_Lazy.force(v);
    return Data_Maybe.maybe(Data_List_Lazy_Types.nil)(function (v2) {
        return Data_List_Lazy_Types.cons(v1.value0)(v2);
    })(Data_List_Lazy.init(v1.value1));
};
var head = function (v) {
    var v1 = Data_Lazy.force(v);
    return v1.value0;
};
var fromList = function (l) {
    var v = Data_List_Lazy_Types.step(l);
    if (v instanceof Data_List_Lazy_Types.Nil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Data_List_Lazy_Types.Cons) {
        return new Data_Maybe.Just(Data_Lazy.defer(function (v1) {
            return new Data_NonEmpty.NonEmpty(v.value0, v.value1);
        }));
    };
    throw new Error("Failed pattern match at Data.List.Lazy.NonEmpty (line 42, column 3 - line 44, column 61): " + [ v.constructor.name ]);
};
var fromFoldable = function (dictFoldable) {
    var $64 = Data_List_Lazy.fromFoldable(dictFoldable);
    return function ($65) {
        return fromList($64($65));
    };
};
var cons = function (y) {
    return function (v) {
        return Data_Lazy.defer(function (v1) {
            var v2 = Data_Lazy.force(v);
            return new Data_NonEmpty.NonEmpty(y, Data_List_Lazy_Types.cons(v2.value0)(v2.value1));
        });
    };
};
var concatMap = /* #__PURE__ */ Data_Function.flip(/* #__PURE__ */ Control_Bind.bind(Data_List_Lazy_Types.bindNonEmptyList));
var appendFoldable = function (dictFoldable) {
    return function (nel) {
        return function (ys) {
            return Data_Lazy.defer(function (v) {
                return new Data_NonEmpty.NonEmpty(head(nel), Data_Semigroup.append(Data_List_Lazy_Types.semigroupList)(tail(nel))(Data_List_Lazy.fromFoldable(dictFoldable)(ys)));
            });
        };
    };
};
export {
    toUnfoldable,
    fromFoldable,
    fromList,
    toList,
    singleton,
    repeat,
    iterate,
    head,
    last,
    tail,
    init,
    cons,
    uncons,
    length,
    concatMap,
    appendFoldable
};
export {
    NonEmptyList
} from "../Data.List.Lazy.Types/index.js";
