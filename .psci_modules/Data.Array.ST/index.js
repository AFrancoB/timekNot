// | Helper functions for working with mutable arrays using the `ST` effect.
// |
// | This module can be used when performance is important and mutation is a local effect.
import * as $foreign from "./foreign.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_ST_Internal from "../Control.Monad.ST.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";

// | Perform an effect requiring a mutable array on a copy of an immutable array,
// | safely returning the result as an immutable array.
var withArray = function (f) {
    return function (xs) {
        return function __do() {
            var result = $foreign.thaw(xs)();
            f(result)();
            return $foreign.unsafeFreeze(result)();
        };
    };
};

// | Append an element to the front of a mutable array. Returns the new length of
// | the array.
var unshift = function (a) {
    return $foreign.unshiftAll([ a ]);
};

// | Sort a mutable array in place using a comparison function. Sorting is
// | stable: the order of elements is preserved if they are equal according to
// | the comparison function.
var sortBy = function (comp) {
    return $foreign.sortByImpl(comp)(function (v) {
        if (v instanceof Data_Ordering.GT) {
            return 1;
        };
        if (v instanceof Data_Ordering.EQ) {
            return 0;
        };
        if (v instanceof Data_Ordering.LT) {
            return -1 | 0;
        };
        throw new Error("Failed pattern match at Data.Array.ST (line 108, column 31 - line 111, column 11): " + [ v.constructor.name ]);
    });
};

// | Sort a mutable array in place based on a projection. Sorting is stable: the
// | order of elements is preserved if they are equal according to the projection.
var sortWith = function (dictOrd) {
    return function (f) {
        return sortBy(Data_Ord.comparing(dictOrd)(f));
    };
};

// | Sort a mutable array in place. Sorting is stable: the order of equal
// | elements is preserved.
var sort = function (dictOrd) {
    return sortBy(Data_Ord.compare(dictOrd));
};

// | Remove the first element from an array and return that element.
var shift = /* #__PURE__ */ (function () {
    return $foreign.shiftImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();

// | A safe way to create and work with a mutable array before returning an
// | immutable array for later perusal. This function avoids copying the array
// | before returning it - it uses unsafeFreeze internally, but this wrapper is
// | a safe interface to that function.
var run = function (st) {
    return Control_Bind.bind(Control_Monad_ST_Internal.bindST)(st)($foreign.unsafeFreeze)();
};

// | Append an element to the end of a mutable array. Returns the new length of
// | the array.
var push = function (a) {
    return $foreign.pushAll([ a ]);
};

// | Remove the last element from an array and return that element.
var pop = /* #__PURE__ */ (function () {
    return $foreign.popImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();

// | Read the value at the specified index in a mutable array.
var peek = /* #__PURE__ */ (function () {
    return $foreign.peekImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();

// | Mutate the element at the specified index using the supplied function.
var modify = function (i) {
    return function (f) {
        return function (xs) {
            return function __do() {
                var entry = peek(i)(xs)();
                if (entry instanceof Data_Maybe.Just) {
                    return $foreign.poke(i)(f(entry.value0))(xs)();
                };
                if (entry instanceof Data_Maybe.Nothing) {
                    return false;
                };
                throw new Error("Failed pattern match at Data.Array.ST (line 193, column 3 - line 195, column 26): " + [ entry.constructor.name ]);
            };
        };
    };
};
export {
    new,
    poke,
    pushAll,
    unshiftAll,
    splice,
    freeze,
    thaw,
    unsafeFreeze,
    unsafeThaw,
    toAssocArray
} from "./foreign.js";
export {
    run,
    withArray,
    peek,
    modify,
    pop,
    push,
    shift,
    unshift,
    sort,
    sortBy,
    sortWith
};
