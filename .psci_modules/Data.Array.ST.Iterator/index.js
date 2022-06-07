import * as Control_Monad_ST_Internal from "../Control.Monad.ST.Internal/index.js";
import * as Data_Array_ST from "../Data.Array.ST/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_HeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";

// | This type provides a slightly easier way of iterating over an array's
// | elements in an STArray computation, without having to keep track of
// | indices.
var Iterator = /* #__PURE__ */ (function () {
    function Iterator(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Iterator.create = function (value0) {
        return function (value1) {
            return new Iterator(value0, value1);
        };
    };
    return Iterator;
})();

// | Get the next item out of an iterator without advancing it.
var peek = function (v) {
    return function __do() {
        var i = Control_Monad_ST_Internal.read(v.value1)();
        return v.value0(i);
    };
};

// | Get the next item out of an iterator, advancing it. Returns Nothing if the
// | Iterator is exhausted.
var next = function (v) {
    return function __do() {
        var i = Control_Monad_ST_Internal.read(v.value1)();
        Control_Monad_ST_Internal.modify(function (v1) {
            return v1 + 1 | 0;
        })(v.value1)();
        return v.value0(i);
    };
};

// | Extract elements from an iterator and push them on to an STArray for as
// | long as those elements satisfy a given predicate.
var pushWhile = function (p) {
    return function (iter) {
        return function (array) {
            return function __do() {
                var $$break = Control_Monad_ST_Internal["new"](false)();
                while (Data_Functor.map(Control_Monad_ST_Internal.functorST)(Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean))(Control_Monad_ST_Internal.read($$break))()) {
                    (function __do() {
                        var mx = peek(iter)();
                        if (mx instanceof Data_Maybe.Just && p(mx.value0)) {
                            Data_Array_ST.push(mx.value0)(array)();
                            return Data_Functor["void"](Control_Monad_ST_Internal.functorST)(next(iter))();
                        };
                        return Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Control_Monad_ST_Internal.write(true)($$break))();
                    })();
                };
                return {};
            };
        };
    };
};

// | Push the entire remaining contents of an iterator onto an STArray.
var pushAll = /* #__PURE__ */ pushWhile(/* #__PURE__ */ Data_Function["const"](true));

// | Make an Iterator given an indexing function into an array (or anything
// | else). If `xs :: Array a`, the standard way to create an iterator over
// | `xs` is to use `iterator (xs !! _)`, where `(!!)` comes from `Data.Array`.
var iterator = function (f) {
    return Data_Functor.map(Control_Monad_ST_Internal.functorST)(Iterator.create(f))(Control_Monad_ST_Internal["new"](0));
};

// | Perform an action once for each item left in an iterator. If the action
// | itself also advances the same iterator, `iterate` will miss those items
// | out.
var iterate = function (iter) {
    return function (f) {
        return function __do() {
            var $$break = Control_Monad_ST_Internal["new"](false)();
            while (Data_Functor.map(Control_Monad_ST_Internal.functorST)(Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean))(Control_Monad_ST_Internal.read($$break))()) {
                (function __do() {
                    var mx = next(iter)();
                    if (mx instanceof Data_Maybe.Just) {
                        return f(mx.value0)();
                    };
                    if (mx instanceof Data_Maybe.Nothing) {
                        return Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Control_Monad_ST_Internal.write(true)($$break))();
                    };
                    throw new Error("Failed pattern match at Data.Array.ST.Iterator (line 42, column 5 - line 44, column 47): " + [ mx.constructor.name ]);
                })();
            };
            return {};
        };
    };
};

// | Check whether an iterator has been exhausted.
var exhausted = /* #__PURE__ */ (function () {
    var $13 = Data_Functor.map(Control_Monad_ST_Internal.functorST)(Data_Maybe.isNothing);
    return function ($14) {
        return $13(peek($14));
    };
})();
export {
    iterator,
    iterate,
    next,
    peek,
    exhausted,
    pushWhile,
    pushAll
};
