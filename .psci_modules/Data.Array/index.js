// | Helper functions for working with immutable Javascript arrays.
// |
// | _Note_: Depending on your use-case, you may prefer to use `Data.List` or
// | `Data.Sequence` instead, which might give better performance for certain
// | use cases. This module is useful when integrating with JavaScript libraries
// | which use arrays, but immutable arrays are not a practical data structure
// | for many use cases due to their poor asymptotics.
// |
// | In addition to the functions in this module, Arrays have a number of
// | useful instances:
// |
// | * `Functor`, which provides `map :: forall a b. (a -> b) -> Array a ->
// |   Array b`
// | * `Apply`, which provides `(<*>) :: forall a b. Array (a -> b) -> Array a
// |   -> Array b`. This function works a bit like a Cartesian product; the
// |   result array is constructed by applying each function in the first
// |   array to each value in the second, so that the result array ends up with
// |   a length equal to the product of the two arguments' lengths.
// | * `Bind`, which provides `(>>=) :: forall a b. (a -> Array b) -> Array a
// |   -> Array b` (this is the same as `concatMap`).
// | * `Semigroup`, which provides `(<>) :: forall a. Array a -> Array a ->
// |   Array a`, for concatenating arrays.
// | * `Foldable`, which provides a slew of functions for *folding* (also known
// |   as *reducing*) arrays down to one value. For example,
// |   `Data.Foldable.or` tests whether an array of `Boolean` values contains
// |   at least one `true` value.
// | * `Traversable`, which provides the PureScript version of a for-loop,
// |   allowing you to STAI.iterate over an array and accumulate effects.
// |
import * as $foreign from "./foreign.js";
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Lazy from "../Control.Lazy/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Control_Monad_ST_Internal from "../Control.Monad.ST.Internal/index.js";
import * as Data_Array_ST from "../Data.Array.ST/index.js";
import * as Data_Array_ST_Iterator from "../Data.Array.ST.Iterator/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_HeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";

// | A generalization of `zipWith` which accumulates results in some
// | `Applicative` functor.
// |
// | ```purescript
// | sndChars = zipWithA (\a b -> charAt 2 (a <> b))
// | sndChars ["a", "b"] ["A", "B"] = Nothing -- since "aA" has no 3rd char
// | sndChars ["aa", "b"] ["AA", "BBB"] = Just ['A', 'B']
// | ```
// |
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Traversable.sequence(Data_Traversable.traversableArray)(dictApplicative)($foreign.zipWith(f)(xs)(ys));
            };
        };
    };
};

// | Takes two arrays and returns an array of corresponding pairs.
// | If one input array is short, excess elements of the longer array are
// | discarded.
// |
// | ```purescript
// | zip [1, 2, 3] ["a", "b"] = [Tuple 1 "a", Tuple 2 "b"]
// | ```
// |
var zip = /* #__PURE__ */ (function () {
    return $foreign.zipWith(Data_Tuple.Tuple.create);
})();

// | Change the elements at the specified indices in index/value pairs.
// | Out-of-bounds indices will have no effect.
// |
// | ```purescript
// | updates = [Tuple 0 "Hi", Tuple 2 "." , Tuple 10 "foobar"]
// |
// | updateAtIndices updates ["Hello", "World", "!"] = ["Hi", "World", "."]
// | ```
// |
var updateAtIndices = function (dictFoldable) {
    return function (us) {
        return function (xs) {
            return Data_Array_ST.withArray(function (res) {
                return Data_Foldable.traverse_(Control_Monad_ST_Internal.applicativeST)(dictFoldable)(function (v) {
                    return Data_Array_ST.poke(v.value0)(v.value1)(res);
                })(us);
            })(xs)();
        };
    };
};

// | Change the element at the specified index, creating a new array, or
// | returning `Nothing` if the index is out of bounds.
// |
// | ```purescript
// | updateAt 1 "World" ["Hello", "Earth"] = Just ["Hello", "World"]
// | updateAt 10 "World" ["Hello", "Earth"] = Nothing
// | ```
// |
var updateAt = /* #__PURE__ */ (function () {
    return $foreign["_updateAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();

// | Find the element of an array at the specified index.
// |
// | ```purescript
// | unsafePartial $ unsafeIndex ["a", "b", "c"] 1 = "b"
// | ```
// |
// | Using `unsafeIndex` with an out-of-range index will not immediately raise a runtime error.
// | Instead, the result will be undefined. Most attempts to subsequently use the result will
// | cause a runtime error, of course, but this is not guaranteed, and is dependent on the backend;
// | some programs will continue to run as if nothing is wrong. For example, in the JavaScript backend,
// | the expression `unsafePartial (unsafeIndex [true] 1)` has type `Boolean`;
// | since this expression evaluates to `undefined`, attempting to use it in an `if` statement will cause
// | the else branch to be taken.
var unsafeIndex = function () {
    return $foreign.unsafeIndexImpl;
};

// | Break an array into its first element and remaining elements.
// |
// | Using `uncons` provides a way of writing code that would use cons patterns
// | in Haskell or pre-PureScript 0.7:
// | ``` purescript
// | f (x : xs) = something
// | f [] = somethingElse
// | ```
// | Becomes:
// | ``` purescript
// | f arr = case uncons arr of
// |   Just { head: x, tail: xs } -> something
// |   Nothing -> somethingElse
// | ```
var uncons = /* #__PURE__ */ (function () {
    return $foreign.unconsImpl(Data_Function["const"](Data_Maybe.Nothing.value))(function (x) {
        return function (xs) {
            return new Data_Maybe.Just({
                head: x,
                tail: xs
            });
        };
    });
})();

// | Convert an `Array` into an `Unfoldable` structure.
var toUnfoldable = function (dictUnfoldable) {
    return function (xs) {
        var len = $foreign.length(xs);
        var f = function (i) {
            if (i < len) {
                return new Data_Maybe.Just(new Data_Tuple.Tuple(unsafeIndex()(xs)(i), i + 1 | 0));
            };
            if (Data_Boolean.otherwise) {
                return Data_Maybe.Nothing.value;
            };
            throw new Error("Failed pattern match at Data.Array (line 156, column 3 - line 158, column 26): " + [ i.constructor.name ]);
        };
        return Data_Unfoldable.unfoldr(dictUnfoldable)(f)(0);
    };
};

// | Keep only a number of elements from the start of an array, creating a new
// | array.
// |
// | ```purescript
// | letters = ["a", "b", "c"]
// |
// | take 2 letters = ["a", "b"]
// | take 100 letters = ["a", "b", "c"]
// | ```
// |
var take = function (n) {
    return function (xs) {
        var $57 = n < 1;
        if ($57) {
            return [  ];
        };
        return $foreign.slice(0)(n)(xs);
    };
};

// | Get all but the first element of an array, creating a new array, or
// | `Nothing` if the array is empty
// |
// | ```purescript
// | tail [1, 2, 3, 4] = Just [2, 3, 4]
// | tail [] = Nothing
// | ```
// |
// | Running time: `O(n)` where `n` is the length of the array
var tail = /* #__PURE__ */ (function () {
    return $foreign.unconsImpl(Data_Function["const"](Data_Maybe.Nothing.value))(function (v) {
        return function (xs) {
            return new Data_Maybe.Just(xs);
        };
    });
})();

// | Splits an array into two subarrays, where `before` contains the elements
// | up to (but not including) the given index, and `after` contains the rest
// | of the elements, from that index on.
// |
// | ```purescript
// | >>> splitAt 3 [1, 2, 3, 4, 5]
// | { before: [1, 2, 3], after: [4, 5] }
// | ```
// |
// | Thus, the length of `(splitAt i arr).before` will equal either `i` or
// | `length arr`, if that is shorter. (Or if `i` is negative the length will
// | be 0.)
// |
// | ```purescript
// | splitAt 2 ([] :: Array Int) == { before: [], after: [] }
// | splitAt 3 [1, 2, 3, 4, 5] == { before: [1, 2, 3], after: [4, 5] }
// | ```
var splitAt = function (i) {
    return function (xs) {
        if (i <= 0) {
            return {
                before: [  ],
                after: xs
            };
        };
        return {
            before: $foreign.slice(0)(i)(xs),
            after: $foreign.slice(i)($foreign.length(xs))(xs)
        };
    };
};

// | Sort the elements of an array in increasing order, where elements are
// | compared using the specified partial ordering, creating a new array.
// | Sorting is stable: the order of elements is preserved if they are equal
// | according to the specified partial ordering.
// |
// | ```purescript
// | compareLength a b = compare (length a) (length b)
// | sortBy compareLength [[1, 2, 3], [7, 9], [-2]] = [[-2],[7,9],[1,2,3]]
// | ```
// |
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
        throw new Error("Failed pattern match at Data.Array (line 829, column 31 - line 832, column 11): " + [ v.constructor.name ]);
    });
};

// | Sort the elements of an array in increasing order, where elements are
// | sorted based on a projection. Sorting is stable: the order of elements is
// | preserved if they are equal according to the projection.
// |
// | ```purescript
// | sortWith (_.age) [{name: "Alice", age: 42}, {name: "Bob", age: 21}]
// |    = [{name: "Bob", age: 21}, {name: "Alice", age: 42}]
// | ```
// |
var sortWith = function (dictOrd) {
    return function (f) {
        return sortBy(Data_Ord.comparing(dictOrd)(f));
    };
};

//------------------------------------------------------------------------------
// Sorting ---------------------------------------------------------------------
//------------------------------------------------------------------------------
// | Sort the elements of an array in increasing order, creating a new array.
// | Sorting is stable: the order of equal elements is preserved.
// |
// | ```purescript
// | sort [2, -3, 1] = [-3, 1, 2]
// | ```
// |
var sort = function (dictOrd) {
    return function (xs) {
        return sortBy(Data_Ord.compare(dictOrd))(xs);
    };
};

// | Append an element to the end of an array, creating a new array.
// |
// | ```purescript
// | snoc [1, 2, 3] 4 = [1, 2, 3, 4]
// | ```
// |
var snoc = function (xs) {
    return function (x) {
        return Data_Array_ST.withArray(Data_Array_ST.push(x))(xs)();
    };
};

// | Create an array of one element
// | ```purescript
// | singleton 2 = [2]
// | ```
var singleton = function (a) {
    return [ a ];
};

//------------------------------------------------------------------------------
// Array size ------------------------------------------------------------------
//------------------------------------------------------------------------------
// | Test whether an array is empty.
// | ```purescript
// | null [] = true
// | null [1, 2] = false
// | ```
var $$null = function (xs) {
    return $foreign.length(xs) === 0;
};

// | Remove the duplicates from an array, where element equality is determined
// | by the specified equivalence relation, creating a new array.
// |
// | This less efficient version of `nubBy` only requires an equivalence
// | relation.
// |
// | ```purescript
// | mod3eq a b = a `mod` 3 == b `mod` 3
// | nubByEq mod3eq [1, 3, 4, 5, 6] = [1, 3, 5]
// | ```
// |
var nubByEq = function (eq) {
    return function (xs) {
        return (function __do() {
            var arr = Data_Array_ST["new"]();
            Control_Monad_ST_Internal.foreach(xs)(function (x) {
                return function __do() {
                    var e = Data_Functor.map(Control_Monad_ST_Internal.functorST)((function () {
                        var $89 = Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean);
                        var $90 = $foreign.any(function (v) {
                            return eq(v)(x);
                        });
                        return function ($91) {
                            return $89($90($91));
                        };
                    })())(Data_Array_ST.unsafeFreeze(arr))();
                    return Control_Applicative.when(Control_Monad_ST_Internal.applicativeST)(e)(Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Data_Array_ST.push(x)(arr)))();
                };
            })();
            return Data_Array_ST.unsafeFreeze(arr)();
        })();
    };
};

// | Remove the duplicates from an array, creating a new array.
// |
// | This less efficient version of `nub` only requires an `Eq` instance.
// |
// | ```purescript
// | nubEq [1, 2, 1, 3, 3] = [1, 2, 3]
// | ```
// |
var nubEq = function (dictEq) {
    return nubByEq(Data_Eq.eq(dictEq));
};

// | Apply a function to the element at the specified indices,
// | creating a new array. Out-of-bounds indices will have no effect.
// |
// | ```purescript
// | indices = [1, 3]
// | modifyAtIndices indices toUpper ["Hello", "World", "and", "others"]
// |    = ["Hello", "WORLD", "and", "OTHERS"]
// | ```
// |
var modifyAtIndices = function (dictFoldable) {
    return function (is) {
        return function (f) {
            return function (xs) {
                return Data_Array_ST.withArray(function (res) {
                    return Data_Foldable.traverse_(Control_Monad_ST_Internal.applicativeST)(dictFoldable)(function (i) {
                        return Data_Array_ST.modify(i)(f)(res);
                    })(is);
                })(xs)();
            };
        };
    };
};

// | Apply a function to each element in an array, supplying a generated
// | zero-based index integer along with the element, creating an array
// | with the new elements.
// |
// | ```purescript
// | prefixIndex index element = show index <> element
// |
// | mapWithIndex prefixIndex ["Hello", "World"] = ["0Hello", "1World"]
// | ```
// |
var mapWithIndex = function (f) {
    return function (xs) {
        return $foreign.zipWith(f)($foreign.range(0)($foreign.length(xs) - 1 | 0))(xs);
    };
};

//------------------------------------------------------------------------------
// Transformations -------------------------------------------------------------
//------------------------------------------------------------------------------
// | Inserts the given element in between each element in the array. The array
// | must have two or more elements for this operation to take effect.
// |
// | ```purescript
// | intersperse " " [ "a", "b" ] == [ "a", " ", "b" ]
// | intersperse 0 [ 1, 2, 3, 4, 5 ] == [ 1, 0, 2, 0, 3, 0, 4, 0, 5 ]
// | ```
// |
// | If the array has less than two elements, the input array is returned.
// | ```purescript
// | intersperse " " [] == []
// | intersperse " " ["a"] == ["a"]
// | ```
var intersperse = function (a) {
    return function (arr) {
        var v = $foreign.length(arr);
        if (v < 2) {
            return arr;
        };
        if (Data_Boolean.otherwise) {
            return Data_Array_ST.run((function () {
                var unsafeGetElem = function (idx) {
                    return unsafeIndex()(arr)(idx);
                };
                return function __do() {
                    var out = Data_Array_ST["new"]();
                    Data_Array_ST.push(unsafeGetElem(0))(out)();
                    Control_Monad_ST_Internal["for"](1)(v)(function (idx) {
                        return function __do() {
                            Data_Array_ST.push(a)(out)();
                            return Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Data_Array_ST.push(unsafeGetElem(idx))(out))();
                        };
                    })();
                    return out;
                };
            })());
        };
        throw new Error("Failed pattern match at Data.Array (line 611, column 21 - line 620, column 19): " + [ v.constructor.name ]);
    };
};
var intercalate = function (dictMonoid) {
    return Data_Foldable.intercalate(Data_Foldable.foldableArray)(dictMonoid);
};

// | Insert an element at the specified index, creating a new array, or
// | returning `Nothing` if the index is out of bounds.
// |
// | ```purescript
// | insertAt 2 "!" ["Hello", "World"] = Just ["Hello", "World", "!"]
// | insertAt 10 "!" ["Hello"] = Nothing
// | ```
// |
var insertAt = /* #__PURE__ */ (function () {
    return $foreign["_insertAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();

// | Get all but the last element of an array, creating a new array, or
// | `Nothing` if the array is empty.
// |
// | ```purescript
// | init [1, 2, 3, 4] = Just [1, 2, 3]
// | init [] = Nothing
// | ```
// |
// | Running time: `O(n)` where `n` is the length of the array
var init = function (xs) {
    if ($$null(xs)) {
        return Data_Maybe.Nothing.value;
    };
    if (Data_Boolean.otherwise) {
        return new Data_Maybe.Just($foreign.slice(0)($foreign.length(xs) - 1 | 0)(xs));
    };
    throw new Error("Failed pattern match at Data.Array (line 338, column 1 - line 338, column 45): " + [ xs.constructor.name ]);
};

//------------------------------------------------------------------------------
// Indexed operations ----------------------------------------------------------
//------------------------------------------------------------------------------
// | This function provides a safe way to read a value at a particular index
// | from an array.
// |
// | ```purescript
// | sentence = ["Hello", "World", "!"]
// |
// | index sentence 0 = Just "Hello"
// | index sentence 7 = Nothing
// | ```
// |
var index = /* #__PURE__ */ (function () {
    return $foreign.indexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();

// | Get the last element in an array, or `Nothing` if the array is empty
// |
// | Running time: `O(1)`.
// |
// | ```purescript
// | last [1, 2] = Just 2
// | last [] = Nothing
// | ```
// |
var last = function (xs) {
    return index(xs)($foreign.length(xs) - 1 | 0);
};

// | Break an array into its last element and all preceding elements.
// |
// | ```purescript
// | unsnoc [1, 2, 3] = Just {init: [1, 2], last: 3}
// | unsnoc [] = Nothing
// | ```
// |
// | Running time: `O(n)` where `n` is the length of the array
var unsnoc = function (xs) {
    return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
        return function (v1) {
            return {
                init: v,
                last: v1
            };
        };
    })(init(xs)))(last(xs));
};

// | Apply a function to the element at the specified index, creating a new
// | array, or returning `Nothing` if the index is out of bounds.
// |
// | ```purescript
// | modifyAt 1 toUpper ["Hello", "World"] = Just ["Hello", "WORLD"]
// | modifyAt 10 toUpper ["Hello", "World"] = Nothing
// | ```
// |
var modifyAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                return updateAt(i)(f(x))(xs);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)(index(xs)(i));
        };
    };
};

// | Split an array into two parts:
// |
// | 1. the longest initial subarray for which all elements satisfy the
// |    specified predicate
// | 2. the remaining elements
// |
// | ```purescript
// | span (\n -> n % 2 == 1) [1,3,2,4,5] == { init: [1,3], rest: [2,4,5] }
// | ```
// |
// | Running time: `O(n)`.
var span = function (p) {
    return function (arr) {
        var go = function ($copy_i) {
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(i) {
                var v = index(arr)(i);
                if (v instanceof Data_Maybe.Just) {
                    var $64 = p(v.value0);
                    if ($64) {
                        $copy_i = i + 1 | 0;
                        return;
                    };
                    $tco_done = true;
                    return new Data_Maybe.Just(i);
                };
                if (v instanceof Data_Maybe.Nothing) {
                    $tco_done = true;
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Data.Array (line 964, column 5 - line 966, column 25): " + [ v.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($copy_i);
            };
            return $tco_result;
        };
        var breakIndex = go(0);
        if (breakIndex instanceof Data_Maybe.Just && breakIndex.value0 === 0) {
            return {
                init: [  ],
                rest: arr
            };
        };
        if (breakIndex instanceof Data_Maybe.Just) {
            return {
                init: $foreign.slice(0)(breakIndex.value0)(arr),
                rest: $foreign.slice(breakIndex.value0)($foreign.length(arr))(arr)
            };
        };
        if (breakIndex instanceof Data_Maybe.Nothing) {
            return {
                init: arr,
                rest: [  ]
            };
        };
        throw new Error("Failed pattern match at Data.Array (line 951, column 3 - line 957, column 30): " + [ breakIndex.constructor.name ]);
    };
};

// | Calculate the longest initial subarray for which all element satisfy the
// | specified predicate, creating a new array.
// |
// | ```purescript
// | takeWhile (_ > 0) [4, 1, 0, -4, 5] = [4, 1]
// | takeWhile (_ > 0) [-1, 4] = []
// | ```
// |
var takeWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).init;
    };
};

// | Transforms an array of pairs into an array of first components and an
// | array of second components.
// |
// | ```purescript
// | unzip [Tuple 1 "a", Tuple 2 "b"] = Tuple [1, 2] ["a", "b"]
// | ```
// |
var unzip = function (xs) {
    return (function __do() {
        var fsts = Data_Array_ST["new"]();
        var snds = Data_Array_ST["new"]();
        var iter = Data_Array_ST_Iterator.iterator(function (v) {
            return index(xs)(v);
        })();
        Data_Array_ST_Iterator.iterate(iter)(function (v) {
            return function __do() {
                Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Data_Array_ST.push(v.value0)(fsts))();
                return Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Data_Array_ST.push(v.value1)(snds))();
            };
        })();
        var fsts$prime = Data_Array_ST.unsafeFreeze(fsts)();
        var snds$prime = Data_Array_ST.unsafeFreeze(snds)();
        return new Data_Tuple.Tuple(fsts$prime, snds$prime);
    })();
};

//------------------------------------------------------------------------------
// Non-indexed reads -----------------------------------------------------------
//------------------------------------------------------------------------------
// | Get the first element in an array, or `Nothing` if the array is empty
// |
// | Running time: `O(1)`.
// |
// | ```purescript
// | head [1, 2] = Just 1
// | head [] = Nothing
// | ```
// |
var head = function (xs) {
    return index(xs)(0);
};

// | Remove the duplicates from an array, where element equality is determined
// | by the specified ordering, creating a new array.
// |
// | ```purescript
// | nubBy compare [1, 3, 4, 2, 2, 1] == [1, 3, 4, 2]
// | ```
// |
var nubBy = function (comp) {
    return function (xs) {
        var indexedAndSorted = sortBy(function (x) {
            return function (y) {
                return comp(Data_Tuple.snd(x))(Data_Tuple.snd(y));
            };
        })(mapWithIndex(Data_Tuple.Tuple.create)(xs));
        var v = head(indexedAndSorted);
        if (v instanceof Data_Maybe.Nothing) {
            return [  ];
        };
        if (v instanceof Data_Maybe.Just) {
            return Data_Functor.map(Data_Functor.functorArray)(Data_Tuple.snd)(sortWith(Data_Ord.ordInt)(Data_Tuple.fst)((function __do() {
                var result = Data_Array_ST.unsafeThaw(singleton(v.value0))();
                Control_Monad_ST_Internal.foreach(indexedAndSorted)(function (v1) {
                    return function __do() {
                        var lst = Data_Functor.map(Control_Monad_ST_Internal.functorST)((function () {
                            var $92 = (function () {
                                var $94 = Data_Maybe.fromJust();
                                return function ($95) {
                                    return $94(last($95));
                                };
                            })();
                            return function ($93) {
                                return Data_Tuple.snd($92($93));
                            };
                        })())(Data_Array_ST.unsafeFreeze(result))();
                        return Control_Applicative.when(Control_Monad_ST_Internal.applicativeST)(Data_Eq.notEq(Data_Ordering.eqOrdering)(comp(lst)(v1.value1))(Data_Ordering.EQ.value))(Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Data_Array_ST.push(v1)(result)))();
                    };
                })();
                return Data_Array_ST.unsafeFreeze(result)();
            })()));
        };
        throw new Error("Failed pattern match at Data.Array (line 1044, column 17 - line 1052, column 29): " + [ v.constructor.name ]);
    };
};

// | Remove the duplicates from an array, creating a new array.
// |
// | ```purescript
// | nub [1, 2, 1, 3, 3] = [1, 2, 3]
// | ```
// |
var nub = function (dictOrd) {
    return nubBy(Data_Ord.compare(dictOrd));
};

// | Group equal, consecutive elements of an array into arrays, using the
// | specified equivalence relation to determine equality.
// |
// | ```purescript
// | groupBy (\a b -> odd a && odd b) [1, 3, 2, 4, 3, 3]
// |    = [NonEmptyArray [1, 3], NonEmptyArray [2], NonEmptyArray [4], NonEmptyArray [3, 3]]
// | ```
// |
var groupBy = function (op) {
    return function (xs) {
        return (function __do() {
            var result = Data_Array_ST["new"]();
            var iter = Data_Array_ST_Iterator.iterator(function (v) {
                return index(xs)(v);
            })();
            Data_Array_ST_Iterator.iterate(iter)(function (x) {
                return Data_Functor["void"](Control_Monad_ST_Internal.functorST)(function __do() {
                    var sub = Data_Array_ST["new"]();
                    Data_Array_ST.push(x)(sub)();
                    Data_Array_ST_Iterator.pushWhile(op(x))(iter)(sub)();
                    var grp = Data_Array_ST.unsafeFreeze(sub)();
                    return Data_Array_ST.push(grp)(result)();
                });
            })();
            return Data_Array_ST.unsafeFreeze(result)();
        })();
    };
};

// | Group equal elements of an array into arrays, using the specified
// | comparison function to determine equality.
// |
// | ```purescript
// | groupAllBy (comparing Down) [1, 3, 2, 4, 3, 3]
// |    = [NonEmptyArray [4], NonEmptyArray [3, 3, 3], NonEmptyArray [2], NonEmptyArray [1]]
// | ```
// |
var groupAllBy = function (cmp) {
    var $96 = groupBy(function (x) {
        return function (y) {
            return Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(x)(y))(Data_Ordering.EQ.value);
        };
    });
    var $97 = sortBy(cmp);
    return function ($98) {
        return $96($97($98));
    };
};

// | Group equal elements of an array into arrays.
// |
// | ```purescript
// | groupAll [1, 1, 2, 2, 1] == [NonEmptyArray [1, 1, 1], NonEmptyArray [2, 2]]
// | ```
var groupAll = function (dictOrd) {
    return groupAllBy(Data_Ord.compare(dictOrd));
};

// | Group equal, consecutive elements of an array into arrays.
// |
// | ```purescript
// | group [1, 1, 2, 2, 1] == [NonEmptyArray [1, 1], NonEmptyArray [2, 2], NonEmptyArray [1]]
// | ```
var group = function (dictEq) {
    return function (xs) {
        return groupBy(Data_Eq.eq(dictEq))(xs);
    };
};

// | Convert a `Foldable` structure into an `Array`.
// |
// | ```purescript
// | fromFoldable (Just 1) = [1]
// | fromFoldable (Nothing) = []
// | ```
// |
var fromFoldable = function (dictFoldable) {
    return $foreign.fromFoldableImpl(Data_Foldable.foldr(dictFoldable));
};
var foldr = /* #__PURE__ */ Data_Foldable.foldr(Data_Foldable.foldableArray);
var foldl = /* #__PURE__ */ Data_Foldable.foldl(Data_Foldable.foldableArray);
var foldRecM = function (dictMonadRec) {
    return function (f) {
        return function (b) {
            return function (array) {
                var go = function (res) {
                    return function (i) {
                        if (i >= $foreign.length(array)) {
                            return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Done(res));
                        };
                        if (Data_Boolean.otherwise) {
                            return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())(f(res)(unsafeIndex()(array)(i)))(function (res$prime) {
                                return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Loop({
                                    a: res$prime,
                                    b: i + 1 | 0
                                }));
                            });
                        };
                        throw new Error("Failed pattern match at Data.Array (line 1263, column 3 - line 1267, column 42): " + [ res.constructor.name, i.constructor.name ]);
                    };
                };
                return Control_Monad_Rec_Class.tailRecM2(dictMonadRec)(go)(b)(0);
            };
        };
    };
};
var foldMap = function (dictMonoid) {
    return Data_Foldable.foldMap(Data_Foldable.foldableArray)(dictMonoid);
};

// | Perform a fold using a monadic step function.
// |
// | ```purescript
// | foldM (\x y -> Just (x + y)) 0 [1, 4] = Just 5
// | ```
var foldM = function (dictMonad) {
    return function (f) {
        return function (b) {
            return $foreign.unconsImpl(function (v) {
                return Control_Applicative.pure(dictMonad.Applicative0())(b);
            })(function (a) {
                return function (as) {
                    return Control_Bind.bind(dictMonad.Bind1())(f(b)(a))(function (b$prime) {
                        return foldM(dictMonad)(f)(b$prime)(as);
                    });
                };
            });
        };
    };
};
var fold = function (dictMonoid) {
    return Data_Foldable.fold(Data_Foldable.foldableArray)(dictMonoid);
};

// | Find the first element in a data structure which satisfies
// | a predicate mapping.
var findMap = /* #__PURE__ */ (function () {
    return $foreign.findMapImpl(Data_Maybe.Nothing.value)(Data_Maybe.isJust);
})();

// | Find the last index for which a predicate holds.
// |
// | ```purescript
// | findLastIndex (contains $ Pattern "b") ["a", "bb", "b", "d"] = Just 2
// | findLastIndex (contains $ Pattern "x") ["a", "bb", "b", "d"] = Nothing
// | ```
// |
var findLastIndex = /* #__PURE__ */ (function () {
    return $foreign.findLastIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();

// | Insert an element into a sorted array, using the specified function to
// | determine the ordering of elements.
// |
// | ```purescript
// | invertCompare a b = invert $ compare a b
// |
// | insertBy invertCompare 10 [21, 20, 2, 1] = [21, 20, 10, 2, 1]
// | ```
// |
var insertBy = function (cmp) {
    return function (x) {
        return function (ys) {
            var i = Data_Maybe.maybe(0)(function (v) {
                return v + 1 | 0;
            })(findLastIndex(function (y) {
                return Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(x)(y))(Data_Ordering.GT.value);
            })(ys));
            return Data_Maybe.fromJust()(insertAt(i)(x)(ys));
        };
    };
};

// | Insert an element into a sorted array.
// |
// | ```purescript
// | insert 10 [1, 2, 20, 21] = [1, 2, 10, 20, 21]
// | ```
// |
var insert = function (dictOrd) {
    return insertBy(Data_Ord.compare(dictOrd));
};

// | Find the first index for which a predicate holds.
// |
// | ```purescript
// | findIndex (contains $ Pattern "b") ["a", "bb", "b", "d"] = Just 1
// | findIndex (contains $ Pattern "x") ["a", "bb", "b", "d"] = Nothing
// | ```
// |
var findIndex = /* #__PURE__ */ (function () {
    return $foreign.findIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();

// | Calculate the intersection of two arrays, using the specified equivalence
// | relation to compare elements, creating a new array. Note that duplicates
// | in the first array are preserved while duplicates in the second array are
// | removed.
// |
// | ```purescript
// | mod3eq a b = a `mod` 3 == b `mod` 3
// | intersectBy mod3eq [1, 2, 3] [4, 6, 7] = [1, 3]
// | ```
// |
var intersectBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return $foreign.filter(function (x) {
                return Data_Maybe.isJust(findIndex(eq(x))(ys));
            })(xs);
        };
    };
};

// | Calculate the intersection of two arrays, creating a new array. Note that
// | duplicates in the first array are preserved while duplicates in the second
// | array are removed.
// |
// | ```purescript
// | intersect [1, 1, 2] [2, 2, 1] = [1, 1, 2]
// | ```
// |
var intersect = function (dictEq) {
    return intersectBy(Data_Eq.eq(dictEq));
};

// | Find the first element for which a predicate holds.
// |
// | ```purescript
// | find (contains $ Pattern "b") ["a", "bb", "b", "d"] = Just "bb"
// | find (contains $ Pattern "x") ["a", "bb", "b", "d"] = Nothing
// | ```
var find = function (f) {
    return function (xs) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(unsafeIndex()(xs))(findIndex(f)(xs));
    };
};

// | Find the index of the last element equal to the specified element.
// |
// | ```purescript
// | elemLastIndex "a" ["a", "b", "a", "c"] = Just 2
// | elemLastIndex "Earth" ["Hello", "World", "!"] = Nothing
// | ```
// |
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};

// | Find the index of the first element equal to the specified element.
// |
// | ```purescript
// | elemIndex "a" ["a", "b", "a", "c"] = Just 0
// | elemIndex "Earth" ["Hello", "World", "!"] = Nothing
// | ```
// |
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};

// | Returns true if the array does not have the given element.
var notElem = function (dictEq) {
    return function (a) {
        return function (arr) {
            return Data_Maybe.isNothing(elemIndex(dictEq)(a)(arr));
        };
    };
};

// | Returns true if the array has the given element.
var elem = function (dictEq) {
    return function (a) {
        return function (arr) {
            return Data_Maybe.isJust(elemIndex(dictEq)(a)(arr));
        };
    };
};

// | Remove the longest initial subarray for which all element satisfy the
// | specified predicate, creating a new array.
// |
// | ```purescript
// | dropWhile (_ < 0) [-3, -1, 0, 4, -6] = [0, 4, -6]
// | ```
// |
var dropWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).rest;
    };
};

// | Drop a number of elements from the end of an array, creating a new array.
// |
// | ```purescript
// | letters = ["a", "b", "c", "d"]
// |
// | dropEnd 2 letters = ["a", "b"]
// | dropEnd 10 letters = []
// | ```
// |
var dropEnd = function (n) {
    return function (xs) {
        return take($foreign.length(xs) - n | 0)(xs);
    };
};

// | Drop a number of elements from the start of an array, creating a new array.
// |
// | ```purescript
// | letters = ["a", "b", "c", "d"]
// |
// | drop 2 letters = ["c", "d"]
// | drop 10 letters = []
// | ```
// |
var drop = function (n) {
    return function (xs) {
        var $79 = n < 1;
        if ($79) {
            return xs;
        };
        return $foreign.slice(n)($foreign.length(xs))(xs);
    };
};

// | Keep only a number of elements from the end of an array, creating a new
// | array.
// |
// | ```purescript
// | letters = ["a", "b", "c"]
// |
// | takeEnd 2 letters = ["b", "c"]
// | takeEnd 100 letters = ["a", "b", "c"]
// | ```
// |
var takeEnd = function (n) {
    return function (xs) {
        return drop($foreign.length(xs) - n | 0)(xs);
    };
};

// | Delete the element at the specified index, creating a new array, or
// | returning `Nothing` if the index is out of bounds.
// |
// | ```purescript
// | deleteAt 0 ["Hello", "World"] = Just ["World"]
// | deleteAt 10 ["Hello", "World"] = Nothing
// | ```
// |
var deleteAt = /* #__PURE__ */ (function () {
    return $foreign["_deleteAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
})();

// | Delete the first element of an array which matches the specified value,
// | under the equivalence relation provided in the first argument, creating a
// | new array.
// |
// | ```purescript
// | mod3eq a b = a `mod` 3 == b `mod` 3
// | deleteBy mod3eq 6 [1, 3, 4, 3] = [1, 4, 3]
// | ```
// |
var deleteBy = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2.length === 0) {
                return [  ];
            };
            return Data_Maybe.maybe(v2)(function (i) {
                return Data_Maybe.fromJust()(deleteAt(i)(v2));
            })(findIndex(v(v1))(v2));
        };
    };
};

// | Calculate the union of two arrays, using the specified function to
// | determine equality of elements. Note that duplicates in the first array
// | are preserved while duplicates in the second array are removed.
// |
// | ```purescript
// | mod3eq a b = a `mod` 3 == b `mod` 3
// | unionBy mod3eq [1, 5, 1, 2] [3, 4, 3, 3] = [1, 5, 1, 2, 3]
// | ```
// |
var unionBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return Data_Semigroup.append(Data_Semigroup.semigroupArray)(xs)(foldl(Data_Function.flip(deleteBy(eq)))(nubByEq(eq)(ys))(xs));
        };
    };
};

// | Calculate the union of two arrays. Note that duplicates in the first array
// | are preserved while duplicates in the second array are removed.
// |
// | Running time: `O(n^2)`
// |
// | ```purescript
// | union [1, 2, 1, 1] [3, 3, 3, 4] = [1, 2, 1, 1, 3, 4]
// | ```
// |
var union = function (dictEq) {
    return unionBy(Data_Eq.eq(dictEq));
};

// | Delete the first element of an array which is equal to the specified value,
// | creating a new array.
// |
// | ```purescript
// | delete 7 [1, 7, 3, 7] = [1, 3, 7]
// | delete 7 [1, 2, 3] = [1, 2, 3]
// | ```
// |
// | Running time: `O(n)`
var $$delete = function (dictEq) {
    return deleteBy(Data_Eq.eq(dictEq));
};

// | Delete the first occurrence of each element in the second array from the
// | first array, creating a new array.
// |
// | ```purescript
// | difference [2, 1] [2, 3] = [1]
// | ```
// |
// | Running time: `O(n*m)`, where n is the length of the first array, and m is
// | the length of the second.
var difference = function (dictEq) {
    return foldr($$delete(dictEq));
};

//------------------------------------------------------------------------------
// Extending arrays ------------------------------------------------------------
//------------------------------------------------------------------------------
// | Attaches an element to the front of an array, creating a new array.
// |
// | ```purescript
// | cons 1 [2, 3, 4] = [1, 2, 3, 4]
// | ```
// |
// | Note, the running time of this function is `O(n)`.
var cons = function (x) {
    return function (xs) {
        return Data_Semigroup.append(Data_Semigroup.semigroupArray)([ x ])(xs);
    };
};

// | Attempt a computation multiple times, requiring at least one success.
// |
// | The `Lazy` constraint is used to generate the result lazily, to ensure
// | termination.
var some = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Apply.apply((dictAlternative.Applicative0()).Apply0())(Data_Functor.map(((dictAlternative.Plus1()).Alt0()).Functor0())(cons)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
                return many(dictAlternative)(dictLazy)(v);
            }));
        };
    };
};

// | Attempt a computation multiple times, returning as many successful results
// | as possible (possibly zero).
// |
// | The `Lazy` constraint is used to generate the result lazily, to ensure
// | termination.
var many = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Alt.alt((dictAlternative.Plus1()).Alt0())(some(dictAlternative)(dictLazy)(v))(Control_Applicative.pure(dictAlternative.Applicative0())([  ]));
        };
    };
};

// | Apply a function to each element in an array, and flatten the results
// | into a single, new array.
// |
// | ```purescript
// | concatMap (split $ Pattern " ") ["Hello World", "other thing"]
// |    = ["Hello", "World", "other", "thing"]
// | ```
// |
var concatMap = /* #__PURE__ */ Data_Function.flip(/* #__PURE__ */ Control_Bind.bind(Control_Bind.bindArray));

// | Apply a function to each element in an array, keeping only the results
// | which contain a value, creating a new array.
// |
// | ```purescript
// | parseEmail :: String -> Maybe Email
// | parseEmail = ...
// |
// | mapMaybe parseEmail ["a.com", "hello@example.com", "--"]
// |    = [Email {user: "hello", domain: "example.com"}]
// | ```
// |
var mapMaybe = function (f) {
    return concatMap((function () {
        var $99 = Data_Maybe.maybe([  ])(singleton);
        return function ($100) {
            return $99(f($100));
        };
    })());
};

// | Filter where the predicate returns a `Boolean` in some `Applicative`.
// |
// | ```purescript
// | powerSet :: forall a. Array a -> Array (Array a)
// | powerSet = filterA (const [true, false])
// | ```
var filterA = function (dictApplicative) {
    return function (p) {
        var $101 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(mapMaybe(function (v) {
            if (v.value1) {
                return new Data_Maybe.Just(v.value0);
            };
            return Data_Maybe.Nothing.value;
        }));
        var $102 = Data_Traversable.traverse(Data_Traversable.traversableArray)(dictApplicative)(function (x) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Tuple.Tuple.create(x))(p(x));
        });
        return function ($103) {
            return $101($102($103));
        };
    };
};

// | Filter an array of optional values, keeping only the elements which contain
// | a value, creating a new array.
// |
// | ```purescript
// | catMaybes [Nothing, Just 2, Nothing, Just 4] = [2, 4]
// | ```
// |
var catMaybes = /* #__PURE__ */ mapMaybe(/* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn));

// | Update or delete the element at the specified index by applying a
// | function to the current value, returning a new array or `Nothing` if the
// | index is out-of-bounds.
// |
// | ```purescript
// | alterAt 1 (stripSuffix $ Pattern "!") ["Hello", "World!"]
// |    = Just ["Hello", "World"]
// |
// | alterAt 1 (stripSuffix $ Pattern "!!!!!") ["Hello", "World!"]
// |    = Just ["Hello"]
// |
// | alterAt 10 (stripSuffix $ Pattern "!") ["Hello", "World!"] = Nothing
// | ```
// |
var alterAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                var v = f(x);
                if (v instanceof Data_Maybe.Nothing) {
                    return deleteAt(i)(xs);
                };
                if (v instanceof Data_Maybe.Just) {
                    return updateAt(i)(v.value0)(xs);
                };
                throw new Error("Failed pattern match at Data.Array (line 589, column 10 - line 591, column 32): " + [ v.constructor.name ]);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)(index(xs)(i));
        };
    };
};
export {
    range,
    replicate,
    length,
    reverse,
    concat,
    filter,
    partition,
    scanl,
    scanr,
    slice,
    zipWith,
    any,
    all
} from "./foreign.js";
export {
    fromFoldable,
    toUnfoldable,
    singleton,
    some,
    many,
    $$null as null,
    cons,
    snoc,
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
    concatMap,
    splitAt,
    filterA,
    mapMaybe,
    catMaybes,
    mapWithIndex,
    foldl,
    foldr,
    foldMap,
    fold,
    intercalate,
    sort,
    sortBy,
    sortWith,
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
    nubEq,
    nubBy,
    nubByEq,
    union,
    unionBy,
    $$delete as delete,
    deleteBy,
    difference,
    intersect,
    intersectBy,
    zipWithA,
    zip,
    unzip,
    foldM,
    foldRecM,
    unsafeIndex
};
