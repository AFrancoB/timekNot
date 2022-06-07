import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_FoldableWithIndex from "../Data.FoldableWithIndex/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_FunctorWithIndex from "../Data.FunctorWithIndex/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_NonEmpty from "../Data.NonEmpty/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Semigroup_Traversable from "../Data.Semigroup.Traversable/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Data_TraversableWithIndex from "../Data.TraversableWithIndex/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var Nil = /* #__PURE__ */ (function () {
    function Nil() {

    };
    Nil.value = new Nil();
    return Nil;
})();
var Cons = /* #__PURE__ */ (function () {
    function Cons(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Cons.create = function (value0) {
        return function (value1) {
            return new Cons(value0, value1);
        };
    };
    return Cons;
})();
var NonEmptyList = function (x) {
    return x;
};
var toList = function (v) {
    return new Cons(v.value0, v.value1);
};
var newtypeNonEmptyList = {
    Coercible0: function () {
        return undefined;
    }
};
var nelCons = function (a) {
    return function (v) {
        return new Data_NonEmpty.NonEmpty(a, new Cons(v.value0, v.value1));
    };
};

// chunked list Functor inspired by OCaml
// https://discuss.ocaml.org/t/a-new-list-map-that-is-both-stack-safe-and-fast/865
// chunk sizes determined through experimentation
var listMap = function (f) {
    var chunkedRevMap = function ($copy_chunksAcc) {
        return function ($copy_v) {
            var $tco_var_chunksAcc = $copy_chunksAcc;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(chunksAcc, v) {
                if (v instanceof Cons && (v.value1 instanceof Cons && v.value1.value1 instanceof Cons)) {
                    $tco_var_chunksAcc = new Cons(v, chunksAcc);
                    $copy_v = v.value1.value1.value1;
                    return;
                };
                var unrolledMap = function (v1) {
                    if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Nil)) {
                        return new Cons(f(v1.value0), new Cons(f(v1.value1.value0), Nil.value));
                    };
                    if (v1 instanceof Cons && v1.value1 instanceof Nil) {
                        return new Cons(f(v1.value0), Nil.value);
                    };
                    return Nil.value;
                };
                var reverseUnrolledMap = function ($copy_v1) {
                    return function ($copy_acc) {
                        var $tco_var_v1 = $copy_v1;
                        var $tco_done1 = false;
                        var $tco_result;
                        function $tco_loop(v1, acc) {
                            if (v1 instanceof Cons && (v1.value0 instanceof Cons && (v1.value0.value1 instanceof Cons && v1.value0.value1.value1 instanceof Cons))) {
                                $tco_var_v1 = v1.value1;
                                $copy_acc = new Cons(f(v1.value0.value0), new Cons(f(v1.value0.value1.value0), new Cons(f(v1.value0.value1.value1.value0), acc)));
                                return;
                            };
                            $tco_done1 = true;
                            return acc;
                        };
                        while (!$tco_done1) {
                            $tco_result = $tco_loop($tco_var_v1, $copy_acc);
                        };
                        return $tco_result;
                    };
                };
                $tco_done = true;
                return reverseUnrolledMap(chunksAcc)(unrolledMap(v));
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_chunksAcc, $copy_v);
            };
            return $tco_result;
        };
    };
    return chunkedRevMap(Nil.value);
};
var functorList = {
    map: listMap
};
var functorNonEmptyList = /* #__PURE__ */ Data_NonEmpty.functorNonEmpty(functorList);
var foldableList = {
    foldr: function (f) {
        return function (b) {
            var rev = (function () {
                var go = function ($copy_acc) {
                    return function ($copy_v) {
                        var $tco_var_acc = $copy_acc;
                        var $tco_done = false;
                        var $tco_result;
                        function $tco_loop(acc, v) {
                            if (v instanceof Nil) {
                                $tco_done = true;
                                return acc;
                            };
                            if (v instanceof Cons) {
                                $tco_var_acc = new Cons(v.value0, acc);
                                $copy_v = v.value1;
                                return;
                            };
                            throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [ acc.constructor.name, v.constructor.name ]);
                        };
                        while (!$tco_done) {
                            $tco_result = $tco_loop($tco_var_acc, $copy_v);
                        };
                        return $tco_result;
                    };
                };
                return go(Nil.value);
            })();
            var $205 = Data_Foldable.foldl(foldableList)(Data_Function.flip(f))(b);
            return function ($206) {
                return $205(rev($206));
            };
        };
    },
    foldl: function (f) {
        var go = function ($copy_b) {
            return function ($copy_v) {
                var $tco_var_b = $copy_b;
                var $tco_done1 = false;
                var $tco_result;
                function $tco_loop(b, v) {
                    if (v instanceof Nil) {
                        $tco_done1 = true;
                        return b;
                    };
                    if (v instanceof Cons) {
                        $tco_var_b = f(b)(v.value0);
                        $copy_v = v.value1;
                        return;
                    };
                    throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [ v.constructor.name ]);
                };
                while (!$tco_done1) {
                    $tco_result = $tco_loop($tco_var_b, $copy_v);
                };
                return $tco_result;
            };
        };
        return go;
    },
    foldMap: function (dictMonoid) {
        return function (f) {
            return Data_Foldable.foldl(foldableList)(function (acc) {
                var $207 = Data_Semigroup.append(dictMonoid.Semigroup0())(acc);
                return function ($208) {
                    return $207(f($208));
                };
            })(Data_Monoid.mempty(dictMonoid));
        };
    }
};
var foldableNonEmptyList = /* #__PURE__ */ Data_NonEmpty.foldableNonEmpty(foldableList);
var foldableWithIndexList = {
    foldrWithIndex: function (f) {
        return function (b) {
            return function (xs) {
                var v = (function () {
                    
                    // As we create our reversed list, we count elements.
var rev = Data_Foldable.foldl(foldableList)(function (v1) {
                        return function (a) {
                            return new Data_Tuple.Tuple(v1.value0 + 1 | 0, new Cons(a, v1.value1));
                        };
                    });
                    return rev(new Data_Tuple.Tuple(0, Nil.value))(xs);
                })();
                return Data_Tuple.snd(Data_Foldable.foldl(foldableList)(function (v1) {
                    return function (a) {
                        return new Data_Tuple.Tuple(v1.value0 - 1 | 0, f(v1.value0 - 1 | 0)(a)(v1.value1));
                    };
                })(new Data_Tuple.Tuple(v.value0, b))(v.value1));
            };
        };
    },
    foldlWithIndex: function (f) {
        return function (acc) {
            var $209 = Data_Foldable.foldl(foldableList)(function (v) {
                return function (a) {
                    return new Data_Tuple.Tuple(v.value0 + 1 | 0, f(v.value0)(v.value1)(a));
                };
            })(new Data_Tuple.Tuple(0, acc));
            return function ($210) {
                return Data_Tuple.snd($209($210));
            };
        };
    },
    foldMapWithIndex: function (dictMonoid) {
        return function (f) {
            return Data_FoldableWithIndex.foldlWithIndex(foldableWithIndexList)(function (i) {
                return function (acc) {
                    var $211 = Data_Semigroup.append(dictMonoid.Semigroup0())(acc);
                    var $212 = f(i);
                    return function ($213) {
                        return $211($212($213));
                    };
                };
            })(Data_Monoid.mempty(dictMonoid));
        };
    },
    Foldable0: function () {
        return foldableList;
    }
};
var foldableWithIndexNonEmptyList = {
    foldMapWithIndex: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return Data_FoldableWithIndex.foldMapWithIndex(Data_NonEmpty.foldableWithIndexNonEmpty(foldableWithIndexList))(dictMonoid)((function () {
                    var $214 = Data_Maybe.maybe(0)(Data_Semiring.add(Data_Semiring.semiringInt)(1));
                    return function ($215) {
                        return f($214($215));
                    };
                })())(v);
            };
        };
    },
    foldlWithIndex: function (f) {
        return function (b) {
            return function (v) {
                return Data_FoldableWithIndex.foldlWithIndex(Data_NonEmpty.foldableWithIndexNonEmpty(foldableWithIndexList))((function () {
                    var $216 = Data_Maybe.maybe(0)(Data_Semiring.add(Data_Semiring.semiringInt)(1));
                    return function ($217) {
                        return f($216($217));
                    };
                })())(b)(v);
            };
        };
    },
    foldrWithIndex: function (f) {
        return function (b) {
            return function (v) {
                return Data_FoldableWithIndex.foldrWithIndex(Data_NonEmpty.foldableWithIndexNonEmpty(foldableWithIndexList))((function () {
                    var $218 = Data_Maybe.maybe(0)(Data_Semiring.add(Data_Semiring.semiringInt)(1));
                    return function ($219) {
                        return f($218($219));
                    };
                })())(b)(v);
            };
        };
    },
    Foldable0: function () {
        return foldableNonEmptyList;
    }
};
var functorWithIndexList = {
    mapWithIndex: function (f) {
        return Data_FoldableWithIndex.foldrWithIndex(foldableWithIndexList)(function (i) {
            return function (x) {
                return function (acc) {
                    return new Cons(f(i)(x), acc);
                };
            };
        })(Nil.value);
    },
    Functor0: function () {
        return functorList;
    }
};
var functorWithIndexNonEmptyList = {
    mapWithIndex: function (fn) {
        return function (v) {
            return Data_FunctorWithIndex.mapWithIndex(Data_NonEmpty.functorWithIndex(functorWithIndexList))((function () {
                var $220 = Data_Maybe.maybe(0)(Data_Semiring.add(Data_Semiring.semiringInt)(1));
                return function ($221) {
                    return fn($220($221));
                };
            })())(v);
        };
    },
    Functor0: function () {
        return functorNonEmptyList;
    }
};
var semigroupList = {
    append: function (xs) {
        return function (ys) {
            return Data_Foldable.foldr(foldableList)(Cons.create)(ys)(xs);
        };
    }
};
var monoidList = /* #__PURE__ */ (function () {
    return {
        mempty: Nil.value,
        Semigroup0: function () {
            return semigroupList;
        }
    };
})();
var semigroupNonEmptyList = {
    append: function (v) {
        return function (as$prime) {
            return new Data_NonEmpty.NonEmpty(v.value0, Data_Semigroup.append(semigroupList)(v.value1)(toList(as$prime)));
        };
    }
};
var showList = function (dictShow) {
    return {
        show: function (v) {
            if (v instanceof Nil) {
                return "Nil";
            };
            return "(" + (Data_Foldable.intercalate(foldableList)(Data_Monoid.monoidString)(" : ")(Data_Functor.map(functorList)(Data_Show.show(dictShow))(v)) + " : Nil)");
        }
    };
};
var showNonEmptyList = function (dictShow) {
    return {
        show: function (v) {
            return "(NonEmptyList " + (Data_Show.show(Data_NonEmpty.showNonEmpty(dictShow)(showList(dictShow)))(v) + ")");
        }
    };
};
var traversableList = {
    traverse: function (dictApplicative) {
        return function (f) {
            var $222 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value));
            var $223 = Data_Foldable.foldl(foldableList)(function (acc) {
                var $225 = Control_Apply.lift2(dictApplicative.Apply0())(Data_Function.flip(Cons.create))(acc);
                return function ($226) {
                    return $225(f($226));
                };
            })(Control_Applicative.pure(dictApplicative)(Nil.value));
            return function ($224) {
                return $222($223($224));
            };
        };
    },
    sequence: function (dictApplicative) {
        return Data_Traversable.traverse(traversableList)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
    },
    Functor0: function () {
        return functorList;
    },
    Foldable1: function () {
        return foldableList;
    }
};
var traversableNonEmptyList = /* #__PURE__ */ Data_NonEmpty.traversableNonEmpty(traversableList);
var traversableWithIndexList = {
    traverseWithIndex: function (dictApplicative) {
        return function (f) {
            var rev = Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value);
            var $227 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(rev);
            var $228 = Data_FoldableWithIndex.foldlWithIndex(foldableWithIndexList)(function (i) {
                return function (acc) {
                    var $230 = Control_Apply.lift2(dictApplicative.Apply0())(Data_Function.flip(Cons.create))(acc);
                    var $231 = f(i);
                    return function ($232) {
                        return $230($231($232));
                    };
                };
            })(Control_Applicative.pure(dictApplicative)(Nil.value));
            return function ($229) {
                return $227($228($229));
            };
        };
    },
    FunctorWithIndex0: function () {
        return functorWithIndexList;
    },
    FoldableWithIndex1: function () {
        return foldableWithIndexList;
    },
    Traversable2: function () {
        return traversableList;
    }
};
var traversableWithIndexNonEmptyList = {
    traverseWithIndex: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(NonEmptyList)(Data_TraversableWithIndex.traverseWithIndex(Data_NonEmpty.traversableWithIndexNonEmpty(traversableWithIndexList))(dictApplicative)((function () {
                    var $233 = Data_Maybe.maybe(0)(Data_Semiring.add(Data_Semiring.semiringInt)(1));
                    return function ($234) {
                        return f($233($234));
                    };
                })())(v));
            };
        };
    },
    FunctorWithIndex0: function () {
        return functorWithIndexNonEmptyList;
    },
    FoldableWithIndex1: function () {
        return foldableWithIndexNonEmptyList;
    },
    Traversable2: function () {
        return traversableNonEmptyList;
    }
};
var unfoldable1List = {
    unfoldr1: function (f) {
        return function (b) {
            var go = function ($copy_source) {
                return function ($copy_memo) {
                    var $tco_var_source = $copy_source;
                    var $tco_done = false;
                    var $tco_result;
                    function $tco_loop(source, memo) {
                        var v = f(source);
                        if (v.value1 instanceof Data_Maybe.Just) {
                            $tco_var_source = v.value1.value0;
                            $copy_memo = new Cons(v.value0, memo);
                            return;
                        };
                        if (v.value1 instanceof Data_Maybe.Nothing) {
                            $tco_done = true;
                            return Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value)(new Cons(v.value0, memo));
                        };
                        throw new Error("Failed pattern match at Data.List.Types (line 135, column 22 - line 137, column 61): " + [ v.constructor.name ]);
                    };
                    while (!$tco_done) {
                        $tco_result = $tco_loop($tco_var_source, $copy_memo);
                    };
                    return $tco_result;
                };
            };
            return go(b)(Nil.value);
        };
    }
};
var unfoldableList = {
    unfoldr: function (f) {
        return function (b) {
            var go = function ($copy_source) {
                return function ($copy_memo) {
                    var $tco_var_source = $copy_source;
                    var $tco_done = false;
                    var $tco_result;
                    function $tco_loop(source, memo) {
                        var v = f(source);
                        if (v instanceof Data_Maybe.Nothing) {
                            $tco_done = true;
                            return Data_Foldable.foldl(foldableList)(Data_Function.flip(Cons.create))(Nil.value)(memo);
                        };
                        if (v instanceof Data_Maybe.Just) {
                            $tco_var_source = v.value0.value1;
                            $copy_memo = new Cons(v.value0.value0, memo);
                            return;
                        };
                        throw new Error("Failed pattern match at Data.List.Types (line 142, column 22 - line 144, column 52): " + [ v.constructor.name ]);
                    };
                    while (!$tco_done) {
                        $tco_result = $tco_loop($tco_var_source, $copy_memo);
                    };
                    return $tco_result;
                };
            };
            return go(b)(Nil.value);
        };
    },
    Unfoldable10: function () {
        return unfoldable1List;
    }
};
var unfoldable1NonEmptyList = /* #__PURE__ */ Data_NonEmpty.unfoldable1NonEmpty(unfoldableList);
var foldable1NonEmptyList = /* #__PURE__ */ Data_NonEmpty.foldable1NonEmpty(foldableList);
var extendNonEmptyList = {
    extend: function (f) {
        return function (v) {
            var go = function (a) {
                return function (v1) {
                    return {
                        val: new Cons(f(new Data_NonEmpty.NonEmpty(a, v1.acc)), v1.val),
                        acc: new Cons(a, v1.acc)
                    };
                };
            };
            return new Data_NonEmpty.NonEmpty(f(v), (Data_Foldable.foldr(foldableList)(go)({
                val: Nil.value,
                acc: Nil.value
            })(v.value1)).val);
        };
    },
    Functor0: function () {
        return functorNonEmptyList;
    }
};
var extendList = {
    extend: function (v) {
        return function (v1) {
            if (v1 instanceof Nil) {
                return Nil.value;
            };
            if (v1 instanceof Cons) {
                var go = function (a$prime) {
                    return function (v2) {
                        var acc$prime = new Cons(a$prime, v2.acc);
                        return {
                            val: new Cons(v(acc$prime), v2.val),
                            acc: acc$prime
                        };
                    };
                };
                return new Cons(v(v1), (Data_Foldable.foldr(foldableList)(go)({
                    val: Nil.value,
                    acc: Nil.value
                })(v1.value1)).val);
            };
            throw new Error("Failed pattern match at Data.List.Types (line 180, column 1 - line 187, column 42): " + [ v.constructor.name, v1.constructor.name ]);
        };
    },
    Functor0: function () {
        return functorList;
    }
};
var eq1List = {
    eq1: function (dictEq) {
        return function (xs) {
            return function (ys) {
                var go = function ($copy_v) {
                    return function ($copy_v1) {
                        return function ($copy_v2) {
                            var $tco_var_v = $copy_v;
                            var $tco_var_v1 = $copy_v1;
                            var $tco_done = false;
                            var $tco_result;
                            function $tco_loop(v, v1, v2) {
                                if (!v2) {
                                    $tco_done = true;
                                    return false;
                                };
                                if (v instanceof Nil && v1 instanceof Nil) {
                                    $tco_done = true;
                                    return v2;
                                };
                                if (v instanceof Cons && v1 instanceof Cons) {
                                    $tco_var_v = v.value1;
                                    $tco_var_v1 = v1.value1;
                                    $copy_v2 = v2 && Data_Eq.eq(dictEq)(v1.value0)(v.value0);
                                    return;
                                };
                                $tco_done = true;
                                return false;
                            };
                            while (!$tco_done) {
                                $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
                            };
                            return $tco_result;
                        };
                    };
                };
                return go(xs)(ys)(true);
            };
        };
    }
};
var eq1NonEmptyList = /* #__PURE__ */ Data_NonEmpty.eq1NonEmpty(eq1List);
var eqList = function (dictEq) {
    return {
        eq: Data_Eq.eq1(eq1List)(dictEq)
    };
};
var eqNonEmptyList = function (dictEq) {
    return Data_NonEmpty.eqNonEmpty(eq1List)(dictEq);
};
var ord1List = {
    compare1: function (dictOrd) {
        return function (xs) {
            return function (ys) {
                var go = function ($copy_v) {
                    return function ($copy_v1) {
                        var $tco_var_v = $copy_v;
                        var $tco_done = false;
                        var $tco_result;
                        function $tco_loop(v, v1) {
                            if (v instanceof Nil && v1 instanceof Nil) {
                                $tco_done = true;
                                return Data_Ordering.EQ.value;
                            };
                            if (v instanceof Nil) {
                                $tco_done = true;
                                return Data_Ordering.LT.value;
                            };
                            if (v1 instanceof Nil) {
                                $tco_done = true;
                                return Data_Ordering.GT.value;
                            };
                            if (v instanceof Cons && v1 instanceof Cons) {
                                var v2 = Data_Ord.compare(dictOrd)(v.value0)(v1.value0);
                                if (v2 instanceof Data_Ordering.EQ) {
                                    $tco_var_v = v.value1;
                                    $copy_v1 = v1.value1;
                                    return;
                                };
                                $tco_done = true;
                                return v2;
                            };
                            throw new Error("Failed pattern match at Data.List.Types (line 60, column 5 - line 60, column 20): " + [ v.constructor.name, v1.constructor.name ]);
                        };
                        while (!$tco_done) {
                            $tco_result = $tco_loop($tco_var_v, $copy_v1);
                        };
                        return $tco_result;
                    };
                };
                return go(xs)(ys);
            };
        };
    },
    Eq10: function () {
        return eq1List;
    }
};
var ord1NonEmptyList = /* #__PURE__ */ Data_NonEmpty.ord1NonEmpty(ord1List);
var ordList = function (dictOrd) {
    return {
        compare: Data_Ord.compare1(ord1List)(dictOrd),
        Eq0: function () {
            return eqList(dictOrd.Eq0());
        }
    };
};
var ordNonEmptyList = function (dictOrd) {
    return Data_NonEmpty.ordNonEmpty(ord1List)(dictOrd);
};
var comonadNonEmptyList = {
    extract: function (v) {
        return v.value0;
    },
    Extend0: function () {
        return extendNonEmptyList;
    }
};
var applyList = {
    apply: function (v) {
        return function (v1) {
            if (v instanceof Nil) {
                return Nil.value;
            };
            if (v instanceof Cons) {
                return Data_Semigroup.append(semigroupList)(Data_Functor.map(functorList)(v.value0)(v1))(Control_Apply.apply(applyList)(v.value1)(v1));
            };
            throw new Error("Failed pattern match at Data.List.Types (line 157, column 1 - line 159, column 48): " + [ v.constructor.name, v1.constructor.name ]);
        };
    },
    Functor0: function () {
        return functorList;
    }
};
var applyNonEmptyList = {
    apply: function (v) {
        return function (v1) {
            return new Data_NonEmpty.NonEmpty(v.value0(v1.value0), Data_Semigroup.append(semigroupList)(Control_Apply.apply(applyList)(v.value1)(new Cons(v1.value0, Nil.value)))(Control_Apply.apply(applyList)(new Cons(v.value0, v.value1))(v1.value1)));
        };
    },
    Functor0: function () {
        return functorNonEmptyList;
    }
};
var bindList = {
    bind: function (v) {
        return function (v1) {
            if (v instanceof Nil) {
                return Nil.value;
            };
            if (v instanceof Cons) {
                return Data_Semigroup.append(semigroupList)(v1(v.value0))(Control_Bind.bind(bindList)(v.value1)(v1));
            };
            throw new Error("Failed pattern match at Data.List.Types (line 164, column 1 - line 166, column 37): " + [ v.constructor.name, v1.constructor.name ]);
        };
    },
    Apply0: function () {
        return applyList;
    }
};
var bindNonEmptyList = {
    bind: function (v) {
        return function (f) {
            var v1 = f(v.value0);
            return new Data_NonEmpty.NonEmpty(v1.value0, Data_Semigroup.append(semigroupList)(v1.value1)(Control_Bind.bind(bindList)(v.value1)(function ($235) {
                return toList(f($235));
            })));
        };
    },
    Apply0: function () {
        return applyNonEmptyList;
    }
};
var applicativeList = {
    pure: function (a) {
        return new Cons(a, Nil.value);
    },
    Apply0: function () {
        return applyList;
    }
};
var monadList = {
    Applicative0: function () {
        return applicativeList;
    },
    Bind1: function () {
        return bindList;
    }
};
var altNonEmptyList = {
    alt: /* #__PURE__ */ Data_Semigroup.append(semigroupNonEmptyList),
    Functor0: function () {
        return functorNonEmptyList;
    }
};
var altList = {
    alt: /* #__PURE__ */ Data_Semigroup.append(semigroupList),
    Functor0: function () {
        return functorList;
    }
};
var plusList = /* #__PURE__ */ (function () {
    return {
        empty: Nil.value,
        Alt0: function () {
            return altList;
        }
    };
})();
var alternativeList = {
    Applicative0: function () {
        return applicativeList;
    },
    Plus1: function () {
        return plusList;
    }
};
var monadPlusList = {
    Monad0: function () {
        return monadList;
    },
    Alternative1: function () {
        return alternativeList;
    }
};
var applicativeNonEmptyList = {
    pure: /* #__PURE__ */ (function () {
        var $236 = Data_NonEmpty.singleton(plusList);
        return function ($237) {
            return NonEmptyList($236($237));
        };
    })(),
    Apply0: function () {
        return applyNonEmptyList;
    }
};
var monadNonEmptyList = {
    Applicative0: function () {
        return applicativeNonEmptyList;
    },
    Bind1: function () {
        return bindNonEmptyList;
    }
};
var traversable1NonEmptyList = {
    traverse1: function (dictApply) {
        return function (f) {
            return function (v) {
                return Data_Functor.mapFlipped(dictApply.Functor0())(Data_Foldable.foldl(foldableList)(function (acc) {
                    var $238 = Control_Apply.lift2(dictApply)(Data_Function.flip(nelCons))(acc);
                    return function ($239) {
                        return $238(f($239));
                    };
                })(Data_Functor.map(dictApply.Functor0())(Control_Applicative.pure(applicativeNonEmptyList))(f(v.value0)))(v.value1))(function (v1) {
                    return Data_Foldable.foldl(foldableList)(Data_Function.flip(nelCons))(Control_Applicative.pure(applicativeNonEmptyList)(v1.value0))(v1.value1);
                });
            };
        };
    },
    sequence1: function (dictApply) {
        return Data_Semigroup_Traversable.traverse1(traversable1NonEmptyList)(dictApply)(Control_Category.identity(Control_Category.categoryFn));
    },
    Foldable10: function () {
        return foldable1NonEmptyList;
    },
    Traversable1: function () {
        return traversableNonEmptyList;
    }
};
export {
    Nil,
    Cons,
    NonEmptyList,
    toList,
    nelCons,
    showList,
    eqList,
    eq1List,
    ordList,
    ord1List,
    semigroupList,
    monoidList,
    functorList,
    functorWithIndexList,
    foldableList,
    foldableWithIndexList,
    unfoldable1List,
    unfoldableList,
    traversableList,
    traversableWithIndexList,
    applyList,
    applicativeList,
    bindList,
    monadList,
    altList,
    plusList,
    alternativeList,
    monadPlusList,
    extendList,
    newtypeNonEmptyList,
    eqNonEmptyList,
    ordNonEmptyList,
    eq1NonEmptyList,
    ord1NonEmptyList,
    showNonEmptyList,
    functorNonEmptyList,
    applyNonEmptyList,
    applicativeNonEmptyList,
    bindNonEmptyList,
    monadNonEmptyList,
    altNonEmptyList,
    extendNonEmptyList,
    comonadNonEmptyList,
    semigroupNonEmptyList,
    foldableNonEmptyList,
    traversableNonEmptyList,
    foldable1NonEmptyList,
    unfoldable1NonEmptyList,
    functorWithIndexNonEmptyList,
    foldableWithIndexNonEmptyList,
    traversableWithIndexNonEmptyList,
    traversable1NonEmptyList
};
