import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Gen_Class from "../Control.Monad.Gen.Class/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Monoid_Additive from "../Data.Monoid.Additive/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Semigroup_Foldable from "../Data.Semigroup.Foldable/index.js";
import * as Data_Semigroup_Last from "../Data.Semigroup.Last/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
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
var Nil = /* #__PURE__ */ (function () {
    function Nil() {

    };
    Nil.value = new Nil();
    return Nil;
})();
var FreqSemigroup = function (x) {
    return x;
};

// | Creates a generator that produces unfoldable structures based on an
// | existing generator for the elements.
// |
// | The size of the unfoldable will be determined by the current size state
// | for the generator. To generate an unfoldable structure of a particular
// | size, use the `resize` function from the `MonadGen` class first.
var unfoldable = function (dictMonadRec) {
    return function (dictMonadGen) {
        return function (dictUnfoldable) {
            return function (gen) {
                var unfold = function (v) {
                    if (v instanceof Nil) {
                        return Data_Maybe.Nothing.value;
                    };
                    if (v instanceof Cons) {
                        return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0, v.value1));
                    };
                    throw new Error("Failed pattern match at Control.Monad.Gen (line 102, column 12 - line 104, column 35): " + [ v.constructor.name ]);
                };
                var loopGen = function (v) {
                    if (v.value1 <= 0) {
                        return Control_Applicative.pure((dictMonadGen.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Done(v.value0));
                    };
                    if (Data_Boolean.otherwise) {
                        return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(gen)(function (x) {
                            return Control_Applicative.pure((dictMonadGen.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Loop(new Data_Tuple.Tuple(new Cons(x, v.value0), v.value1 - 1 | 0)));
                        });
                    };
                    throw new Error("Failed pattern match at Control.Monad.Gen (line 94, column 3 - line 94, column 68): " + [ v.constructor.name ]);
                };
                return Data_Functor.map((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(Data_Unfoldable.unfoldr(dictUnfoldable)(unfold))(Control_Monad_Gen_Class.sized(dictMonadGen)((function () {
                    var $56 = Control_Monad_Rec_Class.tailRecM(dictMonadRec)(loopGen);
                    var $57 = Data_Tuple.Tuple.create(Nil.value);
                    return function ($58) {
                        return $56($57($58));
                    };
                })()));
            };
        };
    };
};
var semigroupFreqSemigroup = {
    append: function (v) {
        return function (v1) {
            return function (pos) {
                var v2 = v(pos);
                if (v2.value0 instanceof Data_Maybe.Just) {
                    return v1(v2.value0.value0);
                };
                return v2;
            };
        };
    }
};
var getFreqVal = function (v) {
    return function ($59) {
        return Data_Tuple.snd(v($59));
    };
};

// | Internal: get the Foldable element at index i.
// | If the index is <= 0, return the first element.
// | If it's >= length, return the last.
var fromIndex = function (dictFoldable1) {
    return function (i) {
        return function (xs) {
            var go = function ($copy_v) {
                return function ($copy_v1) {
                    var $tco_var_v = $copy_v;
                    var $tco_done = false;
                    var $tco_result;
                    function $tco_loop(v, v1) {
                        if (v1 instanceof Cons && v1.value1 instanceof Nil) {
                            $tco_done = true;
                            return v1.value0;
                        };
                        if (v1 instanceof Cons && v <= 0) {
                            $tco_done = true;
                            return v1.value0;
                        };
                        if (v1 instanceof Cons) {
                            $tco_var_v = v - 1 | 0;
                            $copy_v1 = v1.value1;
                            return;
                        };
                        if (v1 instanceof Nil) {
                            $tco_done = true;
                            return Data_Newtype.un()(Data_Semigroup_Last.Last)(Data_Semigroup_Foldable.foldMap1(dictFoldable1)(Data_Semigroup_Last.semigroupLast)(Data_Semigroup_Last.Last)(xs));
                        };
                        throw new Error("Failed pattern match at Control.Monad.Gen (line 128, column 5 - line 128, column 26): " + [ v.constructor.name, v1.constructor.name ]);
                    };
                    while (!$tco_done) {
                        $tco_result = $tco_loop($tco_var_v, $copy_v1);
                    };
                    return $tco_result;
                };
            };
            return go(i)(Data_Foldable.foldr(dictFoldable1.Foldable0())(Cons.create)(Nil.value)(xs));
        };
    };
};

// | Creates a generator that outputs a value chosen from a selection of
// | existing generators with uniform probability.
var oneOf = function (dictMonadGen) {
    return function (dictFoldable1) {
        return function (xs) {
            return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(Control_Monad_Gen_Class.chooseInt(dictMonadGen)(0)(Data_Foldable.length(dictFoldable1.Foldable0())(Data_Semiring.semiringInt)(xs) - 1 | 0))(function (n) {
                return fromIndex(dictFoldable1)(n)(xs);
            });
        };
    };
};
var freqSemigroup = function (v) {
    return function (pos) {
        var $49 = pos >= v.value0;
        if ($49) {
            return new Data_Tuple.Tuple(new Data_Maybe.Just(pos - v.value0), v.value1);
        };
        return new Data_Tuple.Tuple(Data_Maybe.Nothing.value, v.value1);
    };
};

// | Creates a generator that outputs a value chosen from a selection of
// | existing generators, where the selection has weight values for the
// | probability of choice for each generator. The probability values will be
// | normalised.
var frequency = function (dictMonadGen) {
    return function (dictFoldable1) {
        return function (xs) {
            var total = Data_Newtype.alaF()()()()(Data_Monoid_Additive.Additive)(Data_Foldable.foldMap(dictFoldable1.Foldable0())(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringNumber)))(Data_Tuple.fst)(xs);
            return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(Control_Monad_Gen_Class.chooseFloat(dictMonadGen)(0.0)(total))(getFreqVal(Data_Semigroup_Foldable.foldMap1(dictFoldable1)(semigroupFreqSemigroup)(freqSemigroup)(xs)));
        };
    };
};

// | Creates a generator that repeatedly run another generator until it produces
// | `Just` node. This will never halt if the input generator always produces `Nothing`.
var filtered = function (dictMonadRec) {
    return function (dictMonadGen) {
        return function (gen) {
            var go = function (v) {
                return Data_Functor.mapFlipped((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(gen)(function (a) {
                    if (a instanceof Data_Maybe.Nothing) {
                        return new Control_Monad_Rec_Class.Loop(Data_Unit.unit);
                    };
                    if (a instanceof Data_Maybe.Just) {
                        return new Control_Monad_Rec_Class.Done(a.value0);
                    };
                    throw new Error("Failed pattern match at Control.Monad.Gen (line 118, column 24 - line 120, column 23): " + [ a.constructor.name ]);
                });
            };
            return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go)(Data_Unit.unit);
        };
    };
};

// | Creates a generator that repeatedly run another generator until its output
// | matches a given predicate. This will never halt if the predicate always
// | fails.
var suchThat = function (dictMonadRec) {
    return function (dictMonadGen) {
        return function (gen) {
            return function (pred) {
                return filtered(dictMonadRec)(dictMonadGen)(Data_Functor.mapFlipped((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(gen)(function (a) {
                    var $54 = pred(a);
                    if ($54) {
                        return new Data_Maybe.Just(a);
                    };
                    return Data_Maybe.Nothing.value;
                }));
            };
        };
    };
};

// | Creates a generator that outputs a value chosen from a selection with
// | uniform probability.
var elements = function (dictMonadGen) {
    return function (dictFoldable1) {
        return function (xs) {
            return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(Control_Monad_Gen_Class.chooseInt(dictMonadGen)(0)(Data_Foldable.length(dictFoldable1.Foldable0())(Data_Semiring.semiringInt)(xs) - 1 | 0))(function (n) {
                return Control_Applicative.pure((dictMonadGen.Monad0()).Applicative0())(fromIndex(dictFoldable1)(n)(xs));
            });
        };
    };
};

// | Creates a generator that outputs a value chosen from one of two existing
// | existing generators with even probability.
var choose = function (dictMonadGen) {
    return function (genA) {
        return function (genB) {
            return Control_Bind.bind((dictMonadGen.Monad0()).Bind1())(Control_Monad_Gen_Class.chooseBool(dictMonadGen))(function (v) {
                if (v) {
                    return genA;
                };
                return genB;
            });
        };
    };
};
export {
    choose,
    oneOf,
    frequency,
    elements,
    unfoldable,
    suchThat,
    filtered
};
export {
    chooseBool,
    chooseFloat,
    chooseInt,
    resize,
    sized
} from "../Control.Monad.Gen.Class/index.js";
