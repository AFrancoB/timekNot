// | This module defines the list monad transformer, `ListT`.
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Monad from "../Control.Monad/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Control_Monad_Trans_Class from "../Control.Monad.Trans.Class/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Lazy from "../Data.Lazy/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Effect_Class from "../Effect.Class/index.js";

// | The result of a single step in a `ListT` computation. Either:
// |
// | - Computation has finished (`Done`), or
// | - A result has been returned, along with the next part of the computation (`Yield`).
// |
// | The `Skip` constructor allows us to avoid traversing lists during certain operations.
var Yield = /* #__PURE__ */ (function () {
    function Yield(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Yield.create = function (value0) {
        return function (value1) {
            return new Yield(value0, value1);
        };
    };
    return Yield;
})();

// | The result of a single step in a `ListT` computation. Either:
// |
// | - Computation has finished (`Done`), or
// | - A result has been returned, along with the next part of the computation (`Yield`).
// |
// | The `Skip` constructor allows us to avoid traversing lists during certain operations.
var Skip = /* #__PURE__ */ (function () {
    function Skip(value0) {
        this.value0 = value0;
    };
    Skip.create = function (value0) {
        return new Skip(value0);
    };
    return Skip;
})();

// | The result of a single step in a `ListT` computation. Either:
// |
// | - Computation has finished (`Done`), or
// | - A result has been returned, along with the next part of the computation (`Yield`).
// |
// | The `Skip` constructor allows us to avoid traversing lists during certain operations.
var Done = /* #__PURE__ */ (function () {
    function Done() {

    };
    Done.value = new Done();
    return Done;
})();

// | The list monad transformer.
// |
// | This monad transformer extends the base monad with _non-determinism_.
// | That is, the transformed monad supports the same effects as the base monad
// | but with multiple return values.
var ListT = function (x) {
    return x;
};

// | Defer evaluation of a list.
var wrapLazy = function (dictApplicative) {
    return function (v) {
        return Control_Applicative.pure(dictApplicative)(new Skip(v));
    };
};

// | Lift a computation from the base monad.
var wrapEffect = function (dictFunctor) {
    return function (v) {
        return Data_Functor.map(dictFunctor)(function ($180) {
            return Skip.create(Data_Lazy.defer(Data_Function["const"]($180)));
        })(v);
    };
};

// | Unfold a list using an effectful generator function.
var unfold = function (dictMonad) {
    return function (f) {
        return function (z) {
            var g = function (v) {
                if (v instanceof Data_Maybe.Just) {
                    return new Yield(v.value0.value1, Data_Lazy.defer(function (v1) {
                        return unfold(dictMonad)(f)(v.value0.value0);
                    }));
                };
                if (v instanceof Data_Maybe.Nothing) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans (line 129, column 3 - line 129, column 60): " + [ v.constructor.name ]);
            };
            return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(g)(f(z));
        };
    };
};

// | Perform the first step of a computation in the `ListT` monad.
var uncons = function (dictMonad) {
    return function (v) {
        var g = function (v1) {
            if (v1 instanceof Yield) {
                return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Maybe.Just(new Data_Tuple.Tuple(v1.value0, Data_Lazy.force(v1.value1))));
            };
            if (v1 instanceof Skip) {
                return uncons(dictMonad)(Data_Lazy.force(v1.value0));
            };
            if (v1 instanceof Done) {
                return Control_Applicative.pure(dictMonad.Applicative0())(Data_Maybe.Nothing.value);
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans (line 194, column 3 - line 194, column 50): " + [ v1.constructor.name ]);
        };
        return Control_Bind.bind(dictMonad.Bind1())(v)(g);
    };
};

// | Extract all but the first element of a list.
var tail = function (dictMonad) {
    return function (l) {
        return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Tuple.snd))(uncons(dictMonad)(l));
    };
};

// | Lift a computation on list steps to a computation on whole lists.
var stepMap = function (dictFunctor) {
    return function (f) {
        return function (v) {
            return Data_Functor.map(dictFunctor)(f)(v);
        };
    };
};

// | Take elements from the front of a list while a predicate holds.
var takeWhile = function (dictApplicative) {
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                var $97 = f(v.value0);
                if ($97) {
                    return new Yield(v.value0, Data_Functor.map(Data_Lazy.functorLazy)(takeWhile(dictApplicative)(f))(v.value1));
                };
                return Done.value;
            };
            if (v instanceof Skip) {
                return new Skip(Data_Functor.map(Data_Lazy.functorLazy)(takeWhile(dictApplicative)(f))(v.value0));
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans (line 153, column 3 - line 153, column 68): " + [ v.constructor.name ]);
        };
        return stepMap((dictApplicative.Apply0()).Functor0())(g);
    };
};

// | Fold a list from the left, accumulating the list of results using the specified function.
var scanl = function (dictMonad) {
    return function (f) {
        return function (b) {
            return function (l) {
                var g = function (v) {
                    var h = function (v1) {
                        if (v1 instanceof Yield) {
                            var b$prime$prime = f(v.value0)(v1.value0);
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(new Data_Tuple.Tuple(b$prime$prime, Data_Lazy.force(v1.value1)), v.value0));
                        };
                        if (v1 instanceof Skip) {
                            return new Data_Maybe.Just(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v.value0, Data_Lazy.force(v1.value0)), v.value0));
                        };
                        if (v1 instanceof Done) {
                            return Data_Maybe.Nothing.value;
                        };
                        throw new Error("Failed pattern match at Control.Monad.List.Trans (line 247, column 5 - line 247, column 78): " + [ v1.constructor.name ]);
                    };
                    return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(h)(v.value1);
                };
                return unfold(dictMonad)(g)(new Data_Tuple.Tuple(b, l));
            };
        };
    };
};

// | Prepend an element to a lazily-evaluated list.
var prepend$prime = function (dictApplicative) {
    return function (h) {
        return function (t) {
            return Control_Applicative.pure(dictApplicative)(new Yield(h, t));
        };
    };
};

// | Prepend an element to a list.
var prepend = function (dictApplicative) {
    return function (h) {
        return function (t) {
            return prepend$prime(dictApplicative)(h)(Data_Lazy.defer(Data_Function["const"](t)));
        };
    };
};

// | The empty list.
var nil = function (dictApplicative) {
    return Control_Applicative.pure(dictApplicative)(Done.value);
};

// | Create a list with one element.
var singleton = function (dictApplicative) {
    return function (a) {
        return prepend(dictApplicative)(a)(nil(dictApplicative));
    };
};

// | Take a number of elements from the front of a list.
var take = function (dictApplicative) {
    return function (v) {
        return function (v1) {
            if (v === 0) {
                return nil(dictApplicative);
            };
            var f = function (v2) {
                if (v2 instanceof Yield) {
                    return new Yield(v2.value0, Data_Functor.map(Data_Lazy.functorLazy)(take(dictApplicative)(v - 1 | 0))(v2.value1));
                };
                if (v2 instanceof Skip) {
                    return new Skip(Data_Functor.map(Data_Lazy.functorLazy)(take(dictApplicative)(v))(v2.value0));
                };
                if (v2 instanceof Done) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans (line 146, column 3 - line 146, column 47): " + [ v2.constructor.name ]);
            };
            return stepMap((dictApplicative.Apply0()).Functor0())(f)(v1);
        };
    };
};

// | Zip the elements of two lists, combining elements at the same position from each list.
var zipWith$prime = function (dictMonad) {
    return function (f) {
        var g = function (v) {
            return function (v1) {
                if (v1 instanceof Data_Maybe.Nothing) {
                    return Control_Applicative.pure(dictMonad.Applicative0())(nil(dictMonad.Applicative0()));
                };
                if (v instanceof Data_Maybe.Nothing) {
                    return Control_Applicative.pure(dictMonad.Applicative0())(nil(dictMonad.Applicative0()));
                };
                if (v instanceof Data_Maybe.Just && v1 instanceof Data_Maybe.Just) {
                    return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(Data_Function.flip(prepend$prime(dictMonad.Applicative0()))(Data_Lazy.defer(function (v2) {
                        return zipWith$prime(dictMonad)(f)(v.value0.value1)(v1.value0.value1);
                    })))(f(v.value0.value0)(v1.value0.value0));
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans (line 259, column 3 - line 259, column 25): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
        var loop = function (fa) {
            return function (fb) {
                return wrapEffect(((dictMonad.Bind1()).Apply0()).Functor0())(Control_Bind.bind(dictMonad.Bind1())(uncons(dictMonad)(fa))(function (ua) {
                    return Control_Bind.bind(dictMonad.Bind1())(uncons(dictMonad)(fb))(function (ub) {
                        return g(ua)(ub);
                    });
                }));
            };
        };
        return loop;
    };
};

// | Zip the elements of two lists, combining elements at the same position from each list.
var zipWith = function (dictMonad) {
    return function (f) {
        var g = function (a) {
            return function (b) {
                return Control_Applicative.pure(dictMonad.Applicative0())(f(a)(b));
            };
        };
        return zipWith$prime(dictMonad)(g);
    };
};
var newtypeListT = {
    Coercible0: function () {
        return undefined;
    }
};

// | Apply a function to the elements of a list, keeping only those return values which contain a result.
var mapMaybe = function (dictFunctor) {
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                return Data_Maybe.fromMaybe(Skip.create)(Data_Functor.map(Data_Maybe.functorMaybe)(Yield.create)(f(v.value0)))(Data_Functor.map(Data_Lazy.functorLazy)(mapMaybe(dictFunctor)(f))(v.value1));
            };
            if (v instanceof Skip) {
                return new Skip(Data_Functor.map(Data_Lazy.functorLazy)(mapMaybe(dictFunctor)(f))(v.value0));
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans (line 182, column 3 - line 182, column 72): " + [ v.constructor.name ]);
        };
        return stepMap(dictFunctor)(g);
    };
};

// | Generate an infinite list by iterating a function.
var iterate = function (dictMonad) {
    return function (f) {
        return function (a) {
            var g = function (x) {
                return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Maybe.Just(new Data_Tuple.Tuple(f(x), x)));
            };
            return unfold(dictMonad)(g)(a);
        };
    };
};

// | Generate an infinite list by repeating a value.
var repeat = function (dictMonad) {
    return iterate(dictMonad)(Control_Category.identity(Control_Category.categoryFn));
};

// | Extract the first element of a list.
var head = function (dictMonad) {
    return function (l) {
        return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Tuple.fst))(uncons(dictMonad)(l));
    };
};
var functorListT = function (dictFunctor) {
    return {
        map: function (f) {
            var g = function (v) {
                if (v instanceof Yield) {
                    return new Yield(f(v.value0), Data_Functor.map(Data_Lazy.functorLazy)(Data_Functor.map(functorListT(dictFunctor))(f))(v.value1));
                };
                if (v instanceof Skip) {
                    return new Skip(Data_Functor.map(Data_Lazy.functorLazy)(Data_Functor.map(functorListT(dictFunctor))(f))(v.value0));
                };
                if (v instanceof Done) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans (line 279, column 5 - line 279, column 48): " + [ v.constructor.name ]);
            };
            return stepMap(dictFunctor)(g);
        }
    };
};

// | Lift a computation from the base functor.
var fromEffect = function (dictApplicative) {
    return function (fa) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Function.flip(Yield.create)(Data_Lazy.defer(function (v) {
            return nil(dictApplicative);
        })))(fa);
    };
};
var monadTransListT = {
    lift: function (dictMonad) {
        return fromEffect(dictMonad.Applicative0());
    }
};

// | Fold a list from the left, accumulating the result (effectfully) using the specified function.
// | Uses tail call optimization.
var foldlRec$prime = function (dictMonadRec) {
    return function (f) {
        var loop = function (b) {
            return function (l) {
                var g = function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Done(b));
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())(f(b)(v.value0.value0))(function (b$prime) {
                            return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Loop({
                                a: b$prime,
                                b: v.value0.value1
                            }));
                        });
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans (line 220, column 5 - line 220, column 45): " + [ v.constructor.name ]);
                };
                return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())(uncons(dictMonadRec.Monad0())(l))(g);
            };
        };
        return Control_Monad_Rec_Class.tailRecM2(dictMonadRec)(loop);
    };
};

// | Drain a `ListT`, running it to completion and discarding all values.
// | Stack safe: Uses tail call optimization.
var runListTRec = function (dictMonadRec) {
    return foldlRec$prime(dictMonadRec)(function (v) {
        return function (v1) {
            return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(Data_Unit.unit);
        };
    })(Data_Unit.unit);
};

// | Fold a list from the left, accumulating the result using the specified function.
// | Uses tail call optimization.
var foldlRec = function (dictMonadRec) {
    return function (f) {
        var loop = function (b) {
            return function (l) {
                var g = function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Done(b));
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Loop({
                            a: f(b)(v.value0.value0),
                            b: v.value0.value1
                        }));
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans (line 238, column 7 - line 238, column 47): " + [ v.constructor.name ]);
                };
                return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())(uncons(dictMonadRec.Monad0())(l))(g);
            };
        };
        return Control_Monad_Rec_Class.tailRecM2(dictMonadRec)(loop);
    };
};

// | Fold a list from the left, accumulating the result (effectfully) using the specified function.
var foldl$prime = function (dictMonad) {
    return function (f) {
        var loop = function (b) {
            return function (l) {
                var g = function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return Control_Applicative.pure(dictMonad.Applicative0())(b);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return Control_Bind.bind(dictMonad.Bind1())(f(b)(v.value0.value0))(Data_Function.flip(loop)(v.value0.value1));
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans (line 211, column 5 - line 211, column 35): " + [ v.constructor.name ]);
                };
                return Control_Bind.bind(dictMonad.Bind1())(uncons(dictMonad)(l))(g);
            };
        };
        return loop;
    };
};

// | Drain a `ListT`, running it to completion and discarding all values.
var runListT = function (dictMonad) {
    return foldl$prime(dictMonad)(function (v) {
        return function (v1) {
            return Control_Applicative.pure(dictMonad.Applicative0())(Data_Unit.unit);
        };
    })(Data_Unit.unit);
};

// | Fold a list from the left, accumulating the result using the specified function.
var foldl = function (dictMonad) {
    return function (f) {
        var loop = function (b) {
            return function (l) {
                var g = function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return Control_Applicative.pure(dictMonad.Applicative0())(b);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return loop(f(b)(v.value0.value0))(v.value0.value1);
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans (line 228, column 5 - line 228, column 35): " + [ v.constructor.name ]);
                };
                return Control_Bind.bind(dictMonad.Bind1())(uncons(dictMonad)(l))(g);
            };
        };
        return loop;
    };
};

// | Remove elements from a list for which a predicate fails to hold.
var filter = function (dictFunctor) {
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                var s$prime = Data_Functor.map(Data_Lazy.functorLazy)(filter(dictFunctor)(f))(v.value1);
                var $147 = f(v.value0);
                if ($147) {
                    return new Yield(v.value0, s$prime);
                };
                return new Skip(s$prime);
            };
            if (v instanceof Skip) {
                var s$prime = Data_Functor.map(Data_Lazy.functorLazy)(filter(dictFunctor)(f))(v.value0);
                return new Skip(s$prime);
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans (line 175, column 3 - line 175, column 80): " + [ v.constructor.name ]);
        };
        return stepMap(dictFunctor)(g);
    };
};

// | Drop elements from the front of a list while a predicate holds.
var dropWhile = function (dictApplicative) {
    return function (f) {
        var g = function (v) {
            if (v instanceof Yield) {
                var $152 = f(v.value0);
                if ($152) {
                    return new Skip(Data_Functor.map(Data_Lazy.functorLazy)(dropWhile(dictApplicative)(f))(v.value1));
                };
                return new Yield(v.value0, v.value1);
            };
            if (v instanceof Skip) {
                return new Skip(Data_Functor.map(Data_Lazy.functorLazy)(dropWhile(dictApplicative)(f))(v.value0));
            };
            if (v instanceof Done) {
                return Done.value;
            };
            throw new Error("Failed pattern match at Control.Monad.List.Trans (line 168, column 3 - line 168, column 70): " + [ v.constructor.name ]);
        };
        return stepMap((dictApplicative.Apply0()).Functor0())(g);
    };
};

// | Drop a number of elements from the front of a list.
var drop = function (dictApplicative) {
    return function (v) {
        return function (fa) {
            if (v === 0) {
                return fa;
            };
            var f = function (v1) {
                if (v1 instanceof Yield) {
                    return new Skip(Data_Functor.map(Data_Lazy.functorLazy)(drop(dictApplicative)(v - 1 | 0))(v1.value1));
                };
                if (v1 instanceof Skip) {
                    return new Skip(Data_Functor.map(Data_Lazy.functorLazy)(drop(dictApplicative)(v))(v1.value0));
                };
                if (v1 instanceof Done) {
                    return Done.value;
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans (line 161, column 3 - line 161, column 44): " + [ v1.constructor.name ]);
            };
            return stepMap((dictApplicative.Apply0()).Functor0())(f)(fa);
        };
    };
};

// | Attach an element to the front of a list.
var cons = function (dictApplicative) {
    return function (lh) {
        return function (t) {
            return Control_Applicative.pure(dictApplicative)(new Yield(Data_Lazy.force(lh), t));
        };
    };
};
var unfoldable1ListT = function (dictMonad) {
    return {
        unfoldr1: function (f) {
            return function (b) {
                var go = function (v) {
                    if (v.value1 instanceof Data_Maybe.Nothing) {
                        return singleton(dictMonad.Applicative0())(v.value0);
                    };
                    if (v.value1 instanceof Data_Maybe.Just) {
                        return cons(dictMonad.Applicative0())(Control_Applicative.pure(Data_Lazy.applicativeLazy)(v.value0))(Data_Lazy.defer(function (v1) {
                            return go(f(v.value1.value0));
                        }));
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans (line 293, column 12 - line 295, column 67): " + [ v.constructor.name ]);
                };
                return go(f(b));
            };
        }
    };
};
var unfoldableListT = function (dictMonad) {
    return {
        unfoldr: function (f) {
            return function (b) {
                var go = function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return nil(dictMonad.Applicative0());
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return cons(dictMonad.Applicative0())(Control_Applicative.pure(Data_Lazy.applicativeLazy)(v.value0.value0))(Data_Lazy.defer(function (v1) {
                            return go(f(v.value0.value1));
                        }));
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans (line 286, column 12 - line 288, column 67): " + [ v.constructor.name ]);
                };
                return go(f(b));
            };
        },
        Unfoldable10: function () {
            return unfoldable1ListT(dictMonad);
        }
    };
};
var semigroupListT = function (dictApplicative) {
    return {
        append: concat(dictApplicative)
    };
};

// | Append one list to another.
var concat = function (dictApplicative) {
    return function (x) {
        return function (y) {
            var f = function (v) {
                if (v instanceof Yield) {
                    return new Yield(v.value0, Data_Functor.map(Data_Lazy.functorLazy)(function (v1) {
                        return Data_Semigroup.append(semigroupListT(dictApplicative))(v1)(y);
                    })(v.value1));
                };
                if (v instanceof Skip) {
                    return new Skip(Data_Functor.map(Data_Lazy.functorLazy)(function (v1) {
                        return Data_Semigroup.append(semigroupListT(dictApplicative))(v1)(y);
                    })(v.value0));
                };
                if (v instanceof Done) {
                    return new Skip(Data_Lazy.defer(Data_Function["const"](y)));
                };
                throw new Error("Failed pattern match at Control.Monad.List.Trans (line 105, column 3 - line 105, column 43): " + [ v.constructor.name ]);
            };
            return stepMap((dictApplicative.Apply0()).Functor0())(f)(x);
        };
    };
};
var monoidListT = function (dictApplicative) {
    return {
        mempty: nil(dictApplicative),
        Semigroup0: function () {
            return semigroupListT(dictApplicative);
        }
    };
};

// | Remove elements from a list which do not contain a value.
var catMaybes = function (dictFunctor) {
    return mapMaybe(dictFunctor)(Control_Category.identity(Control_Category.categoryFn));
};
var monadListT = function (dictMonad) {
    return {
        Applicative0: function () {
            return applicativeListT(dictMonad);
        },
        Bind1: function () {
            return bindListT(dictMonad);
        }
    };
};
var bindListT = function (dictMonad) {
    return {
        bind: function (fa) {
            return function (f) {
                var g = function (v) {
                    if (v instanceof Yield) {
                        var h = function (s$prime) {
                            return Data_Semigroup.append(semigroupListT(dictMonad.Applicative0()))(f(v.value0))(Control_Bind.bind(bindListT(dictMonad))(s$prime)(f));
                        };
                        return new Skip(Data_Functor.map(Data_Lazy.functorLazy)(h)(v.value1));
                    };
                    if (v instanceof Skip) {
                        return new Skip(Data_Functor.map(Data_Lazy.functorLazy)(function (v1) {
                            return Control_Bind.bind(bindListT(dictMonad))(v1)(f);
                        })(v.value0));
                    };
                    if (v instanceof Done) {
                        return Done.value;
                    };
                    throw new Error("Failed pattern match at Control.Monad.List.Trans (line 305, column 5 - line 307, column 31): " + [ v.constructor.name ]);
                };
                return stepMap(((dictMonad.Bind1()).Apply0()).Functor0())(g)(fa);
            };
        },
        Apply0: function () {
            return applyListT(dictMonad);
        }
    };
};
var applyListT = function (dictMonad) {
    return {
        apply: Control_Monad.ap(monadListT(dictMonad)),
        Functor0: function () {
            return functorListT(((dictMonad.Bind1()).Apply0()).Functor0());
        }
    };
};
var applicativeListT = function (dictMonad) {
    return {
        pure: singleton(dictMonad.Applicative0()),
        Apply0: function () {
            return applyListT(dictMonad);
        }
    };
};
var monadEffectListT = function (dictMonadEffect) {
    return {
        liftEffect: (function () {
            var $181 = Control_Monad_Trans_Class.lift(monadTransListT)(dictMonadEffect.Monad0());
            var $182 = Effect_Class.liftEffect(dictMonadEffect);
            return function ($183) {
                return $181($182($183));
            };
        })(),
        Monad0: function () {
            return monadListT(dictMonadEffect.Monad0());
        }
    };
};
var altListT = function (dictApplicative) {
    return {
        alt: concat(dictApplicative),
        Functor0: function () {
            return functorListT((dictApplicative.Apply0()).Functor0());
        }
    };
};
var plusListT = function (dictMonad) {
    return {
        empty: nil(dictMonad.Applicative0()),
        Alt0: function () {
            return altListT(dictMonad.Applicative0());
        }
    };
};
var alternativeListT = function (dictMonad) {
    return {
        Applicative0: function () {
            return applicativeListT(dictMonad);
        },
        Plus1: function () {
            return plusListT(dictMonad);
        }
    };
};
var monadPlusListT = function (dictMonad) {
    return {
        Monad0: function () {
            return monadListT(dictMonad);
        },
        Alternative1: function () {
            return alternativeListT(dictMonad);
        }
    };
};
export {
    ListT,
    Yield,
    Skip,
    Done,
    catMaybes,
    cons,
    drop,
    dropWhile,
    filter,
    foldl,
    foldlRec,
    foldl$prime,
    foldlRec$prime,
    fromEffect,
    head,
    iterate,
    mapMaybe,
    nil,
    prepend,
    prepend$prime,
    repeat,
    runListT,
    runListTRec,
    scanl,
    singleton,
    tail,
    take,
    takeWhile,
    uncons,
    unfold,
    wrapEffect,
    wrapLazy,
    zipWith,
    zipWith$prime,
    newtypeListT,
    semigroupListT,
    monoidListT,
    functorListT,
    unfoldableListT,
    unfoldable1ListT,
    applyListT,
    applicativeListT,
    bindListT,
    monadListT,
    monadTransListT,
    altListT,
    plusListT,
    alternativeListT,
    monadPlusListT,
    monadEffectListT
};
export {
    lift
} from "../Control.Monad.Trans.Class/index.js";
