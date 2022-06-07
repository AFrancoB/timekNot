// | A data type and functions for working with ordered pairs.
import * as Control_Lazy from "../Control.Lazy/index.js";
import * as Data_Bounded from "../Data.Bounded/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor_Invariant from "../Data.Functor.Invariant/index.js";
import * as Data_Generic_Rep from "../Data.Generic.Rep/index.js";
import * as Data_HeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Ring from "../Data.Ring/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Unit from "../Data.Unit/index.js";

// | A simple product type for wrapping a pair of component values.
var Tuple = /* #__PURE__ */ (function () {
    function Tuple(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Tuple.create = function (value0) {
        return function (value1) {
            return new Tuple(value0, value1);
        };
    };
    return Tuple;
})();

// | Turn a function of two arguments into a function that expects a tuple.
var uncurry = function (f) {
    return function (v) {
        return f(v.value0)(v.value1);
    };
};

// | Exchange the first and second components of a tuple.
var swap = function (v) {
    return new Tuple(v.value1, v.value0);
};

// | Returns the second component of a tuple.
var snd = function (v) {
    return v.value1;
};

// | Allows `Tuple`s to be rendered as a string with `show` whenever there are
// | `Show` instances for both component types.
var showTuple = function (dictShow) {
    return function (dictShow1) {
        return {
            show: function (v) {
                return "(Tuple " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(dictShow1)(v.value1) + ")")));
            }
        };
    };
};
var semiringTuple = function (dictSemiring) {
    return function (dictSemiring1) {
        return {
            add: function (v) {
                return function (v1) {
                    return new Tuple(Data_Semiring.add(dictSemiring)(v.value0)(v1.value0), Data_Semiring.add(dictSemiring1)(v.value1)(v1.value1));
                };
            },
            one: new Tuple(Data_Semiring.one(dictSemiring), Data_Semiring.one(dictSemiring1)),
            mul: function (v) {
                return function (v1) {
                    return new Tuple(Data_Semiring.mul(dictSemiring)(v.value0)(v1.value0), Data_Semiring.mul(dictSemiring1)(v.value1)(v1.value1));
                };
            },
            zero: new Tuple(Data_Semiring.zero(dictSemiring), Data_Semiring.zero(dictSemiring1))
        };
    };
};
var semigroupoidTuple = {
    compose: function (v) {
        return function (v1) {
            return new Tuple(v1.value0, v.value1);
        };
    }
};

// | The `Semigroup` instance enables use of the associative operator `<>` on
// | `Tuple`s whenever there are `Semigroup` instances for the component
// | types. The `<>` operator is applied pairwise, so:
// | ```purescript
// | (Tuple a1 b1) <> (Tuple a2 b2) = Tuple (a1 <> a2) (b1 <> b2)
// | ```
var semigroupTuple = function (dictSemigroup) {
    return function (dictSemigroup1) {
        return {
            append: function (v) {
                return function (v1) {
                    return new Tuple(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), Data_Semigroup.append(dictSemigroup1)(v.value1)(v1.value1));
                };
            }
        };
    };
};
var ringTuple = function (dictRing) {
    return function (dictRing1) {
        return {
            sub: function (v) {
                return function (v1) {
                    return new Tuple(Data_Ring.sub(dictRing)(v.value0)(v1.value0), Data_Ring.sub(dictRing1)(v.value1)(v1.value1));
                };
            },
            Semiring0: function () {
                return semiringTuple(dictRing.Semiring0())(dictRing1.Semiring0());
            }
        };
    };
};
var monoidTuple = function (dictMonoid) {
    return function (dictMonoid1) {
        return {
            mempty: new Tuple(Data_Monoid.mempty(dictMonoid), Data_Monoid.mempty(dictMonoid1)),
            Semigroup0: function () {
                return semigroupTuple(dictMonoid.Semigroup0())(dictMonoid1.Semigroup0());
            }
        };
    };
};
var heytingAlgebraTuple = function (dictHeytingAlgebra) {
    return function (dictHeytingAlgebra1) {
        return {
            tt: new Tuple(Data_HeytingAlgebra.tt(dictHeytingAlgebra), Data_HeytingAlgebra.tt(dictHeytingAlgebra1)),
            ff: new Tuple(Data_HeytingAlgebra.ff(dictHeytingAlgebra), Data_HeytingAlgebra.ff(dictHeytingAlgebra1)),
            implies: function (v) {
                return function (v1) {
                    return new Tuple(Data_HeytingAlgebra.implies(dictHeytingAlgebra)(v.value0)(v1.value0), Data_HeytingAlgebra.implies(dictHeytingAlgebra1)(v.value1)(v1.value1));
                };
            },
            conj: function (v) {
                return function (v1) {
                    return new Tuple(Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v.value0)(v1.value0), Data_HeytingAlgebra.conj(dictHeytingAlgebra1)(v.value1)(v1.value1));
                };
            },
            disj: function (v) {
                return function (v1) {
                    return new Tuple(Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v.value0)(v1.value0), Data_HeytingAlgebra.disj(dictHeytingAlgebra1)(v.value1)(v1.value1));
                };
            },
            not: function (v) {
                return new Tuple(Data_HeytingAlgebra.not(dictHeytingAlgebra)(v.value0), Data_HeytingAlgebra.not(dictHeytingAlgebra1)(v.value1));
            }
        };
    };
};
var genericTuple = {
    to: function (x) {
        return new Tuple(x.value0, x.value1);
    },
    from: function (x) {
        return new Data_Generic_Rep.Product(x.value0, x.value1);
    }
};

// | The `Functor` instance allows functions to transform the contents of a
// | `Tuple` with the `<$>` operator, applying the function to the second
// | component, so:
// | ```purescript
// | f <$> (Tuple x y) = Tuple x (f y)
// | ````
var functorTuple = {
    map: function (f) {
        return function (m) {
            return new Tuple(m.value0, f(m.value1));
        };
    }
};
var invariantTuple = {
    imap: /* #__PURE__ */ Data_Functor_Invariant.imapF(functorTuple)
};

// | Returns the first component of a tuple.
var fst = function (v) {
    return v.value0;
};
var lazyTuple = function (dictLazy) {
    return function (dictLazy1) {
        return {
            defer: function (f) {
                return new Tuple(Control_Lazy.defer(dictLazy)(function (v) {
                    return fst(f(Data_Unit.unit));
                }), Control_Lazy.defer(dictLazy1)(function (v) {
                    return snd(f(Data_Unit.unit));
                }));
            }
        };
    };
};
var extendTuple = {
    extend: function (f) {
        return function (v) {
            return new Tuple(v.value0, f(v));
        };
    },
    Functor0: function () {
        return functorTuple;
    }
};

// | Allows `Tuple`s to be checked for equality with `==` and `/=` whenever
// | there are `Eq` instances for both component types.
var eqTuple = function (dictEq) {
    return function (dictEq1) {
        return {
            eq: function (x) {
                return function (y) {
                    return Data_Eq.eq(dictEq)(x.value0)(y.value0) && Data_Eq.eq(dictEq1)(x.value1)(y.value1);
                };
            }
        };
    };
};

// | Allows `Tuple`s to be compared with `compare`, `>`, `>=`, `<` and `<=`
// | whenever there are `Ord` instances for both component types. To obtain
// | the result, the `fst`s are `compare`d, and if they are `EQ`ual, the
// | `snd`s are `compare`d.
var ordTuple = function (dictOrd) {
    return function (dictOrd1) {
        return {
            compare: function (x) {
                return function (y) {
                    var v = Data_Ord.compare(dictOrd)(x.value0)(y.value0);
                    if (v instanceof Data_Ordering.LT) {
                        return Data_Ordering.LT.value;
                    };
                    if (v instanceof Data_Ordering.GT) {
                        return Data_Ordering.GT.value;
                    };
                    return Data_Ord.compare(dictOrd1)(x.value1)(y.value1);
                };
            },
            Eq0: function () {
                return eqTuple(dictOrd.Eq0())(dictOrd1.Eq0());
            }
        };
    };
};
var eq1Tuple = function (dictEq) {
    return {
        eq1: function (dictEq1) {
            return Data_Eq.eq(eqTuple(dictEq)(dictEq1));
        }
    };
};
var ord1Tuple = function (dictOrd) {
    return {
        compare1: function (dictOrd1) {
            return Data_Ord.compare(ordTuple(dictOrd)(dictOrd1));
        },
        Eq10: function () {
            return eq1Tuple(dictOrd.Eq0());
        }
    };
};

// | Turn a function that expects a tuple into a function of two arguments.
var curry = function (f) {
    return function (a) {
        return function (b) {
            return f(new Tuple(a, b));
        };
    };
};
var comonadTuple = {
    extract: snd,
    Extend0: function () {
        return extendTuple;
    }
};
var commutativeRingTuple = function (dictCommutativeRing) {
    return function (dictCommutativeRing1) {
        return {
            Ring0: function () {
                return ringTuple(dictCommutativeRing.Ring0())(dictCommutativeRing1.Ring0());
            }
        };
    };
};
var boundedTuple = function (dictBounded) {
    return function (dictBounded1) {
        return {
            top: new Tuple(Data_Bounded.top(dictBounded), Data_Bounded.top(dictBounded1)),
            bottom: new Tuple(Data_Bounded.bottom(dictBounded), Data_Bounded.bottom(dictBounded1)),
            Ord0: function () {
                return ordTuple(dictBounded.Ord0())(dictBounded1.Ord0());
            }
        };
    };
};
var booleanAlgebraTuple = function (dictBooleanAlgebra) {
    return function (dictBooleanAlgebra1) {
        return {
            HeytingAlgebra0: function () {
                return heytingAlgebraTuple(dictBooleanAlgebra.HeytingAlgebra0())(dictBooleanAlgebra1.HeytingAlgebra0());
            }
        };
    };
};

// | The `Apply` instance allows functions to transform the contents of a
// | `Tuple` with the `<*>` operator whenever there is a `Semigroup` instance
// | for the `fst` component, so:
// | ```purescript
// | (Tuple a1 f) <*> (Tuple a2 x) == Tuple (a1 <> a2) (f x)
// | ```
var applyTuple = function (dictSemigroup) {
    return {
        apply: function (v) {
            return function (v1) {
                return new Tuple(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), v.value1(v1.value1));
            };
        },
        Functor0: function () {
            return functorTuple;
        }
    };
};
var bindTuple = function (dictSemigroup) {
    return {
        bind: function (v) {
            return function (f) {
                var v1 = f(v.value1);
                return new Tuple(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), v1.value1);
            };
        },
        Apply0: function () {
            return applyTuple(dictSemigroup);
        }
    };
};
var applicativeTuple = function (dictMonoid) {
    return {
        pure: Tuple.create(Data_Monoid.mempty(dictMonoid)),
        Apply0: function () {
            return applyTuple(dictMonoid.Semigroup0());
        }
    };
};
var monadTuple = function (dictMonoid) {
    return {
        Applicative0: function () {
            return applicativeTuple(dictMonoid);
        },
        Bind1: function () {
            return bindTuple(dictMonoid.Semigroup0());
        }
    };
};
export {
    Tuple,
    fst,
    snd,
    curry,
    uncurry,
    swap,
    showTuple,
    eqTuple,
    eq1Tuple,
    ordTuple,
    ord1Tuple,
    boundedTuple,
    semigroupoidTuple,
    semigroupTuple,
    monoidTuple,
    semiringTuple,
    ringTuple,
    commutativeRingTuple,
    heytingAlgebraTuple,
    booleanAlgebraTuple,
    functorTuple,
    genericTuple,
    invariantTuple,
    applyTuple,
    applicativeTuple,
    bindTuple,
    monadTuple,
    extendTuple,
    comonadTuple,
    lazyTuple
};
