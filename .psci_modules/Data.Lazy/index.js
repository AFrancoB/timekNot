import * as $foreign from "./foreign.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Bounded from "../Data.Bounded/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Functor_Invariant from "../Data.Functor.Invariant/index.js";
import * as Data_HeytingAlgebra from "../Data.HeytingAlgebra/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ring from "../Data.Ring/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
var showLazy = function (dictShow) {
    return {
        show: function (x) {
            return "(defer \\_ -> " + (Data_Show.show(dictShow)($foreign.force(x)) + ")");
        }
    };
};
var semiringLazy = function (dictSemiring) {
    return {
        add: function (a) {
            return function (b) {
                return $foreign.defer(function (v) {
                    return Data_Semiring.add(dictSemiring)($foreign.force(a))($foreign.force(b));
                });
            };
        },
        zero: $foreign.defer(function (v) {
            return Data_Semiring.zero(dictSemiring);
        }),
        mul: function (a) {
            return function (b) {
                return $foreign.defer(function (v) {
                    return Data_Semiring.mul(dictSemiring)($foreign.force(a))($foreign.force(b));
                });
            };
        },
        one: $foreign.defer(function (v) {
            return Data_Semiring.one(dictSemiring);
        })
    };
};
var semigroupLazy = function (dictSemigroup) {
    return {
        append: function (a) {
            return function (b) {
                return $foreign.defer(function (v) {
                    return Data_Semigroup.append(dictSemigroup)($foreign.force(a))($foreign.force(b));
                });
            };
        }
    };
};
var ringLazy = function (dictRing) {
    return {
        sub: function (a) {
            return function (b) {
                return $foreign.defer(function (v) {
                    return Data_Ring.sub(dictRing)($foreign.force(a))($foreign.force(b));
                });
            };
        },
        Semiring0: function () {
            return semiringLazy(dictRing.Semiring0());
        }
    };
};
var monoidLazy = function (dictMonoid) {
    return {
        mempty: $foreign.defer(function (v) {
            return Data_Monoid.mempty(dictMonoid);
        }),
        Semigroup0: function () {
            return semigroupLazy(dictMonoid.Semigroup0());
        }
    };
};
var lazyLazy = {
    defer: function (f) {
        return $foreign.defer(function (v) {
            return $foreign.force(f(Data_Unit.unit));
        });
    }
};
var functorLazy = {
    map: function (f) {
        return function (l) {
            return $foreign.defer(function (v) {
                return f($foreign.force(l));
            });
        };
    }
};
var functorWithIndexLazy = {
    mapWithIndex: function (f) {
        return Data_Functor.map(functorLazy)(f(Data_Unit.unit));
    },
    Functor0: function () {
        return functorLazy;
    }
};
var invariantLazy = {
    imap: /* #__PURE__ */ Data_Functor_Invariant.imapF(functorLazy)
};
var foldableLazy = {
    foldr: function (f) {
        return function (z) {
            return function (l) {
                return f($foreign.force(l))(z);
            };
        };
    },
    foldl: function (f) {
        return function (z) {
            return function (l) {
                return f(z)($foreign.force(l));
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (f) {
            return function (l) {
                return f($foreign.force(l));
            };
        };
    }
};
var foldableWithIndexLazy = {
    foldrWithIndex: function (f) {
        return Data_Foldable.foldr(foldableLazy)(f(Data_Unit.unit));
    },
    foldlWithIndex: function (f) {
        return Data_Foldable.foldl(foldableLazy)(f(Data_Unit.unit));
    },
    foldMapWithIndex: function (dictMonoid) {
        return function (f) {
            return Data_Foldable.foldMap(foldableLazy)(dictMonoid)(f(Data_Unit.unit));
        };
    },
    Foldable0: function () {
        return foldableLazy;
    }
};
var traversableLazy = {
    traverse: function (dictApplicative) {
        return function (f) {
            return function (l) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(function ($43) {
                    return $foreign.defer(Data_Function["const"]($43));
                })(f($foreign.force(l)));
            };
        };
    },
    sequence: function (dictApplicative) {
        return function (l) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(function ($44) {
                return $foreign.defer(Data_Function["const"]($44));
            })($foreign.force(l));
        };
    },
    Functor0: function () {
        return functorLazy;
    },
    Foldable1: function () {
        return foldableLazy;
    }
};
var traversableWithIndexLazy = {
    traverseWithIndex: function (dictApplicative) {
        return function (f) {
            return Data_Traversable.traverse(traversableLazy)(dictApplicative)(f(Data_Unit.unit));
        };
    },
    FunctorWithIndex0: function () {
        return functorWithIndexLazy;
    },
    FoldableWithIndex1: function () {
        return foldableWithIndexLazy;
    },
    Traversable2: function () {
        return traversableLazy;
    }
};
var foldable1Lazy = {
    foldMap1: function (dictSemigroup) {
        return function (f) {
            return function (l) {
                return f($foreign.force(l));
            };
        };
    },
    foldr1: function (v) {
        return function (l) {
            return $foreign.force(l);
        };
    },
    foldl1: function (v) {
        return function (l) {
            return $foreign.force(l);
        };
    },
    Foldable0: function () {
        return foldableLazy;
    }
};
var traversable1Lazy = {
    traverse1: function (dictApply) {
        return function (f) {
            return function (l) {
                return Data_Functor.map(dictApply.Functor0())(function ($45) {
                    return $foreign.defer(Data_Function["const"]($45));
                })(f($foreign.force(l)));
            };
        };
    },
    sequence1: function (dictApply) {
        return function (l) {
            return Data_Functor.map(dictApply.Functor0())(function ($46) {
                return $foreign.defer(Data_Function["const"]($46));
            })($foreign.force(l));
        };
    },
    Foldable10: function () {
        return foldable1Lazy;
    },
    Traversable1: function () {
        return traversableLazy;
    }
};
var extendLazy = {
    extend: function (f) {
        return function (x) {
            return $foreign.defer(function (v) {
                return f(x);
            });
        };
    },
    Functor0: function () {
        return functorLazy;
    }
};
var eqLazy = function (dictEq) {
    return {
        eq: function (x) {
            return function (y) {
                return Data_Eq.eq(dictEq)($foreign.force(x))($foreign.force(y));
            };
        }
    };
};
var ordLazy = function (dictOrd) {
    return {
        compare: function (x) {
            return function (y) {
                return Data_Ord.compare(dictOrd)($foreign.force(x))($foreign.force(y));
            };
        },
        Eq0: function () {
            return eqLazy(dictOrd.Eq0());
        }
    };
};
var eq1Lazy = {
    eq1: function (dictEq) {
        return Data_Eq.eq(eqLazy(dictEq));
    }
};
var ord1Lazy = {
    compare1: function (dictOrd) {
        return Data_Ord.compare(ordLazy(dictOrd));
    },
    Eq10: function () {
        return eq1Lazy;
    }
};
var comonadLazy = {
    extract: $foreign.force,
    Extend0: function () {
        return extendLazy;
    }
};
var commutativeRingLazy = function (dictCommutativeRing) {
    return {
        Ring0: function () {
            return ringLazy(dictCommutativeRing.Ring0());
        }
    };
};
var euclideanRingLazy = function (dictEuclideanRing) {
    return {
        degree: (function () {
            var $47 = Data_EuclideanRing.degree(dictEuclideanRing);
            return function ($48) {
                return $47($foreign.force($48));
            };
        })(),
        div: function (a) {
            return function (b) {
                return $foreign.defer(function (v) {
                    return Data_EuclideanRing.div(dictEuclideanRing)($foreign.force(a))($foreign.force(b));
                });
            };
        },
        mod: function (a) {
            return function (b) {
                return $foreign.defer(function (v) {
                    return Data_EuclideanRing.mod(dictEuclideanRing)($foreign.force(a))($foreign.force(b));
                });
            };
        },
        CommutativeRing0: function () {
            return commutativeRingLazy(dictEuclideanRing.CommutativeRing0());
        }
    };
};
var boundedLazy = function (dictBounded) {
    return {
        top: $foreign.defer(function (v) {
            return Data_Bounded.top(dictBounded);
        }),
        bottom: $foreign.defer(function (v) {
            return Data_Bounded.bottom(dictBounded);
        }),
        Ord0: function () {
            return ordLazy(dictBounded.Ord0());
        }
    };
};
var applyLazy = {
    apply: function (f) {
        return function (x) {
            return $foreign.defer(function (v) {
                return $foreign.force(f)($foreign.force(x));
            });
        };
    },
    Functor0: function () {
        return functorLazy;
    }
};
var bindLazy = {
    bind: function (l) {
        return function (f) {
            return $foreign.defer(function (v) {
                return $foreign.force(f($foreign.force(l)));
            });
        };
    },
    Apply0: function () {
        return applyLazy;
    }
};
var heytingAlgebraLazy = function (dictHeytingAlgebra) {
    return {
        ff: $foreign.defer(function (v) {
            return Data_HeytingAlgebra.ff(dictHeytingAlgebra);
        }),
        tt: $foreign.defer(function (v) {
            return Data_HeytingAlgebra.tt(dictHeytingAlgebra);
        }),
        implies: function (a) {
            return function (b) {
                return Control_Apply.apply(applyLazy)(Data_Functor.map(functorLazy)(Data_HeytingAlgebra.implies(dictHeytingAlgebra))(a))(b);
            };
        },
        conj: function (a) {
            return function (b) {
                return Control_Apply.apply(applyLazy)(Data_Functor.map(functorLazy)(Data_HeytingAlgebra.conj(dictHeytingAlgebra))(a))(b);
            };
        },
        disj: function (a) {
            return function (b) {
                return Control_Apply.apply(applyLazy)(Data_Functor.map(functorLazy)(Data_HeytingAlgebra.disj(dictHeytingAlgebra))(a))(b);
            };
        },
        not: function (a) {
            return Data_Functor.map(functorLazy)(Data_HeytingAlgebra.not(dictHeytingAlgebra))(a);
        }
    };
};
var booleanAlgebraLazy = function (dictBooleanAlgebra) {
    return {
        HeytingAlgebra0: function () {
            return heytingAlgebraLazy(dictBooleanAlgebra.HeytingAlgebra0());
        }
    };
};
var applicativeLazy = {
    pure: function (a) {
        return $foreign.defer(function (v) {
            return a;
        });
    },
    Apply0: function () {
        return applyLazy;
    }
};
var monadLazy = {
    Applicative0: function () {
        return applicativeLazy;
    },
    Bind1: function () {
        return bindLazy;
    }
};
export {
    defer,
    force
} from "./foreign.js";
export {
    semiringLazy,
    ringLazy,
    commutativeRingLazy,
    euclideanRingLazy,
    eqLazy,
    eq1Lazy,
    ordLazy,
    ord1Lazy,
    boundedLazy,
    semigroupLazy,
    monoidLazy,
    heytingAlgebraLazy,
    booleanAlgebraLazy,
    functorLazy,
    functorWithIndexLazy,
    foldableLazy,
    foldableWithIndexLazy,
    foldable1Lazy,
    traversableLazy,
    traversableWithIndexLazy,
    traversable1Lazy,
    invariantLazy,
    applyLazy,
    applicativeLazy,
    bindLazy,
    monadLazy,
    extendLazy,
    comonadLazy,
    showLazy,
    lazyLazy
};
