import * as $foreign from "./foreign.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Const from "../Data.Const/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Functor_App from "../Data.Functor.App/index.js";
import * as Data_Functor_Compose from "../Data.Functor.Compose/index.js";
import * as Data_Functor_Coproduct from "../Data.Functor.Coproduct/index.js";
import * as Data_Functor_Product from "../Data.Functor.Product/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Maybe_First from "../Data.Maybe.First/index.js";
import * as Data_Maybe_Last from "../Data.Maybe.Last/index.js";
import * as Data_Monoid_Additive from "../Data.Monoid.Additive/index.js";
import * as Data_Monoid_Conj from "../Data.Monoid.Conj/index.js";
import * as Data_Monoid_Disj from "../Data.Monoid.Disj/index.js";
import * as Data_Monoid_Dual from "../Data.Monoid.Dual/index.js";
import * as Data_Monoid_Multiplicative from "../Data.Monoid.Multiplicative/index.js";
import * as Data_Traversable_Accum from "../Data.Traversable.Accum/index.js";
import * as Data_Traversable_Accum_Internal from "../Data.Traversable.Accum.Internal/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var traverse = function (dict) {
    return dict.traverse;
};
var traversableTuple = {
    traverse: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Tuple.Tuple.create(v.value0))(f(v.value1));
            };
        };
    },
    sequence: function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Tuple.Tuple.create(v.value0))(v.value1);
        };
    },
    Functor0: function () {
        return Data_Tuple.functorTuple;
    },
    Foldable1: function () {
        return Data_Foldable.foldableTuple;
    }
};
var traversableMultiplicative = {
    traverse: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Multiplicative.Multiplicative)(f(v));
            };
        };
    },
    sequence: function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Multiplicative.Multiplicative)(v);
        };
    },
    Functor0: function () {
        return Data_Monoid_Multiplicative.functorMultiplicative;
    },
    Foldable1: function () {
        return Data_Foldable.foldableMultiplicative;
    }
};
var traversableMaybe = {
    traverse: function (dictApplicative) {
        return function (v) {
            return function (v1) {
                if (v1 instanceof Data_Maybe.Nothing) {
                    return Control_Applicative.pure(dictApplicative)(Data_Maybe.Nothing.value);
                };
                if (v1 instanceof Data_Maybe.Just) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe.Just.create)(v(v1.value0));
                };
                throw new Error("Failed pattern match at Data.Traversable (line 115, column 1 - line 119, column 33): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    },
    sequence: function (dictApplicative) {
        return function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return Control_Applicative.pure(dictApplicative)(Data_Maybe.Nothing.value);
            };
            if (v instanceof Data_Maybe.Just) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe.Just.create)(v.value0);
            };
            throw new Error("Failed pattern match at Data.Traversable (line 115, column 1 - line 119, column 33): " + [ v.constructor.name ]);
        };
    },
    Functor0: function () {
        return Data_Maybe.functorMaybe;
    },
    Foldable1: function () {
        return Data_Foldable.foldableMaybe;
    }
};
var traversableIdentity = {
    traverse: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Identity.Identity)(f(v));
            };
        };
    },
    sequence: function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Identity.Identity)(v);
        };
    },
    Functor0: function () {
        return Data_Identity.functorIdentity;
    },
    Foldable1: function () {
        return Data_Foldable.foldableIdentity;
    }
};
var traversableEither = {
    traverse: function (dictApplicative) {
        return function (v) {
            return function (v1) {
                if (v1 instanceof Data_Either.Left) {
                    return Control_Applicative.pure(dictApplicative)(new Data_Either.Left(v1.value0));
                };
                if (v1 instanceof Data_Either.Right) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Either.Right.create)(v(v1.value0));
                };
                throw new Error("Failed pattern match at Data.Traversable (line 149, column 1 - line 153, column 36): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    },
    sequence: function (dictApplicative) {
        return function (v) {
            if (v instanceof Data_Either.Left) {
                return Control_Applicative.pure(dictApplicative)(new Data_Either.Left(v.value0));
            };
            if (v instanceof Data_Either.Right) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Either.Right.create)(v.value0);
            };
            throw new Error("Failed pattern match at Data.Traversable (line 149, column 1 - line 153, column 36): " + [ v.constructor.name ]);
        };
    },
    Functor0: function () {
        return Data_Either.functorEither;
    },
    Foldable1: function () {
        return Data_Foldable.foldableEither;
    }
};
var traversableDual = {
    traverse: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Dual.Dual)(f(v));
            };
        };
    },
    sequence: function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Dual.Dual)(v);
        };
    },
    Functor0: function () {
        return Data_Monoid_Dual.functorDual;
    },
    Foldable1: function () {
        return Data_Foldable.foldableDual;
    }
};
var traversableDisj = {
    traverse: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Disj.Disj)(f(v));
            };
        };
    },
    sequence: function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Disj.Disj)(v);
        };
    },
    Functor0: function () {
        return Data_Monoid_Disj.functorDisj;
    },
    Foldable1: function () {
        return Data_Foldable.foldableDisj;
    }
};
var traversableConst = {
    traverse: function (dictApplicative) {
        return function (v) {
            return function (v1) {
                return Control_Applicative.pure(dictApplicative)(v1);
            };
        };
    },
    sequence: function (dictApplicative) {
        return function (v) {
            return Control_Applicative.pure(dictApplicative)(v);
        };
    },
    Functor0: function () {
        return Data_Const.functorConst;
    },
    Foldable1: function () {
        return Data_Foldable.foldableConst;
    }
};
var traversableConj = {
    traverse: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Conj.Conj)(f(v));
            };
        };
    },
    sequence: function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Conj.Conj)(v);
        };
    },
    Functor0: function () {
        return Data_Monoid_Conj.functorConj;
    },
    Foldable1: function () {
        return Data_Foldable.foldableConj;
    }
};
var traversableCompose = function (dictTraversable) {
    return function (dictTraversable1) {
        return {
            traverse: function (dictApplicative) {
                return function (f) {
                    return function (v) {
                        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Functor_Compose.Compose)(traverse(dictTraversable)(dictApplicative)(traverse(dictTraversable1)(dictApplicative)(f))(v));
                    };
                };
            },
            sequence: function (dictApplicative) {
                return traverse(traversableCompose(dictTraversable)(dictTraversable1))(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
            },
            Functor0: function () {
                return Data_Functor_Compose.functorCompose(dictTraversable.Functor0())(dictTraversable1.Functor0());
            },
            Foldable1: function () {
                return Data_Foldable.foldableCompose(dictTraversable.Foldable1())(dictTraversable1.Foldable1());
            }
        };
    };
};
var traversableAdditive = {
    traverse: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Additive.Additive)(f(v));
            };
        };
    },
    sequence: function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Monoid_Additive.Additive)(v);
        };
    },
    Functor0: function () {
        return Data_Monoid_Additive.functorAdditive;
    },
    Foldable1: function () {
        return Data_Foldable.foldableAdditive;
    }
};

// | A default implementation of `sequence` using `traverse`.
var sequenceDefault = function (dictTraversable) {
    return function (dictApplicative) {
        return traverse(dictTraversable)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
    };
};
var traversableArray = {
    traverse: function (dictApplicative) {
        return $foreign.traverseArrayImpl(Control_Apply.apply(dictApplicative.Apply0()))(Data_Functor.map((dictApplicative.Apply0()).Functor0()))(Control_Applicative.pure(dictApplicative));
    },
    sequence: function (dictApplicative) {
        return sequenceDefault(traversableArray)(dictApplicative);
    },
    Functor0: function () {
        return Data_Functor.functorArray;
    },
    Foldable1: function () {
        return Data_Foldable.foldableArray;
    }
};
var sequence = function (dict) {
    return dict.sequence;
};
var traversableApp = function (dictTraversable) {
    return {
        traverse: function (dictApplicative) {
            return function (f) {
                return function (v) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Functor_App.App)(traverse(dictTraversable)(dictApplicative)(f)(v));
                };
            };
        },
        sequence: function (dictApplicative) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Functor_App.App)(sequence(dictTraversable)(dictApplicative)(v));
            };
        },
        Functor0: function () {
            return Data_Functor_App.functorApp(dictTraversable.Functor0());
        },
        Foldable1: function () {
            return Data_Foldable.foldableApp(dictTraversable.Foldable1());
        }
    };
};
var traversableCoproduct = function (dictTraversable) {
    return function (dictTraversable1) {
        return {
            traverse: function (dictApplicative) {
                return function (f) {
                    return Data_Functor_Coproduct.coproduct((function () {
                        var $143 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(function ($146) {
                            return Data_Functor_Coproduct.Coproduct(Data_Either.Left.create($146));
                        });
                        var $144 = traverse(dictTraversable)(dictApplicative)(f);
                        return function ($145) {
                            return $143($144($145));
                        };
                    })())((function () {
                        var $147 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(function ($150) {
                            return Data_Functor_Coproduct.Coproduct(Data_Either.Right.create($150));
                        });
                        var $148 = traverse(dictTraversable1)(dictApplicative)(f);
                        return function ($149) {
                            return $147($148($149));
                        };
                    })());
                };
            },
            sequence: function (dictApplicative) {
                return Data_Functor_Coproduct.coproduct((function () {
                    var $151 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(function ($154) {
                        return Data_Functor_Coproduct.Coproduct(Data_Either.Left.create($154));
                    });
                    var $152 = sequence(dictTraversable)(dictApplicative);
                    return function ($153) {
                        return $151($152($153));
                    };
                })())((function () {
                    var $155 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(function ($158) {
                        return Data_Functor_Coproduct.Coproduct(Data_Either.Right.create($158));
                    });
                    var $156 = sequence(dictTraversable1)(dictApplicative);
                    return function ($157) {
                        return $155($156($157));
                    };
                })());
            },
            Functor0: function () {
                return Data_Functor_Coproduct.functorCoproduct(dictTraversable.Functor0())(dictTraversable1.Functor0());
            },
            Foldable1: function () {
                return Data_Foldable.foldableCoproduct(dictTraversable.Foldable1())(dictTraversable1.Foldable1());
            }
        };
    };
};
var traversableFirst = {
    traverse: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe_First.First)(traverse(traversableMaybe)(dictApplicative)(f)(v));
            };
        };
    },
    sequence: function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe_First.First)(sequence(traversableMaybe)(dictApplicative)(v));
        };
    },
    Functor0: function () {
        return Data_Maybe_First.functorFirst;
    },
    Foldable1: function () {
        return Data_Foldable.foldableFirst;
    }
};
var traversableLast = {
    traverse: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe_Last.Last)(traverse(traversableMaybe)(dictApplicative)(f)(v));
            };
        };
    },
    sequence: function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Maybe_Last.Last)(sequence(traversableMaybe)(dictApplicative)(v));
        };
    },
    Functor0: function () {
        return Data_Maybe_Last.functorLast;
    },
    Foldable1: function () {
        return Data_Foldable.foldableLast;
    }
};
var traversableProduct = function (dictTraversable) {
    return function (dictTraversable1) {
        return {
            traverse: function (dictApplicative) {
                return function (f) {
                    return function (v) {
                        return Control_Apply.lift2(dictApplicative.Apply0())(Data_Functor_Product.product)(traverse(dictTraversable)(dictApplicative)(f)(v.value0))(traverse(dictTraversable1)(dictApplicative)(f)(v.value1));
                    };
                };
            },
            sequence: function (dictApplicative) {
                return function (v) {
                    return Control_Apply.lift2(dictApplicative.Apply0())(Data_Functor_Product.product)(sequence(dictTraversable)(dictApplicative)(v.value0))(sequence(dictTraversable1)(dictApplicative)(v.value1));
                };
            },
            Functor0: function () {
                return Data_Functor_Product.functorProduct(dictTraversable.Functor0())(dictTraversable1.Functor0());
            },
            Foldable1: function () {
                return Data_Foldable.foldableProduct(dictTraversable.Foldable1())(dictTraversable1.Foldable1());
            }
        };
    };
};

// | A default implementation of `traverse` using `sequence` and `map`.
var traverseDefault = function (dictTraversable) {
    return function (dictApplicative) {
        return function (f) {
            return function (ta) {
                return sequence(dictTraversable)(dictApplicative)(Data_Functor.map(dictTraversable.Functor0())(f)(ta));
            };
        };
    };
};

// | Fold a data structure from the right, keeping all intermediate results
// | instead of only the final result.
// |
// | Unlike `scanr`, `mapAccumR` allows the type of accumulator to differ
// | from the element type of the final data structure.
var mapAccumR = function (dictTraversable) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return Data_Traversable_Accum_Internal.stateR(traverse(dictTraversable)(Data_Traversable_Accum_Internal.applicativeStateR)(function (a) {
                    return function (s) {
                        return f(s)(a);
                    };
                })(xs))(s0);
            };
        };
    };
};

// | Fold a data structure from the right, keeping all intermediate results
// | instead of only the final result. Note that the initial value does not
// | appear in the result (unlike Haskell's `Prelude.scanr`).
// |
// | ```purescript
// | scanr (+) 0 [1,2,3] = [6,5,3]
// | scanr (flip (-)) 10 [1,2,3] = [4,5,7]
// | ```
var scanr = function (dictTraversable) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumR(dictTraversable)(function (b) {
                    return function (a) {
                        var b$prime = f(a)(b);
                        return {
                            accum: b$prime,
                            value: b$prime
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};

// | Fold a data structure from the left, keeping all intermediate results
// | instead of only the final result.
// |
// | Unlike `scanl`, `mapAccumL` allows the type of accumulator to differ
// | from the element type of the final data structure.
var mapAccumL = function (dictTraversable) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return Data_Traversable_Accum_Internal.stateL(traverse(dictTraversable)(Data_Traversable_Accum_Internal.applicativeStateL)(function (a) {
                    return function (s) {
                        return f(s)(a);
                    };
                })(xs))(s0);
            };
        };
    };
};

// | Fold a data structure from the left, keeping all intermediate results
// | instead of only the final result. Note that the initial value does not
// | appear in the result (unlike Haskell's `Prelude.scanl`).
// |
// | ```purescript
// | scanl (+) 0  [1,2,3] = [1,3,6]
// | scanl (-) 10 [1,2,3] = [9,7,4]
// | ```
var scanl = function (dictTraversable) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumL(dictTraversable)(function (b) {
                    return function (a) {
                        var b$prime = f(b)(a);
                        return {
                            accum: b$prime,
                            value: b$prime
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};

// | A version of `traverse` with its arguments flipped.
// |
// |
// | This can be useful when running an action written using do notation
// | for every element in a data structure:
// |
// | For example:
// |
// | ```purescript
// | for [1, 2, 3] \n -> do
// |   print n
// |   return (n * n)
// | ```
var $$for = function (dictApplicative) {
    return function (dictTraversable) {
        return function (x) {
            return function (f) {
                return traverse(dictTraversable)(dictApplicative)(f)(x);
            };
        };
    };
};
export {
    traverse,
    sequence,
    traverseDefault,
    sequenceDefault,
    $$for as for,
    scanl,
    scanr,
    mapAccumL,
    mapAccumR,
    traversableArray,
    traversableMaybe,
    traversableFirst,
    traversableLast,
    traversableAdditive,
    traversableDual,
    traversableConj,
    traversableDisj,
    traversableMultiplicative,
    traversableEither,
    traversableTuple,
    traversableIdentity,
    traversableConst,
    traversableProduct,
    traversableCoproduct,
    traversableCompose,
    traversableApp
};
export {
    all,
    and,
    any,
    elem,
    find,
    fold,
    foldMap,
    foldMapDefaultL,
    foldMapDefaultR,
    foldl,
    foldlDefault,
    foldr,
    foldrDefault,
    for_,
    intercalate,
    maximum,
    maximumBy,
    minimum,
    minimumBy,
    notElem,
    oneOf,
    or,
    sequence_,
    sum,
    traverse_
} from "../Data.Foldable/index.js";
