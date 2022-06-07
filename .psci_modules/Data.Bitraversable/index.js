import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Bifoldable from "../Data.Bifoldable/index.js";
import * as Data_Bifunctor from "../Data.Bifunctor/index.js";
import * as Data_Const from "../Data.Const/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Functor_Clown from "../Data.Functor.Clown/index.js";
import * as Data_Functor_Flip from "../Data.Functor.Flip/index.js";
import * as Data_Functor_Joker from "../Data.Functor.Joker/index.js";
import * as Data_Functor_Product2 from "../Data.Functor.Product2/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var bitraverse = function (dict) {
    return dict.bitraverse;
};
var lfor = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return bitraverse(dictBitraversable)(dictApplicative)(f)(Control_Applicative.pure(dictApplicative))(t);
            };
        };
    };
};
var ltraverse = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (f) {
            return bitraverse(dictBitraversable)(dictApplicative)(f)(Control_Applicative.pure(dictApplicative));
        };
    };
};
var rfor = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return bitraverse(dictBitraversable)(dictApplicative)(Control_Applicative.pure(dictApplicative))(f)(t);
            };
        };
    };
};
var rtraverse = function (dictBitraversable) {
    return function (dictApplicative) {
        return bitraverse(dictBitraversable)(dictApplicative)(Control_Applicative.pure(dictApplicative));
    };
};
var bitraversableTuple = {
    bitraverse: function (dictApplicative) {
        return function (f) {
            return function (g) {
                return function (v) {
                    return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Tuple.Tuple.create)(f(v.value0)))(g(v.value1));
                };
            };
        };
    },
    bisequence: function (dictApplicative) {
        return function (v) {
            return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Tuple.Tuple.create)(v.value0))(v.value1);
        };
    },
    Bifunctor0: function () {
        return Data_Bifunctor.bifunctorTuple;
    },
    Bifoldable1: function () {
        return Data_Bifoldable.bifoldableTuple;
    }
};
var bitraversableJoker = function (dictTraversable) {
    return {
        bitraverse: function (dictApplicative) {
            return function (v) {
                return function (r) {
                    return function (v1) {
                        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Functor_Joker.Joker)(Data_Traversable.traverse(dictTraversable)(dictApplicative)(r)(v1));
                    };
                };
            };
        },
        bisequence: function (dictApplicative) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Functor_Joker.Joker)(Data_Traversable.sequence(dictTraversable)(dictApplicative)(v));
            };
        },
        Bifunctor0: function () {
            return Data_Functor_Joker.bifunctorJoker(dictTraversable.Functor0());
        },
        Bifoldable1: function () {
            return Data_Bifoldable.bifoldableJoker(dictTraversable.Foldable1());
        }
    };
};
var bitraversableEither = {
    bitraverse: function (dictApplicative) {
        return function (v) {
            return function (v1) {
                return function (v2) {
                    if (v2 instanceof Data_Either.Left) {
                        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Either.Left.create)(v(v2.value0));
                    };
                    if (v2 instanceof Data_Either.Right) {
                        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Either.Right.create)(v1(v2.value0));
                    };
                    throw new Error("Failed pattern match at Data.Bitraversable (line 57, column 1 - line 61, column 37): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
                };
            };
        };
    },
    bisequence: function (dictApplicative) {
        return function (v) {
            if (v instanceof Data_Either.Left) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Either.Left.create)(v.value0);
            };
            if (v instanceof Data_Either.Right) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Either.Right.create)(v.value0);
            };
            throw new Error("Failed pattern match at Data.Bitraversable (line 57, column 1 - line 61, column 37): " + [ v.constructor.name ]);
        };
    },
    Bifunctor0: function () {
        return Data_Bifunctor.bifunctorEither;
    },
    Bifoldable1: function () {
        return Data_Bifoldable.bifoldableEither;
    }
};
var bitraversableConst = {
    bitraverse: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return function (v1) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Const.Const)(f(v1));
                };
            };
        };
    },
    bisequence: function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Const.Const)(v);
        };
    },
    Bifunctor0: function () {
        return Data_Bifunctor.bifunctorConst;
    },
    Bifoldable1: function () {
        return Data_Bifoldable.bifoldableConst;
    }
};
var bitraversableClown = function (dictTraversable) {
    return {
        bitraverse: function (dictApplicative) {
            return function (l) {
                return function (v) {
                    return function (v1) {
                        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Functor_Clown.Clown)(Data_Traversable.traverse(dictTraversable)(dictApplicative)(l)(v1));
                    };
                };
            };
        },
        bisequence: function (dictApplicative) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Functor_Clown.Clown)(Data_Traversable.sequence(dictTraversable)(dictApplicative)(v));
            };
        },
        Bifunctor0: function () {
            return Data_Functor_Clown.bifunctorClown(dictTraversable.Functor0());
        },
        Bifoldable1: function () {
            return Data_Bifoldable.bifoldableClown(dictTraversable.Foldable1());
        }
    };
};

// | A default implementation of `bisequence` using `bitraverse`.
var bisequenceDefault = function (dictBitraversable) {
    return function (dictApplicative) {
        return bitraverse(dictBitraversable)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn))(Control_Category.identity(Control_Category.categoryFn));
    };
};
var bisequence = function (dict) {
    return dict.bisequence;
};
var bitraversableFlip = function (dictBitraversable) {
    return {
        bitraverse: function (dictApplicative) {
            return function (r) {
                return function (l) {
                    return function (v) {
                        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Functor_Flip.Flip)(bitraverse(dictBitraversable)(dictApplicative)(l)(r)(v));
                    };
                };
            };
        },
        bisequence: function (dictApplicative) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Functor_Flip.Flip)(bisequence(dictBitraversable)(dictApplicative)(v));
            };
        },
        Bifunctor0: function () {
            return Data_Functor_Flip.bifunctorFlip(dictBitraversable.Bifunctor0());
        },
        Bifoldable1: function () {
            return Data_Bifoldable.bifoldableFlip(dictBitraversable.Bifoldable1());
        }
    };
};
var bitraversableProduct2 = function (dictBitraversable) {
    return function (dictBitraversable1) {
        return {
            bitraverse: function (dictApplicative) {
                return function (l) {
                    return function (r) {
                        return function (v) {
                            return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Functor_Product2.Product2.create)(bitraverse(dictBitraversable)(dictApplicative)(l)(r)(v.value0)))(bitraverse(dictBitraversable1)(dictApplicative)(l)(r)(v.value1));
                        };
                    };
                };
            },
            bisequence: function (dictApplicative) {
                return function (v) {
                    return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Functor_Product2.Product2.create)(bisequence(dictBitraversable)(dictApplicative)(v.value0)))(bisequence(dictBitraversable1)(dictApplicative)(v.value1));
                };
            },
            Bifunctor0: function () {
                return Data_Functor_Product2.bifunctorProduct2(dictBitraversable.Bifunctor0())(dictBitraversable1.Bifunctor0());
            },
            Bifoldable1: function () {
                return Data_Bifoldable.bifoldableProduct2(dictBitraversable.Bifoldable1())(dictBitraversable1.Bifoldable1());
            }
        };
    };
};

// | A default implementation of `bitraverse` using `bisequence` and `bimap`.
var bitraverseDefault = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (f) {
            return function (g) {
                return function (t) {
                    return bisequence(dictBitraversable)(dictApplicative)(Data_Bifunctor.bimap(dictBitraversable.Bifunctor0())(f)(g)(t));
                };
            };
        };
    };
};

// | Traverse a data structure, accumulating effects and results using an `Applicative` functor.
var bifor = function (dictBitraversable) {
    return function (dictApplicative) {
        return function (t) {
            return function (f) {
                return function (g) {
                    return bitraverse(dictBitraversable)(dictApplicative)(f)(g)(t);
                };
            };
        };
    };
};
export {
    bitraverse,
    bisequence,
    bitraverseDefault,
    bisequenceDefault,
    ltraverse,
    rtraverse,
    bifor,
    lfor,
    rfor,
    bitraversableClown,
    bitraversableJoker,
    bitraversableFlip,
    bitraversableProduct2,
    bitraversableEither,
    bitraversableTuple,
    bitraversableConst
};
export {
    biall,
    biany,
    bifold,
    bifoldMap,
    bifoldMapDefaultL,
    bifoldMapDefaultR,
    bifoldl,
    bifoldlDefault,
    bifoldr,
    bifoldrDefault,
    bifor_,
    bisequence_,
    bitraverse_
} from "../Data.Bifoldable/index.js";
