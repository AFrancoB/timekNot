import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Plus from "../Control.Plus/index.js";
import * as Data_Distributive from "../Data.Distributive/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Functor_Invariant from "../Data.Functor.Invariant/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var Star = function (x) {
    return x;
};
var semigroupoidStar = function (dictBind) {
    return {
        compose: function (v) {
            return function (v1) {
                return function (x) {
                    return Control_Bind.bind(dictBind)(v1(x))(v);
                };
            };
        }
    };
};
var profunctorStar = function (dictFunctor) {
    return {
        dimap: function (f) {
            return function (g) {
                return function (v) {
                    var $74 = Data_Functor.map(dictFunctor)(g);
                    return function ($75) {
                        return $74(v(f($75)));
                    };
                };
            };
        }
    };
};
var strongStar = function (dictFunctor) {
    return {
        first: function (v) {
            return function (v1) {
                return Data_Functor.map(dictFunctor)(function (v2) {
                    return new Data_Tuple.Tuple(v2, v1.value1);
                })(v(v1.value0));
            };
        },
        second: function (v) {
            return function (v1) {
                return Data_Functor.map(dictFunctor)(Data_Tuple.Tuple.create(v1.value0))(v(v1.value1));
            };
        },
        Profunctor0: function () {
            return profunctorStar(dictFunctor);
        }
    };
};
var newtypeStar = {
    Coercible0: function () {
        return undefined;
    }
};
var invariantStar = function (dictInvariant) {
    return {
        imap: function (f) {
            return function (g) {
                return function (v) {
                    var $76 = Data_Functor_Invariant.imap(dictInvariant)(f)(g);
                    return function ($77) {
                        return $76(v($77));
                    };
                };
            };
        }
    };
};
var hoistStar = function (f) {
    return function (v) {
        return function ($78) {
            return f(v($78));
        };
    };
};
var functorStar = function (dictFunctor) {
    return {
        map: function (f) {
            return function (v) {
                var $79 = Data_Functor.map(dictFunctor)(f);
                return function ($80) {
                    return $79(v($80));
                };
            };
        }
    };
};
var distributiveStar = function (dictDistributive) {
    return {
        distribute: function (dictFunctor) {
            return function (f) {
                return function (a) {
                    return Data_Distributive.collect(dictDistributive)(dictFunctor)(function (v) {
                        return v(a);
                    })(f);
                };
            };
        },
        collect: function (dictFunctor) {
            return function (f) {
                var $81 = Data_Distributive.distribute(distributiveStar(dictDistributive))(dictFunctor);
                var $82 = Data_Functor.map(dictFunctor)(f);
                return function ($83) {
                    return $81($82($83));
                };
            };
        },
        Functor0: function () {
            return functorStar(dictDistributive.Functor0());
        }
    };
};
var closedStar = function (dictDistributive) {
    return {
        closed: function (v) {
            return function (g) {
                return Data_Distributive.distribute(dictDistributive)(Data_Functor.functorFn)(function ($84) {
                    return v(g($84));
                });
            };
        },
        Profunctor0: function () {
            return profunctorStar(dictDistributive.Functor0());
        }
    };
};
var choiceStar = function (dictApplicative) {
    return {
        left: function (v) {
            return Data_Either.either((function () {
                var $85 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Either.Left.create);
                return function ($86) {
                    return $85(v($86));
                };
            })())((function () {
                var $87 = Control_Applicative.pure(dictApplicative);
                return function ($88) {
                    return $87(Data_Either.Right.create($88));
                };
            })());
        },
        right: function (v) {
            return Data_Either.either((function () {
                var $89 = Control_Applicative.pure(dictApplicative);
                return function ($90) {
                    return $89(Data_Either.Left.create($90));
                };
            })())((function () {
                var $91 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Either.Right.create);
                return function ($92) {
                    return $91(v($92));
                };
            })());
        },
        Profunctor0: function () {
            return profunctorStar((dictApplicative.Apply0()).Functor0());
        }
    };
};
var categoryStar = function (dictMonad) {
    return {
        identity: Control_Applicative.pure(dictMonad.Applicative0()),
        Semigroupoid0: function () {
            return semigroupoidStar(dictMonad.Bind1());
        }
    };
};
var applyStar = function (dictApply) {
    return {
        apply: function (v) {
            return function (v1) {
                return function (a) {
                    return Control_Apply.apply(dictApply)(v(a))(v1(a));
                };
            };
        },
        Functor0: function () {
            return functorStar(dictApply.Functor0());
        }
    };
};
var bindStar = function (dictBind) {
    return {
        bind: function (v) {
            return function (f) {
                return function (x) {
                    return Control_Bind.bind(dictBind)(v(x))(function (a) {
                        var v1 = f(a);
                        return v1(x);
                    });
                };
            };
        },
        Apply0: function () {
            return applyStar(dictBind.Apply0());
        }
    };
};
var applicativeStar = function (dictApplicative) {
    return {
        pure: function (a) {
            return function (v) {
                return Control_Applicative.pure(dictApplicative)(a);
            };
        },
        Apply0: function () {
            return applyStar(dictApplicative.Apply0());
        }
    };
};
var monadStar = function (dictMonad) {
    return {
        Applicative0: function () {
            return applicativeStar(dictMonad.Applicative0());
        },
        Bind1: function () {
            return bindStar(dictMonad.Bind1());
        }
    };
};
var altStar = function (dictAlt) {
    return {
        alt: function (v) {
            return function (v1) {
                return function (a) {
                    return Control_Alt.alt(dictAlt)(v(a))(v1(a));
                };
            };
        },
        Functor0: function () {
            return functorStar(dictAlt.Functor0());
        }
    };
};
var plusStar = function (dictPlus) {
    return {
        empty: function (v) {
            return Control_Plus.empty(dictPlus);
        },
        Alt0: function () {
            return altStar(dictPlus.Alt0());
        }
    };
};
var alternativeStar = function (dictAlternative) {
    return {
        Applicative0: function () {
            return applicativeStar(dictAlternative.Applicative0());
        },
        Plus1: function () {
            return plusStar(dictAlternative.Plus1());
        }
    };
};
var monadPlusStar = function (dictMonadPlus) {
    return {
        Monad0: function () {
            return monadStar(dictMonadPlus.Monad0());
        },
        Alternative1: function () {
            return alternativeStar(dictMonadPlus.Alternative1());
        }
    };
};
export {
    Star,
    hoistStar,
    newtypeStar,
    semigroupoidStar,
    categoryStar,
    functorStar,
    invariantStar,
    applyStar,
    applicativeStar,
    bindStar,
    monadStar,
    altStar,
    plusStar,
    alternativeStar,
    monadPlusStar,
    distributiveStar,
    profunctorStar,
    strongStar,
    choiceStar,
    closedStar
};
