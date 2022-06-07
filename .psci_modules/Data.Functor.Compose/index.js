import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Plus from "../Control.Plus/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Functor_App from "../Data.Functor.App/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Show from "../Data.Show/index.js";
var Compose = function (x) {
    return x;
};
var showCompose = function (dictShow) {
    return {
        show: function (v) {
            return "(Compose " + (Data_Show.show(dictShow)(v) + ")");
        }
    };
};
var newtypeCompose = {
    Coercible0: function () {
        return undefined;
    }
};
var functorCompose = function (dictFunctor) {
    return function (dictFunctor1) {
        return {
            map: function (f) {
                return function (v) {
                    return Data_Functor.map(dictFunctor)(Data_Functor.map(dictFunctor1)(f))(v);
                };
            }
        };
    };
};
var eqCompose = function (dictEq1) {
    return function (dictEq11) {
        return function (dictEq) {
            return {
                eq: function (v) {
                    return function (v1) {
                        return Data_Eq.eq1(dictEq1)(Data_Functor_App.eqApp(dictEq11)(dictEq))(Data_Functor_App.hoistLiftApp(v))(Data_Functor_App.hoistLiftApp(v1));
                    };
                }
            };
        };
    };
};
var ordCompose = function (dictOrd1) {
    return function (dictOrd11) {
        return function (dictOrd) {
            return {
                compare: function (v) {
                    return function (v1) {
                        return Data_Ord.compare1(dictOrd1)(Data_Functor_App.ordApp(dictOrd11)(dictOrd))(Data_Functor_App.hoistLiftApp(v))(Data_Functor_App.hoistLiftApp(v1));
                    };
                },
                Eq0: function () {
                    return eqCompose(dictOrd1.Eq10())(dictOrd11.Eq10())(dictOrd.Eq0());
                }
            };
        };
    };
};
var eq1Compose = function (dictEq1) {
    return function (dictEq11) {
        return {
            eq1: function (dictEq) {
                return Data_Eq.eq(eqCompose(dictEq1)(dictEq11)(dictEq));
            }
        };
    };
};
var ord1Compose = function (dictOrd1) {
    return function (dictOrd11) {
        return {
            compare1: function (dictOrd) {
                return Data_Ord.compare(ordCompose(dictOrd1)(dictOrd11)(dictOrd));
            },
            Eq10: function () {
                return eq1Compose(dictOrd1.Eq10())(dictOrd11.Eq10());
            }
        };
    };
};
var bihoistCompose = function (dictFunctor) {
    return function (natF) {
        return function (natG) {
            return function (v) {
                return natF(Data_Functor.map(dictFunctor)(natG)(v));
            };
        };
    };
};
var applyCompose = function (dictApply) {
    return function (dictApply1) {
        return {
            apply: function (v) {
                return function (v1) {
                    return Control_Apply.apply(dictApply)(Data_Functor.map(dictApply.Functor0())(Control_Apply.apply(dictApply1))(v))(v1);
                };
            },
            Functor0: function () {
                return functorCompose(dictApply.Functor0())(dictApply1.Functor0());
            }
        };
    };
};
var applicativeCompose = function (dictApplicative) {
    return function (dictApplicative1) {
        return {
            pure: (function () {
                var $51 = Control_Applicative.pure(dictApplicative);
                var $52 = Control_Applicative.pure(dictApplicative1);
                return function ($53) {
                    return Compose($51($52($53)));
                };
            })(),
            Apply0: function () {
                return applyCompose(dictApplicative.Apply0())(dictApplicative1.Apply0());
            }
        };
    };
};
var altCompose = function (dictAlt) {
    return function (dictFunctor) {
        return {
            alt: function (v) {
                return function (v1) {
                    return Control_Alt.alt(dictAlt)(v)(v1);
                };
            },
            Functor0: function () {
                return functorCompose(dictAlt.Functor0())(dictFunctor);
            }
        };
    };
};
var plusCompose = function (dictPlus) {
    return function (dictFunctor) {
        return {
            empty: Control_Plus.empty(dictPlus),
            Alt0: function () {
                return altCompose(dictPlus.Alt0())(dictFunctor);
            }
        };
    };
};
var alternativeCompose = function (dictAlternative) {
    return function (dictApplicative) {
        return {
            Applicative0: function () {
                return applicativeCompose(dictAlternative.Applicative0())(dictApplicative);
            },
            Plus1: function () {
                return plusCompose(dictAlternative.Plus1())((dictApplicative.Apply0()).Functor0());
            }
        };
    };
};
export {
    Compose,
    bihoistCompose,
    newtypeCompose,
    eqCompose,
    eq1Compose,
    ordCompose,
    ord1Compose,
    showCompose,
    functorCompose,
    applyCompose,
    applicativeCompose,
    altCompose,
    plusCompose,
    alternativeCompose
};
