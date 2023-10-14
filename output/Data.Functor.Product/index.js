// Generated by purs version 0.15.10
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Bifunctor from "../Data.Bifunctor/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var bimap = /* #__PURE__ */ Data_Bifunctor.bimap(Data_Bifunctor.bifunctorTuple);
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
var Product = function (x) {
    return x;
};
var showProduct = function (dictShow) {
    var show = Data_Show.show(dictShow);
    return function (dictShow1) {
        var show1 = Data_Show.show(dictShow1);
        return {
            show: function (v) {
                return "(product " + (show(v.value0) + (" " + (show1(v.value1) + ")")));
            }
        };
    };
};
var product = function (fa) {
    return function (ga) {
        return new Data_Tuple.Tuple(fa, ga);
    };
};
var newtypeProduct = {
    Coercible0: function () {
        return undefined;
    }
};
var functorProduct = function (dictFunctor) {
    var map = Data_Functor.map(dictFunctor);
    return function (dictFunctor1) {
        var map1 = Data_Functor.map(dictFunctor1);
        return {
            map: function (f) {
                return function (v) {
                    return bimap(map(f))(map1(f))(v);
                };
            }
        };
    };
};
var eq1Product = function (dictEq1) {
    var eq1 = Data_Eq.eq1(dictEq1);
    return function (dictEq11) {
        var eq11 = Data_Eq.eq1(dictEq11);
        return {
            eq1: function (dictEq) {
                var eq12 = eq1(dictEq);
                var eq13 = eq11(dictEq);
                return function (v) {
                    return function (v1) {
                        return eq12(v.value0)(v1.value0) && eq13(v.value1)(v1.value1);
                    };
                };
            }
        };
    };
};
var eqProduct = function (dictEq1) {
    var eq1Product1 = eq1Product(dictEq1);
    return function (dictEq11) {
        var eq1 = Data_Eq.eq1(eq1Product1(dictEq11));
        return function (dictEq) {
            return {
                eq: eq1(dictEq)
            };
        };
    };
};
var ord1Product = function (dictOrd1) {
    var compare1 = Data_Ord.compare1(dictOrd1);
    var eq1Product1 = eq1Product(dictOrd1.Eq10());
    return function (dictOrd11) {
        var compare11 = Data_Ord.compare1(dictOrd11);
        var eq1Product2 = eq1Product1(dictOrd11.Eq10());
        return {
            compare1: function (dictOrd) {
                var compare12 = compare1(dictOrd);
                var compare13 = compare11(dictOrd);
                return function (v) {
                    return function (v1) {
                        var v2 = compare12(v.value0)(v1.value0);
                        if (v2 instanceof Data_Ordering.EQ) {
                            return compare13(v.value1)(v1.value1);
                        };
                        return v2;
                    };
                };
            },
            Eq10: function () {
                return eq1Product2;
            }
        };
    };
};
var ordProduct = function (dictOrd1) {
    var ord1Product1 = ord1Product(dictOrd1);
    var eqProduct1 = eqProduct(dictOrd1.Eq10());
    return function (dictOrd11) {
        var compare1 = Data_Ord.compare1(ord1Product1(dictOrd11));
        var eqProduct2 = eqProduct1(dictOrd11.Eq10());
        return function (dictOrd) {
            var eqProduct3 = eqProduct2(dictOrd.Eq0());
            return {
                compare: compare1(dictOrd),
                Eq0: function () {
                    return eqProduct3;
                }
            };
        };
    };
};
var bihoistProduct = function (natF) {
    return function (natG) {
        return function (v) {
            return bimap(natF)(natG)(v);
        };
    };
};
var applyProduct = function (dictApply) {
    var apply = Control_Apply.apply(dictApply);
    var functorProduct1 = functorProduct(dictApply.Functor0());
    return function (dictApply1) {
        var apply1 = Control_Apply.apply(dictApply1);
        var functorProduct2 = functorProduct1(dictApply1.Functor0());
        return {
            apply: function (v) {
                return function (v1) {
                    return product(apply(v.value0)(v1.value0))(apply1(v.value1)(v1.value1));
                };
            },
            Functor0: function () {
                return functorProduct2;
            }
        };
    };
};
var bindProduct = function (dictBind) {
    var bind = Control_Bind.bind(dictBind);
    var applyProduct1 = applyProduct(dictBind.Apply0());
    return function (dictBind1) {
        var bind1 = Control_Bind.bind(dictBind1);
        var applyProduct2 = applyProduct1(dictBind1.Apply0());
        return {
            bind: function (v) {
                return function (f) {
                    return product(bind(v.value0)(function ($128) {
                        return Data_Tuple.fst(unwrap(f($128)));
                    }))(bind1(v.value1)(function ($129) {
                        return Data_Tuple.snd(unwrap(f($129)));
                    }));
                };
            },
            Apply0: function () {
                return applyProduct2;
            }
        };
    };
};
var applicativeProduct = function (dictApplicative) {
    var pure = Control_Applicative.pure(dictApplicative);
    var applyProduct1 = applyProduct(dictApplicative.Apply0());
    return function (dictApplicative1) {
        var pure1 = Control_Applicative.pure(dictApplicative1);
        var applyProduct2 = applyProduct1(dictApplicative1.Apply0());
        return {
            pure: function (a) {
                return product(pure(a))(pure1(a));
            },
            Apply0: function () {
                return applyProduct2;
            }
        };
    };
};
var monadProduct = function (dictMonad) {
    var applicativeProduct1 = applicativeProduct(dictMonad.Applicative0());
    var bindProduct1 = bindProduct(dictMonad.Bind1());
    return function (dictMonad1) {
        var applicativeProduct2 = applicativeProduct1(dictMonad1.Applicative0());
        var bindProduct2 = bindProduct1(dictMonad1.Bind1());
        return {
            Applicative0: function () {
                return applicativeProduct2;
            },
            Bind1: function () {
                return bindProduct2;
            }
        };
    };
};
export {
    Product,
    product,
    bihoistProduct,
    newtypeProduct,
    eqProduct,
    eq1Product,
    ordProduct,
    ord1Product,
    showProduct,
    functorProduct,
    applyProduct,
    applicativeProduct,
    bindProduct,
    monadProduct
};
