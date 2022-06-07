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
var Product = function (x) {
    return x;
};
var showProduct = function (dictShow) {
    return function (dictShow1) {
        return {
            show: function (v) {
                return "(product " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(dictShow1)(v.value1) + ")")));
            }
        };
    };
};

// | Create a product.
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
    return function (dictFunctor1) {
        return {
            map: function (f) {
                return function (v) {
                    return Data_Bifunctor.bimap(Data_Bifunctor.bifunctorTuple)(Data_Functor.map(dictFunctor)(f))(Data_Functor.map(dictFunctor1)(f))(v);
                };
            }
        };
    };
};
var eq1Product = function (dictEq1) {
    return function (dictEq11) {
        return {
            eq1: function (dictEq) {
                return function (v) {
                    return function (v1) {
                        return Data_Eq.eq1(dictEq1)(dictEq)(v.value0)(v1.value0) && Data_Eq.eq1(dictEq11)(dictEq)(v.value1)(v1.value1);
                    };
                };
            }
        };
    };
};
var eqProduct = function (dictEq1) {
    return function (dictEq11) {
        return function (dictEq) {
            return {
                eq: Data_Eq.eq1(eq1Product(dictEq1)(dictEq11))(dictEq)
            };
        };
    };
};
var ord1Product = function (dictOrd1) {
    return function (dictOrd11) {
        return {
            compare1: function (dictOrd) {
                return function (v) {
                    return function (v1) {
                        var v2 = Data_Ord.compare1(dictOrd1)(dictOrd)(v.value0)(v1.value0);
                        if (v2 instanceof Data_Ordering.EQ) {
                            return Data_Ord.compare1(dictOrd11)(dictOrd)(v.value1)(v1.value1);
                        };
                        return v2;
                    };
                };
            },
            Eq10: function () {
                return eq1Product(dictOrd1.Eq10())(dictOrd11.Eq10());
            }
        };
    };
};
var ordProduct = function (dictOrd1) {
    return function (dictOrd11) {
        return function (dictOrd) {
            return {
                compare: Data_Ord.compare1(ord1Product(dictOrd1)(dictOrd11))(dictOrd),
                Eq0: function () {
                    return eqProduct(dictOrd1.Eq10())(dictOrd11.Eq10())(dictOrd.Eq0());
                }
            };
        };
    };
};
var bihoistProduct = function (natF) {
    return function (natG) {
        return function (v) {
            return Data_Bifunctor.bimap(Data_Bifunctor.bifunctorTuple)(natF)(natG)(v);
        };
    };
};
var applyProduct = function (dictApply) {
    return function (dictApply1) {
        return {
            apply: function (v) {
                return function (v1) {
                    return product(Control_Apply.apply(dictApply)(v.value0)(v1.value0))(Control_Apply.apply(dictApply1)(v.value1)(v1.value1));
                };
            },
            Functor0: function () {
                return functorProduct(dictApply.Functor0())(dictApply1.Functor0());
            }
        };
    };
};
var bindProduct = function (dictBind) {
    return function (dictBind1) {
        return {
            bind: function (v) {
                return function (f) {
                    return product(Control_Bind.bind(dictBind)(v.value0)((function () {
                        var $67 = Data_Newtype.unwrap();
                        return function ($68) {
                            return Data_Tuple.fst($67(f($68)));
                        };
                    })()))(Control_Bind.bind(dictBind1)(v.value1)((function () {
                        var $69 = Data_Newtype.unwrap();
                        return function ($70) {
                            return Data_Tuple.snd($69(f($70)));
                        };
                    })()));
                };
            },
            Apply0: function () {
                return applyProduct(dictBind.Apply0())(dictBind1.Apply0());
            }
        };
    };
};
var applicativeProduct = function (dictApplicative) {
    return function (dictApplicative1) {
        return {
            pure: function (a) {
                return product(Control_Applicative.pure(dictApplicative)(a))(Control_Applicative.pure(dictApplicative1)(a));
            },
            Apply0: function () {
                return applyProduct(dictApplicative.Apply0())(dictApplicative1.Apply0());
            }
        };
    };
};
var monadProduct = function (dictMonad) {
    return function (dictMonad1) {
        return {
            Applicative0: function () {
                return applicativeProduct(dictMonad.Applicative0())(dictMonad1.Applicative0());
            },
            Bind1: function () {
                return bindProduct(dictMonad.Bind1())(dictMonad1.Bind1());
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
