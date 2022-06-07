import * as Control_Category from "../Control.Category/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
var profunctorFn = {
    dimap: function (a2b) {
        return function (c2d) {
            return function (b2c) {
                return function ($8) {
                    return c2d(b2c(a2b($8)));
                };
            };
        };
    }
};
var dimap = function (dict) {
    return dict.dimap;
};

// | Map a function over the (contravariant) first type argument only.
var lcmap = function (dictProfunctor) {
    return function (a2b) {
        return dimap(dictProfunctor)(a2b)(Control_Category.identity(Control_Category.categoryFn));
    };
};

// | Map a function over the (covariant) second type argument only.
var rmap = function (dictProfunctor) {
    return function (b2c) {
        return dimap(dictProfunctor)(Control_Category.identity(Control_Category.categoryFn))(b2c);
    };
};
var unwrapIso = function (dictProfunctor) {
    return function () {
        return dimap(dictProfunctor)(Data_Newtype.wrap())(Data_Newtype.unwrap());
    };
};
var wrapIso = function (dictProfunctor) {
    return function () {
        return function (v) {
            return dimap(dictProfunctor)(Data_Newtype.unwrap())(Data_Newtype.wrap());
        };
    };
};

// | Lift a pure function into any `Profunctor` which is also a `Category`.
var arr = function (dictCategory) {
    return function (dictProfunctor) {
        return function (f) {
            return rmap(dictProfunctor)(f)(Control_Category.identity(dictCategory));
        };
    };
};
export {
    dimap,
    lcmap,
    rmap,
    arr,
    unwrapIso,
    wrapIso,
    profunctorFn
};
