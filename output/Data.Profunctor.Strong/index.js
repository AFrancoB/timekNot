// Generated by purs version 0.15.10
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Semigroupoid from "../Control.Semigroupoid/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Profunctor from "../Data.Profunctor/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
var strongFn = {
    first: function (a2b) {
        return function (v) {
            return new Data_Tuple.Tuple(a2b(v.value0), v.value1);
        };
    },
    second: /* #__PURE__ */ Data_Functor.map(Data_Tuple.functorTuple),
    Profunctor0: function () {
        return Data_Profunctor.profunctorFn;
    }
};
var second = function (dict) {
    return dict.second;
};
var first = function (dict) {
    return dict.first;
};
var splitStrong = function (dictCategory) {
    var composeFlipped = Control_Semigroupoid.composeFlipped(dictCategory.Semigroupoid0());
    return function (dictStrong) {
        var first1 = first(dictStrong);
        var second1 = second(dictStrong);
        return function (l) {
            return function (r) {
                return composeFlipped(first1(l))(second1(r));
            };
        };
    };
};
var fanout = function (dictCategory) {
    var identity1 = Control_Category.identity(dictCategory);
    var composeFlipped = Control_Semigroupoid.composeFlipped(dictCategory.Semigroupoid0());
    var splitStrong1 = splitStrong(dictCategory);
    return function (dictStrong) {
        var dimap = Data_Profunctor.dimap(dictStrong.Profunctor0());
        var splitStrong2 = splitStrong1(dictStrong);
        return function (l) {
            return function (r) {
                var split = dimap(identity)(function (a) {
                    return new Data_Tuple.Tuple(a, a);
                })(identity1);
                return composeFlipped(split)(splitStrong2(l)(r));
            };
        };
    };
};
export {
    first,
    second,
    splitStrong,
    fanout,
    strongFn
};
