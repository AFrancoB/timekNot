import * as Control_Category from "../Control.Category/index.js";
import * as Data_Exists from "../Data.Exists/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Functor_Invariant from "../Data.Functor.Invariant/index.js";
var SplitF = /* #__PURE__ */ (function () {
    function SplitF(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    SplitF.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new SplitF(value0, value1, value2);
            };
        };
    };
    return SplitF;
})();
var Split = function (x) {
    return x;
};
var unSplit = function (f) {
    return function (v) {
        return Data_Exists.runExists(function (v1) {
            return f(v1.value0)(v1.value1)(v1.value2);
        })(v);
    };
};
var split = function (f) {
    return function (g) {
        return function (fx) {
            return Data_Exists.mkExists(new SplitF(f, g, fx));
        };
    };
};
var profunctorSplit = {
    dimap: function (f) {
        return function (g) {
            return unSplit(function (h) {
                return function (i) {
                    return split(function ($9) {
                        return h(f($9));
                    })(function ($10) {
                        return g(i($10));
                    });
                };
            });
        };
    }
};
var lowerSplit = function (dictInvariant) {
    return unSplit(Data_Function.flip(Data_Functor_Invariant.imap(dictInvariant)));
};
var liftSplit = /* #__PURE__ */ split(/* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn))(/* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn));
var hoistSplit = function (nat) {
    return unSplit(function (f) {
        return function (g) {
            var $11 = split(f)(g);
            return function ($12) {
                return $11(nat($12));
            };
        };
    });
};
var functorSplit = {
    map: function (f) {
        return unSplit(function (g) {
            return function (h) {
                return function (fx) {
                    return split(g)(function ($13) {
                        return f(h($13));
                    })(fx);
                };
            };
        });
    }
};
export {
    split,
    unSplit,
    liftSplit,
    lowerSplit,
    hoistSplit,
    functorSplit,
    profunctorSplit
};
