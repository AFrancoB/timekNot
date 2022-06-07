import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Ring from "../Data.Ring/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Data_Show from "../Data.Show/index.js";
var Ratio = /* #__PURE__ */ (function () {
    function Ratio(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Ratio.create = function (value0) {
        return function (value1) {
            return new Ratio(value0, value1);
        };
    };
    return Ratio;
})();
var showRatio = function (dictShow) {
    return {
        show: function (v) {
            return Data_Show.show(dictShow)(v.value0) + (" % " + Data_Show.show(dictShow)(v.value1));
        }
    };
};
var reduce = function (dictOrd) {
    return function (dictEuclideanRing) {
        return function (n) {
            return function (d) {
                var g = Data_EuclideanRing.gcd(dictOrd.Eq0())(dictEuclideanRing)(n)(d);
                var d$prime = Data_EuclideanRing.div(dictEuclideanRing)(d)(g);
                return new Ratio(Data_Semiring.mul(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())(Data_EuclideanRing.div(dictEuclideanRing)(n)(g))(Data_Ord.signum(dictOrd)((dictEuclideanRing.CommutativeRing0()).Ring0())(d$prime)), Data_Ord.abs(dictOrd)((dictEuclideanRing.CommutativeRing0()).Ring0())(d$prime));
            };
        };
    };
};
var semiringRatio = function (dictOrd) {
    return function (dictEuclideanRing) {
        return {
            one: new Ratio(Data_Semiring.one(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0()), Data_Semiring.one(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())),
            mul: function (v) {
                return function (v1) {
                    return reduce(dictOrd)(dictEuclideanRing)(Data_Semiring.mul(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())(v.value0)(v1.value0))(Data_Semiring.mul(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())(v.value1)(v1.value1));
                };
            },
            zero: new Ratio(Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0()), Data_Semiring.one(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())),
            add: function (v) {
                return function (v1) {
                    return reduce(dictOrd)(dictEuclideanRing)(Data_Semiring.add(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())(Data_Semiring.mul(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())(v.value0)(v1.value1))(Data_Semiring.mul(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())(v.value1)(v1.value0)))(Data_Semiring.mul(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())(v.value1)(v1.value1));
                };
            }
        };
    };
};
var ringRatio = function (dictOrd) {
    return function (dictEuclideanRing) {
        return {
            sub: function (v) {
                return function (v1) {
                    return reduce(dictOrd)(dictEuclideanRing)(Data_Ring.sub((dictEuclideanRing.CommutativeRing0()).Ring0())(Data_Semiring.mul(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())(v.value0)(v1.value1))(Data_Semiring.mul(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())(v.value1)(v1.value0)))(Data_Semiring.mul(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())(v.value1)(v1.value1));
                };
            },
            Semiring0: function () {
                return semiringRatio(dictOrd)(dictEuclideanRing);
            }
        };
    };
};
var numerator = function (v) {
    return v.value0;
};
var eqRatio = function (dictEq) {
    return {
        eq: function (v) {
            return function (v1) {
                return Data_Eq.eq(dictEq)(v.value0)(v1.value0) && Data_Eq.eq(dictEq)(v.value1)(v1.value1);
            };
        }
    };
};
var ordRatio = function (dictOrd) {
    return function (dictEuclideanRing) {
        return {
            compare: function (x) {
                return function (y) {
                    var v = Data_Ring.sub(ringRatio(dictOrd)(dictEuclideanRing))(x)(y);
                    var $70 = Data_Eq.eq(dictOrd.Eq0())(v.value0)(Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0()));
                    if ($70) {
                        return Data_Ordering.EQ.value;
                    };
                    var v1 = Data_Ord.greaterThan(dictOrd)(v.value1)(Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0()));
                    var v2 = Data_Ord.greaterThan(dictOrd)(v.value0)(Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0()));
                    if (v2 && v1) {
                        return Data_Ordering.GT.value;
                    };
                    if (!v2 && !v1) {
                        return Data_Ordering.GT.value;
                    };
                    return Data_Ordering.LT.value;
                };
            },
            Eq0: function () {
                return eqRatio(dictOrd.Eq0());
            }
        };
    };
};
var divisionRingRatio = function (dictOrd) {
    return function (dictEuclideanRing) {
        return {
            recip: function (v) {
                return new Ratio(v.value1, v.value0);
            },
            Ring0: function () {
                return ringRatio(dictOrd)(dictEuclideanRing);
            }
        };
    };
};
var denominator = function (v) {
    return v.value1;
};
var commutativeRingRatio = function (dictOrd) {
    return function (dictEuclideanRing) {
        return {
            Ring0: function () {
                return ringRatio(dictOrd)(dictEuclideanRing);
            }
        };
    };
};
var euclideanRingRatio = function (dictOrd) {
    return function (dictEuclideanRing) {
        return {
            degree: function (v) {
                return 1;
            },
            div: function (v) {
                return function (v1) {
                    return reduce(dictOrd)(dictEuclideanRing)(Data_Semiring.mul(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())(v.value0)(v1.value1))(Data_Semiring.mul(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())(v.value1)(v1.value0));
                };
            },
            mod: function (v) {
                return function (v1) {
                    return Data_Semiring.zero(semiringRatio(dictOrd)(dictEuclideanRing));
                };
            },
            CommutativeRing0: function () {
                return commutativeRingRatio(dictOrd)(dictEuclideanRing);
            }
        };
    };
};
export {
    reduce,
    numerator,
    denominator,
    showRatio,
    eqRatio,
    ordRatio,
    semiringRatio,
    ringRatio,
    commutativeRingRatio,
    euclideanRingRatio,
    divisionRingRatio
};
