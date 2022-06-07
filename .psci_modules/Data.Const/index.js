import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor_Invariant from "../Data.Functor.Invariant/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Show from "../Data.Show/index.js";
var Const = function (x) {
    return x;
};
var showConst = function (dictShow) {
    return {
        show: function (v) {
            return "(Const " + (Data_Show.show(dictShow)(v) + ")");
        }
    };
};
var semiringConst = function (dictSemiring) {
    return dictSemiring;
};
var semigroupoidConst = {
    compose: function (v) {
        return function (v1) {
            return v1;
        };
    }
};
var semigroupConst = function (dictSemigroup) {
    return dictSemigroup;
};
var ringConst = function (dictRing) {
    return dictRing;
};
var ordConst = function (dictOrd) {
    return dictOrd;
};
var newtypeConst = {
    Coercible0: function () {
        return undefined;
    }
};
var monoidConst = function (dictMonoid) {
    return dictMonoid;
};
var heytingAlgebraConst = function (dictHeytingAlgebra) {
    return dictHeytingAlgebra;
};
var functorConst = {
    map: function (f) {
        return function (m) {
            return m;
        };
    }
};
var invariantConst = {
    imap: /* #__PURE__ */ Data_Functor_Invariant.imapF(functorConst)
};
var euclideanRingConst = function (dictEuclideanRing) {
    return dictEuclideanRing;
};
var eqConst = function (dictEq) {
    return dictEq;
};
var eq1Const = function (dictEq) {
    return {
        eq1: function (dictEq1) {
            return Data_Eq.eq(eqConst(dictEq));
        }
    };
};
var ord1Const = function (dictOrd) {
    return {
        compare1: function (dictOrd1) {
            return Data_Ord.compare(ordConst(dictOrd));
        },
        Eq10: function () {
            return eq1Const(dictOrd.Eq0());
        }
    };
};
var commutativeRingConst = function (dictCommutativeRing) {
    return dictCommutativeRing;
};
var boundedConst = function (dictBounded) {
    return dictBounded;
};
var booleanAlgebraConst = function (dictBooleanAlgebra) {
    return dictBooleanAlgebra;
};
var applyConst = function (dictSemigroup) {
    return {
        apply: function (v) {
            return function (v1) {
                return Data_Semigroup.append(dictSemigroup)(v)(v1);
            };
        },
        Functor0: function () {
            return functorConst;
        }
    };
};
var applicativeConst = function (dictMonoid) {
    return {
        pure: function (v) {
            return Data_Monoid.mempty(dictMonoid);
        },
        Apply0: function () {
            return applyConst(dictMonoid.Semigroup0());
        }
    };
};
export {
    Const,
    newtypeConst,
    eqConst,
    eq1Const,
    ordConst,
    ord1Const,
    boundedConst,
    showConst,
    semigroupoidConst,
    semigroupConst,
    monoidConst,
    semiringConst,
    ringConst,
    euclideanRingConst,
    commutativeRingConst,
    heytingAlgebraConst,
    booleanAlgebraConst,
    functorConst,
    invariantConst,
    applyConst,
    applicativeConst
};
