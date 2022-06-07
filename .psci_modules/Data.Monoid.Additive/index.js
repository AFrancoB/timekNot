import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Data_Show from "../Data.Show/index.js";

// | Monoid and semigroup for semirings under addition.
// |
// | ``` purescript
// | Additive x <> Additive y == Additive (x + y)
// | (mempty :: Additive _) == Additive zero
// | ```
var Additive = function (x) {
    return x;
};
var showAdditive = function (dictShow) {
    return {
        show: function (v) {
            return "(Additive " + (Data_Show.show(dictShow)(v) + ")");
        }
    };
};
var semigroupAdditive = function (dictSemiring) {
    return {
        append: function (v) {
            return function (v1) {
                return Data_Semiring.add(dictSemiring)(v)(v1);
            };
        }
    };
};
var ordAdditive = function (dictOrd) {
    return dictOrd;
};
var monoidAdditive = function (dictSemiring) {
    return {
        mempty: Data_Semiring.zero(dictSemiring),
        Semigroup0: function () {
            return semigroupAdditive(dictSemiring);
        }
    };
};
var functorAdditive = {
    map: function (f) {
        return function (m) {
            return f(m);
        };
    }
};
var eqAdditive = function (dictEq) {
    return dictEq;
};
var eq1Additive = {
    eq1: function (dictEq) {
        return Data_Eq.eq(eqAdditive(dictEq));
    }
};
var ord1Additive = {
    compare1: function (dictOrd) {
        return Data_Ord.compare(ordAdditive(dictOrd));
    },
    Eq10: function () {
        return eq1Additive;
    }
};
var boundedAdditive = function (dictBounded) {
    return dictBounded;
};
var applyAdditive = {
    apply: function (v) {
        return function (v1) {
            return v(v1);
        };
    },
    Functor0: function () {
        return functorAdditive;
    }
};
var bindAdditive = {
    bind: function (v) {
        return function (f) {
            return f(v);
        };
    },
    Apply0: function () {
        return applyAdditive;
    }
};
var applicativeAdditive = {
    pure: Additive,
    Apply0: function () {
        return applyAdditive;
    }
};
var monadAdditive = {
    Applicative0: function () {
        return applicativeAdditive;
    },
    Bind1: function () {
        return bindAdditive;
    }
};
export {
    Additive,
    eqAdditive,
    eq1Additive,
    ordAdditive,
    ord1Additive,
    boundedAdditive,
    showAdditive,
    functorAdditive,
    applyAdditive,
    applicativeAdditive,
    bindAdditive,
    monadAdditive,
    semigroupAdditive,
    monoidAdditive
};
