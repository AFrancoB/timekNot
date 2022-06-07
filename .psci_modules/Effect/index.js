// | This module provides the `Effect` type, which is used to represent
// | _native_ effects. The `Effect` type provides a typed API for effectful
// | computations, while at the same time generating efficient JavaScript.
import * as $foreign from "./foreign.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Monad from "../Control.Monad/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
var $runtime_lazy = function (name, moduleName, init) {
    var state = 0;
    var val;
    return function (lineNumber) {
        if (state === 2) return val;
        if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
        state = 1;
        val = init();
        state = 2;
        return val;
    };
};
var monadEffect = {
    Applicative0: function () {
        return applicativeEffect;
    },
    Bind1: function () {
        return bindEffect;
    }
};
var bindEffect = {
    bind: $foreign.bindE,
    Apply0: function () {
        return $lazy_applyEffect(0);
    }
};
var applicativeEffect = {
    pure: $foreign.pureE,
    Apply0: function () {
        return $lazy_applyEffect(0);
    }
};
var $lazy_functorEffect = /* #__PURE__ */ $runtime_lazy("functorEffect", "Effect", function () {
    return {
        map: Control_Applicative.liftA1(applicativeEffect)
    };
});
var $lazy_applyEffect = /* #__PURE__ */ $runtime_lazy("applyEffect", "Effect", function () {
    return {
        apply: Control_Monad.ap(monadEffect),
        Functor0: function () {
            return $lazy_functorEffect(0);
        }
    };
});
var functorEffect = /* #__PURE__ */ $lazy_functorEffect(20);
var applyEffect = /* #__PURE__ */ $lazy_applyEffect(23);

// | The `Semigroup` instance for effects allows you to run two effects, one
// | after the other, and then combine their results using the result type's
// | `Semigroup` instance.
var semigroupEffect = function (dictSemigroup) {
    return {
        append: Control_Apply.lift2(applyEffect)(Data_Semigroup.append(dictSemigroup))
    };
};

// | If you have a `Monoid a` instance, then `mempty :: Effect a` is defined as
// | `pure mempty`.
var monoidEffect = function (dictMonoid) {
    return {
        mempty: $foreign.pureE(Data_Monoid.mempty(dictMonoid)),
        Semigroup0: function () {
            return semigroupEffect(dictMonoid.Semigroup0());
        }
    };
};
export {
    untilE,
    whileE,
    forE,
    foreachE
} from "./foreign.js";
export {
    functorEffect,
    applyEffect,
    applicativeEffect,
    bindEffect,
    monadEffect,
    semigroupEffect,
    monoidEffect
};
