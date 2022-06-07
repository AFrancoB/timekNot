import * as $foreign from "./foreign.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad from "../Control.Monad/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
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

// | Update the value of a mutable reference by applying a function
// | to the current value, computing a new state value for the reference and
// | a return value.
var modify$prime = $foreign.modifyImpl;

// | Modify the value of a mutable reference by applying a function to the
// | current value. The modified value is returned.
var modify = function (f) {
    return modify$prime(function (s) {
        var s$prime = f(s);
        return {
            state: s$prime,
            value: s$prime
        };
    });
};
var functorST = {
    map: $foreign.map_
};
var monadST = {
    Applicative0: function () {
        return applicativeST;
    },
    Bind1: function () {
        return bindST;
    }
};
var bindST = {
    bind: $foreign.bind_,
    Apply0: function () {
        return $lazy_applyST(0);
    }
};
var applicativeST = {
    pure: $foreign.pure_,
    Apply0: function () {
        return $lazy_applyST(0);
    }
};
var $lazy_applyST = /* #__PURE__ */ $runtime_lazy("applyST", "Control.Monad.ST.Internal", function () {
    return {
        apply: Control_Monad.ap(monadST),
        Functor0: function () {
            return functorST;
        }
    };
});
var applyST = /* #__PURE__ */ $lazy_applyST(46);
var monadRecST = {
    tailRecM: function (f) {
        return function (a) {
            var isLooping = function (v) {
                if (v instanceof Control_Monad_Rec_Class.Loop) {
                    return true;
                };
                return false;
            };
            var fromDone = function (v) {
                if (v instanceof Control_Monad_Rec_Class.Done) {
                    return v.value0;
                };
                throw new Error("Failed pattern match at Control.Monad.ST.Internal (line 69, column 32 - line 69, column 46): " + [ v.constructor.name ]);
            };
            return Control_Bind.bind(bindST)(Control_Bind.bindFlipped(bindST)($foreign["new"])(f(a)))(function (r) {
                return Control_Bind.discard(Control_Bind.discardUnit)(bindST)($foreign["while"](Data_Functor.map(functorST)(isLooping)($foreign.read(r)))(Control_Bind.bind(bindST)($foreign.read(r))(function (v) {
                    if (v instanceof Control_Monad_Rec_Class.Loop) {
                        return Control_Bind.bind(bindST)(f(v.value0))(function (e) {
                            return Data_Functor["void"](functorST)($foreign.write(e)(r));
                        });
                    };
                    if (v instanceof Control_Monad_Rec_Class.Done) {
                        return Control_Applicative.pure(applicativeST)(Data_Unit.unit);
                    };
                    throw new Error("Failed pattern match at Control.Monad.ST.Internal (line 61, column 18 - line 65, column 28): " + [ v.constructor.name ]);
                })))(function () {
                    return Data_Functor.map(functorST)(fromDone)($foreign.read(r));
                });
            });
        };
    },
    Monad0: function () {
        return monadST;
    }
};
export {
    run,
    while,
    for,
    foreach,
    new,
    read,
    write
} from "./foreign.js";
export {
    modify$prime,
    modify,
    functorST,
    applyST,
    applicativeST,
    bindST,
    monadST,
    monadRecST
};
