import * as Control_Semigroupoid from "../Control.Semigroupoid/index.js";
var identity = function (dict) {
    return dict.identity;
};
var categoryFn = {
    identity: function (x) {
        return x;
    },
    Semigroupoid0: function () {
        return Control_Semigroupoid.semigroupoidFn;
    }
};
export {
    identity,
    categoryFn
};
export {
    compose
} from "../Control.Semigroupoid/index.js";
