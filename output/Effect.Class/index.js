// Generated by purs version 0.15.10
import * as Control_Category from "../Control.Category/index.js";
import * as Effect from "../Effect/index.js";
var monadEffectEffect = {
    liftEffect: /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn),
    Monad0: function () {
        return Effect.monadEffect;
    }
};
var liftEffect = function (dict) {
    return dict.liftEffect;
};
export {
    liftEffect,
    monadEffectEffect
};
