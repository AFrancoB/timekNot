import * as Effect_Class from "../Effect.Class/index.js";
import * as Effect_Class_Console from "../Effect.Class.Console/index.js";
var main = function __do() {
    Effect_Class_Console.log(Effect_Class.monadEffectEffect)("\ud83c\udf5d")();
    return Effect_Class_Console.log(Effect_Class.monadEffectEffect)("You should add some tests.")();
};
export {
    main
};
