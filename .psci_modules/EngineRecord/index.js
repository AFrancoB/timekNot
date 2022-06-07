import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ratio from "../Data.Ratio/index.js";
import * as Data_Tempo from "../Data.Tempo/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect_Console from "../Effect.Console/index.js";
import * as Effect_Ref from "../Effect.Ref/index.js";
var setTempo = function (timekNot) {
    return function (t) {
        return Effect_Ref.write(Data_Tempo.fromForeignTempo(t))(timekNot.tempo);
    };
};
var scheduleNoteEvents = function (v) {
    return function (v1) {
        return function (v2) {
            return Control_Applicative.pure(Effect.applicativeEffect)(Data_List.singleton({
                s: "cp"
            }));
        };
    };
};
var launch = function __do() {
    Effect_Console.log("testLang: launch")();
    var ast = Effect_Ref["new"](false)();
    var tempo = Control_Bind.bind(Effect.bindEffect)(Data_Tempo.newTempo(Data_Ratio.reduce(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt)(1)(2)))(Effect_Ref["new"])();
    return {
        ast: ast,
        tempo: tempo
    };
};
var evaluate = function (timekNot) {
    return function (v) {
        return function (v1) {
            return function __do() {
                Effect_Console.log("testLang: evaluate")();
                var pr = new Data_Either.Right(true);
                if (pr instanceof Data_Either.Left) {
                    return {
                        success: false,
                        error: pr.value0
                    };
                };
                if (pr instanceof Data_Either.Right) {
                    Effect_Ref.write(pr.value0)(timekNot.ast)();
                    return {
                        success: true,
                        error: ""
                    };
                };
                throw new Error("Failed pattern match at EngineRecord (line 38, column 3 - line 42, column 42): " + [ pr.constructor.name ]);
            };
        };
    };
};
export {
    launch,
    evaluate,
    setTempo,
    scheduleNoteEvents
};
