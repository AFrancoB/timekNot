// Generated by purs version 0.15.2
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_DateTime from "../Data.DateTime/index.js";
import * as Data_DateTime_Instant from "../Data.DateTime.Instant/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Lazy from "../Data.List.Lazy/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ratio from "../Data.Ratio/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Tempo from "../Data.Tempo/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Effect from "../Effect/index.js";
import * as Effect_Console from "../Effect.Console/index.js";
import * as Effect_Now from "../Effect.Now/index.js";
import * as Effect_Ref from "../Effect.Ref/index.js";
import * as Foreign from "../Foreign/index.js";
import * as Motor from "../Motor/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Rhythmic from "../Rhythmic/index.js";
var unsafeMaybeMilliseconds = function ($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
        if (v instanceof Data_Maybe.Just) {
            $tco_done = true;
            return v.value0;
        };
        if (v instanceof Data_Maybe.Nothing) {
            $copy_v = Data_DateTime_Instant.instant(0.0);
            return;
        };
        throw new Error("Failed pattern match at Main (line 108, column 1 - line 108, column 51): " + [ v.constructor.name ]);
    };
    while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
    };
    return $tco_result;
};
var testMaybeInstant = function (x) {
    return Data_DateTime_Instant.instant(x);
};
var setTempo = function (timekNot) {
    return function (t) {
        return Effect_Ref.write(Data_Tempo.fromForeignTempo(t))(timekNot.tempo);
    };
};
var pErrorToString = function (v) {
    if (v instanceof Data_Either.Left) {
        return new Data_Either.Left(Parsing.parseErrorMessage(v.value0));
    };
    if (v instanceof Data_Either.Right) {
        return new Data_Either.Right(v.value0);
    };
    throw new Error("Failed pattern match at Main (line 82, column 1 - line 82, column 70): " + [ v.constructor.name ]);
};
var numToDateTime = function (x) {
    var asMaybeInstant = Data_DateTime_Instant.instant(x);
    var asInstant = unsafeMaybeMilliseconds(asMaybeInstant);
    return Data_DateTime_Instant.toDateTime(asInstant);
};
var launch = function __do() {
    Effect_Console.log("timekNot-CU: launch")();
    var ast = Effect_Ref["new"](new Rhythmic.Onsets(Data_List_Lazy.fromFoldable(Data_Foldable.foldableArray)([ false ])))();
    var tempo = Control_Bind.bind(Effect.bindEffect)(Data_Tempo.newTempo(Data_Ratio.reduce(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt)(4)(1)))(Effect_Ref["new"])();
    var $$eval = Control_Bind.bind(Effect.bindEffect)(Effect_Now.nowDateTime)(Effect_Ref["new"])();
    return {
        ast: ast,
        tempo: tempo,
        "eval": $$eval
    };
};
var evaluate = function (timekNot) {
    return function (str) {
        return function __do() {
            Effect_Console.log("timekNot-CU: evaluate")();
            var rhythmic = Effect_Ref.read(timekNot.ast)();
            var $$eval = Effect_Now.nowDateTime();
            var pr = pErrorToString(Parsing.runParser(str)(Rhythmic.topRhythmic));
            if (pr instanceof Data_Either.Left) {
                return {
                    success: false,
                    error: pr.value0
                };
            };
            if (pr instanceof Data_Either.Right) {
                Effect_Ref.write($$eval)(timekNot["eval"])();
                Effect_Ref.write(pr.value0)(timekNot.ast)();
                return {
                    success: true,
                    error: ""
                };
            };
            throw new Error("Failed pattern match at Main (line 75, column 3 - line 80, column 42): " + [ pr.constructor.name ]);
        };
    };
};
var debugging = function (a) {
    return function __do() {
        Effect_Console.log(Data_Show.show(Data_Show.showNumber)(a))();
        return Data_Unit.unit;
    };
};
var coordToEvent = function (v) {
    return {
        whenPosix: v.value0,
        s: "cp",
        n: 0
    };
};
var fromCoordenateToArray = function (x) {
    return function (t) {
        return function (ws) {
            return function (we) {
                return function ($$eval) {
                    var coords = Motor.fromPassageToCoord(x)(t)(ws)(we)($$eval);
                    var coordsfromMapToArray = Data_List.toUnfoldable(Data_Unfoldable.unfoldableArray)(Data_Map_Internal.values(coords));
                    var events = Data_Functor.map(Data_Functor.functorArray)(coordToEvent)(coordsfromMapToArray);
                    return events;
                };
            };
        };
    };
};
var timekNotToEvents = function (tk) {
    return function (ws) {
        return function (we) {
            return function __do() {
                var rhy = Effect_Ref.read(tk.ast)();
                var t = Effect_Ref.read(tk.tempo)();
                var $$eval = Effect_Ref.read(tk["eval"])();
                Effect_Console.log(Data_Show.show(Rhythmic.rhythmicShowInstance)(rhy))();
                Effect_Console.log(Data_Show.show(Data_DateTime.showDateTime)(ws))();
                Effect_Console.log(Data_Show.show(Data_DateTime.showDateTime)(we))();
                var events = fromCoordenateToArray(rhy)(t)(ws)(we)($$eval);
                Effect_Console.log(Data_Show.show(Data_Show.showArray(Data_Show.showRecord()()(Data_Show.showRecordFieldsCons({
                    reflectSymbol: function () {
                        return "n";
                    }
                })(Data_Show.showRecordFieldsCons({
                    reflectSymbol: function () {
                        return "s";
                    }
                })(Data_Show.showRecordFieldsCons({
                    reflectSymbol: function () {
                        return "whenPosix";
                    }
                })(Data_Show.showRecordFieldsNil)(Data_Show.showNumber))(Data_Show.showString))(Data_Show.showInt))))(events))();
                return Data_Functor.map(Data_Functor.functorArray)(Foreign.unsafeToForeign)(events);
            };
        };
    };
};
var scheduleNoteEvents = function (tk) {
    return function (ws) {
        return function (we) {
            var d2 = debugging(we);
            var d1 = debugging(ws);
            return timekNotToEvents(tk)(numToDateTime(ws))(numToDateTime(we));
        };
    };
};
export {
    launch,
    evaluate,
    pErrorToString,
    setTempo,
    scheduleNoteEvents,
    numToDateTime,
    unsafeMaybeMilliseconds,
    timekNotToEvents,
    fromCoordenateToArray,
    debugging,
    coordToEvent,
    testMaybeInstant
};
