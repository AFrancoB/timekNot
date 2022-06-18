// Generated by purs version 0.15.2
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Date from "../Data.Date/index.js";
import * as Data_Date_Component from "../Data.Date.Component/index.js";
import * as Data_DateTime from "../Data.DateTime/index.js";
import * as Data_DateTime_Instant from "../Data.DateTime.Instant/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Lazy from "../Data.List.Lazy/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ratio from "../Data.Ratio/index.js";
import * as Data_Rational from "../Data.Rational/index.js";
import * as Data_Tempo from "../Data.Tempo/index.js";
import * as Data_Time from "../Data.Time/index.js";
import * as Data_Time_Component from "../Data.Time.Component/index.js";
import * as Data_Unfoldable from "../Data.Unfoldable/index.js";
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
        throw new Error("Failed pattern match at Main (line 111, column 1 - line 111, column 51): " + [ v.constructor.name ]);
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
    return function (t1) {
        return Effect_Ref.write(Data_Tempo.fromForeignTempo(t1))(timekNot.tempo);
    };
};
var pErrorToString = function (v) {
    if (v instanceof Data_Either.Left) {
        return new Data_Either.Left(Parsing.parseErrorMessage(v.value0));
    };
    if (v instanceof Data_Either.Right) {
        return new Data_Either.Right(v.value0);
    };
    throw new Error("Failed pattern match at Main (line 74, column 1 - line 74, column 70): " + [ v.constructor.name ]);
};
var numToDateTime = function (x) {
    var asMaybeInstant = Data_DateTime_Instant.instant(x);
    var asInstant = unsafeMaybeMilliseconds(asMaybeInstant);
    return Data_DateTime_Instant.toDateTime(asInstant);
};
var makeTime = function (h) {
    return function (min) {
        return function (sec) {
            return function (milisec) {
                return Data_Maybe.fromJust()(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Time.Time.create)(Data_Enum.toEnum(Data_Time_Component.boundedEnumHour)(h)))(Data_Enum.toEnum(Data_Time_Component.boundedEnumMinute)(min)))(Data_Enum.toEnum(Data_Time_Component.boundedEnumSecond)(sec)))(Data_Enum.toEnum(Data_Time_Component.boundedEnumMillisecond)(milisec)));
            };
        };
    };
};
var makeDate = function (y) {
    return function (m) {
        return function (d) {
            return Data_Maybe.fromJust()(Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.flap(Data_Maybe.functorMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Date.canonicalDate)(Data_Enum.toEnum(Data_Date_Component.boundedEnumYear)(y)))(m))(Data_Enum.toEnum(Data_Date_Component.boundedEnumDay)(d)));
        };
    };
};
var t = /* #__PURE__ */ (function () {
    return {
        freq: Data_Ratio.reduce(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt)(2)(1),
        time: new Data_DateTime.DateTime(makeDate(2022)(Data_Date_Component.June.value)(3), makeTime(19)(11)(25)(100)),
        count: Data_Rational.fromInt(0)
    };
})();
var we = function (x) {
    return function (y) {
        return new Data_DateTime.DateTime(makeDate(2022)(Data_Date_Component.June.value)(3), makeTime(19)(15)(x)(y));
    };
};
var ws = function (x) {
    return function (y) {
        return new Data_DateTime.DateTime(makeDate(2022)(Data_Date_Component.June.value)(3), makeTime(19)(15)(x)(y));
    };
};
var launch = function __do() {
    Effect_Console.log("testLang: launch")();
    var ast = Effect_Ref["new"](new Rhythmic.Onsets(Data_List_Lazy.fromFoldable(Data_Foldable.foldableArray)([ false ])))();
    var tempo = Control_Bind.bind(Effect.bindEffect)(Data_Tempo.newTempo(Data_Ratio.reduce(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt)(4)(1)))(Effect_Ref["new"])();
    var eval1 = Control_Bind.bind(Effect.bindEffect)(Effect_Now.nowDateTime)(Effect_Ref["new"])();
    return {
        ast: ast,
        tempo: tempo,
        "eval": eval1
    };
};
var evaluate = function (timekNot) {
    return function (str) {
        return function __do() {
            Effect_Console.log("testLang: evaluate")();
            var eval1 = Effect_Now.nowDateTime();
            var pr = pErrorToString(Parsing.runParser(str)(Rhythmic.topRhythmic));
            if (pr instanceof Data_Either.Left) {
                return {
                    success: false,
                    error: pr.value0
                };
            };
            if (pr instanceof Data_Either.Right) {
                Effect_Ref.write(eval1)(timekNot["eval"])();
                Effect_Ref.write(pr.value0)(timekNot.ast)();
                return {
                    success: true,
                    error: ""
                };
            };
            throw new Error("Failed pattern match at Main (line 67, column 3 - line 72, column 42): " + [ pr.constructor.name ]);
        };
    };
};
var $$eval = /* #__PURE__ */ (function () {
    return new Data_DateTime.DateTime(makeDate(2022)(Data_Date_Component.June.value)(3), makeTime(19)(13)(5)(150));
})();
var coordToEvent = function (v) {
    return {
        whenPosix: v.value0,
        s: "cp",
        n: 0
    };
};
var fromCoordenateToArray = function (x) {
    return function (t1) {
        return function (ws1) {
            return function (we1) {
                return function (eval1) {
                    var coords = Motor.fromPassageToCoord(x)(t1)(ws1)(we1)(eval1);
                    var coordsfromMapToArray = Data_List.toUnfoldable(Data_Unfoldable.unfoldableArray)(Data_Map_Internal.values(coords));
                    var events = Data_Functor.map(Data_Functor.functorArray)(coordToEvent)(coordsfromMapToArray);
                    return events;
                };
            };
        };
    };
};
var timekNotToEvents = function (tk) {
    return function (ws1) {
        return function (we1) {
            return function __do() {
                var rhy = Effect_Ref.read(tk.ast)();
                var t1 = Effect_Ref.read(tk.tempo)();
                var eval1 = Effect_Ref.read(tk["eval"])();
                var events = fromCoordenateToArray(rhy)(t1)(ws1)(we1)(eval1);
                return Data_Functor.map(Data_Functor.functorArray)(Foreign.unsafeToForeign)(events);
            };
        };
    };
};
var scheduleNoteEvents = function (tk) {
    return function (ws1) {
        return function (we1) {
            return timekNotToEvents(tk)(ws1)(we1);
        };
    };
};
export {
    launch,
    evaluate,
    pErrorToString,
    setTempo,
    scheduleNoteEvents,
    timekNotToEvents,
    fromCoordenateToArray,
    coordToEvent,
    numToDateTime,
    unsafeMaybeMilliseconds,
    testMaybeInstant,
    makeDate,
    makeTime,
    t,
    ws,
    we,
    $$eval as eval
};
