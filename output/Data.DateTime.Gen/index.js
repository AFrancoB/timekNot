// Generated by purs version 0.15.10
import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Date_Gen from "../Data.Date.Gen/index.js";
import * as Data_DateTime from "../Data.DateTime/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Time_Gen from "../Data.Time.Gen/index.js";
var genDateTime = function (dictMonadGen) {
    var Apply0 = ((dictMonadGen.Monad0()).Bind1()).Apply0();
    return Control_Apply.apply(Apply0)(Data_Functor.map(Apply0.Functor0())(Data_DateTime.DateTime.create)(Data_Date_Gen.genDate(dictMonadGen)))(Data_Time_Gen.genTime(dictMonadGen));
};
export {
    genDateTime
};
export {
    genDate,
    genDay,
    genMonth,
    genWeekday,
    genYear
} from "../Data.Date.Gen/index.js";
export {
    genHour,
    genMillisecond,
    genMinute,
    genSecond,
    genTime
} from "../Data.Time.Gen/index.js";
