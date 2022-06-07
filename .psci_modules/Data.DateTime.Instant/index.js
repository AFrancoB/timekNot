import * as $foreign from "./foreign.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Bounded from "../Data.Bounded/index.js";
import * as Data_Date from "../Data.Date/index.js";
import * as Data_Date_Component from "../Data.Date.Component/index.js";
import * as Data_DateTime from "../Data.DateTime/index.js";
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Time from "../Data.Time/index.js";
import * as Data_Time_Component from "../Data.Time.Component/index.js";
import * as Data_Time_Duration from "../Data.Time.Duration/index.js";

// | An instant is a duration in milliseconds relative to the Unix epoch
// | (1970-01-01 00:00:00 UTC).
// |
// | The constructor is private as the `Instant` range matches that of the
// | `DateTime` type.
var Instant = function (x) {
    return x;
};

// | Lowers an `Instant` to a `Milliseconds` duration.
var unInstant = function (v) {
    return v;
};

// | Creates a `DateTime` value from an `Instant`.
var toDateTime = /* #__PURE__ */ (function () {
    var mkDateTime = function (y) {
        return function (mo) {
            return function (d) {
                return function (h) {
                    return function (mi) {
                        return function (s) {
                            return function (ms) {
                                return new Data_DateTime.DateTime(Data_Date.canonicalDate(y)(Data_Maybe.fromJust()(Data_Enum.toEnum(Data_Date_Component.boundedEnumMonth)(mo)))(d), new Data_Time.Time(h, mi, s, ms));
                            };
                        };
                    };
                };
            };
        };
    };
    return $foreign.toDateTimeImpl(mkDateTime);
})();
var showInstant = {
    show: function (v) {
        return "(Instant " + (Data_Show.show(Data_Time_Duration.showMilliseconds)(v) + ")");
    }
};
var ordDateTime = Data_Time_Duration.ordMilliseconds;

// Unfortunately Instant cannot be made a `BoundedEnum` as it "should" be,
// unless enum cardinality and from/to range is extended to use a numeric type
// bigger than Int32
// | Attempts to create an `Instant` from a `Milliseconds` duration. The
// | minimum acceptable value equates to the `bottom` `DateTime` and the maximum
// | acceptable value equates to the `top` `DateTime`.
var instant = function (v) {
    if (v >= -8.6399778816e15 && v <= 8.639977881599999e15) {
        return new Data_Maybe.Just(v);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.DateTime.Instant (line 44, column 1 - line 44, column 41): " + [ v.constructor.name ]);
};

// | Creates an `Instant` from a `DateTime` value.
var fromDateTime = function (v) {
    return $foreign.fromDateTimeImpl(Data_Date.year(v.value0), Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(Data_Date.month(v.value0)), Data_Date.day(v.value0), Data_Time.hour(v.value1), Data_Time.minute(v.value1), Data_Time.second(v.value1), Data_Time.millisecond(v.value1));
};

// | Creates an `Instant` from a `Date` value, using the assumed time 00:00:00.
var fromDate = function (d) {
    return $foreign.fromDateTimeImpl(Data_Date.year(d), Data_Enum.fromEnum(Data_Date_Component.boundedEnumMonth)(Data_Date.month(d)), Data_Date.day(d), Data_Bounded.bottom(Data_Time_Component.boundedHour), Data_Bounded.bottom(Data_Time_Component.boundedMinute), Data_Bounded.bottom(Data_Time_Component.boundedSecond), Data_Bounded.bottom(Data_Time_Component.boundedMillisecond));
};
var eqDateTime = Data_Time_Duration.eqMilliseconds;
var boundedInstant = /* #__PURE__ */ (function () {
    return {
        bottom: -8.6399778816e15,
        top: 8.639977881599999e15,
        Ord0: function () {
            return ordDateTime;
        }
    };
})();
export {
    instant,
    unInstant,
    fromDateTime,
    fromDate,
    toDateTime,
    eqDateTime,
    ordDateTime,
    boundedInstant,
    showInstant
};
