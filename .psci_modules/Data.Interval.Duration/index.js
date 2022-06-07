import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Data_Show from "../Data.Show/index.js";
var Second = /* #__PURE__ */ (function () {
    function Second() {

    };
    Second.value = new Second();
    return Second;
})();
var Minute = /* #__PURE__ */ (function () {
    function Minute() {

    };
    Minute.value = new Minute();
    return Minute;
})();
var Hour = /* #__PURE__ */ (function () {
    function Hour() {

    };
    Hour.value = new Hour();
    return Hour;
})();
var Day = /* #__PURE__ */ (function () {
    function Day() {

    };
    Day.value = new Day();
    return Day;
})();
var Week = /* #__PURE__ */ (function () {
    function Week() {

    };
    Week.value = new Week();
    return Week;
})();
var Month = /* #__PURE__ */ (function () {
    function Month() {

    };
    Month.value = new Month();
    return Month;
})();
var Year = /* #__PURE__ */ (function () {
    function Year() {

    };
    Year.value = new Year();
    return Year;
})();
var Duration = function (x) {
    return x;
};
var showDurationComponent = {
    show: function (v) {
        if (v instanceof Minute) {
            return "Minute";
        };
        if (v instanceof Second) {
            return "Second";
        };
        if (v instanceof Hour) {
            return "Hour";
        };
        if (v instanceof Day) {
            return "Day";
        };
        if (v instanceof Week) {
            return "Week";
        };
        if (v instanceof Month) {
            return "Month";
        };
        if (v instanceof Year) {
            return "Year";
        };
        throw new Error("Failed pattern match at Data.Interval.Duration (line 38, column 1 - line 45, column 21): " + [ v.constructor.name ]);
    }
};
var showDuration = {
    show: function (v) {
        return "(Duration " + (Data_Show.show(Data_Map_Internal.showMap(showDurationComponent)(Data_Show.showNumber))(v) + ")");
    }
};
var newtypeDuration = {
    Coercible0: function () {
        return undefined;
    }
};
var eqDurationComponent = {
    eq: function (x) {
        return function (y) {
            if (x instanceof Second && y instanceof Second) {
                return true;
            };
            if (x instanceof Minute && y instanceof Minute) {
                return true;
            };
            if (x instanceof Hour && y instanceof Hour) {
                return true;
            };
            if (x instanceof Day && y instanceof Day) {
                return true;
            };
            if (x instanceof Week && y instanceof Week) {
                return true;
            };
            if (x instanceof Month && y instanceof Month) {
                return true;
            };
            if (x instanceof Year && y instanceof Year) {
                return true;
            };
            return false;
        };
    }
};
var ordDurationComponent = {
    compare: function (x) {
        return function (y) {
            if (x instanceof Second && y instanceof Second) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Second) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Second) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Minute && y instanceof Minute) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Minute) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Minute) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Hour && y instanceof Hour) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Hour) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Hour) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Day && y instanceof Day) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Day) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Day) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Week && y instanceof Week) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Week) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Week) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Month && y instanceof Month) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Month) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Month) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Year && y instanceof Year) {
                return Data_Ordering.EQ.value;
            };
            throw new Error("Failed pattern match at Data.Interval.Duration (line 0, column 0 - line 0, column 0): " + [ x.constructor.name, y.constructor.name ]);
        };
    },
    Eq0: function () {
        return eqDurationComponent;
    }
};
var semigroupDuration = {
    append: function (v) {
        return function (v1) {
            return Data_Map_Internal.unionWith(ordDurationComponent)(Data_Semiring.add(Data_Semiring.semiringNumber))(v)(v1);
        };
    }
};
var monoidDuration = {
    mempty: Data_Map_Internal.empty,
    Semigroup0: function () {
        return semigroupDuration;
    }
};
var eqDuration = {
    eq: function (x) {
        return function (y) {
            return Data_Eq.eq(Data_Map_Internal.eqMap(eqDurationComponent)(Data_Eq.eqNumber))(x)(y);
        };
    }
};
var ordDuration = {
    compare: function (x) {
        return function (y) {
            return Data_Ord.compare(Data_Map_Internal.ordMap(ordDurationComponent)(Data_Ord.ordNumber))(x)(y);
        };
    },
    Eq0: function () {
        return eqDuration;
    }
};
var durationFromComponent = function (k) {
    return function (v) {
        return Data_Map_Internal.singleton(k)(v);
    };
};
var hour = /* #__PURE__ */ (function () {
    return durationFromComponent(Hour.value);
})();
var millisecond = /* #__PURE__ */ (function () {
    var $29 = durationFromComponent(Second.value);
    return function ($30) {
        return $29((function (v) {
            return v / 1000.0;
        })($30));
    };
})();
var minute = /* #__PURE__ */ (function () {
    return durationFromComponent(Minute.value);
})();
var month = /* #__PURE__ */ (function () {
    return durationFromComponent(Month.value);
})();
var second = /* #__PURE__ */ (function () {
    return durationFromComponent(Second.value);
})();
var week = /* #__PURE__ */ (function () {
    return durationFromComponent(Week.value);
})();
var year = /* #__PURE__ */ (function () {
    return durationFromComponent(Year.value);
})();
var day = /* #__PURE__ */ (function () {
    return durationFromComponent(Day.value);
})();
export {
    Duration,
    Second,
    Minute,
    Hour,
    Day,
    Week,
    Month,
    Year,
    year,
    month,
    week,
    day,
    hour,
    minute,
    second,
    millisecond,
    eqDuration,
    ordDuration,
    newtypeDuration,
    showDuration,
    semigroupDuration,
    monoidDuration,
    eqDurationComponent,
    ordDurationComponent,
    showDurationComponent
};
