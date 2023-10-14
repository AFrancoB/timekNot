// Generated by purs version 0.15.10
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Show from "../Data.Show/index.js";
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
var show = /* #__PURE__ */ Data_Show.show(Data_Show.showInt);
var Year = function (x) {
    return x;
};
var Monday = /* #__PURE__ */ (function () {
    function Monday() {

    };
    Monday.value = new Monday();
    return Monday;
})();
var Tuesday = /* #__PURE__ */ (function () {
    function Tuesday() {

    };
    Tuesday.value = new Tuesday();
    return Tuesday;
})();
var Wednesday = /* #__PURE__ */ (function () {
    function Wednesday() {

    };
    Wednesday.value = new Wednesday();
    return Wednesday;
})();
var Thursday = /* #__PURE__ */ (function () {
    function Thursday() {

    };
    Thursday.value = new Thursday();
    return Thursday;
})();
var Friday = /* #__PURE__ */ (function () {
    function Friday() {

    };
    Friday.value = new Friday();
    return Friday;
})();
var Saturday = /* #__PURE__ */ (function () {
    function Saturday() {

    };
    Saturday.value = new Saturday();
    return Saturday;
})();
var Sunday = /* #__PURE__ */ (function () {
    function Sunday() {

    };
    Sunday.value = new Sunday();
    return Sunday;
})();
var January = /* #__PURE__ */ (function () {
    function January() {

    };
    January.value = new January();
    return January;
})();
var February = /* #__PURE__ */ (function () {
    function February() {

    };
    February.value = new February();
    return February;
})();
var March = /* #__PURE__ */ (function () {
    function March() {

    };
    March.value = new March();
    return March;
})();
var April = /* #__PURE__ */ (function () {
    function April() {

    };
    April.value = new April();
    return April;
})();
var May = /* #__PURE__ */ (function () {
    function May() {

    };
    May.value = new May();
    return May;
})();
var June = /* #__PURE__ */ (function () {
    function June() {

    };
    June.value = new June();
    return June;
})();
var July = /* #__PURE__ */ (function () {
    function July() {

    };
    July.value = new July();
    return July;
})();
var August = /* #__PURE__ */ (function () {
    function August() {

    };
    August.value = new August();
    return August;
})();
var September = /* #__PURE__ */ (function () {
    function September() {

    };
    September.value = new September();
    return September;
})();
var October = /* #__PURE__ */ (function () {
    function October() {

    };
    October.value = new October();
    return October;
})();
var November = /* #__PURE__ */ (function () {
    function November() {

    };
    November.value = new November();
    return November;
})();
var December = /* #__PURE__ */ (function () {
    function December() {

    };
    December.value = new December();
    return December;
})();
var Day = function (x) {
    return x;
};
var showYear = {
    show: function (v) {
        return "(Year " + (show(v) + ")");
    }
};
var showWeekday = {
    show: function (v) {
        if (v instanceof Monday) {
            return "Monday";
        };
        if (v instanceof Tuesday) {
            return "Tuesday";
        };
        if (v instanceof Wednesday) {
            return "Wednesday";
        };
        if (v instanceof Thursday) {
            return "Thursday";
        };
        if (v instanceof Friday) {
            return "Friday";
        };
        if (v instanceof Saturday) {
            return "Saturday";
        };
        if (v instanceof Sunday) {
            return "Sunday";
        };
        throw new Error("Failed pattern match at Data.Date.Component (line 184, column 1 - line 191, column 25): " + [ v.constructor.name ]);
    }
};
var showMonth = {
    show: function (v) {
        if (v instanceof January) {
            return "January";
        };
        if (v instanceof February) {
            return "February";
        };
        if (v instanceof March) {
            return "March";
        };
        if (v instanceof April) {
            return "April";
        };
        if (v instanceof May) {
            return "May";
        };
        if (v instanceof June) {
            return "June";
        };
        if (v instanceof July) {
            return "July";
        };
        if (v instanceof August) {
            return "August";
        };
        if (v instanceof September) {
            return "September";
        };
        if (v instanceof October) {
            return "October";
        };
        if (v instanceof November) {
            return "November";
        };
        if (v instanceof December) {
            return "December";
        };
        throw new Error("Failed pattern match at Data.Date.Component (line 101, column 1 - line 113, column 29): " + [ v.constructor.name ]);
    }
};
var showDay = {
    show: function (v) {
        return "(Day " + (show(v) + ")");
    }
};
var ordYear = Data_Ord.ordInt;
var ordDay = Data_Ord.ordInt;
var eqYear = Data_Eq.eqInt;
var eqWeekday = {
    eq: function (x) {
        return function (y) {
            if (x instanceof Monday && y instanceof Monday) {
                return true;
            };
            if (x instanceof Tuesday && y instanceof Tuesday) {
                return true;
            };
            if (x instanceof Wednesday && y instanceof Wednesday) {
                return true;
            };
            if (x instanceof Thursday && y instanceof Thursday) {
                return true;
            };
            if (x instanceof Friday && y instanceof Friday) {
                return true;
            };
            if (x instanceof Saturday && y instanceof Saturday) {
                return true;
            };
            if (x instanceof Sunday && y instanceof Sunday) {
                return true;
            };
            return false;
        };
    }
};
var ordWeekday = {
    compare: function (x) {
        return function (y) {
            if (x instanceof Monday && y instanceof Monday) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Monday) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Monday) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Tuesday && y instanceof Tuesday) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Tuesday) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Tuesday) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Wednesday && y instanceof Wednesday) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Wednesday) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Wednesday) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Thursday && y instanceof Thursday) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Thursday) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Thursday) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Friday && y instanceof Friday) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Friday) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Friday) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Saturday && y instanceof Saturday) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof Saturday) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Saturday) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Sunday && y instanceof Sunday) {
                return Data_Ordering.EQ.value;
            };
            throw new Error("Failed pattern match at Data.Date.Component (line 0, column 0 - line 0, column 0): " + [ x.constructor.name, y.constructor.name ]);
        };
    },
    Eq0: function () {
        return eqWeekday;
    }
};
var eqMonth = {
    eq: function (x) {
        return function (y) {
            if (x instanceof January && y instanceof January) {
                return true;
            };
            if (x instanceof February && y instanceof February) {
                return true;
            };
            if (x instanceof March && y instanceof March) {
                return true;
            };
            if (x instanceof April && y instanceof April) {
                return true;
            };
            if (x instanceof May && y instanceof May) {
                return true;
            };
            if (x instanceof June && y instanceof June) {
                return true;
            };
            if (x instanceof July && y instanceof July) {
                return true;
            };
            if (x instanceof August && y instanceof August) {
                return true;
            };
            if (x instanceof September && y instanceof September) {
                return true;
            };
            if (x instanceof October && y instanceof October) {
                return true;
            };
            if (x instanceof November && y instanceof November) {
                return true;
            };
            if (x instanceof December && y instanceof December) {
                return true;
            };
            return false;
        };
    }
};
var ordMonth = {
    compare: function (x) {
        return function (y) {
            if (x instanceof January && y instanceof January) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof January) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof January) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof February && y instanceof February) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof February) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof February) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof March && y instanceof March) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof March) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof March) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof April && y instanceof April) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof April) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof April) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof May && y instanceof May) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof May) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof May) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof June && y instanceof June) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof June) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof June) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof July && y instanceof July) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof July) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof July) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof August && y instanceof August) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof August) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof August) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof September && y instanceof September) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof September) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof September) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof October && y instanceof October) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof October) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof October) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof November && y instanceof November) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof November) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof November) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof December && y instanceof December) {
                return Data_Ordering.EQ.value;
            };
            throw new Error("Failed pattern match at Data.Date.Component (line 0, column 0 - line 0, column 0): " + [ x.constructor.name, y.constructor.name ]);
        };
    },
    Eq0: function () {
        return eqMonth;
    }
};
var eqDay = Data_Eq.eqInt;
var boundedYear = /* #__PURE__ */ (function () {
    return {
        bottom: -271820 | 0,
        top: 275759,
        Ord0: function () {
            return ordYear;
        }
    };
})();
var boundedWeekday = /* #__PURE__ */ (function () {
    return {
        bottom: Monday.value,
        top: Sunday.value,
        Ord0: function () {
            return ordWeekday;
        }
    };
})();
var boundedMonth = /* #__PURE__ */ (function () {
    return {
        bottom: January.value,
        top: December.value,
        Ord0: function () {
            return ordMonth;
        }
    };
})();
var boundedEnumYear = {
    cardinality: 547580,
    toEnum: function (n) {
        if (n >= (-271820 | 0) && n <= 275759) {
            return new Data_Maybe.Just(n);
        };
        if (Data_Boolean.otherwise) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at Data.Date.Component (line 35, column 1 - line 40, column 24): " + [ n.constructor.name ]);
    },
    fromEnum: function (v) {
        return v;
    },
    Bounded0: function () {
        return boundedYear;
    },
    Enum1: function () {
        return $lazy_enumYear(0);
    }
};
var $lazy_enumYear = /* #__PURE__ */ $runtime_lazy("enumYear", "Data.Date.Component", function () {
    return {
        succ: (function () {
            var $55 = Data_Enum.toEnum(boundedEnumYear);
            var $56 = Data_Enum.fromEnum(boundedEnumYear);
            return function ($57) {
                return $55((function (v) {
                    return v + 1 | 0;
                })($56($57)));
            };
        })(),
        pred: (function () {
            var $58 = Data_Enum.toEnum(boundedEnumYear);
            var $59 = Data_Enum.fromEnum(boundedEnumYear);
            return function ($60) {
                return $58((function (v) {
                    return v - 1 | 0;
                })($59($60)));
            };
        })(),
        Ord0: function () {
            return ordYear;
        }
    };
});
var enumYear = /* #__PURE__ */ $lazy_enumYear(31);
var boundedEnumWeekday = {
    cardinality: 7,
    toEnum: function (v) {
        if (v === 1) {
            return new Data_Maybe.Just(Monday.value);
        };
        if (v === 2) {
            return new Data_Maybe.Just(Tuesday.value);
        };
        if (v === 3) {
            return new Data_Maybe.Just(Wednesday.value);
        };
        if (v === 4) {
            return new Data_Maybe.Just(Thursday.value);
        };
        if (v === 5) {
            return new Data_Maybe.Just(Friday.value);
        };
        if (v === 6) {
            return new Data_Maybe.Just(Saturday.value);
        };
        if (v === 7) {
            return new Data_Maybe.Just(Sunday.value);
        };
        return Data_Maybe.Nothing.value;
    },
    fromEnum: function (v) {
        if (v instanceof Monday) {
            return 1;
        };
        if (v instanceof Tuesday) {
            return 2;
        };
        if (v instanceof Wednesday) {
            return 3;
        };
        if (v instanceof Thursday) {
            return 4;
        };
        if (v instanceof Friday) {
            return 5;
        };
        if (v instanceof Saturday) {
            return 6;
        };
        if (v instanceof Sunday) {
            return 7;
        };
        throw new Error("Failed pattern match at Data.Date.Component (line 175, column 14 - line 182, column 16): " + [ v.constructor.name ]);
    },
    Bounded0: function () {
        return boundedWeekday;
    },
    Enum1: function () {
        return $lazy_enumWeekday(0);
    }
};
var $lazy_enumWeekday = /* #__PURE__ */ $runtime_lazy("enumWeekday", "Data.Date.Component", function () {
    return {
        succ: (function () {
            var $61 = Data_Enum.toEnum(boundedEnumWeekday);
            var $62 = Data_Enum.fromEnum(boundedEnumWeekday);
            return function ($63) {
                return $61((function (v) {
                    return v + 1 | 0;
                })($62($63)));
            };
        })(),
        pred: (function () {
            var $64 = Data_Enum.toEnum(boundedEnumWeekday);
            var $65 = Data_Enum.fromEnum(boundedEnumWeekday);
            return function ($66) {
                return $64((function (v) {
                    return v - 1 | 0;
                })($65($66)));
            };
        })(),
        Ord0: function () {
            return ordWeekday;
        }
    };
});
var enumWeekday = /* #__PURE__ */ $lazy_enumWeekday(160);
var boundedEnumMonth = {
    cardinality: 12,
    toEnum: function (v) {
        if (v === 1) {
            return new Data_Maybe.Just(January.value);
        };
        if (v === 2) {
            return new Data_Maybe.Just(February.value);
        };
        if (v === 3) {
            return new Data_Maybe.Just(March.value);
        };
        if (v === 4) {
            return new Data_Maybe.Just(April.value);
        };
        if (v === 5) {
            return new Data_Maybe.Just(May.value);
        };
        if (v === 6) {
            return new Data_Maybe.Just(June.value);
        };
        if (v === 7) {
            return new Data_Maybe.Just(July.value);
        };
        if (v === 8) {
            return new Data_Maybe.Just(August.value);
        };
        if (v === 9) {
            return new Data_Maybe.Just(September.value);
        };
        if (v === 10) {
            return new Data_Maybe.Just(October.value);
        };
        if (v === 11) {
            return new Data_Maybe.Just(November.value);
        };
        if (v === 12) {
            return new Data_Maybe.Just(December.value);
        };
        return Data_Maybe.Nothing.value;
    },
    fromEnum: function (v) {
        if (v instanceof January) {
            return 1;
        };
        if (v instanceof February) {
            return 2;
        };
        if (v instanceof March) {
            return 3;
        };
        if (v instanceof April) {
            return 4;
        };
        if (v instanceof May) {
            return 5;
        };
        if (v instanceof June) {
            return 6;
        };
        if (v instanceof July) {
            return 7;
        };
        if (v instanceof August) {
            return 8;
        };
        if (v instanceof September) {
            return 9;
        };
        if (v instanceof October) {
            return 10;
        };
        if (v instanceof November) {
            return 11;
        };
        if (v instanceof December) {
            return 12;
        };
        throw new Error("Failed pattern match at Data.Date.Component (line 87, column 14 - line 99, column 19): " + [ v.constructor.name ]);
    },
    Bounded0: function () {
        return boundedMonth;
    },
    Enum1: function () {
        return $lazy_enumMonth(0);
    }
};
var $lazy_enumMonth = /* #__PURE__ */ $runtime_lazy("enumMonth", "Data.Date.Component", function () {
    return {
        succ: (function () {
            var $67 = Data_Enum.toEnum(boundedEnumMonth);
            var $68 = Data_Enum.fromEnum(boundedEnumMonth);
            return function ($69) {
                return $67((function (v) {
                    return v + 1 | 0;
                })($68($69)));
            };
        })(),
        pred: (function () {
            var $70 = Data_Enum.toEnum(boundedEnumMonth);
            var $71 = Data_Enum.fromEnum(boundedEnumMonth);
            return function ($72) {
                return $70((function (v) {
                    return v - 1 | 0;
                })($71($72)));
            };
        })(),
        Ord0: function () {
            return ordMonth;
        }
    };
});
var enumMonth = /* #__PURE__ */ $lazy_enumMonth(67);
var boundedDay = {
    bottom: 1,
    top: 31,
    Ord0: function () {
        return ordDay;
    }
};
var boundedEnumDay = {
    cardinality: 31,
    toEnum: function (n) {
        if (n >= 1 && n <= 31) {
            return new Data_Maybe.Just(n);
        };
        if (Data_Boolean.otherwise) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at Data.Date.Component (line 133, column 1 - line 138, column 23): " + [ n.constructor.name ]);
    },
    fromEnum: function (v) {
        return v;
    },
    Bounded0: function () {
        return boundedDay;
    },
    Enum1: function () {
        return $lazy_enumDay(0);
    }
};
var $lazy_enumDay = /* #__PURE__ */ $runtime_lazy("enumDay", "Data.Date.Component", function () {
    return {
        succ: (function () {
            var $73 = Data_Enum.toEnum(boundedEnumDay);
            var $74 = Data_Enum.fromEnum(boundedEnumDay);
            return function ($75) {
                return $73((function (v) {
                    return v + 1 | 0;
                })($74($75)));
            };
        })(),
        pred: (function () {
            var $76 = Data_Enum.toEnum(boundedEnumDay);
            var $77 = Data_Enum.fromEnum(boundedEnumDay);
            return function ($78) {
                return $76((function (v) {
                    return v - 1 | 0;
                })($77($78)));
            };
        })(),
        Ord0: function () {
            return ordDay;
        }
    };
});
var enumDay = /* #__PURE__ */ $lazy_enumDay(129);
export {
    January,
    February,
    March,
    April,
    May,
    June,
    July,
    August,
    September,
    October,
    November,
    December,
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
    Sunday,
    eqYear,
    ordYear,
    boundedYear,
    enumYear,
    boundedEnumYear,
    showYear,
    eqMonth,
    ordMonth,
    boundedMonth,
    enumMonth,
    boundedEnumMonth,
    showMonth,
    eqDay,
    ordDay,
    boundedDay,
    enumDay,
    boundedEnumDay,
    showDay,
    eqWeekday,
    ordWeekday,
    boundedWeekday,
    enumWeekday,
    boundedEnumWeekday,
    showWeekday
};
