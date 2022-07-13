// Generated by purs version 0.15.2
import * as AST from "../AST/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Date from "../Data.Date/index.js";
import * as Data_Date_Component from "../Data.Date.Component/index.js";
import * as Data_DateTime from "../Data.DateTime/index.js";
import * as Data_DateTime_Instant from "../Data.DateTime.Instant/index.js";
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_List_Lazy from "../Data.List.Lazy/index.js";
import * as Data_List_Lazy_Types from "../Data.List.Lazy.Types/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ratio from "../Data.Ratio/index.js";
import * as Data_Rational from "../Data.Rational/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Data_Tempo from "../Data.Tempo/index.js";
import * as Data_Time from "../Data.Time/index.js";
import * as Data_Time_Component from "../Data.Time.Component/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
var toL = function (dictFoldable) {
    return function (x) {
        return Data_List_Lazy.fromFoldable(dictFoldable)(x);
    };
};
var oDur = /* #__PURE__ */ Data_Ratio.reduce(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt)(1)(2);
var o = /* #__PURE__ */ Data_List_Lazy.fromFoldable(Data_Foldable.foldableArray)([ 0.0, 0.2, 0.5 ]);
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
var iToN = function (x) {
    return Data_Int.toNumber(x);
};
var multiplePatternW = function (indexAtPhrase) {
    return function (mo) {
        return function (first) {
            return function (o1) {
                return function (last$prime) {
                    var o$prime = Data_List_Lazy.concat(Data_List_Lazy.replicate(mo - 1 | 0)(o1));
                    var lenO = Data_List_Lazy.length(o1);
                    var middle = Data_Functor.map(Data_List_Lazy_Types.functorList)(function (x) {
                        return Data_Tuple.fst(x) + Data_Tuple.snd(x);
                    })(Data_List_Lazy.zip(o$prime)(Data_Functor.map(Data_List_Lazy_Types.functorList)(iToN)(Data_List_Lazy.concat(toL(Data_List_Lazy_Types.foldableList)(Data_Functor.map(Data_List_Lazy_Types.functorList)(function (x) {
                        return Data_List_Lazy.replicate(lenO)(x);
                    })(toL(Data_List_Lazy_Types.foldableList)(Data_List_Lazy.range(1)(mo - 1 | 0))))))));
                    var last = (function () {
                        var $7 = Data_Eq.eq(Data_List_Lazy_Types.eqList(Data_Eq.eqNumber))(last$prime)(toL(Data_Foldable.foldableArray)([  ]));
                        if ($7) {
                            return toL(Data_Foldable.foldableArray)([  ]);
                        };
                        return Data_Functor.map(Data_List_Lazy_Types.functorList)(function (v) {
                            return v + iToN(mo);
                        })(last$prime);
                    })();
                    return Data_Functor.map(Data_List_Lazy_Types.functorList)(function (v) {
                        return v + iToN(indexAtPhrase);
                    })(Data_List_Lazy.concat(toL(Data_Foldable.foldableArray)([ first, middle, last ])));
                };
            };
        };
    };
};
var twoPatternW = function (indexPhrase) {
    return function (first) {
        return function (last$prime) {
            var last = (function () {
                var $8 = Data_Eq.eq(Data_List_Lazy_Types.eqList(Data_Eq.eqNumber))(last$prime)(toL(Data_Foldable.foldableArray)([  ]));
                if ($8) {
                    return toL(Data_Foldable.foldableArray)([  ]);
                };
                return Data_Functor.map(Data_List_Lazy_Types.functorList)(function (v) {
                    return v + 1.0;
                })(toL(Data_List_Lazy_Types.foldableList)(last$prime));
            })();
            return Data_Functor.map(Data_List_Lazy_Types.functorList)(function (v) {
                return v + iToN(indexPhrase);
            })(Data_List_Lazy.concat(toL(Data_Foldable.foldableArray)([ first, last ])));
        };
    };
};
var getIndexSimple = function (start) {
    return function (end) {
        return function (o1) {
            var between = Data_List_Lazy.filter(function (x) {
                return x > start && end >= x;
            })(o1);
            var before = Data_List_Lazy.length(Data_List_Lazy.filter(function (x) {
                return x < start;
            })(o1)) - 1 | 0;
            var $9 = before === 0;
            if ($9) {
                return Data_List_Lazy.range(0)(Data_List_Lazy.length(between) - 1 | 0);
            };
            return Data_List_Lazy.range(before)(Data_List_Lazy.length(between) - 1 | 0);
        };
    };
};
var getIndexOfMiddleList = function (middleLen) {
    return function (o1) {
        var x = toL(Data_List_Lazy_Types.foldableList)(Data_List_Lazy.range(0)(Data_List_Lazy.length(o1) - 1 | 0));
        return Data_List_Lazy.take(middleLen)(Data_List_Lazy.cycle(x));
    };
};
var getIndexOfLastList = function (x) {
    return function (o1) {
        if (Data_Eq.eq(Data_List_Lazy_Types.eqList(Data_Eq.eqNumber))(x)(toL(Data_Foldable.foldableArray)([  ]))) {
            return toL(Data_Foldable.foldableArray)([  ]);
        };
        if (Data_Boolean.otherwise) {
            return Data_List_Lazy.range(0)(Data_List_Lazy.length(x) - 1 | 0);
        };
        throw new Error("Failed pattern match at Motor (line 139, column 1 - line 139, column 60): " + [ x.constructor.name, o1.constructor.name ]);
    };
};
var getIndexOfFirstList = function (x) {
    return function (o1) {
        if (Data_Eq.eq(Data_List_Lazy_Types.eqList(Data_Eq.eqNumber))(x)(toL(Data_Foldable.foldableArray)([  ]))) {
            return toL(Data_Foldable.foldableArray)([  ]);
        };
        if (Data_Boolean.otherwise) {
            return Data_List_Lazy.range(Data_List_Lazy.length(o1) - Data_List_Lazy.length(x) | 0)(Data_List_Lazy.length(o1) - 1 | 0);
        };
        throw new Error("Failed pattern match at Motor (line 130, column 1 - line 130, column 61): " + [ x.constructor.name, o1.constructor.name ]);
    };
};
var floor = function (x) {
    return Data_Int.floor(x);
};
var justFractional = function (x) {
    return x - iToN(floor(x));
};
var toRat = function (x) {
    var floored = floor(x);
    var fract = x - iToN(floored);
    var fract$prime = Data_Int.round(fract * iToN(1000000));
    return Data_Semiring.add(Data_Ratio.semiringRatio(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt))(Data_Ratio.reduce(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt)(floored)(1))(Data_Ratio.reduce(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt)(fract$prime)(1000000));
};
var positionToTime = function (t1) {
    return function (lenPasaje) {
        return function (v) {
            var posInTempo = Data_Semiring.mul(Data_Ratio.semiringRatio(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt))(toRat(v.value0))(lenPasaje);
            var countInTime = Data_Tempo.countToTime(t1)(posInTempo);
            return new AST.Coord(Data_Newtype.unwrap()(Data_DateTime_Instant.unInstant(Data_DateTime_Instant.fromDateTime(countInTime))) / 1000.0, floor(v.value0), v.value1);
        };
    };
};
var filterEvents = function (nPassages) {
    return function (start) {
        return function (end) {
            return function (passageAtStart) {
                return function (o1) {
                    if (nPassages === 0) {
                        var x = Data_Functor.map(Data_List_Lazy_Types.functorList)(function (v) {
                            return v + iToN(floor(passageAtStart));
                        })(Data_List_Lazy.filter(function (x1) {
                            return x1 >= start && x1 < end;
                        })(o1));
                        return Data_List_Lazy.zip(x)(getIndexSimple(start)(end)(o1));
                    };
                    if (nPassages === 1) {
                        var lastList = Data_List_Lazy.filter(function (x) {
                            return x >= 0.0 && x < end;
                        })(o1);
                        var indexLast = getIndexOfLastList(lastList)(o1);
                        var firstList = Data_List_Lazy.filter(function (x) {
                            return x >= start && x < 1.0;
                        })(o1);
                        var indexFst = getIndexOfFirstList(firstList)(o1);
                        var listOfIndexes = Data_List_Lazy.concat(toL(Data_Foldable.foldableArray)([ indexFst, indexLast ]));
                        var listOfEvents = twoPatternW(floor(passageAtStart))(firstList)(lastList);
                        return Data_List_Lazy.zip(listOfEvents)(listOfIndexes);
                    };
                    if (Data_Boolean.otherwise) {
                        var middleList = Data_List_Lazy.take(floor(iToN(Data_List_Lazy.length(o1)) * (iToN(nPassages) - 1.0)))(Data_List_Lazy.cycle(o1));
                        var middleIndex = getIndexOfMiddleList(Data_List_Lazy.length(middleList))(o1);
                        var lastList = Data_List_Lazy.filter(function (x) {
                            return x >= 0.0 && x < end;
                        })(o1);
                        var lastIndex = getIndexOfLastList(lastList)(o1);
                        var firstList = Data_List_Lazy.filter(function (x) {
                            return x >= start && x < 1.0;
                        })(o1);
                        var fstIndex = getIndexOfFirstList(firstList)(o1);
                        var listOfIndexes = Data_List_Lazy.concat(toL(Data_Foldable.foldableArray)([ fstIndex, middleIndex, lastIndex ]));
                        var listOfEvents = multiplePatternW(floor(passageAtStart))(nPassages)(firstList)(o1)(lastList);
                        return Data_List_Lazy.zip(listOfEvents)(listOfIndexes);
                    };
                    throw new Error("Failed pattern match at Motor (line 101, column 1 - line 101, column 92): " + [ nPassages.constructor.name, start.constructor.name, end.constructor.name, passageAtStart.constructor.name, o1.constructor.name ]);
                };
            };
        };
    };
};
var passagePosition = function (o1) {
    return function (lenPasaje) {
        return function (t1) {
            return function (ws1) {
                return function (we1) {
                    return function (eval1) {
                        var countAtStart = Data_Tempo.timeToCountNumber(t1)(ws1);
                        var passageAtStart = countAtStart / Data_Rational.toNumber(lenPasaje);
                        var percentAtStart = passageAtStart - iToN(floor(passageAtStart));
                        var countAtEnd = Data_Tempo.timeToCountNumber(t1)(we1);
                        var passageAtEnd = countAtEnd / Data_Rational.toNumber(lenPasaje);
                        var nPassages = floor(passageAtEnd) - floor(passageAtStart) | 0;
                        var percentAtEnd = passageAtEnd - iToN(floor(passageAtEnd));
                        var filtrado = filterEvents(nPassages)(percentAtStart)(percentAtEnd)(passageAtStart)(o1);
                        var posToTime = Data_Functor.map(Data_List_Lazy_Types.functorList)(function (x) {
                            return positionToTime(t1)(lenPasaje)(x);
                        })(filtrado);
                        return Data_Map_Internal.fromFoldableWithIndex(Data_Ord.ordInt)(Data_List_Lazy_Types.foldableWithIndexList)(posToTime);
                    };
                };
            };
        };
    };
};
var $$eval = /* #__PURE__ */ (function () {
    return new Data_DateTime.DateTime(makeDate(2022)(Data_Date_Component.June.value)(3), makeTime(19)(13)(5)(150));
})();
var countToStart = 327;
export {
    passagePosition
};
