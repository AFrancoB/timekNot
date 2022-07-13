// Generated by purs version 0.15.2
import * as AST from "../AST/index.js";
import * as Aural from "../Aural/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Date from "../Data.Date/index.js";
import * as Data_Date_Component from "../Data.Date.Component/index.js";
import * as Data_DateTime from "../Data.DateTime/index.js";
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Lazy from "../Data.List.Lazy/index.js";
import * as Data_List_Lazy_Types from "../Data.List.Lazy.Types/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ratio from "../Data.Ratio/index.js";
import * as Data_Rational from "../Data.Rational/index.js";
import * as Data_Time from "../Data.Time/index.js";
import * as Data_Time_Component from "../Data.Time.Component/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Motor from "../Motor/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_Language from "../Parsing.Language/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as Parsing_String_Basic from "../Parsing.String.Basic/index.js";
import * as Parsing_Token from "../Parsing.Token/index.js";
var tokenParser = /* #__PURE__ */ Parsing_Token.makeTokenParser(Parsing_Language.haskellStyle);
var whitespace = /* #__PURE__ */ (function () {
    return tokenParser.whiteSpace;
})();
var toEvent = function (v) {
    if (v instanceof Data_Maybe.Just) {
        return new Data_Maybe.Just({
            whenPosix: v.value0.value0,
            s: v.value0.value1,
            n: 0
        });
    };
    if (v instanceof Data_Maybe.Nothing) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Rhythmic (line 78, column 1 - line 78, column 53): " + [ v.constructor.name ]);
};
var stringLit = /* #__PURE__ */ (function () {
    return tokenParser.stringLiteral;
})();
var semi = /* #__PURE__ */ (function () {
    return tokenParser.semi;
})();
var sampleWithIndex = function (v) {
    if (v instanceof Data_Maybe.Just && v.value0 instanceof AST.Sample) {
        var au = Data_List_Lazy.fromFoldable(Data_List_Types.foldableList)(v.value0.value0);
        return Data_List_Lazy.zip(au)(Data_List_Lazy.range(0)(Data_List_Lazy.length(au)));
    };
    return Data_List_Lazy.fromFoldable(Data_Foldable.foldableArray)([  ]);
};
var reserved = /* #__PURE__ */ (function () {
    return tokenParser.reserved;
})();
var parens = /* #__PURE__ */ (function () {
    return tokenParser.parens;
})();
var onset = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Parsing_Combinators.choice(Data_Foldable.foldableArray)([ /* #__PURE__ */ Control_Apply.applySecond(Parsing.applyParserT)(/* #__PURE__ */ Parsing_String_Basic.oneOf([ "x" ]))(/* #__PURE__ */ Control_Applicative.pure(Parsing.applicativeParserT)(true)), /* #__PURE__ */ Control_Apply.applySecond(Parsing.applyParserT)(/* #__PURE__ */ Parsing_String_Basic.oneOf([ "o" ]))(/* #__PURE__ */ Control_Applicative.pure(Parsing.applicativeParserT)(false)) ]))(function (x) {
    return Control_Bind.bind(Parsing.bindParserT)(Control_Applicative.pure(Parsing.applicativeParserT)(1))(function () {
        return Control_Applicative.pure(Parsing.applicativeParserT)(x);
    });
});
var onsets = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Parsing_Combinators.many(onset))(function (xs) {
    return Control_Bind.bind(Parsing.bindParserT)(Control_Applicative.pure(Parsing.applicativeParserT)(1))(function () {
        return Control_Applicative.pure(Parsing.applicativeParserT)(new AST.Onsets(Data_List.fromFoldable(Data_List_Types.foldableList)(xs)));
    });
});
var topRhythmic = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Parsing_Combinators.choice(Data_Foldable.foldableArray)([ onsets ]))(function (r) {
    return Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT)(whitespace)(function () {
        return Control_Bind.bind(Parsing.bindParserT)(Parsing_String.string("||"))(function () {
            return Control_Bind.bind(Parsing.bindParserT)(Control_Applicative.pure(Parsing.applicativeParserT)(1))(function () {
                return Control_Applicative.pure(Parsing.applicativeParserT)(r);
            });
        });
    });
});
var topPassageParser = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(topRhythmic)(function (rhy) {
    return Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT)(whitespace)(function () {
        return Control_Bind.bind(Parsing.bindParserT)(Aural.samples)(function (aur) {
            return Control_Bind.bind(Parsing.bindParserT)(Control_Applicative.pure(Parsing.applicativeParserT)(1))(function () {
                return Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT)(Parsing_String.eof)(function () {
                    return Control_Applicative.pure(Parsing.applicativeParserT)(new AST.Passage(rhy, Data_List.fromFoldable(Data_Foldable.foldableArray)([ aur ])));
                });
            });
        });
    });
});
var oDur = /* #__PURE__ */ Data_Ratio.reduce(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt)(1)(2);
var o = /* #__PURE__ */ Data_List_Lazy.fromFoldable(Data_Foldable.foldableArray)([ 0.0, 0.2, 0.5 ]);
var naturalOrFloat = /* #__PURE__ */ (function () {
    return tokenParser.naturalOrFloat;
})();
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
var isSample = function (v) {
    if (v instanceof AST.Sample) {
        return true;
    };
    return false;
};
var isN = function (v) {
    if (v instanceof AST.N) {
        return true;
    };
    return false;
};
var integer = /* #__PURE__ */ (function () {
    return tokenParser.integer;
})();
var identifier = /* #__PURE__ */ (function () {
    return tokenParser.identifier;
})();
var getEventIndex = function (p$prime) {
    return function (len$prime) {
        return function (e$prime) {
            var p = Data_Int.toNumber(p$prime);
            var len = Data_Int.toNumber(len$prime);
            var e = Data_Int.toNumber(e$prime);
            return Data_Int.floor(p * len + e);
        };
    };
};
var fromPatternToList = function (v) {
    if (v instanceof AST.Onsets) {
        return Data_List_Lazy.fromFoldable(Data_List_Types.foldableList)(v.value0);
    };
    return Data_List_Lazy.fromFoldable(Data_Foldable.foldableArray)([ false ]);
};
var fromRhythmicToList = function (v) {
    if (v instanceof AST.Onsets) {
        return Data_List_Lazy.fromFoldable(Data_List_Types.foldableList)(v.value0);
    };
    if (v instanceof AST.Patron) {
        return Data_List_Lazy.concat(Data_Functor.map(Data_List_Lazy_Types.functorList)(fromPatternToList)(Data_List_Lazy.fromFoldable(Data_List_Types.foldableList)(v.value0)));
    };
    return Data_List_Lazy.fromFoldable(Data_Foldable.foldableArray)([ false ]);
};
var fromPassageToCoord = function (rhy) {
    return function (t1) {
        return function (ws1) {
            return function (we1) {
                return function (eval1) {
                    var x = fromRhythmicToList(rhy);
                    var passageLength = Data_Rational.fromInt(Data_List_Lazy.length(x));
                    var onsets1 = Data_Functor.map(Data_List_Lazy_Types.functorList)(function ($55) {
                        return Data_Rational.fromInt(Data_Tuple.snd($55));
                    })(Data_List_Lazy.filter(function (x1) {
                        return Data_Tuple.fst(x1) === true;
                    })(Data_List_Lazy.zip(x)(Data_List_Lazy.range(0)(Data_List_Lazy.length(x)))));
                    var oPercen = Data_Functor.map(Data_List_Lazy_Types.functorList)(function ($56) {
                        return Data_Rational.toNumber((function (v) {
                            return Data_EuclideanRing.div(Data_Ratio.euclideanRingRatio(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt))(v)(passageLength);
                        })($56));
                    })(onsets1);
                    return Motor.passagePosition(oPercen)(passageLength)(t1)(ws1)(we1)(eval1);
                };
            };
        };
    };
};
var $$float = /* #__PURE__ */ (function () {
    return tokenParser["float"];
})();
var f$prime = function (x) {
    return function (v) {
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(new Data_Tuple.Tuple(x, v.value0.value0));
        };
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at Rhythmic (line 112, column 1 - line 112, column 71): " + [ x.constructor.name, v.constructor.name ]);
    };
};
var f = function (v) {
    return function (len) {
        return function (samples) {
            return function (v1) {
                if (v instanceof AST.EventI) {
                    return f$prime(v1.value0)(Data_List_Lazy.head(Data_List_Lazy.filter(function (s) {
                        return Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(getEventIndex(v1.value1)(len)(v1.value2))(len) === Data_Tuple.snd(s);
                    })(samples)));
                };
                if (v instanceof AST.PassageI) {
                    return f$prime(v1.value0)(Data_List_Lazy.head(Data_List_Lazy.filter(function (s) {
                        return Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(v1.value1)(len) === Data_Tuple.snd(s);
                    })(samples)));
                };
                if (v instanceof AST.MetreI) {
                    return f$prime(v1.value0)(Data_List_Lazy.head(Data_List_Lazy.fromFoldable(Data_Foldable.foldableArray)([  ])));
                };
                throw new Error("Failed pattern match at Rhythmic (line 101, column 1 - line 101, column 89): " + [ v.constructor.name, len.constructor.name, samples.constructor.name, v1.constructor.name ]);
            };
        };
    };
};
var samplesWithPosix = function (index) {
    return function (len) {
        return function (samples) {
            return function (coords) {
                return Data_Functor.map(Data_List_Lazy_Types.functorList)(f(index)(len)(samples))(coords);
            };
        };
    };
};
var $$eval = /* #__PURE__ */ (function () {
    return new Data_DateTime.DateTime(makeDate(2022)(Data_Date_Component.June.value)(3), makeTime(19)(13)(5)(150));
})();
var comma = /* #__PURE__ */ (function () {
    return tokenParser.comma;
})();
var colon = /* #__PURE__ */ (function () {
    return tokenParser.colon;
})();
var brackets = /* #__PURE__ */ (function () {
    return tokenParser.brackets;
})();
var braces = /* #__PURE__ */ (function () {
    return tokenParser.braces;
})();
var auralIndex = function (v) {
    if (v instanceof Data_Maybe.Just && v.value0 instanceof AST.Sample) {
        return v.value0.value1;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof AST.N) {
        return v.value0.value1;
    };
    if (v instanceof Data_Maybe.Nothing) {
        return AST.EventI.value;
    };
    throw new Error("Failed pattern match at Rhythmic (line 82, column 1 - line 82, column 34): " + [ v.constructor.name ]);
};
var passageToEvents = function (rhy) {
    return function (au) {
        return function (t1) {
            return function (ws1) {
                return function (we1) {
                    return function (eval1) {
                        var samplesI = auralIndex(Data_List_Lazy.last(Data_List_Lazy.filter(isSample)(au)));
                        var samples = sampleWithIndex(Data_List_Lazy.last(Data_List_Lazy.filter(isSample)(au)));
                        var coords = fromPassageToCoord(rhy)(t1)(ws1)(we1)(eval1);
                        var lCoord = Data_Functor.map(Data_List_Lazy_Types.functorList)(Data_Tuple.snd)(Data_Map_Internal.toUnfoldable(Data_List_Lazy_Types.unfoldableList)(coords));
                        var s = samplesWithPosix(samplesI)(Data_List_Lazy.length(samples))(samples)(lCoord);
                        return Data_Functor.map(Data_List_Lazy_Types.functorList)(toEvent)(s);
                    };
                };
            };
        };
    };
};
var passageToEvents$prime = function (rhy) {
    return function (au) {
        return function (t1) {
            return function (ws1) {
                return function (we1) {
                    return function (eval1) {
                        var samplesI = auralIndex(Data_List_Lazy.last(Data_List_Lazy.filter(isSample)(au)));
                        var samples = sampleWithIndex(Data_List_Lazy.last(Data_List_Lazy.filter(isSample)(au)));
                        var coords = fromPassageToCoord(rhy)(t1)(ws1)(we1)(eval1);
                        var lCoord = Data_Functor.map(Data_List_Lazy_Types.functorList)(Data_Tuple.snd)(Data_Map_Internal.toUnfoldable(Data_List_Lazy_Types.unfoldableList)(coords));
                        var s = samplesWithPosix(samplesI)(Data_List_Lazy.length(samples))(samples)(lCoord);
                        return lCoord;
                    };
                };
            };
        };
    };
};
var test = function (secSt) {
    return function (milSt) {
        return function (secEn) {
            return function (milEn) {
                return passageToEvents$prime(new AST.Onsets(Data_List.fromFoldable(Data_Foldable.foldableArray)([ true, true, false, true, false ])))(Data_List_Lazy.fromFoldable(Data_Foldable.foldableArray)([ new AST.Sample(Data_List.fromFoldable(Data_Foldable.foldableArray)([ "bd", "cp", "808" ]), AST.EventI.value) ]))(t)(ws(secSt)(milSt))(we(secEn)(milEn))($$eval);
            };
        };
    };
};
export {
    topRhythmic,
    topPassageParser,
    fromPassageToCoord,
    passageToEvents,
    f,
    test
};
