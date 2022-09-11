// Generated by purs version 0.15.4
import * as AST from "../AST/index.js";
import * as Aural from "../Aural/index.js";
import * as Control_Alt from "../Control.Alt/index.js";
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
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Lazy from "../Data.List.Lazy/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ratio from "../Data.Ratio/index.js";
import * as Data_Rational from "../Data.Rational/index.js";
import * as Data_Time from "../Data.Time/index.js";
import * as Data_Time_Component from "../Data.Time.Component/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_Language from "../Parsing.Language/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as Parsing_String_Basic from "../Parsing.String.Basic/index.js";
import * as Parsing_Token from "../Parsing.Token/index.js";
var bind = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT);
var choice = /* #__PURE__ */ Parsing_Combinators.choice(Data_Foldable.foldableArray);
var applySecond = /* #__PURE__ */ Control_Apply.applySecond(Parsing.applyParserT);
var pure = /* #__PURE__ */ Control_Applicative.pure(Parsing.applicativeParserT);
var fromFoldable = /* #__PURE__ */ Data_List.fromFoldable(Data_List_Types.foldableList);
var reduce = /* #__PURE__ */ Data_Ratio.reduce(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt);
var fromJust = /* #__PURE__ */ Data_Maybe.fromJust();
var apply = /* #__PURE__ */ Control_Apply.apply(Data_Maybe.applyMaybe);
var map = /* #__PURE__ */ Data_Functor.map(Data_Maybe.functorMaybe);
var toEnum = /* #__PURE__ */ Data_Enum.toEnum(Data_Time_Component.boundedEnumHour);
var toEnum1 = /* #__PURE__ */ Data_Enum.toEnum(Data_Time_Component.boundedEnumMinute);
var toEnum2 = /* #__PURE__ */ Data_Enum.toEnum(Data_Time_Component.boundedEnumSecond);
var toEnum3 = /* #__PURE__ */ Data_Enum.toEnum(Data_Time_Component.boundedEnumMillisecond);
var flap = /* #__PURE__ */ Data_Functor.flap(Data_Maybe.functorMaybe);
var toEnum4 = /* #__PURE__ */ Data_Enum.toEnum(Data_Date_Component.boundedEnumYear);
var toEnum5 = /* #__PURE__ */ Data_Enum.toEnum(Data_Date_Component.boundedEnumDay);
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT);
var fromFoldable1 = /* #__PURE__ */ Data_List.fromFoldable(Data_Foldable.foldableArray);
var tokenParser = /* #__PURE__ */ Parsing_Token.makeTokenParser(Parsing_Language.haskellStyle);
var whitespace = /* #__PURE__ */ (function () {
    return tokenParser.whiteSpace;
})();
var stringLit = /* #__PURE__ */ (function () {
    return tokenParser.stringLiteral;
})();
var semi = /* #__PURE__ */ (function () {
    return tokenParser.semi;
})();
var reserved = /* #__PURE__ */ (function () {
    return tokenParser.reserved;
})();
var parens = /* #__PURE__ */ (function () {
    return tokenParser.parens;
})();
var onset = /* #__PURE__ */ bind(/* #__PURE__ */ choice([ /* #__PURE__ */ applySecond(/* #__PURE__ */ Parsing_String_Basic.oneOf([ "x" ]))(/* #__PURE__ */ pure(true)), /* #__PURE__ */ applySecond(/* #__PURE__ */ Parsing_String_Basic.oneOf([ "o" ]))(/* #__PURE__ */ pure(false)) ]))(function (x) {
    return bind(pure(1))(function () {
        return pure(x);
    });
});
var onsets = /* #__PURE__ */ bind(/* #__PURE__ */ Parsing_Combinators.many(onset))(function (xs) {
    return bind(pure(1))(function () {
        return pure(new AST.Onsets(fromFoldable(xs)));
    });
});
var oDur = /* #__PURE__ */ reduce(1)(2);
var o = /* #__PURE__ */ Data_List_Lazy.fromFoldable(Data_Foldable.foldableArray)([ 0.0, 0.2, 0.5 ]);
var noseParser = /* #__PURE__ */ (function () {
    return bind(Control_Alt.alt(Parsing.altParserT)(choice([ applySecond(Parsing_Combinators["try"](Parsing_String.string("eval")))(pure(AST.Eval.value)), applySecond(Parsing_Combinators["try"](Parsing_String.string("origin")))(pure(AST.Origin.value)) ]))(pure(AST.Origin.value)))(function (x) {
        return bind(pure(1))(function () {
            return pure(x);
        });
    });
})();
var naturalOrFloat = /* #__PURE__ */ (function () {
    return tokenParser.naturalOrFloat;
})();
var makeTime = function (h) {
    return function (min) {
        return function (sec) {
            return function (milisec) {
                return fromJust(apply(apply(apply(map(Data_Time.Time.create)(toEnum(h)))(toEnum1(min)))(toEnum2(sec)))(toEnum3(milisec)));
            };
        };
    };
};
var makeDate = function (y) {
    return function (m) {
        return function (d) {
            return fromJust(apply(flap(map(Data_Date.canonicalDate)(toEnum4(y)))(m))(toEnum5(d)));
        };
    };
};
var t = /* #__PURE__ */ (function () {
    return {
        freq: reduce(2)(1),
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
var integer = /* #__PURE__ */ (function () {
    return tokenParser.integer;
})();
var identifier = /* #__PURE__ */ (function () {
    return tokenParser.identifier;
})();
var $$float = /* #__PURE__ */ (function () {
    return tokenParser["float"];
})();
var $$eval = /* #__PURE__ */ (function () {
    return new Data_DateTime.DateTime(makeDate(2022)(Data_Date_Component.June.value)(3), makeTime(19)(14)(59)(500));
})();
var endOfRhythmic = /* #__PURE__ */ bind(/* #__PURE__ */ choice([ /* #__PURE__ */ applySecond(/* #__PURE__ */ Parsing_String.string(":||"))(/* #__PURE__ */ pure(true)), /* #__PURE__ */ applySecond(/* #__PURE__ */ Parsing_String.string("||"))(/* #__PURE__ */ pure(false)) ]))(function (x) {
    return pure(x);
});
var topRhythmic = /* #__PURE__ */ bind(/* #__PURE__ */ choice([ onsets ]))(function (r) {
    return discard(whitespace)(function () {
        return bind(endOfRhythmic)(function (end) {
            return bind(pure(1))(function () {
                return pure(new Data_Tuple.Tuple(r, end));
            });
        });
    });
});
var topPassageParser = /* #__PURE__ */ bind(topRhythmic)(function (rhy) {
    return discard(whitespace)(function () {
        return bind(Aural.samples)(function (aur) {
            return discard(whitespace)(function () {
                return bind(noseParser)(function (nose) {
                    return bind(pure(1))(function () {
                        return discard(Parsing_String.eof)(function () {
                            return pure(new AST.Passage(Data_Tuple.fst(rhy), fromFoldable1([ aur ]), nose, Data_Tuple.snd(rhy)));
                        });
                    });
                });
            });
        });
    });
});
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
export {
    topPassageParser,
    endOfRhythmic
};
