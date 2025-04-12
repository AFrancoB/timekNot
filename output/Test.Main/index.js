// Generated by purs version 0.15.15
import * as AST from "../AST/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Data_Date from "../Data.Date/index.js";
import * as Data_Date_Component from "../Data.Date.Component/index.js";
import * as Data_DateTime from "../Data.DateTime/index.js";
import * as Data_DateTime_Instant from "../Data.DateTime.Instant/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Rational from "../Data.Rational/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Tempo from "../Data.Tempo/index.js";
import * as Data_Time from "../Data.Time/index.js";
import * as Data_Time_Component from "../Data.Time.Component/index.js";
import * as Data_Traversable from "../Data.Traversable/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Effect from "../Effect/index.js";
import * as Parser from "../Parser/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Rhythm from "../Rhythm/index.js";
import * as Test_QuickCheck from "../Test.QuickCheck/index.js";
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
var eq = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Either.eqEither(Parsing.eqParseError)(Data_Eq.eqString));
var map1 = /* #__PURE__ */ Data_Functor.map(Data_Either.functorEither);
var show = /* #__PURE__ */ Data_Show.show(AST.showRhythmic);
var show1 = /* #__PURE__ */ Data_Show.show(/* #__PURE__ */ Data_List_Types.showList(AST.expressionShow));
var unwrap = /* #__PURE__ */ Data_Newtype.unwrap();
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
        freq: Data_Rational.toRational(Data_Rational.toRationalInt)(2)(1),
        time: new Data_DateTime.DateTime(makeDate(2022)(Data_Date_Component.June.value)(3), makeTime(19)(11)(25)(100)),
        count: Data_Rational.fromInt(0)
    };
})();
var o = /* #__PURE__ */ Data_Tempo.origin(t);
var wP = function (sm) {
    var secs = Data_Int.floor(sm);
    var mili = Data_Int.round((sm - Data_Int.toNumber(Data_Int.floor(sm))) * 1000.0);
    return new Data_DateTime.DateTime(makeDate(2022)(Data_Date_Component.June.value)(3), makeTime(19)(11)(25 + secs | 0)(100 + mili | 0));
};
var main = function __do() {
    Data_Traversable.traverse(Data_Traversable.traversableArray)(Effect.applicativeEffect)(Test_QuickCheck.quickCheck(Test_QuickCheck.testableResult))([ Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("x")(Rhythm.rhythmic)))(new Data_Either.Right("x")))("x broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("o")(Rhythm.rhythmic)))(new Data_Either.Right("o")))("o broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("xxxxx")(Rhythm.rhythmic)))(new Data_Either.Right("xxxxx")))("xxxx broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("xxx xxx")(Rhythm.rhythmic)))(new Data_Either.Right("xxxxxx")))("xxx xxx broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("oxox")(Rhythm.rhythmic)))(new Data_Either.Right("oxox")))("oxox broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("oxo xox")(Rhythm.rhythmic)))(new Data_Either.Right("oxoxox")))("oxo xox broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("[x]")(Rhythm.rhythmic)))(new Data_Either.Right("[x]")))("[x] broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("[xxxx]")(Rhythm.rhythmic)))(new Data_Either.Right("[xxxx]")))("[xxxx] broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("[oxox]")(Rhythm.rhythmic)))(new Data_Either.Right("[oxox]")))("[oxox] broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("[oxox] x")(Rhythm.rhythmic)))(new Data_Either.Right("[oxox]x")))(" [oxox] x broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("x [oxxxxo]")(Rhythm.rhythmic)))(new Data_Either.Right("x[oxxxxo]")))(" x[oxxxxo] broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("[xoxox[oxoxo]]")(Rhythm.rhythmic)))(new Data_Either.Right("[xoxox[oxoxo]]")))(" [xoxox[oxoxo]] broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("[xx[ox[xxx]]]")(Rhythm.rhythmic)))(new Data_Either.Right("[xx[ox[xxx]]]")))(" [xx[ox[xxx]]] broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("xox  [xx  [ox  [xxx]]]")(Rhythm.rhythmic)))(new Data_Either.Right("xox[xx[ox[xxx]]]")))(" xox[xx[ox[xxx]]] broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("[[[xx[o[oxo]o]xx]]]")(Rhythm.rhythmic)))(new Data_Either.Right("[[[xx[o[oxo]o]xx]]]")))(" [[[xx[o[oxo]o]xx]]] broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("[(3,8,5)]")(Rhythm.rhythmic)))(new Data_Either.Right("[(3,8,5)]")))(" [(3,8,5)] broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("[(x,3,8)]")(Rhythm.rhythmic)))(new Data_Either.Right("[(x,3,8,0)]")))(" [(x,3,8,0)] broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("[(xx,ox,5,8)]")(Rhythm.rhythmic)))(new Data_Either.Right("[(xx,ox,5,8,0)]")))(" [(xx,ox,5,8,0)] broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("[_(x,7,12)]")(Rhythm.rhythmic)))(new Data_Either.Right("[_(x,7,12,0)]")))(" [_(x,7,12,0)] broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("[!oxox#2]")(Rhythm.rhythmic)))(new Data_Either.Right("[!oxox#2]")))(" [!oxox#2] broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("(3,5)")(Rhythm.rhythmic)))(new Data_Either.Right("(3,5,0)")))(" (3,5,0) broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("(x,4,7,1)")(Rhythm.rhythmic)))(new Data_Either.Right("(x,4,7,1)")))(" (x,4,7,1) broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("(ox,xx,3,11)")(Rhythm.rhythmic)))(new Data_Either.Right("(ox,xx,3,11,0)")))(" (ox,xx,3,11,0) broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("_(xox,5,9)")(Rhythm.rhythmic)))(new Data_Either.Right("_(xox,5,9,0)")))(" _(xox,5,9,0) broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("(!oxxxo#3,5,7)")(Rhythm.rhythmic)))(new Data_Either.Right("(!oxxxo#3,5,7,0)")))(" (!oxxxo#3,5,7,0) broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("([xxx],4,7)")(Rhythm.rhythmic)))(new Data_Either.Right("([xxx],4,7,0)")))(" ([xxx],4,7,0) broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("((3,8),2,7,1)")(Rhythm.rhythmic)))(new Data_Either.Right("((3,8,0),2,7,1)")))(" ((3,8),2,7,1) broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("![ox]#5")(Rhythm.rhythmic)))(new Data_Either.Right("![ox]#5")))("![ox]#5 broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("!_(ox,3,8)#4")(Rhythm.rhythmic)))(new Data_Either.Right("!_(ox,3,8,0)#4")))("!_(ox,3,8,0)#4 broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("!oxox xxx#2")(Rhythm.rhythmic)))(new Data_Either.Right("!oxoxxxx#2")))("!oxox xxx#2 broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("!ox [xx] (3,8) !xx#3 #4")(Rhythm.rhythmic)))(new Data_Either.Right("!ox[xx](3,8,0)!xx#3#4")))("!ox [xx] (3,8) !xx#3 #4 broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show)(Parsing.runParser("ox (3,8) [xxx] !xx#2 ([xx[(3,5)]],4,7,2) x [(xx,3,5,1) !!xxx [ox] _(ox,2,6,3)#2 [xxx]#5]")(Rhythm.rhythmic)))(new Data_Either.Right("ox(3,8,0)[xxx]!xx#2([xx[(3,5,0)]],4,7,2)x[(xx,3,5,1)!!xxx[ox]_(ox,2,6,3)#2[xxx]#5]")))("ox(3,8,0)[xxx]!xx#2([xx[(3,5,0)]],4,7,2)x[(xx,3,5,1)!!xxx[ox]_(ox,2,6,3)#2[xxx]#5] broken in rhythm parser"), Test_QuickCheck.withHelp(eq(map1(show1)(Parsing.runParser("a 300cpm | x :|")(Parser.parseProgram)))(new Data_Either.Right("(TimeExpression (fromFoldable [(Tuple \"a-0\" Metric 0 Origin 0 300 % 1cpm x looped)]) : Nil)")))("a 300cpm | x :| broken in program parser"), Test_QuickCheck.withHelp(eq(map1(show1)(Parsing.runParser("a [300cpm, 1/4 = 120bpm, mu 3:2, 2cps] | x :|")(Parser.parseProgram)))(new Data_Either.Right("(TimeExpression (fromFoldable [(Tuple \"a-0\" Metric 0 Origin 0 300 % 1cpm x looped),(Tuple \"a-1\" Converge \"a-0\" 0 Origin 0 1 % 4 = 120 % 1bpm x looped),(Tuple \"a-2\" Converge \"a-0\" 0 Origin 0 mu-0 3:2 x looped),(Tuple \"a-3\" Converge \"a-0\" 0 Origin 0 2 % 1cps x looped)]) : Nil)")))("a [300cpm, 1/4 = 120bpm, mu 3:2, 2cps] | x :| broken in programParser"), Test_QuickCheck.withHelp(eq(map1(show1)(Parsing.runParser("a 300cpm*[1,1.1,1.2] | x :|")(Parser.parseProgram)))(new Data_Either.Right("(TimeExpression (fromFoldable [(Tuple \"a-0\" Metric 0 Origin 0 300 % 1cpm x looped),(Tuple \"a-1\" Converge \"a-0\" 0 Origin 0 330 % 1cpm x looped),(Tuple \"a-2\" Converge \"a-0\" 0 Origin 0 360 % 1cpm x looped)]) : Nil)")))("a 300cpm*[1,1.1,1.2] | x :| broken at parseProgram"), Test_QuickCheck.withHelp(eq(map1(show1)(Parsing.runParser("a[10] 300cpm | x :|")(Parser.parseProgram)))(new Data_Either.Right("(TimeExpression (fromFoldable [(Tuple \"a-0\" Metric 0 Origin 10 300 % 1cpm x looped)]) : Nil)")))("a[10] 300cpm | x :| broken at parseProgram"), Test_QuickCheck.withHelp(eq(map1(show1)(Parsing.runParser("a[10] <- [6>>] 300cpm | x :|")(Parser.parseProgram)))(new Data_Either.Right("(TimeExpression (fromFoldable [(Tuple \"a-0\" Metric 6 Snap 10 300 % 1cpm x looped)]) : Nil)")))("a[10] <- [6>>] 300cpm | x :| broken at parseProgram"), Test_QuickCheck.withHelp(eq(map1(show1)(Parsing.runParser("a[1:0] <- [6>>] [300cpm,500cpm] | x :|")(Parser.parseProgram)))(new Data_Either.Right("(TimeExpression (fromFoldable [(Tuple \"a-0\" Converge \"a-1\" 0 Origin 0 300 % 1cpm x looped),(Tuple \"a-1\" Metric 6 Snap 0 500 % 1cpm x looped)]) : Nil)")))("a[1:0] <- [6>>] [300cpm,500cpm] | x :| broken at parseProgram"), Test_QuickCheck.withHelp(eq(map1(show1)(Parsing.runParser("a <- mu [300cpm,500cpm] | x :|")(Parser.parseProgram)))(new Data_Either.Right("(TimeExpression (fromFoldable [(Tuple \"a-0\" Converge \"mu-0\" 0 Origin 0 300 % 1cpm x looped),(Tuple \"a-1\" Converge \"a-0\" 0 Origin 0 500 % 1cpm x looped)]) : Nil)")))("a <- mu [300cpm,500cpm] | x :| broken at parseProgram"), Test_QuickCheck.withHelp(eq(map1(show1)(Parsing.runParser("a[1:13] <- mu[1:17] [300cpm,500cpm] | x :|")(Parser.parseProgram)))(new Data_Either.Right("(TimeExpression (fromFoldable [(Tuple \"a-0\" Converge \"a-1\" 13 Origin 13 300 % 1cpm x looped),(Tuple \"a-1\" Converge \"mu-1\" 17 Origin 13 500 % 1cpm x looped)]) : Nil)")))("a[1:13] <- mu[1:17] [300cpm,500cpm] | x :| broken at parseProgram") ])();
    return Data_Unit.unit;
};
var fromDateTimeToPosixMaybe = function (v) {
    if (v instanceof Data_Maybe.Just) {
        return new Data_Maybe.Just(unwrap(Data_DateTime_Instant.unInstant(Data_DateTime_Instant.fromDateTime(v.value0))) / 1000.0);
    };
    if (v instanceof Data_Maybe.Nothing) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Test.Main (line 172, column 1 - line 172, column 58): " + [ v.constructor.name ]);
};
var fromDateTimeToPosix = function (x) {
    return unwrap(Data_DateTime_Instant.unInstant(Data_DateTime_Instant.fromDateTime(x))) / 1000.0;
};
var oPosix = /* #__PURE__ */ fromDateTimeToPosix(o);
var freqToDur = function (freq) {
    return 1.0 / freq;
};
var $$eval = /* #__PURE__ */ (function () {
    return new Data_DateTime.DateTime(makeDate(2022)(Data_Date_Component.June.value)(3), makeTime(19)(14)(59)(500));
})();
var bpmToFreq = function (bpm) {
    return (1.0 / 60.0) * bpm;
};
var bpmToDur = function (bpm) {
    return 1.0 / bpmToFreq(bpm);
};
var durInSecs = function (dur) {
    return function (tempo) {
        return dur * bpmToDur(tempo);
    };
};
var voice = /* #__PURE__ */ durInSecs(6.0)(120.0);
var voiceFromOrigin = function (units) {
    return function (tempo) {
        return function (sm) {
            return (fromDateTimeToPosix(wP(sm)) - oPosix) / durInSecs(units)(tempo);
        };
    };
};
export {
    main,
    makeDate,
    makeTime,
    voice,
    t,
    wP,
    $$eval as eval,
    o,
    oPosix,
    voiceFromOrigin,
    fromDateTimeToPosix,
    fromDateTimeToPosixMaybe,
    durInSecs,
    bpmToFreq,
    freqToDur,
    bpmToDur
};
