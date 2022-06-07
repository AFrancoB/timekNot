import * as Control_Category from "../Control.Category/index.js";
import * as Data_DateTime from "../Data.DateTime/index.js";
import * as Data_DateTime_Instant from "../Data.DateTime.Instant/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ratio from "../Data.Ratio/index.js";
import * as Data_Rational from "../Data.Rational/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Data_Time_Duration from "../Data.Time.Duration/index.js";
import * as Effect_Now from "../Effect.Now/index.js";

// | I think there might be numerical precision issues with timeToCount and PureScript's Rational type
// so am also experimenting with timeToCountNumber as well (it might have different precision issues...)
var timeToCountNumber = function (x) {
    return function (t) {
        var timeDiff = Data_Newtype.unwrap()(Data_DateTime.diff(Data_Time_Duration.durationMilliseconds)(t)(x.time));
        var df = (timeDiff * Data_Rational.toNumber(x.freq)) / 1000.0;
        return df + Data_Rational.toNumber(x.count);
    };
};

// | Given a Tempo and a clock time (DateTime), timeToCount tells us how many cycles/beats
// have elapsed at that time.
var timeToCount = function (x) {
    return function (t) {
        var d = Data_Newtype.unwrap()(Data_DateTime.diff(Data_Time_Duration.durationMilliseconds)(t)(x.time));
        var d$prime = Data_Ratio.reduce(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt)(Data_Int.floor(d))(1000);
        return Data_Semiring.add(Data_Ratio.semiringRatio(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt))(Data_Semiring.mul(Data_Ratio.semiringRatio(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt))(d$prime)(x.freq))(x.count);
    };
};

// | The 'origin' of a Tempo is the time at which the number of elapsed cycles/beats
// would have been 0. (Note that unlike the Haskell version of this function, there is potential
// silent failure/error built-in in cases where adjust does not return a valid DateTime - in such
// hopefully very rare, liminal cases, this function will simply return the time anchor from the
// Tempo record as a kind of "default" value.)
var origin = function (x) {
    var increment = Data_EuclideanRing.div(Data_Ratio.euclideanRingRatio(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt))(Data_Semiring.mul(Data_Ratio.semiringRatio(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt))(x.count)(Data_Rational.fromInt(-1000 | 0)))(x.freq);
    return Data_Maybe.maybe(x.time)(Control_Category.identity(Control_Category.categoryFn))(Data_DateTime.adjust(Data_Time_Duration.durationMilliseconds)(Data_Rational.toNumber(increment))(x.time));
};

// | Create a new Tempo record with a given frequency, with count 0 anchored in the present moment.
var newTempo = function (freq) {
    return function __do() {
        var time = Effect_Now.nowDateTime();
        return {
            freq: freq,
            time: time,
            count: Data_Rational.fromInt(0)
        };
    };
};
var fromForeignTempo = function (x) {
    var time = Data_DateTime_Instant.toDateTime(Data_Maybe.fromJust()(Data_DateTime_Instant.instant(x.time)));
    var freq = Data_Ratio.reduce(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt)(x.freqNumerator)(x.freqDenominator);
    var count = Data_Ratio.reduce(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt)(x.countNumerator)(x.countDenominator);
    return {
        freq: freq,
        time: time,
        count: count
    };
};

// | Given a Tempo and a count of elapsed cycles/beats, countToTime tells us when that "beat"
// will (or would have) take(n) place. (note: countToTime has the same caveat about a liminal potential
// silent failure as origin above.)
var countToTime = function (x) {
    return function (c) {
        return Data_Maybe.maybe(x.time)(Control_Category.identity(Control_Category.categoryFn))(Data_DateTime.adjust(Data_Time_Duration.durationSeconds)(Data_Rational.toNumber(Data_EuclideanRing.div(Data_Ratio.euclideanRingRatio(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt))(c)(x.freq)))(origin(x)));
    };
};
export {
    newTempo,
    origin,
    timeToCount,
    timeToCountNumber,
    countToTime,
    fromForeignTempo
};
