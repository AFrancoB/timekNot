import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_Ratio from "../Data.Ratio/index.js";
var toNumber = function (x) {
    return Data_Int.toNumber(Data_Ratio.numerator(x)) / Data_Int.toNumber(Data_Ratio.denominator(x));
};
var fromInt = function (i) {
    return Data_Ratio.reduce(Data_Ord.ordInt)(Data_EuclideanRing.euclideanRingInt)(i)(1);
};
export {
    toNumber,
    fromInt
};
export {
    denominator,
    numerator
} from "../Data.Ratio/index.js";
