import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Monad_Gen from "../Control.Monad.Gen/index.js";
import * as Data_Bounded from "../Data.Bounded/index.js";
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_NonEmpty from "../Data.NonEmpty/index.js";
import * as Data_Unfoldable1 from "../Data.Unfoldable1/index.js";

// | Create a random generator for a finite enumeration.
var genBoundedEnum = function (dictMonadGen) {
    return function (dictBoundedEnum) {
        var v = Data_Enum.succ(dictBoundedEnum.Enum1())(Data_Bounded.bottom(dictBoundedEnum.Bounded0()));
        if (v instanceof Data_Maybe.Just) {
            var possibilities = Data_Enum.enumFromTo(dictBoundedEnum.Enum1())(Data_Unfoldable1.unfoldable1Array)(v.value0)(Data_Bounded.top(dictBoundedEnum.Bounded0()));
            return Control_Monad_Gen.elements(dictMonadGen)(Data_NonEmpty.foldable1NonEmpty(Data_Foldable.foldableArray))(new Data_NonEmpty.NonEmpty(Data_Bounded.bottom(dictBoundedEnum.Bounded0()), possibilities));
        };
        if (v instanceof Data_Maybe.Nothing) {
            return Control_Applicative.pure((dictMonadGen.Monad0()).Applicative0())(Data_Bounded.bottom(dictBoundedEnum.Bounded0()));
        };
        throw new Error("Failed pattern match at Data.Enum.Gen (line 13, column 3 - line 18, column 18): " + [ v.constructor.name ]);
    };
};
export {
    genBoundedEnum
};
