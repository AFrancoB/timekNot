import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_List_Lazy from "../Data.List.Lazy/index.js";
import * as Data_List_Lazy_Types from "../Data.List.Lazy.Types/index.js";
import * as Data_Map_Internal from "../Data.Map.Internal/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
var Pru = /* #__PURE__ */ (function () {
    function Pru(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    Pru.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new Pru(value0, value1, value2);
            };
        };
    };
    return Pru;
})();
var mapaDePosiciones = /* #__PURE__ */ (function () {
    return Data_Map_Internal.singleton(0)(new Pru(2.666, 230, 0));
})();
var func = /* #__PURE__ */ Data_Map_Internal.fromFoldableWithIndex(Data_Ord.ordInt)(Data_List_Lazy_Types.foldableWithIndexList)(/* #__PURE__ */ Data_List_Lazy.fromFoldable(Data_Foldable.foldableArray)([ 0.0, 1.0, 2.0 ]));
export {
    Pru,
    mapaDePosiciones,
    func
};
