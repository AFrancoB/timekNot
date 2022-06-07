import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_List_Lazy from "../Data.List.Lazy/index.js";
import * as Data_List_Lazy_Types from "../Data.List.Lazy.Types/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as PSCI_Support from "../PSCI.Support/index.js";
var it = /* #__PURE__ */ Data_List_Lazy.fromFoldable(Data_Foldable.foldableArray)([ 0, 1 ]);
var $dollarmain = /* #__PURE__ */ PSCI_Support["eval"](/* #__PURE__ */ PSCI_Support.evalShow(/* #__PURE__ */ Data_List_Lazy_Types.showList(Data_Show.showInt)))(it);
export {
    it,
    $dollarmain as $main
};
