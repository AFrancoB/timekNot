import * as $foreign from "./foreign.js";
import * as Data_DateTime from "../Data.DateTime/index.js";
import * as Data_DateTime_Instant from "../Data.DateTime.Instant/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Effect from "../Effect/index.js";

// | Gets the time according to the current machine’s clock.
var nowTime = /* #__PURE__ */ Data_Functor.map(Effect.functorEffect)(function ($0) {
    return Data_DateTime.time(Data_DateTime_Instant.toDateTime($0));
})($foreign.now);

// | Gets a `DateTime` value for the date and time according to the current
// | machine’s clock.
var nowDateTime = /* #__PURE__ */ Data_Functor.map(Effect.functorEffect)(Data_DateTime_Instant.toDateTime)($foreign.now);

// | Gets the date according to the current machine’s clock.
var nowDate = /* #__PURE__ */ Data_Functor.map(Effect.functorEffect)(function ($1) {
    return Data_DateTime.date(Data_DateTime_Instant.toDateTime($1));
})($foreign.now);
export {
    now,
    getTimezoneOffset
} from "./foreign.js";
export {
    nowDateTime,
    nowDate,
    nowTime
};
