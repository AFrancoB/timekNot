import * as $foreign from "./foreign.js";
import * as Data_Show from "../Data.Show/index.js";

// | Write an warning value to the console, using its `Show` instance to produce
// | a `String`.
var warnShow = function (dictShow) {
    return function (a) {
        return $foreign.warn(Data_Show.show(dictShow)(a));
    };
};

// | Write a value to the console, using its `Show` instance to produce a
// | `String`.
var logShow = function (dictShow) {
    return function (a) {
        return $foreign.log(Data_Show.show(dictShow)(a));
    };
};

// | Write an info value to the console, using its `Show` instance to produce a
// | `String`.
var infoShow = function (dictShow) {
    return function (a) {
        return $foreign.info(Data_Show.show(dictShow)(a));
    };
};

// | Write an error value to the console, using its `Show` instance to produce a
// | `String`.
var errorShow = function (dictShow) {
    return function (a) {
        return $foreign.error(Data_Show.show(dictShow)(a));
    };
};

// | Write an debug value to the console, using its `Show` instance to produce a
// | `String`.
var debugShow = function (dictShow) {
    return function (a) {
        return $foreign.debug(Data_Show.show(dictShow)(a));
    };
};
export {
    log,
    warn,
    error,
    info,
    debug,
    time,
    timeLog,
    timeEnd,
    clear
} from "./foreign.js";
export {
    logShow,
    warnShow,
    errorShow,
    infoShow,
    debugShow
};
