// Generated by purs version 0.15.2
import * as $foreign from "./foreign.js";
import * as Data_Show from "../Data.Show/index.js";
var warnShow = function (dictShow) {
    return function (a) {
        return $foreign.warn(Data_Show.show(dictShow)(a));
    };
};
var logShow = function (dictShow) {
    return function (a) {
        return $foreign.log(Data_Show.show(dictShow)(a));
    };
};
var infoShow = function (dictShow) {
    return function (a) {
        return $foreign.info(Data_Show.show(dictShow)(a));
    };
};
var errorShow = function (dictShow) {
    return function (a) {
        return $foreign.error(Data_Show.show(dictShow)(a));
    };
};
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
