// Generated by purs version 0.15.15
import * as $foreign from "./foreign.js";
import * as Data_Show from "../Data.Show/index.js";
var warnShow = function (dictShow) {
    var show = Data_Show.show(dictShow);
    return function (a) {
        return $foreign.warn(show(a));
    };
};
var logShow = function (dictShow) {
    var show = Data_Show.show(dictShow);
    return function (a) {
        return $foreign.log(show(a));
    };
};
var infoShow = function (dictShow) {
    var show = Data_Show.show(dictShow);
    return function (a) {
        return $foreign.info(show(a));
    };
};
var errorShow = function (dictShow) {
    var show = Data_Show.show(dictShow);
    return function (a) {
        return $foreign.error(show(a));
    };
};
var debugShow = function (dictShow) {
    var show = Data_Show.show(dictShow);
    return function (a) {
        return $foreign.debug(show(a));
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
