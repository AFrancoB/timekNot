import * as Effect_Class from "../Effect.Class/index.js";
import * as Effect_Console from "../Effect.Console/index.js";
var warnShow = function (dictMonadEffect) {
    return function (dictShow) {
        var $19 = Effect_Class.liftEffect(dictMonadEffect);
        var $20 = Effect_Console.warnShow(dictShow);
        return function ($21) {
            return $19($20($21));
        };
    };
};
var warn = function (dictMonadEffect) {
    var $22 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($23) {
        return $22(Effect_Console.warn($23));
    };
};
var timeLog = function (dictMonadEffect) {
    var $24 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($25) {
        return $24(Effect_Console.timeLog($25));
    };
};
var timeEnd = function (dictMonadEffect) {
    var $26 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($27) {
        return $26(Effect_Console.timeEnd($27));
    };
};
var time = function (dictMonadEffect) {
    var $28 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($29) {
        return $28(Effect_Console.time($29));
    };
};
var logShow = function (dictMonadEffect) {
    return function (dictShow) {
        var $30 = Effect_Class.liftEffect(dictMonadEffect);
        var $31 = Effect_Console.logShow(dictShow);
        return function ($32) {
            return $30($31($32));
        };
    };
};
var log = function (dictMonadEffect) {
    var $33 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($34) {
        return $33(Effect_Console.log($34));
    };
};
var infoShow = function (dictMonadEffect) {
    return function (dictShow) {
        var $35 = Effect_Class.liftEffect(dictMonadEffect);
        var $36 = Effect_Console.infoShow(dictShow);
        return function ($37) {
            return $35($36($37));
        };
    };
};
var info = function (dictMonadEffect) {
    var $38 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($39) {
        return $38(Effect_Console.info($39));
    };
};
var errorShow = function (dictMonadEffect) {
    return function (dictShow) {
        var $40 = Effect_Class.liftEffect(dictMonadEffect);
        var $41 = Effect_Console.errorShow(dictShow);
        return function ($42) {
            return $40($41($42));
        };
    };
};
var error = function (dictMonadEffect) {
    var $43 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($44) {
        return $43(Effect_Console.error($44));
    };
};
var debugShow = function (dictMonadEffect) {
    return function (dictShow) {
        var $45 = Effect_Class.liftEffect(dictMonadEffect);
        var $46 = Effect_Console.debugShow(dictShow);
        return function ($47) {
            return $45($46($47));
        };
    };
};
var debug = function (dictMonadEffect) {
    var $48 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($49) {
        return $48(Effect_Console.debug($49));
    };
};
var clear = function (dictMonadEffect) {
    return Effect_Class.liftEffect(dictMonadEffect)(Effect_Console.clear);
};
export {
    log,
    logShow,
    warn,
    warnShow,
    error,
    errorShow,
    info,
    infoShow,
    debug,
    debugShow,
    time,
    timeLog,
    timeEnd,
    clear
};
