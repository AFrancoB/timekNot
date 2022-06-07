// | This module defines the `RWS` monad.
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Monad_RWS_Trans from "../Control.Monad.RWS.Trans/index.js";
import * as Control_Monad_Reader_Class from "../Control.Monad.Reader.Class/index.js";
import * as Control_Monad_State_Class from "../Control.Monad.State.Class/index.js";
import * as Control_Monad_Writer_Class from "../Control.Monad.Writer.Class/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";

// | Change the type of the context in a `RWS` action
var withRWS = Control_Monad_RWS_Trans.withRWST;

// | Create an action in the `RWS` monad from a function which uses the
// | global context and state explicitly.
var rws = function (f) {
    return function (r) {
        return function (s) {
            return Control_Applicative.pure(Data_Identity.applicativeIdentity)(f(r)(s));
        };
    };
};

// | Run a computation in the `RWS` monad.
var runRWS = function (m) {
    return function (r) {
        return function (s) {
            var v = m(r)(s);
            return v;
        };
    };
};

// | Change the types of the result and accumulator in a `RWS` action
var mapRWS = function (f) {
    return Control_Monad_RWS_Trans.mapRWST((function () {
        var $3 = Data_Newtype.unwrap();
        return function ($4) {
            return Data_Identity.Identity(f($3($4)));
        };
    })());
};

// | Run a computation in the `RWS` monad, discarding the result
var execRWS = function (m) {
    return function (r) {
        return function (s) {
            return Data_Newtype.unwrap()(Control_Monad_RWS_Trans.execRWST(Data_Identity.monadIdentity)(m)(r)(s));
        };
    };
};

// | Run a computation in the `RWS` monad, discarding the final state
var evalRWS = function (m) {
    return function (r) {
        return function (s) {
            return Data_Newtype.unwrap()(Control_Monad_RWS_Trans.evalRWST(Data_Identity.monadIdentity)(m)(r)(s));
        };
    };
};
export {
    rws,
    runRWS,
    evalRWS,
    execRWS,
    mapRWS,
    withRWS
};
export {
    RWSResult,
    RWST,
    evalRWST,
    execRWST,
    lift,
    mapRWST,
    runRWST,
    withRWST
} from "../Control.Monad.RWS.Trans/index.js";
export {
    ask,
    asks,
    local
} from "../Control.Monad.Reader.Class/index.js";
export {
    get,
    gets,
    modify,
    modify_,
    put,
    state
} from "../Control.Monad.State.Class/index.js";
export {
    censor,
    listen,
    listens,
    pass,
    tell
} from "../Control.Monad.Writer.Class/index.js";
