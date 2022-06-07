// | This module defines the `State` monad.
import * as Control_Monad_State_Class from "../Control.Monad.State.Class/index.js";
import * as Control_Monad_State_Trans from "../Control.Monad.State.Trans/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";

// | Modify the state in a `State` action
var withState = Control_Monad_State_Trans.withStateT;

// | Run a computation in the `State` monad
var runState = function (v) {
    var $16 = Data_Newtype.unwrap();
    return function ($17) {
        return $16(v($17));
    };
};

// | Change the type of the result in a `State` action
var mapState = function (f) {
    return Control_Monad_State_Trans.mapStateT((function () {
        var $18 = Data_Newtype.unwrap();
        return function ($19) {
            return Data_Identity.Identity(f($18($19)));
        };
    })());
};

// | Run a computation in the `State` monad, discarding the result
var execState = function (v) {
    return function (s) {
        var v1 = v(s);
        return v1.value1;
    };
};

// | Run a computation in the `State` monad, discarding the final state
var evalState = function (v) {
    return function (s) {
        var v1 = v(s);
        return v1.value0;
    };
};
export {
    runState,
    evalState,
    execState,
    mapState,
    withState
};
export {
    get,
    gets,
    modify,
    modify_,
    put,
    state
} from "../Control.Monad.State.Class/index.js";
export {
    StateT,
    evalStateT,
    execStateT,
    lift,
    mapStateT,
    runStateT,
    withStateT
} from "../Control.Monad.State.Trans/index.js";
