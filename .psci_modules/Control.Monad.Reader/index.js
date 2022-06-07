// | This module defines the `Reader` monad.
import * as Control_Monad_Reader_Class from "../Control.Monad.Reader.Class/index.js";
import * as Control_Monad_Reader_Trans from "../Control.Monad.Reader.Trans/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";

// | Change the type of the context in a `Reader` monad action.
var withReader = Control_Monad_Reader_Trans.withReaderT;

// | Run a computation in the `Reader` monad.
var runReader = function (v) {
    var $2 = Data_Newtype.unwrap();
    return function ($3) {
        return $2(v($3));
    };
};

// | Change the type of the result in a `Reader` monad action.
var mapReader = function (f) {
    return Control_Monad_Reader_Trans.mapReaderT((function () {
        var $4 = Data_Newtype.unwrap();
        return function ($5) {
            return Data_Identity.Identity(f($4($5)));
        };
    })());
};
export {
    runReader,
    mapReader,
    withReader
};
export {
    ask,
    asks,
    local
} from "../Control.Monad.Reader.Class/index.js";
export {
    ReaderT,
    lift,
    mapReaderT,
    runReaderT,
    withReaderT
} from "../Control.Monad.Reader.Trans/index.js";
