import * as Control_Alt from "../Control.Alt/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
var plusArray = {
    empty: [  ],
    Alt0: function () {
        return Control_Alt.altArray;
    }
};
var empty = function (dict) {
    return dict.empty;
};
export {
    empty,
    plusArray
};
export {
    alt
} from "../Control.Alt/index.js";
export {
    map,
    void
} from "../Data.Functor/index.js";
