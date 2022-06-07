import * as $foreign from "./foreign.js";
import * as Data_CommutativeRing from "../Data.CommutativeRing/index.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Ring from "../Data.Ring/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
var mod = function (dict) {
    return dict.mod;
};

// | The *greatest common divisor* of two values.
var gcd = function ($copy_dictEq) {
    return function ($copy_dictEuclideanRing) {
        return function ($copy_a) {
            return function ($copy_b) {
                var $tco_var_dictEq = $copy_dictEq;
                var $tco_var_dictEuclideanRing = $copy_dictEuclideanRing;
                var $tco_var_a = $copy_a;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(dictEq, dictEuclideanRing, a, b) {
                    var $8 = Data_Eq.eq(dictEq)(b)(Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0()));
                    if ($8) {
                        $tco_done = true;
                        return a;
                    };
                    $tco_var_dictEq = dictEq;
                    $tco_var_dictEuclideanRing = dictEuclideanRing;
                    $tco_var_a = b;
                    $copy_b = mod(dictEuclideanRing)(a)(b);
                    return;
                };
                while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_dictEq, $tco_var_dictEuclideanRing, $tco_var_a, $copy_b);
                };
                return $tco_result;
            };
        };
    };
};
var euclideanRingNumber = {
    degree: function (v) {
        return 1;
    },
    div: $foreign.numDiv,
    mod: function (v) {
        return function (v1) {
            return 0.0;
        };
    },
    CommutativeRing0: function () {
        return Data_CommutativeRing.commutativeRingNumber;
    }
};
var euclideanRingInt = {
    degree: $foreign.intDegree,
    div: $foreign.intDiv,
    mod: $foreign.intMod,
    CommutativeRing0: function () {
        return Data_CommutativeRing.commutativeRingInt;
    }
};
var div = function (dict) {
    return dict.div;
};

// | The *least common multiple* of two values.
var lcm = function (dictEq) {
    return function (dictEuclideanRing) {
        return function (a) {
            return function (b) {
                var $10 = Data_Eq.eq(dictEq)(a)(Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())) || Data_Eq.eq(dictEq)(b)(Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0()));
                if ($10) {
                    return Data_Semiring.zero(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0());
                };
                return div(dictEuclideanRing)(Data_Semiring.mul(((dictEuclideanRing.CommutativeRing0()).Ring0()).Semiring0())(a)(b))(gcd(dictEq)(dictEuclideanRing)(a)(b));
            };
        };
    };
};
var degree = function (dict) {
    return dict.degree;
};
export {
    degree,
    div,
    mod,
    gcd,
    lcm,
    euclideanRingInt,
    euclideanRingNumber
};
export {
    sub
} from "../Data.Ring/index.js";
export {
    add,
    mul,
    one,
    zero
} from "../Data.Semiring/index.js";
