import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_EuclideanRing from "../Data.EuclideanRing/index.js";
import * as Data_Typelevel_Undefined from "../Data.Typelevel.Undefined/index.js";
import * as Partial_Unsafe from "../Partial.Unsafe/index.js";
var toInt = function (dict) {
    return dict.toInt;
};

// | Sugar for `toInt`, providing a more intuitive type
var toInt$prime = function (dictNat) {
    return function (v) {
        return toInt(dictNat)(Data_Typelevel_Undefined["undefined"]);
    };
};
var natD9 = {
    toInt: function (v) {
        return 9;
    }
};
var posD9 = {
    Nat0: function () {
        return natD9;
    }
};
var natD8 = {
    toInt: function (v) {
        return 8;
    }
};
var posD8 = {
    Nat0: function () {
        return natD8;
    }
};
var natD7 = {
    toInt: function (v) {
        return 7;
    }
};
var posD7 = {
    Nat0: function () {
        return natD7;
    }
};
var natD6 = {
    toInt: function (v) {
        return 6;
    }
};
var posD6 = {
    Nat0: function () {
        return natD6;
    }
};
var natD5 = {
    toInt: function (v) {
        return 5;
    }
};
var posD5 = {
    Nat0: function () {
        return natD5;
    }
};
var natD4 = {
    toInt: function (v) {
        return 4;
    }
};
var posD4 = {
    Nat0: function () {
        return natD4;
    }
};
var natD3 = {
    toInt: function (v) {
        return 3;
    }
};
var posD3 = {
    Nat0: function () {
        return natD3;
    }
};
var natD2 = {
    toInt: function (v) {
        return 2;
    }
};
var posD2 = {
    Nat0: function () {
        return natD2;
    }
};
var natD1 = {
    toInt: function (v) {
        return 1;
    }
};
var posD1 = {
    Nat0: function () {
        return natD1;
    }
};
var natD0 = {
    toInt: function (v) {
        return 0;
    }
};
var div10Dec = function (dictNat) {
    return function (v) {
        return Data_Typelevel_Undefined["undefined"];
    };
};
var subLastDec = function (dictNat) {
    return function (dictNat1) {
        var $74 = toInt(dictNat1);
        var $75 = div10Dec(dictNat);
        return function ($76) {
            return (function (v) {
                return 10 * v | 0;
            })($74($75($76)));
        };
    };
};
var posNatD0 = function (dictPos) {
    return {
        toInt: function (n) {
            return subLastDec(posNatD0(dictPos))(dictPos.Nat0())(n);
        }
    };
};
var posPosD0 = function (dictPos) {
    return {
        Nat0: function () {
            return posNatD0(dictPos);
        }
    };
};
var posNatD1 = function (dictPos) {
    return {
        toInt: function (n) {
            return subLastDec(posNatD1(dictPos))(dictPos.Nat0())(n) + 1 | 0;
        }
    };
};
var posPosD1 = function (dictPos) {
    return {
        Nat0: function () {
            return posNatD1(dictPos);
        }
    };
};
var posNatD2 = function (dictPos) {
    return {
        toInt: function (n) {
            return subLastDec(posNatD2(dictPos))(dictPos.Nat0())(n) + 2 | 0;
        }
    };
};
var posPosD2 = function (dictPos) {
    return {
        Nat0: function () {
            return posNatD2(dictPos);
        }
    };
};
var posNatD3 = function (dictPos) {
    return {
        toInt: function (n) {
            return subLastDec(posNatD3(dictPos))(dictPos.Nat0())(n) + 3 | 0;
        }
    };
};
var posPosD3 = function (dictPos) {
    return {
        Nat0: function () {
            return posNatD3(dictPos);
        }
    };
};
var posNatD4 = function (dictPos) {
    return {
        toInt: function (n) {
            return subLastDec(posNatD4(dictPos))(dictPos.Nat0())(n) + 4 | 0;
        }
    };
};
var posPosD4 = function (dictPos) {
    return {
        Nat0: function () {
            return posNatD4(dictPos);
        }
    };
};
var posNatD5 = function (dictPos) {
    return {
        toInt: function (n) {
            return subLastDec(posNatD5(dictPos))(dictPos.Nat0())(n) + 5 | 0;
        }
    };
};
var posPosD5 = function (dictPos) {
    return {
        Nat0: function () {
            return posNatD5(dictPos);
        }
    };
};
var posNatD6 = function (dictPos) {
    return {
        toInt: function (n) {
            return subLastDec(posNatD6(dictPos))(dictPos.Nat0())(n) + 6 | 0;
        }
    };
};
var posPosD6 = function (dictPos) {
    return {
        Nat0: function () {
            return posNatD6(dictPos);
        }
    };
};
var posNatD7 = function (dictPos) {
    return {
        toInt: function (n) {
            return subLastDec(posNatD7(dictPos))(dictPos.Nat0())(n) + 7 | 0;
        }
    };
};
var posPosD7 = function (dictPos) {
    return {
        Nat0: function () {
            return posNatD7(dictPos);
        }
    };
};
var posNatD8 = function (dictPos) {
    return {
        toInt: function (n) {
            return subLastDec(posNatD8(dictPos))(dictPos.Nat0())(n) + 8 | 0;
        }
    };
};
var posPosD8 = function (dictPos) {
    return {
        Nat0: function () {
            return posNatD8(dictPos);
        }
    };
};
var posNatD9 = function (dictPos) {
    return {
        toInt: function (n) {
            return subLastDec(posNatD9(dictPos))(dictPos.Nat0())(n) + 9 | 0;
        }
    };
};
var posPosD9 = function (dictPos) {
    return {
        Nat0: function () {
            return posNatD9(dictPos);
        }
    };
};
var reifyIntP = function ($copy_i) {
    return function ($copy_f) {
        var $tco_var_i = $copy_i;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(i, f) {
            if (i < 1) {
                $tco_done = true;
                return Partial_Unsafe.unsafeCrashWith("reifyIntP: integral < 1");
            };
            if (i === 1) {
                $tco_done = true;
                return f(posD1)(Data_Typelevel_Undefined["undefined"]);
            };
            if (i === 2) {
                $tco_done = true;
                return f(posD2)(Data_Typelevel_Undefined["undefined"]);
            };
            if (i === 3) {
                $tco_done = true;
                return f(posD3)(Data_Typelevel_Undefined["undefined"]);
            };
            if (i === 4) {
                $tco_done = true;
                return f(posD4)(Data_Typelevel_Undefined["undefined"]);
            };
            if (i === 5) {
                $tco_done = true;
                return f(posD5)(Data_Typelevel_Undefined["undefined"]);
            };
            if (i === 6) {
                $tco_done = true;
                return f(posD6)(Data_Typelevel_Undefined["undefined"]);
            };
            if (i === 7) {
                $tco_done = true;
                return f(posD7)(Data_Typelevel_Undefined["undefined"]);
            };
            if (i === 8) {
                $tco_done = true;
                return f(posD8)(Data_Typelevel_Undefined["undefined"]);
            };
            if (i === 9) {
                $tco_done = true;
                return f(posD9)(Data_Typelevel_Undefined["undefined"]);
            };
            if (Data_Boolean.otherwise) {
                var f9 = function (dictPos) {
                    return function (v) {
                        return f(posPosD9(dictPos))(Data_Typelevel_Undefined["undefined"]);
                    };
                };
                var f8 = function (dictPos) {
                    return function (v) {
                        return f(posPosD8(dictPos))(Data_Typelevel_Undefined["undefined"]);
                    };
                };
                var f7 = function (dictPos) {
                    return function (v) {
                        return f(posPosD7(dictPos))(Data_Typelevel_Undefined["undefined"]);
                    };
                };
                var f6 = function (dictPos) {
                    return function (v) {
                        return f(posPosD6(dictPos))(Data_Typelevel_Undefined["undefined"]);
                    };
                };
                var f5 = function (dictPos) {
                    return function (v) {
                        return f(posPosD5(dictPos))(Data_Typelevel_Undefined["undefined"]);
                    };
                };
                var f4 = function (dictPos) {
                    return function (v) {
                        return f(posPosD4(dictPos))(Data_Typelevel_Undefined["undefined"]);
                    };
                };
                var f3 = function (dictPos) {
                    return function (v) {
                        return f(posPosD3(dictPos))(Data_Typelevel_Undefined["undefined"]);
                    };
                };
                var f2 = function (dictPos) {
                    return function (v) {
                        return f(posPosD2(dictPos))(Data_Typelevel_Undefined["undefined"]);
                    };
                };
                var f11 = function (dictPos) {
                    return function (v) {
                        return f(posPosD1(dictPos))(Data_Typelevel_Undefined["undefined"]);
                    };
                };
                var f0 = function (dictPos) {
                    return function (v) {
                        return f(posPosD0(dictPos))(Data_Typelevel_Undefined["undefined"]);
                    };
                };
                var m = Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(i)(10);
                var d = Data_EuclideanRing.div(Data_EuclideanRing.euclideanRingInt)(i)(10);
                if (m === 0) {
                    $tco_var_i = d;
                    $copy_f = function (dictPos) {
                        return f0(dictPos);
                    };
                    return;
                };
                if (m === 1) {
                    $tco_var_i = d;
                    $copy_f = function (dictPos) {
                        return f11(dictPos);
                    };
                    return;
                };
                if (m === 2) {
                    $tco_var_i = d;
                    $copy_f = function (dictPos) {
                        return f2(dictPos);
                    };
                    return;
                };
                if (m === 3) {
                    $tco_var_i = d;
                    $copy_f = function (dictPos) {
                        return f3(dictPos);
                    };
                    return;
                };
                if (m === 4) {
                    $tco_var_i = d;
                    $copy_f = function (dictPos) {
                        return f4(dictPos);
                    };
                    return;
                };
                if (m === 5) {
                    $tco_var_i = d;
                    $copy_f = function (dictPos) {
                        return f5(dictPos);
                    };
                    return;
                };
                if (m === 6) {
                    $tco_var_i = d;
                    $copy_f = function (dictPos) {
                        return f6(dictPos);
                    };
                    return;
                };
                if (m === 7) {
                    $tco_var_i = d;
                    $copy_f = function (dictPos) {
                        return f7(dictPos);
                    };
                    return;
                };
                if (m === 8) {
                    $tco_var_i = d;
                    $copy_f = function (dictPos) {
                        return f8(dictPos);
                    };
                    return;
                };
                if (m === 9) {
                    $tco_var_i = d;
                    $copy_f = function (dictPos) {
                        return f9(dictPos);
                    };
                    return;
                };
                throw new Error("Failed pattern match at Data.Typelevel.Num.Sets (line 88, column 24 - line 98, column 26): " + [ m.constructor.name ]);
            };
            throw new Error("Failed pattern match at Data.Typelevel.Num.Sets (line 73, column 1 - line 73, column 63): " + [ i.constructor.name, f.constructor.name ]);
        };
        while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_i, $copy_f);
        };
        return $tco_result;
    };
};
var reifyInt = function (i) {
    return function (f) {
        if (i < 0) {
            return Partial_Unsafe.unsafeCrashWith("reifyInt: integral < 0");
        };
        if (i === 0) {
            return f(natD0)(Data_Typelevel_Undefined["undefined"]);
        };
        if (Data_Boolean.otherwise) {
            return reifyIntP(i)(function (dictPos) {
                return f(dictPos.Nat0());
            });
        };
        throw new Error("Failed pattern match at Data.Typelevel.Num.Sets (line 67, column 1 - line 67, column 62): " + [ i.constructor.name, f.constructor.name ]);
    };
};
export {
    toInt,
    toInt$prime,
    subLastDec,
    div10Dec,
    reifyInt,
    reifyIntP,
    natD0,
    natD1,
    natD2,
    natD3,
    natD4,
    natD5,
    natD6,
    natD7,
    natD8,
    natD9,
    posNatD0,
    posNatD1,
    posNatD2,
    posNatD3,
    posNatD4,
    posNatD5,
    posNatD6,
    posNatD7,
    posNatD8,
    posNatD9,
    posD1,
    posD2,
    posD3,
    posD4,
    posD5,
    posD6,
    posD7,
    posD8,
    posD9,
    posPosD0,
    posPosD1,
    posPosD2,
    posPosD3,
    posPosD4,
    posPosD5,
    posPosD6,
    posPosD7,
    posPosD8,
    posPosD9
};
