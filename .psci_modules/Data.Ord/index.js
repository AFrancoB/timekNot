import * as $foreign from "./foreign.js";
import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
import * as Data_Ring from "../Data.Ring/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
import * as Data_Symbol from "../Data.Symbol/index.js";
import * as Record_Unsafe from "../Record.Unsafe/index.js";
import * as Type_Proxy from "../Type.Proxy/index.js";
var ordVoid = {
    compare: function (v) {
        return function (v1) {
            return Data_Ordering.EQ.value;
        };
    },
    Eq0: function () {
        return Data_Eq.eqVoid;
    }
};
var ordUnit = {
    compare: function (v) {
        return function (v1) {
            return Data_Ordering.EQ.value;
        };
    },
    Eq0: function () {
        return Data_Eq.eqUnit;
    }
};
var ordString = /* #__PURE__ */ (function () {
    return {
        compare: $foreign.ordStringImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value),
        Eq0: function () {
            return Data_Eq.eqString;
        }
    };
})();
var ordRecordNil = {
    compareRecord: function (v) {
        return function (v1) {
            return function (v2) {
                return Data_Ordering.EQ.value;
            };
        };
    },
    EqRecord0: function () {
        return Data_Eq.eqRowNil;
    }
};
var ordProxy = {
    compare: function (v) {
        return function (v1) {
            return Data_Ordering.EQ.value;
        };
    },
    Eq0: function () {
        return Data_Eq.eqProxy;
    }
};
var ordOrdering = {
    compare: function (v) {
        return function (v1) {
            if (v instanceof Data_Ordering.LT && v1 instanceof Data_Ordering.LT) {
                return Data_Ordering.EQ.value;
            };
            if (v instanceof Data_Ordering.EQ && v1 instanceof Data_Ordering.EQ) {
                return Data_Ordering.EQ.value;
            };
            if (v instanceof Data_Ordering.GT && v1 instanceof Data_Ordering.GT) {
                return Data_Ordering.EQ.value;
            };
            if (v instanceof Data_Ordering.LT) {
                return Data_Ordering.LT.value;
            };
            if (v instanceof Data_Ordering.EQ && v1 instanceof Data_Ordering.LT) {
                return Data_Ordering.GT.value;
            };
            if (v instanceof Data_Ordering.EQ && v1 instanceof Data_Ordering.GT) {
                return Data_Ordering.LT.value;
            };
            if (v instanceof Data_Ordering.GT) {
                return Data_Ordering.GT.value;
            };
            throw new Error("Failed pattern match at Data.Ord (line 126, column 1 - line 133, column 20): " + [ v.constructor.name, v1.constructor.name ]);
        };
    },
    Eq0: function () {
        return Data_Ordering.eqOrdering;
    }
};
var ordNumber = /* #__PURE__ */ (function () {
    return {
        compare: $foreign.ordNumberImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value),
        Eq0: function () {
            return Data_Eq.eqNumber;
        }
    };
})();
var ordInt = /* #__PURE__ */ (function () {
    return {
        compare: $foreign.ordIntImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value),
        Eq0: function () {
            return Data_Eq.eqInt;
        }
    };
})();
var ordChar = /* #__PURE__ */ (function () {
    return {
        compare: $foreign.ordCharImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value),
        Eq0: function () {
            return Data_Eq.eqChar;
        }
    };
})();
var ordBoolean = /* #__PURE__ */ (function () {
    return {
        compare: $foreign.ordBooleanImpl(Data_Ordering.LT.value)(Data_Ordering.EQ.value)(Data_Ordering.GT.value),
        Eq0: function () {
            return Data_Eq.eqBoolean;
        }
    };
})();
var compareRecord = function (dict) {
    return dict.compareRecord;
};
var ordRecord = function () {
    return function (dictOrdRecord) {
        return {
            compare: compareRecord(dictOrdRecord)(Type_Proxy["Proxy"].value),
            Eq0: function () {
                return Data_Eq.eqRec()(dictOrdRecord.EqRecord0());
            }
        };
    };
};
var compare1 = function (dict) {
    return dict.compare1;
};
var compare = function (dict) {
    return dict.compare;
};

// | Compares two values by mapping them to a type with an `Ord` instance.
var comparing = function (dictOrd) {
    return function (f) {
        return function (x) {
            return function (y) {
                return compare(dictOrd)(f(x))(f(y));
            };
        };
    };
};

// | Test whether one value is _strictly greater than_ another.
var greaterThan = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var v = compare(dictOrd)(a1)(a2);
            if (v instanceof Data_Ordering.GT) {
                return true;
            };
            return false;
        };
    };
};

// | Test whether one value is _non-strictly greater than_ another.
var greaterThanOrEq = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var v = compare(dictOrd)(a1)(a2);
            if (v instanceof Data_Ordering.LT) {
                return false;
            };
            return true;
        };
    };
};

// | Test whether one value is _strictly less than_ another.
var lessThan = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var v = compare(dictOrd)(a1)(a2);
            if (v instanceof Data_Ordering.LT) {
                return true;
            };
            return false;
        };
    };
};

// | The sign function; returns `one` if the argument is positive,
// | `negate one` if the argument is negative, or `zero` if the argument is `zero`.
// | For floating point numbers with signed zeroes, when called with a zero,
// | this function returns the argument in order to preserve the sign.
// | For any `x`, we should have `signum x * abs x == x`.
var signum = function (dictOrd) {
    return function (dictRing) {
        return function (x) {
            var $47 = lessThan(dictOrd)(x)(Data_Semiring.zero(dictRing.Semiring0()));
            if ($47) {
                return Data_Ring.negate(dictRing)(Data_Semiring.one(dictRing.Semiring0()));
            };
            var $48 = greaterThan(dictOrd)(x)(Data_Semiring.zero(dictRing.Semiring0()));
            if ($48) {
                return Data_Semiring.one(dictRing.Semiring0());
            };
            return x;
        };
    };
};

// | Test whether one value is _non-strictly less than_ another.
var lessThanOrEq = function (dictOrd) {
    return function (a1) {
        return function (a2) {
            var v = compare(dictOrd)(a1)(a2);
            if (v instanceof Data_Ordering.GT) {
                return false;
            };
            return true;
        };
    };
};

// | Take the maximum of two values. If they are considered equal, the first
// | argument is chosen.
var max = function (dictOrd) {
    return function (x) {
        return function (y) {
            var v = compare(dictOrd)(x)(y);
            if (v instanceof Data_Ordering.LT) {
                return y;
            };
            if (v instanceof Data_Ordering.EQ) {
                return x;
            };
            if (v instanceof Data_Ordering.GT) {
                return x;
            };
            throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): " + [ v.constructor.name ]);
        };
    };
};

// | Take the minimum of two values. If they are considered equal, the first
// | argument is chosen.
var min = function (dictOrd) {
    return function (x) {
        return function (y) {
            var v = compare(dictOrd)(x)(y);
            if (v instanceof Data_Ordering.LT) {
                return x;
            };
            if (v instanceof Data_Ordering.EQ) {
                return x;
            };
            if (v instanceof Data_Ordering.GT) {
                return y;
            };
            throw new Error("Failed pattern match at Data.Ord (line 172, column 3 - line 175, column 12): " + [ v.constructor.name ]);
        };
    };
};
var ordArray = function (dictOrd) {
    return {
        compare: (function () {
            var toDelta = function (x) {
                return function (y) {
                    var v = compare(dictOrd)(x)(y);
                    if (v instanceof Data_Ordering.EQ) {
                        return 0;
                    };
                    if (v instanceof Data_Ordering.LT) {
                        return 1;
                    };
                    if (v instanceof Data_Ordering.GT) {
                        return -1 | 0;
                    };
                    throw new Error("Failed pattern match at Data.Ord (line 79, column 7 - line 82, column 17): " + [ v.constructor.name ]);
                };
            };
            return function (xs) {
                return function (ys) {
                    return compare(ordInt)(0)($foreign.ordArrayImpl(toDelta)(xs)(ys));
                };
            };
        })(),
        Eq0: function () {
            return Data_Eq.eqArray(dictOrd.Eq0());
        }
    };
};
var ord1Array = {
    compare1: function (dictOrd) {
        return compare(ordArray(dictOrd));
    },
    Eq10: function () {
        return Data_Eq.eq1Array;
    }
};
var ordRecordCons = function (dictOrdRecord) {
    return function () {
        return function (dictIsSymbol) {
            return function (dictOrd) {
                return {
                    compareRecord: function (v) {
                        return function (ra) {
                            return function (rb) {
                                var key = Data_Symbol.reflectSymbol(dictIsSymbol)(Type_Proxy["Proxy"].value);
                                var left = compare(dictOrd)(Record_Unsafe.unsafeGet(key)(ra))(Record_Unsafe.unsafeGet(key)(rb));
                                var $53 = Data_Eq.notEq(Data_Ordering.eqOrdering)(left)(Data_Ordering.EQ.value);
                                if ($53) {
                                    return left;
                                };
                                return compareRecord(dictOrdRecord)(Type_Proxy["Proxy"].value)(ra)(rb);
                            };
                        };
                    },
                    EqRecord0: function () {
                        return Data_Eq.eqRowCons(dictOrdRecord.EqRecord0())()(dictIsSymbol)(dictOrd.Eq0());
                    }
                };
            };
        };
    };
};

// | Clamp a value between a minimum and a maximum. For example:
// |
// | ``` purescript
// | let f = clamp 0 10
// | f (-5) == 0
// | f 5    == 5
// | f 15   == 10
// | ```
var clamp = function (dictOrd) {
    return function (low) {
        return function (hi) {
            return function (x) {
                return min(dictOrd)(hi)(max(dictOrd)(low)(x));
            };
        };
    };
};

// | Test whether a value is between a minimum and a maximum (inclusive).
// | For example:
// |
// | ``` purescript
// | let f = between 0 10
// | f 0    == true
// | f (-5) == false
// | f 5    == true
// | f 10   == true
// | f 15   == false
// | ```
var between = function (dictOrd) {
    return function (low) {
        return function (hi) {
            return function (x) {
                if (lessThan(dictOrd)(x)(low)) {
                    return false;
                };
                if (greaterThan(dictOrd)(x)(hi)) {
                    return false;
                };
                return true;
            };
        };
    };
};

// | The absolute value function. `abs x` is defined as `if x >= zero then x
// | else negate x`.
var abs = function (dictOrd) {
    return function (dictRing) {
        return function (x) {
            var $57 = greaterThanOrEq(dictOrd)(x)(Data_Semiring.zero(dictRing.Semiring0()));
            if ($57) {
                return x;
            };
            return Data_Ring.negate(dictRing)(x);
        };
    };
};
export {
    compare,
    compare1,
    lessThan,
    lessThanOrEq,
    greaterThan,
    greaterThanOrEq,
    comparing,
    min,
    max,
    clamp,
    between,
    abs,
    signum,
    compareRecord,
    ordBoolean,
    ordInt,
    ordNumber,
    ordString,
    ordChar,
    ordUnit,
    ordVoid,
    ordProxy,
    ordArray,
    ordOrdering,
    ord1Array,
    ordRecordNil,
    ordRecordCons,
    ordRecord
};
export {
    EQ,
    GT,
    LT
} from "../Data.Ordering/index.js";
