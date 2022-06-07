import * as Data_Ring from "../Data.Ring/index.js";
import * as Data_Semiring from "../Data.Semiring/index.js";
var commutativeRingUnit = {
    Ring0: function () {
        return Data_Ring.ringUnit;
    }
};
var commutativeRingRecordNil = {
    RingRecord0: function () {
        return Data_Ring.ringRecordNil;
    }
};
var commutativeRingRecordCons = function (dictIsSymbol) {
    return function () {
        return function (dictCommutativeRingRecord) {
            return function (dictCommutativeRing) {
                return {
                    RingRecord0: function () {
                        return Data_Ring.ringRecordCons(dictIsSymbol)()(dictCommutativeRingRecord.RingRecord0())(dictCommutativeRing.Ring0());
                    }
                };
            };
        };
    };
};
var commutativeRingRecord = function () {
    return function (dictCommutativeRingRecord) {
        return {
            Ring0: function () {
                return Data_Ring.ringRecord()(dictCommutativeRingRecord.RingRecord0());
            }
        };
    };
};
var commutativeRingProxy = {
    Ring0: function () {
        return Data_Ring.ringProxy;
    }
};
var commutativeRingNumber = {
    Ring0: function () {
        return Data_Ring.ringNumber;
    }
};
var commutativeRingInt = {
    Ring0: function () {
        return Data_Ring.ringInt;
    }
};
var commutativeRingFn = function (dictCommutativeRing) {
    return {
        Ring0: function () {
            return Data_Ring.ringFn(dictCommutativeRing.Ring0());
        }
    };
};
export {
    commutativeRingInt,
    commutativeRingNumber,
    commutativeRingUnit,
    commutativeRingFn,
    commutativeRingRecord,
    commutativeRingProxy,
    commutativeRingRecordNil,
    commutativeRingRecordCons
};
export {
    add,
    mul,
    one,
    zero
} from "../Data.Semiring/index.js";
