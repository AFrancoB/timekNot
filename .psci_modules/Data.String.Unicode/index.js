import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_CodePoint_Unicode from "../Data.CodePoint.Unicode/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_String_CodePoints from "../Data.String.CodePoints/index.js";
var convertFull = function (f) {
    var $0 = Control_Bind.bindFlipped(Control_Bind.bindArray)(f);
    return function ($1) {
        return Data_String_CodePoints.fromCodePointArray($0(Data_String_CodePoints.toCodePointArray($1)));
    };
};

// | Convert each code point in the string to its corresponding lower
// | sequence. This is the full (locale-independent) Unicode algorithm,
// | and may map single code points to more than one code point. For example,
// | `toLower "\x0130" == "\x0069\x0307"`.
// |
// | Because this matches on more rules, it may be slower than `toLowerSimple`,
// | but it provides more correct results.
var toLower = /* #__PURE__ */ convertFull(Data_CodePoint_Unicode.toLower);

// Full Unicode conversions
// | Convert each code point in the string to its corresponding uppercase
// | sequence. This is the full (locale-independent) Unicode algorithm,
// | and may map single code points to more than one code point. For example,
// | `toUpper "ß" == "SS"`.
// |
// | Because this matches on more rules, it may be slower than `toUpperSimple`,
// | but it provides more correct results.
var toUpper = /* #__PURE__ */ convertFull(Data_CodePoint_Unicode.toUpper);

// Helper functions
var convert = function (f) {
    var $2 = Data_Functor.map(Data_Functor.functorArray)(f);
    return function ($3) {
        return Data_String_CodePoints.fromCodePointArray($2(Data_String_CodePoints.toCodePointArray($3)));
    };
};

// | Convert each code point in the string to its corresponding lowercase
// | code point. This will preserve the number of code points in the string.
// |
// | Note: this is not the full Unicode algorithm, see `toLower`.
var toLowerSimple = /* #__PURE__ */ convert(Data_CodePoint_Unicode.toLowerSimple);

// Simple code-point-to-code-point conversion algorithms
// | Convert each code point in the string to its corresponding uppercase
// | code point. This will preserve the number of code points in the string.
// |
// | Note: this is not the full Unicode algorithm, see `toUpper`.
var toUpperSimple = /* #__PURE__ */ convert(Data_CodePoint_Unicode.toUpperSimple);

// | Convert each code point in the string to its corresponding case-folded
// | code point. This will preserve the number of code points in the string.
// |
// | Note: this is not the full Unicode algorithm, see `caseFold`.
var caseFoldSimple = /* #__PURE__ */ convert(Data_CodePoint_Unicode.caseFoldSimple);

// | The full Unicode case folding algorithm, may increase the length of the
// | string by mapping individual code points to longer sequences.
var caseFold = /* #__PURE__ */ convertFull(Data_CodePoint_Unicode.caseFold);

// | Caseless matching, based on `caseFold`.
var caselessMatch = function (s1) {
    return function (s2) {
        return caseFold(s1) === caseFold(s2);
    };
};
export {
    toUpper,
    toLower,
    caseFold,
    caselessMatch,
    toUpperSimple,
    toLowerSimple,
    caseFoldSimple
};
