// Generated by purs version 0.15.4
import * as Data_Boolean from "../Data.Boolean/index.js";
import * as Data_Char from "../Data.Char/index.js";
import * as Data_CodePoint_Unicode_Internal from "../Data.CodePoint.Unicode.Internal/index.js";
import * as Data_CodePoint_Unicode_Internal_Casing from "../Data.CodePoint.Unicode.Internal.Casing/index.js";
import * as Data_Enum from "../Data.Enum/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Ord from "../Data.Ord/index.js";
import * as Data_String_CodePoints from "../Data.String.CodePoints/index.js";
import * as Unsafe_Coerce from "../Unsafe.Coerce/index.js";
var fromEnum = /* #__PURE__ */ Data_Enum.fromEnum(Data_String_CodePoints.boundedEnumCodePoint);
var lessThanOrEq1 = /* #__PURE__ */ Data_Ord.lessThanOrEq(Data_String_CodePoints.ordCodePoint);
var greaterThanOrEq1 = /* #__PURE__ */ Data_Ord.greaterThanOrEq(Data_String_CodePoints.ordCodePoint);
var lessThan = /* #__PURE__ */ Data_Ord.lessThan(Data_String_CodePoints.ordCodePoint);
var compare = /* #__PURE__ */ Data_Ord.compare(Data_Ord.ordInt);
var UppercaseLetter = /* #__PURE__ */ (function () {
    function UppercaseLetter() {

    };
    UppercaseLetter.value = new UppercaseLetter();
    return UppercaseLetter;
})();
var LowercaseLetter = /* #__PURE__ */ (function () {
    function LowercaseLetter() {

    };
    LowercaseLetter.value = new LowercaseLetter();
    return LowercaseLetter;
})();
var TitlecaseLetter = /* #__PURE__ */ (function () {
    function TitlecaseLetter() {

    };
    TitlecaseLetter.value = new TitlecaseLetter();
    return TitlecaseLetter;
})();
var ModifierLetter = /* #__PURE__ */ (function () {
    function ModifierLetter() {

    };
    ModifierLetter.value = new ModifierLetter();
    return ModifierLetter;
})();
var OtherLetter = /* #__PURE__ */ (function () {
    function OtherLetter() {

    };
    OtherLetter.value = new OtherLetter();
    return OtherLetter;
})();
var NonSpacingMark = /* #__PURE__ */ (function () {
    function NonSpacingMark() {

    };
    NonSpacingMark.value = new NonSpacingMark();
    return NonSpacingMark;
})();
var SpacingCombiningMark = /* #__PURE__ */ (function () {
    function SpacingCombiningMark() {

    };
    SpacingCombiningMark.value = new SpacingCombiningMark();
    return SpacingCombiningMark;
})();
var EnclosingMark = /* #__PURE__ */ (function () {
    function EnclosingMark() {

    };
    EnclosingMark.value = new EnclosingMark();
    return EnclosingMark;
})();
var DecimalNumber = /* #__PURE__ */ (function () {
    function DecimalNumber() {

    };
    DecimalNumber.value = new DecimalNumber();
    return DecimalNumber;
})();
var LetterNumber = /* #__PURE__ */ (function () {
    function LetterNumber() {

    };
    LetterNumber.value = new LetterNumber();
    return LetterNumber;
})();
var OtherNumber = /* #__PURE__ */ (function () {
    function OtherNumber() {

    };
    OtherNumber.value = new OtherNumber();
    return OtherNumber;
})();
var ConnectorPunctuation = /* #__PURE__ */ (function () {
    function ConnectorPunctuation() {

    };
    ConnectorPunctuation.value = new ConnectorPunctuation();
    return ConnectorPunctuation;
})();
var DashPunctuation = /* #__PURE__ */ (function () {
    function DashPunctuation() {

    };
    DashPunctuation.value = new DashPunctuation();
    return DashPunctuation;
})();
var OpenPunctuation = /* #__PURE__ */ (function () {
    function OpenPunctuation() {

    };
    OpenPunctuation.value = new OpenPunctuation();
    return OpenPunctuation;
})();
var ClosePunctuation = /* #__PURE__ */ (function () {
    function ClosePunctuation() {

    };
    ClosePunctuation.value = new ClosePunctuation();
    return ClosePunctuation;
})();
var InitialQuote = /* #__PURE__ */ (function () {
    function InitialQuote() {

    };
    InitialQuote.value = new InitialQuote();
    return InitialQuote;
})();
var FinalQuote = /* #__PURE__ */ (function () {
    function FinalQuote() {

    };
    FinalQuote.value = new FinalQuote();
    return FinalQuote;
})();
var OtherPunctuation = /* #__PURE__ */ (function () {
    function OtherPunctuation() {

    };
    OtherPunctuation.value = new OtherPunctuation();
    return OtherPunctuation;
})();
var MathSymbol = /* #__PURE__ */ (function () {
    function MathSymbol() {

    };
    MathSymbol.value = new MathSymbol();
    return MathSymbol;
})();
var CurrencySymbol = /* #__PURE__ */ (function () {
    function CurrencySymbol() {

    };
    CurrencySymbol.value = new CurrencySymbol();
    return CurrencySymbol;
})();
var ModifierSymbol = /* #__PURE__ */ (function () {
    function ModifierSymbol() {

    };
    ModifierSymbol.value = new ModifierSymbol();
    return ModifierSymbol;
})();
var OtherSymbol = /* #__PURE__ */ (function () {
    function OtherSymbol() {

    };
    OtherSymbol.value = new OtherSymbol();
    return OtherSymbol;
})();
var Space = /* #__PURE__ */ (function () {
    function Space() {

    };
    Space.value = new Space();
    return Space;
})();
var LineSeparator = /* #__PURE__ */ (function () {
    function LineSeparator() {

    };
    LineSeparator.value = new LineSeparator();
    return LineSeparator;
})();
var ParagraphSeparator = /* #__PURE__ */ (function () {
    function ParagraphSeparator() {

    };
    ParagraphSeparator.value = new ParagraphSeparator();
    return ParagraphSeparator;
})();
var Control = /* #__PURE__ */ (function () {
    function Control() {

    };
    Control.value = new Control();
    return Control;
})();
var Format = /* #__PURE__ */ (function () {
    function Format() {

    };
    Format.value = new Format();
    return Format;
})();
var Surrogate = /* #__PURE__ */ (function () {
    function Surrogate() {

    };
    Surrogate.value = new Surrogate();
    return Surrogate;
})();
var PrivateUse = /* #__PURE__ */ (function () {
    function PrivateUse() {

    };
    PrivateUse.value = new PrivateUse();
    return PrivateUse;
})();
var NotAssigned = /* #__PURE__ */ (function () {
    function NotAssigned() {

    };
    NotAssigned.value = new NotAssigned();
    return NotAssigned;
})();
var unicodeCatToGeneralCat = function (v) {
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_LU) {
        return UppercaseLetter.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_LL) {
        return LowercaseLetter.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_LT) {
        return TitlecaseLetter.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_LM) {
        return ModifierLetter.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_LO) {
        return OtherLetter.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_MN) {
        return NonSpacingMark.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_MC) {
        return SpacingCombiningMark.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_ME) {
        return EnclosingMark.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_ND) {
        return DecimalNumber.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_NL) {
        return LetterNumber.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_NO) {
        return OtherNumber.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_PC) {
        return ConnectorPunctuation.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_PD) {
        return DashPunctuation.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_PS) {
        return OpenPunctuation.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_PE) {
        return ClosePunctuation.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_PI) {
        return InitialQuote.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_PF) {
        return FinalQuote.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_PO) {
        return OtherPunctuation.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_SM) {
        return MathSymbol.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_SC) {
        return CurrencySymbol.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_SK) {
        return ModifierSymbol.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_SO) {
        return OtherSymbol.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_ZS) {
        return Space.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_ZL) {
        return LineSeparator.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_ZP) {
        return ParagraphSeparator.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_CC) {
        return Control.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_CF) {
        return Format.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_CS) {
        return Surrogate.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_CO) {
        return PrivateUse.value;
    };
    if (v instanceof Data_CodePoint_Unicode_Internal.NUMCAT_CN) {
        return NotAssigned.value;
    };
    throw new Error("Failed pattern match at Data.CodePoint.Unicode (line 206, column 1 - line 206, column 61): " + [ v.constructor.name ]);
};
var showGeneralCategory = {
    show: function (v) {
        if (v instanceof UppercaseLetter) {
            return "UppercaseLetter";
        };
        if (v instanceof LowercaseLetter) {
            return "LowercaseLetter";
        };
        if (v instanceof TitlecaseLetter) {
            return "TitlecaseLetter";
        };
        if (v instanceof ModifierLetter) {
            return "ModifierLetter";
        };
        if (v instanceof OtherLetter) {
            return "OtherLetter";
        };
        if (v instanceof NonSpacingMark) {
            return "NonSpacingMark";
        };
        if (v instanceof SpacingCombiningMark) {
            return "SpacingCombiningMark";
        };
        if (v instanceof EnclosingMark) {
            return "EnclosingMark";
        };
        if (v instanceof DecimalNumber) {
            return "DecimalNumber";
        };
        if (v instanceof LetterNumber) {
            return "LetterNumber";
        };
        if (v instanceof OtherNumber) {
            return "OtherNumber";
        };
        if (v instanceof ConnectorPunctuation) {
            return "ConnectorPunctuation";
        };
        if (v instanceof DashPunctuation) {
            return "DashPunctuation";
        };
        if (v instanceof OpenPunctuation) {
            return "OpenPunctuation";
        };
        if (v instanceof ClosePunctuation) {
            return "ClosePunctuation";
        };
        if (v instanceof InitialQuote) {
            return "InitialQuote";
        };
        if (v instanceof FinalQuote) {
            return "FinalQuote";
        };
        if (v instanceof OtherPunctuation) {
            return "OtherPunctuation";
        };
        if (v instanceof MathSymbol) {
            return "MathSymbol";
        };
        if (v instanceof CurrencySymbol) {
            return "CurrencySymbol";
        };
        if (v instanceof ModifierSymbol) {
            return "ModifierSymbol";
        };
        if (v instanceof OtherSymbol) {
            return "OtherSymbol";
        };
        if (v instanceof Space) {
            return "Space";
        };
        if (v instanceof LineSeparator) {
            return "LineSeparator";
        };
        if (v instanceof ParagraphSeparator) {
            return "ParagraphSeparator";
        };
        if (v instanceof Control) {
            return "Control";
        };
        if (v instanceof Format) {
            return "Format";
        };
        if (v instanceof Surrogate) {
            return "Surrogate";
        };
        if (v instanceof PrivateUse) {
            return "PrivateUse";
        };
        if (v instanceof NotAssigned) {
            return "NotAssigned";
        };
        throw new Error("Failed pattern match at Data.CodePoint.Unicode (line 238, column 1 - line 268, column 35): " + [ v.constructor.name ]);
    }
};
var modifyFull = Unsafe_Coerce.unsafeCoerce;
var toLower = /* #__PURE__ */ modifyFull(Data_CodePoint_Unicode_Internal_Casing.lower);
var toTitle = /* #__PURE__ */ modifyFull(Data_CodePoint_Unicode_Internal_Casing.title);
var toUpper = /* #__PURE__ */ modifyFull(Data_CodePoint_Unicode_Internal_Casing.upper);
var modify = Unsafe_Coerce.unsafeCoerce;
var toLowerSimple = /* #__PURE__ */ modify(Data_CodePoint_Unicode_Internal.uTowlower);
var toTitleSimple = /* #__PURE__ */ modify(Data_CodePoint_Unicode_Internal.uTowtitle);
var toUpperSimple = /* #__PURE__ */ modify(Data_CodePoint_Unicode_Internal.uTowupper);
var isUpper = function ($66) {
    return Data_CodePoint_Unicode_Internal.uIswupper(fromEnum($66));
};
var isSpace = function (c) {
    var uc = fromEnum(c);
    var $28 = uc <= 823;
    if ($28) {
        return uc === 32 || (uc >= 9 && uc <= 13 || uc === 160);
    };
    return Data_CodePoint_Unicode_Internal.uIswspace(uc);
};
var isPrint = function ($67) {
    return Data_CodePoint_Unicode_Internal.uIswprint(fromEnum($67));
};
var isOctDigit = function (c) {
    var diff = fromEnum(c) - Data_Char.toCharCode("0") | 0;
    return diff <= 7 && diff >= 0;
};
var octDigitToInt = function (c) {
    if (isOctDigit(c)) {
        return new Data_Maybe.Just(fromEnum(c) - Data_Char.toCharCode("0") | 0);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.CodePoint.Unicode (line 636, column 1 - line 636, column 40): " + [ c.constructor.name ]);
};
var isLower = function ($68) {
    return Data_CodePoint_Unicode_Internal.uIswlower(fromEnum($68));
};
var isLatin1 = function (c) {
    return lessThanOrEq1(c)(Data_String_CodePoints.codePointFromChar("\xff"));
};
var isDecDigit = function (c) {
    var diff = fromEnum(c) - Data_Char.toCharCode("0") | 0;
    return diff <= 9 && diff >= 0;
};
var isDigit = function () {
    return isDecDigit;
};
var isHexDigit = function (c) {
    return isDecDigit(c) || ((function () {
        var diff = fromEnum(c) - Data_Char.toCharCode("A") | 0;
        return diff <= 5 && diff >= 0;
    })() || (function () {
        var diff = fromEnum(c) - Data_Char.toCharCode("a") | 0;
        return diff <= 5 && diff >= 0;
    })());
};
var isControl = function ($69) {
    return Data_CodePoint_Unicode_Internal.uIswcntrl(fromEnum($69));
};
var isAsciiUpper = function (c) {
    return greaterThanOrEq1(c)(Data_String_CodePoints.codePointFromChar("A")) && lessThanOrEq1(c)(Data_String_CodePoints.codePointFromChar("Z"));
};
var isAsciiLower = function (c) {
    return greaterThanOrEq1(c)(Data_String_CodePoints.codePointFromChar("a")) && lessThanOrEq1(c)(Data_String_CodePoints.codePointFromChar("z"));
};
var isAscii = function (c) {
    return lessThan(c)(Data_String_CodePoints.codePointFromChar("\x80"));
};
var isAlphaNum = function ($70) {
    return Data_CodePoint_Unicode_Internal.uIswalnum(fromEnum($70));
};
var isAlpha = function ($71) {
    return Data_CodePoint_Unicode_Internal.uIswalpha(fromEnum($71));
};
var hexDigitToInt = function (c) {
    var hexUpper = fromEnum(c) - Data_Char.toCharCode("A") | 0;
    var hexLower = fromEnum(c) - Data_Char.toCharCode("a") | 0;
    var dec = fromEnum(c) - Data_Char.toCharCode("0") | 0;
    var result = (function () {
        if (dec <= 9 && dec >= 0) {
            return new Data_Maybe.Just(dec);
        };
        if (hexLower <= 5 && hexLower >= 0) {
            return new Data_Maybe.Just(hexLower + 10 | 0);
        };
        if (hexUpper <= 5 && hexUpper >= 0) {
            return new Data_Maybe.Just(hexUpper + 10 | 0);
        };
        if (Data_Boolean.otherwise) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at Data.CodePoint.Unicode (line 591, column 3 - line 591, column 22): " + [  ]);
    })();
    return result;
};
var generalCategory = /* #__PURE__ */ (function () {
    var $72 = Data_Functor.map(Data_Maybe.functorMaybe)(unicodeCatToGeneralCat);
    return function ($73) {
        return $72(Data_CodePoint_Unicode_Internal.uGencat(fromEnum($73)));
    };
})();
var isLetter = function (c) {
    var v = generalCategory(c);
    if (v instanceof Data_Maybe.Just && v.value0 instanceof UppercaseLetter) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof LowercaseLetter) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof TitlecaseLetter) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof ModifierLetter) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof OtherLetter) {
        return true;
    };
    return false;
};
var isMark = function (c) {
    var v = generalCategory(c);
    if (v instanceof Data_Maybe.Just && v.value0 instanceof NonSpacingMark) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof SpacingCombiningMark) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof EnclosingMark) {
        return true;
    };
    return false;
};
var isNumber = function (c) {
    var v = generalCategory(c);
    if (v instanceof Data_Maybe.Just && v.value0 instanceof DecimalNumber) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof LetterNumber) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof OtherNumber) {
        return true;
    };
    return false;
};
var isPunctuation = function (c) {
    var v = generalCategory(c);
    if (v instanceof Data_Maybe.Just && v.value0 instanceof ConnectorPunctuation) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof DashPunctuation) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof OpenPunctuation) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof ClosePunctuation) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof InitialQuote) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof FinalQuote) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof OtherPunctuation) {
        return true;
    };
    return false;
};
var isSeparator = function (c) {
    var v = generalCategory(c);
    if (v instanceof Data_Maybe.Just && v.value0 instanceof Space) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof LineSeparator) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof ParagraphSeparator) {
        return true;
    };
    return false;
};
var isSymbol = function (c) {
    var v = generalCategory(c);
    if (v instanceof Data_Maybe.Just && v.value0 instanceof MathSymbol) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof CurrencySymbol) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof ModifierSymbol) {
        return true;
    };
    if (v instanceof Data_Maybe.Just && v.value0 instanceof OtherSymbol) {
        return true;
    };
    return false;
};
var generalCatToUnicodeCat = function (v) {
    if (v instanceof UppercaseLetter) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_LU.value;
    };
    if (v instanceof LowercaseLetter) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_LL.value;
    };
    if (v instanceof TitlecaseLetter) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_LT.value;
    };
    if (v instanceof ModifierLetter) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_LM.value;
    };
    if (v instanceof OtherLetter) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_LO.value;
    };
    if (v instanceof NonSpacingMark) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_MN.value;
    };
    if (v instanceof SpacingCombiningMark) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_MC.value;
    };
    if (v instanceof EnclosingMark) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_ME.value;
    };
    if (v instanceof DecimalNumber) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_ND.value;
    };
    if (v instanceof LetterNumber) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_NL.value;
    };
    if (v instanceof OtherNumber) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_NO.value;
    };
    if (v instanceof ConnectorPunctuation) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_PC.value;
    };
    if (v instanceof DashPunctuation) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_PD.value;
    };
    if (v instanceof OpenPunctuation) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_PS.value;
    };
    if (v instanceof ClosePunctuation) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_PE.value;
    };
    if (v instanceof InitialQuote) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_PI.value;
    };
    if (v instanceof FinalQuote) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_PF.value;
    };
    if (v instanceof OtherPunctuation) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_PO.value;
    };
    if (v instanceof MathSymbol) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_SM.value;
    };
    if (v instanceof CurrencySymbol) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_SC.value;
    };
    if (v instanceof ModifierSymbol) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_SK.value;
    };
    if (v instanceof OtherSymbol) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_SO.value;
    };
    if (v instanceof Space) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_ZS.value;
    };
    if (v instanceof LineSeparator) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_ZL.value;
    };
    if (v instanceof ParagraphSeparator) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_ZP.value;
    };
    if (v instanceof Control) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_CC.value;
    };
    if (v instanceof Format) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_CF.value;
    };
    if (v instanceof Surrogate) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_CS.value;
    };
    if (v instanceof PrivateUse) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_CO.value;
    };
    if (v instanceof NotAssigned) {
        return Data_CodePoint_Unicode_Internal.NUMCAT_CN.value;
    };
    throw new Error("Failed pattern match at Data.CodePoint.Unicode (line 174, column 1 - line 174, column 61): " + [ v.constructor.name ]);
};
var generalCatToInt = function (v) {
    if (v instanceof UppercaseLetter) {
        return 1;
    };
    if (v instanceof LowercaseLetter) {
        return 2;
    };
    if (v instanceof TitlecaseLetter) {
        return 3;
    };
    if (v instanceof ModifierLetter) {
        return 4;
    };
    if (v instanceof OtherLetter) {
        return 5;
    };
    if (v instanceof NonSpacingMark) {
        return 6;
    };
    if (v instanceof SpacingCombiningMark) {
        return 7;
    };
    if (v instanceof EnclosingMark) {
        return 8;
    };
    if (v instanceof DecimalNumber) {
        return 9;
    };
    if (v instanceof LetterNumber) {
        return 10;
    };
    if (v instanceof OtherNumber) {
        return 11;
    };
    if (v instanceof ConnectorPunctuation) {
        return 12;
    };
    if (v instanceof DashPunctuation) {
        return 13;
    };
    if (v instanceof OpenPunctuation) {
        return 14;
    };
    if (v instanceof ClosePunctuation) {
        return 15;
    };
    if (v instanceof InitialQuote) {
        return 16;
    };
    if (v instanceof FinalQuote) {
        return 17;
    };
    if (v instanceof OtherPunctuation) {
        return 18;
    };
    if (v instanceof MathSymbol) {
        return 19;
    };
    if (v instanceof CurrencySymbol) {
        return 20;
    };
    if (v instanceof ModifierSymbol) {
        return 21;
    };
    if (v instanceof OtherSymbol) {
        return 22;
    };
    if (v instanceof Space) {
        return 23;
    };
    if (v instanceof LineSeparator) {
        return 24;
    };
    if (v instanceof ParagraphSeparator) {
        return 25;
    };
    if (v instanceof Control) {
        return 26;
    };
    if (v instanceof Format) {
        return 27;
    };
    if (v instanceof Surrogate) {
        return 28;
    };
    if (v instanceof PrivateUse) {
        return 29;
    };
    if (v instanceof NotAssigned) {
        return 30;
    };
    throw new Error("Failed pattern match at Data.CodePoint.Unicode (line 142, column 1 - line 142, column 42): " + [ v.constructor.name ]);
};
var eqGeneralCategory = {
    eq: function (v) {
        return function (v1) {
            if (v instanceof UppercaseLetter && v1 instanceof UppercaseLetter) {
                return true;
            };
            if (v instanceof LowercaseLetter && v1 instanceof LowercaseLetter) {
                return true;
            };
            if (v instanceof TitlecaseLetter && v1 instanceof TitlecaseLetter) {
                return true;
            };
            if (v instanceof ModifierLetter && v1 instanceof ModifierLetter) {
                return true;
            };
            if (v instanceof OtherLetter && v1 instanceof OtherLetter) {
                return true;
            };
            if (v instanceof NonSpacingMark && v1 instanceof NonSpacingMark) {
                return true;
            };
            if (v instanceof SpacingCombiningMark && v1 instanceof SpacingCombiningMark) {
                return true;
            };
            if (v instanceof EnclosingMark && v1 instanceof EnclosingMark) {
                return true;
            };
            if (v instanceof DecimalNumber && v1 instanceof DecimalNumber) {
                return true;
            };
            if (v instanceof LetterNumber && v1 instanceof LetterNumber) {
                return true;
            };
            if (v instanceof OtherNumber && v1 instanceof OtherNumber) {
                return true;
            };
            if (v instanceof ConnectorPunctuation && v1 instanceof ConnectorPunctuation) {
                return true;
            };
            if (v instanceof DashPunctuation && v1 instanceof DashPunctuation) {
                return true;
            };
            if (v instanceof OpenPunctuation && v1 instanceof OpenPunctuation) {
                return true;
            };
            if (v instanceof ClosePunctuation && v1 instanceof ClosePunctuation) {
                return true;
            };
            if (v instanceof InitialQuote && v1 instanceof InitialQuote) {
                return true;
            };
            if (v instanceof FinalQuote && v1 instanceof FinalQuote) {
                return true;
            };
            if (v instanceof OtherPunctuation && v1 instanceof OtherPunctuation) {
                return true;
            };
            if (v instanceof MathSymbol && v1 instanceof MathSymbol) {
                return true;
            };
            if (v instanceof CurrencySymbol && v1 instanceof CurrencySymbol) {
                return true;
            };
            if (v instanceof ModifierSymbol && v1 instanceof ModifierSymbol) {
                return true;
            };
            if (v instanceof OtherSymbol && v1 instanceof OtherSymbol) {
                return true;
            };
            if (v instanceof Space && v1 instanceof Space) {
                return true;
            };
            if (v instanceof LineSeparator && v1 instanceof LineSeparator) {
                return true;
            };
            if (v instanceof ParagraphSeparator && v1 instanceof ParagraphSeparator) {
                return true;
            };
            if (v instanceof Control && v1 instanceof Control) {
                return true;
            };
            if (v instanceof Format && v1 instanceof Format) {
                return true;
            };
            if (v instanceof Surrogate && v1 instanceof Surrogate) {
                return true;
            };
            if (v instanceof PrivateUse && v1 instanceof PrivateUse) {
                return true;
            };
            if (v instanceof NotAssigned && v1 instanceof NotAssigned) {
                return true;
            };
            return false;
        };
    }
};
var ordGeneralCategory = {
    compare: function (catA) {
        return function (catB) {
            return compare(generalCatToInt(catA))(generalCatToInt(catB));
        };
    },
    Eq0: function () {
        return eqGeneralCategory;
    }
};
var digitToInt = function () {
    return hexDigitToInt;
};
var decDigitToInt = function (c) {
    if (isDecDigit(c)) {
        return new Data_Maybe.Just(fromEnum(c) - Data_Char.toCharCode("0") | 0);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.CodePoint.Unicode (line 619, column 1 - line 619, column 40): " + [ c.constructor.name ]);
};
var caseFoldSimple = /* #__PURE__ */ modify(Data_CodePoint_Unicode_Internal_Casing.fold);
var caseFold = /* #__PURE__ */ modifyFull(Data_CodePoint_Unicode_Internal_Casing.foldFull);
var boundedGeneralCategory = /* #__PURE__ */ (function () {
    return {
        bottom: UppercaseLetter.value,
        top: NotAssigned.value,
        Ord0: function () {
            return ordGeneralCategory;
        }
    };
})();
export {
    isAscii,
    isAsciiLower,
    isAsciiUpper,
    isLatin1,
    isLower,
    isUpper,
    isAlpha,
    isAlphaNum,
    isLetter,
    isDigit,
    isDecDigit,
    isOctDigit,
    isHexDigit,
    isControl,
    isPrint,
    isSpace,
    isSymbol,
    isSeparator,
    isPunctuation,
    isMark,
    isNumber,
    digitToInt,
    hexDigitToInt,
    decDigitToInt,
    octDigitToInt,
    toLower,
    toUpper,
    toTitle,
    caseFold,
    toLowerSimple,
    toUpperSimple,
    toTitleSimple,
    caseFoldSimple,
    UppercaseLetter,
    LowercaseLetter,
    TitlecaseLetter,
    ModifierLetter,
    OtherLetter,
    NonSpacingMark,
    SpacingCombiningMark,
    EnclosingMark,
    DecimalNumber,
    LetterNumber,
    OtherNumber,
    ConnectorPunctuation,
    DashPunctuation,
    OpenPunctuation,
    ClosePunctuation,
    InitialQuote,
    FinalQuote,
    OtherPunctuation,
    MathSymbol,
    CurrencySymbol,
    ModifierSymbol,
    OtherSymbol,
    Space,
    LineSeparator,
    ParagraphSeparator,
    Control,
    Format,
    Surrogate,
    PrivateUse,
    NotAssigned,
    unicodeCatToGeneralCat,
    generalCatToInt,
    generalCatToUnicodeCat,
    generalCategory,
    showGeneralCategory,
    eqGeneralCategory,
    ordGeneralCategory,
    boundedGeneralCategory
};
