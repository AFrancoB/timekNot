// | This module is a port of the Haskell
// | [__Text.Parsec.Language__](https://hackage.haskell.org/package/parsec/docs/Text-Parsec-Language.html)
// | module.
import * as Control_Alt from "../Control.Alt/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as Parsing_String_Basic from "../Parsing.String.Basic/index.js";
import * as Parsing_Token from "../Parsing.Token/index.js";

//---------------------------------------------------------
// minimal language definition
//------------------------------------------------------
// | This is the most minimal token definition. It is recommended to use
// | this definition as the basis for other definitions. `emptyDef` has
// | no reserved names or operators, is case sensitive and doesn't accept
// | comments, identifiers or operators.
var emptyDef = /* #__PURE__ */ (function () {
    var op$prime = Parsing_String_Basic.oneOf([ ":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "\\", "^", "|", "-", "~" ]);
    return {
        commentStart: "",
        commentEnd: "",
        commentLine: "",
        nestedComments: true,
        identStart: Control_Alt.alt(Parsing.altParserT)(Parsing_String_Basic.letter)(Parsing_String["char"]("_")),
        identLetter: Control_Alt.alt(Parsing.altParserT)(Parsing_String_Basic.alphaNum)(Parsing_String_Basic.oneOf([ "_", "'" ])),
        opStart: op$prime,
        opLetter: op$prime,
        reservedOpNames: [  ],
        reservedNames: [  ],
        caseSensitive: true
    };
})();

//---------------------------------------------------------
// Styles: haskellStyle, javaStyle
//---------------------------------------------------------
// | This is a minimal token definition for Haskell style languages. It
// | defines the style of comments, valid identifiers and case
// | sensitivity. It does not define any reserved words or operators.
var haskellStyle = /* #__PURE__ */ (function () {
    var op$prime = Parsing_String_Basic.oneOf([ ":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "\\", "^", "|", "-", "~" ]);
    var v = Parsing_Token.unGenLanguageDef(emptyDef);
    return {
        commentStart: "{-",
        commentEnd: "-}",
        commentLine: "--",
        nestedComments: true,
        identStart: Parsing_String_Basic.letter,
        identLetter: Control_Alt.alt(Parsing.altParserT)(Parsing_String_Basic.alphaNum)(Parsing_String_Basic.oneOf([ "_", "'" ])),
        opStart: op$prime,
        opLetter: op$prime,
        reservedNames: [  ],
        reservedOpNames: [  ],
        caseSensitive: true
    };
})();

// | The language definition for the language Haskell98.
var haskell98Def = /* #__PURE__ */ (function () {
    var v = Parsing_Token.unGenLanguageDef(haskellStyle);
    return {
        commentStart: v.commentStart,
        commentEnd: v.commentEnd,
        commentLine: v.commentLine,
        nestedComments: v.nestedComments,
        identStart: v.identStart,
        identLetter: v.identLetter,
        opStart: v.opStart,
        opLetter: v.opLetter,
        reservedNames: [ "let", "in", "case", "of", "if", "then", "else", "data", "type", "class", "default", "deriving", "do", "import", "infix", "infixl", "infixr", "instance", "module", "newtype", "where", "primitive" ],
        reservedOpNames: [ "::", "..", "=", "\\", "|", "<-", "->", "@", "~", "=>" ],
        caseSensitive: v.caseSensitive
    };
})();

// | The language definition for the Haskell language.
var haskellDef = /* #__PURE__ */ (function () {
    return {
        commentStart: haskell98Def.commentStart,
        commentEnd: haskell98Def.commentEnd,
        commentLine: haskell98Def.commentLine,
        nestedComments: haskell98Def.nestedComments,
        identStart: haskell98Def.identStart,
        identLetter: Control_Alt.alt(Parsing.altParserT)(haskell98Def.identLetter)(Parsing_String["char"]("#")),
        opStart: haskell98Def.opStart,
        opLetter: haskell98Def.opLetter,
        reservedNames: Data_Semigroup.append(Data_Semigroup.semigroupArray)(haskell98Def.reservedNames)([ "foreign", "import", "export", "primitive", "_ccall_", "_casm_", "forall" ]),
        reservedOpNames: haskell98Def.reservedOpNames,
        caseSensitive: haskell98Def.caseSensitive
    };
})();

// -----------------------------------------------------------
// -- Haskell
// -----------------------------------------------------------
// | A lexer for the haskell language.
var haskell = /* #__PURE__ */ Parsing_Token.makeTokenParser(haskellDef);

// | This is a minimal token definition for Java style languages. It
// | defines the style of comments, valid identifiers and case
// | sensitivity. It does not define any reserved words or operators.
var javaStyle = /* #__PURE__ */ (function () {
    var v = Parsing_Token.unGenLanguageDef(emptyDef);
    return {
        commentStart: "/*",
        commentEnd: "*/",
        commentLine: "//",
        nestedComments: true,
        identStart: Control_Alt.alt(Parsing.altParserT)(Parsing_String_Basic.letter)(Parsing_String_Basic.oneOf([ "_", "$" ])),
        identLetter: Control_Alt.alt(Parsing.altParserT)(Parsing_String_Basic.alphaNum)(Parsing_String_Basic.oneOf([ "_", "$" ])),
        opStart: v.opStart,
        opLetter: v.opLetter,
        reservedNames: [  ],
        reservedOpNames: [  ],
        caseSensitive: false
    };
})();
export {
    haskellDef,
    haskell,
    emptyDef,
    haskellStyle,
    javaStyle
};
