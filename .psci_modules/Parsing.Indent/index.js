// | This module is a port of the Haskell
// | [__Text.Parsec.Indent__](https://hackage.haskell.org/package/indents-0.3.3/docs/Text-Parsec-Indent.html)
// | module from 2016-05-07.
// |
// | A module to construct indentation aware parsers. Many programming
// | language have indentation based syntax rules e.g. python and Haskell.
// | This module exports combinators to create such parsers.
// |
// | The input source can be thought of as a list of tokens. Abstractly
// | each token occurs at a line and a column and has a width. The column
// | number of a token measures is indentation. If t1 and t2 are two tokens
// | then we say that indentation of t1 is more than t2 if the column
// | number of occurrence of t1 is greater than that of t2.
// |
// | Currently this module supports two kind of indentation based syntactic
// | structures which we now describe:
// |
// | - **Block**
// |
// |   A block of indentation /c/ is a sequence of tokens with
// |   indentation at least /c/.  Examples for a block is a where clause of
// |   Haskell with no explicit braces.
// |
// | - **Line fold**
// |
// |   A line fold starting at line /l/ and indentation /c/ is a
// |   sequence of tokens that start at line /l/ and possibly continue to
// |   subsequent lines as long as the indentation is greater than /c/. Such
// |   a sequence of lines need to be /folded/ to a single line. An example
// |   is MIME headers. Line folding based binding separation is used in
// |   Haskell as well.
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Control_Monad from "../Control.Monad/index.js";
import * as Control_Monad_State from "../Control.Monad.State/index.js";
import * as Control_Monad_State_Class from "../Control.Monad.State.Class/index.js";
import * as Control_Monad_State_Trans from "../Control.Monad.State.Trans/index.js";
import * as Control_Monad_Trans_Class from "../Control.Monad.Trans.Class/index.js";
import * as Data_Function from "../Data.Function/index.js";
import * as Data_Identity from "../Data.Identity/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as Parsing_String_Basic from "../Parsing.String.Basic/index.js";

// | Data type used to optional parsing
var Opt = /* #__PURE__ */ (function () {
    function Opt(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Opt.create = function (value0) {
        return function (value1) {
            return new Opt(value0, value1);
        };
    };
    return Opt;
})();
var symbol = function (name) {
    return Control_Apply.applySecond(Parsing.applyParserT)(Data_List.many(Parsing.alternativeParserT)(Parsing.lazyParserT)(Parsing_String_Basic.oneOf([ " ", "\x09" ])))(Parsing_String.string(name));
};

// | Run the result of an indentation sensitive parse
var runIndent = /* #__PURE__ */ Data_Function.flip(Control_Monad_State.evalState)(Parsing.initialPos);

// | simple helper function to avoid typ-problems with MonadState instance
var put$prime = function (p) {
    return Control_Monad_Trans_Class.lift(Parsing.monadTransParserT)(Control_Monad_State_Trans.monadStateT(Data_Identity.monadIdentity))(Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(p));
};
var many1 = function (p) {
    return Control_Apply.lift2(Parsing.applyParserT)(Data_List_Types.Cons.create)(p)(Data_List.many(Parsing.alternativeParserT)(Parsing.lazyParserT)(p));
};

// | simple helper function to avoid typ-problems with MonadState instance
var get$prime = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Control_Monad_Trans_Class.lift(Parsing.monadTransParserT)(/* #__PURE__ */ Control_Monad_State_Trans.monadStateT(Data_Identity.monadIdentity))(/* #__PURE__ */ Control_Monad_State_Class.get(/* #__PURE__ */ Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))))(function (g) {
    return Control_Applicative.pure(Parsing.applicativeParserT)(g);
});

// | Parses only when indented past the level of the reference
var indented = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(Parsing.position)(function (v) {
    return Control_Bind.bind(Parsing.bindParserT)(get$prime)(function (v1) {
        var $11 = v.column <= v1.column;
        if ($11) {
            return Parsing.fail("not indented");
        };
        return put$prime({
            index: 0,
            line: v.line,
            column: v1.column
        });
    });
});

// | Same as `indented`, but does not change internal state
var indented$prime = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(Parsing.position)(function (v) {
    return Control_Bind.bind(Parsing.bindParserT)(get$prime)(function (v1) {
        var $14 = v.column <= v1.column;
        if ($14) {
            return Parsing.fail("not indented");
        };
        return Control_Applicative.pure(Parsing.applicativeParserT)(Data_Unit.unit);
    });
});

// | Parses only on the same line as the reference
var sameLine = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(Parsing.position)(function (v) {
    return Control_Bind.bind(Parsing.bindParserT)(get$prime)(function (v1) {
        var $17 = v.line === v1.line;
        if ($17) {
            return Control_Applicative.pure(Parsing.applicativeParserT)(Data_Unit.unit);
        };
        return Parsing.fail("over one line");
    });
});

// | Parses only when indented past the level of the reference or on the same line
var sameOrIndented = /* #__PURE__ */ Control_Alt.alt(Parsing.altParserT)(sameLine)(indented);

// | `<+/>` is to indentation sensitive parsers what `ap` is to monads
var indentAp = function (a) {
    return function (b) {
        return Control_Monad.ap(Parsing.monadParserT)(a)(Control_Apply.applySecond(Parsing.applyParserT)(sameOrIndented)(b));
    };
};

// | Like `<+/>` but applies the second parser many times
var indentMany = function (a) {
    return function (b) {
        return Control_Monad.ap(Parsing.monadParserT)(a)(Data_List.many(Parsing.alternativeParserT)(Parsing.lazyParserT)(Control_Apply.applySecond(Parsing.applyParserT)(sameOrIndented)(b)));
    };
};

// | Like `<+/>` but doesn't apply the function to the parsed value
var indentNoAp = function (a) {
    return function (b) {
        return Control_Apply.lift2(Parsing.applyParserT)(Data_Function["const"])(a)(Control_Apply.applySecond(Parsing.applyParserT)(sameOrIndented)(b));
    };
};

// | Like `<+/>` but applies the second parser optionally using the `Optional` datatype
var indentOp = function (a) {
    return function (v) {
        return Control_Monad.ap(Parsing.monadParserT)(a)(Parsing_Combinators.option(v.value0)(Control_Apply.applySecond(Parsing.applyParserT)(sameOrIndented)(v.value1)));
    };
};

// | Parses using the current location for indentation reference
var withPos = function (x) {
    return Control_Bind.bind(Parsing.bindParserT)(get$prime)(function (a) {
        return Control_Bind.bind(Parsing.bindParserT)(Parsing.position)(function (p) {
            return Control_Bind.bind(Parsing.bindParserT)(Control_Apply.applySecond(Parsing.applyParserT)(put$prime(p))(x))(function (r) {
                return Control_Apply.applySecond(Parsing.applyParserT)(put$prime(a))(Control_Applicative.pure(Parsing.applicativeParserT)(r));
            });
        });
    });
};

// | Parses with surrounding angle brackets
var indentAngles = function (p) {
    return withPos(indentAp(indentNoAp(Control_Applicative.pure(Parsing.applicativeParserT)(Control_Category.identity(Control_Category.categoryFn)))(symbol("<")))(indentNoAp(p)(symbol(">"))));
};

// | Parses with surrounding braces
var indentBraces = function (p) {
    return withPos(indentAp(indentNoAp(Control_Applicative.pure(Parsing.applicativeParserT)(Control_Category.identity(Control_Category.categoryFn)))(symbol("{")))(indentNoAp(p)(symbol("}"))));
};

// | Parses with surrounding brackets
var indentBrackets = function (p) {
    return withPos(indentAp(indentNoAp(Control_Applicative.pure(Parsing.applicativeParserT)(Control_Category.identity(Control_Category.categoryFn)))(symbol("[")))(indentNoAp(p)(symbol("]"))));
};

// | Parses with surrounding parentheses
var indentParens = function (p) {
    return withPos(indentAp(indentNoAp(Control_Applicative.pure(Parsing.applicativeParserT)(Control_Category.identity(Control_Category.categoryFn)))(symbol("(")))(indentNoAp(p)(symbol(")"))));
};

// | Ensures the current indentation level matches that of the reference
var checkIndent = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(Parsing.position)(function (v) {
    return Control_Bind.bind(Parsing.bindParserT)(get$prime)(function (v1) {
        var $24 = v.column === v1.column;
        if ($24) {
            return Control_Applicative.pure(Parsing.applicativeParserT)(Data_Unit.unit);
        };
        return Parsing.fail("indentation doesn't match");
    });
});

// | Parses a block of lines at the same indentation level
var block1 = function (p) {
    return withPos(Control_Bind.bind(Parsing.bindParserT)(many1(Control_Apply.applySecond(Parsing.applyParserT)(checkIndent)(p)))(function (r) {
        return Control_Applicative.pure(Parsing.applicativeParserT)(r);
    }));
};

// | Parses a block of lines at the same indentation level , empty Blocks allowed
var block = function (p) {
    return withPos(Control_Bind.bind(Parsing.bindParserT)(Data_List.many(Parsing.alternativeParserT)(Parsing.lazyParserT)(Control_Apply.applySecond(Parsing.applyParserT)(checkIndent)(p)))(function (r) {
        return Control_Applicative.pure(Parsing.applicativeParserT)(r);
    }));
};

// | `withBlock f a p` parses `a`
// | followed by an indented block of `p`
// | combining them with `f`.
var withBlock = function (f) {
    return function (a) {
        return function (p) {
            return withPos(Control_Bind.bind(Parsing.bindParserT)(a)(function (r1) {
                return Control_Bind.bind(Parsing.bindParserT)(Parsing_Combinators.optionMaybe(Control_Apply.applySecond(Parsing.applyParserT)(indented)(block(p))))(function (r) {
                    if (r instanceof Data_Maybe.Nothing) {
                        return Control_Applicative.pure(Parsing.applicativeParserT)(f(r1)(Data_List_Types.Nil.value));
                    };
                    if (r instanceof Data_Maybe.Just) {
                        return Control_Applicative.pure(Parsing.applicativeParserT)(f(r1)(r.value0));
                    };
                    throw new Error("Failed pattern match at Parsing.Indent (line 101, column 3 - line 103, column 30): " + [ r.constructor.name ]);
                });
            }));
        };
    };
};

// | Like 'withBlock', but throws away initial parse result
var withBlock$prime = /* #__PURE__ */ withBlock(/* #__PURE__ */ Data_Function.flip(Data_Function["const"]));
export {
    runIndent,
    withBlock,
    withBlock$prime,
    block,
    block1,
    indented,
    indented$prime,
    sameLine,
    sameOrIndented,
    checkIndent,
    withPos,
    indentAp,
    indentNoAp,
    indentMany,
    indentOp,
    indentBrackets,
    indentAngles,
    indentBraces,
    indentParens,
    Opt
};
