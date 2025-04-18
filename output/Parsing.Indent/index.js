// Generated by purs version 0.15.15
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
var applySecond = /* #__PURE__ */ Control_Apply.applySecond(Parsing.applyParserT);
var many = /* #__PURE__ */ Data_List.many(Parsing.alternativeParserT)(Parsing.lazyParserT);
var lift = /* #__PURE__ */ Control_Monad_Trans_Class.lift(Parsing.monadTransParserT)(/* #__PURE__ */ Control_Monad_State_Trans.monadStateT(Data_Identity.monadIdentity));
var monadStateStateT = /* #__PURE__ */ Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity);
var put = /* #__PURE__ */ Control_Monad_State_Class.put(monadStateStateT);
var lift2 = /* #__PURE__ */ Control_Apply.lift2(Parsing.applyParserT);
var bind = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT);
var pure = /* #__PURE__ */ Control_Applicative.pure(Parsing.applicativeParserT);
var ap = /* #__PURE__ */ Control_Monad.ap(Parsing.monadParserT);
var identity = /* #__PURE__ */ Control_Category.identity(Control_Category.categoryFn);
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
    return applySecond(many(Parsing_String_Basic.oneOf([ " ", "\x09" ])))(Parsing_String.string(name));
};
var runIndent = /* #__PURE__ */ Data_Function.flip(Control_Monad_State.evalState)(Parsing.initialPos);
var put$prime = function (p) {
    return lift(put(p));
};
var many1 = function (p) {
    return lift2(Data_List_Types.Cons.create)(p)(many(p));
};
var get$prime = /* #__PURE__ */ bind(/* #__PURE__ */ lift(/* #__PURE__ */ Control_Monad_State_Class.get(monadStateStateT)))(function (g) {
    return pure(g);
});
var indented = /* #__PURE__ */ bind(Parsing.position)(function (v) {
    return bind(get$prime)(function (v1) {
        var $28 = v.column <= v1.column;
        if ($28) {
            return Parsing.fail("not indented");
        };
        return put$prime({
            index: 0,
            line: v.line,
            column: v1.column
        });
    });
});
var indented$prime = /* #__PURE__ */ bind(Parsing.position)(function (v) {
    return bind(get$prime)(function (v1) {
        var $31 = v.column <= v1.column;
        if ($31) {
            return Parsing.fail("not indented");
        };
        return pure(Data_Unit.unit);
    });
});
var sameLine = /* #__PURE__ */ bind(Parsing.position)(function (v) {
    return bind(get$prime)(function (v1) {
        var $34 = v.line === v1.line;
        if ($34) {
            return pure(Data_Unit.unit);
        };
        return Parsing.fail("over one line");
    });
});
var sameOrIndented = /* #__PURE__ */ Control_Alt.alt(Parsing.altParserT)(sameLine)(indented);
var indentAp = function (a) {
    return function (b) {
        return ap(a)(applySecond(sameOrIndented)(b));
    };
};
var indentMany = function (a) {
    return function (b) {
        return ap(a)(many(applySecond(sameOrIndented)(b)));
    };
};
var indentNoAp = function (a) {
    return function (b) {
        return lift2(Data_Function["const"])(a)(applySecond(sameOrIndented)(b));
    };
};
var indentOp = function (a) {
    return function (v) {
        return ap(a)(Parsing_Combinators.option(v.value0)(applySecond(sameOrIndented)(v.value1)));
    };
};
var withPos = function (x) {
    return bind(get$prime)(function (a) {
        return bind(Parsing.position)(function (p) {
            return bind(applySecond(put$prime(p))(x))(function (r) {
                return applySecond(put$prime(a))(pure(r));
            });
        });
    });
};
var indentAngles = function (p) {
    return withPos(indentAp(indentNoAp(pure(identity))(symbol("<")))(indentNoAp(p)(symbol(">"))));
};
var indentBraces = function (p) {
    return withPos(indentAp(indentNoAp(pure(identity))(symbol("{")))(indentNoAp(p)(symbol("}"))));
};
var indentBrackets = function (p) {
    return withPos(indentAp(indentNoAp(pure(identity))(symbol("[")))(indentNoAp(p)(symbol("]"))));
};
var indentParens = function (p) {
    return withPos(indentAp(indentNoAp(pure(identity))(symbol("(")))(indentNoAp(p)(symbol(")"))));
};
var checkIndent = /* #__PURE__ */ bind(Parsing.position)(function (v) {
    return bind(get$prime)(function (v1) {
        var $41 = v.column === v1.column;
        if ($41) {
            return pure(Data_Unit.unit);
        };
        return Parsing.fail("indentation doesn't match");
    });
});
var block1 = function (p) {
    return withPos(bind(many1(applySecond(checkIndent)(p)))(function (r) {
        return pure(r);
    }));
};
var block = function (p) {
    return withPos(bind(many(applySecond(checkIndent)(p)))(function (r) {
        return pure(r);
    }));
};
var withBlock = function (f) {
    return function (a) {
        return function (p) {
            return withPos(bind(a)(function (r1) {
                return bind(Parsing_Combinators.optionMaybe(applySecond(indented)(block(p))))(function (r) {
                    if (r instanceof Data_Maybe.Nothing) {
                        return pure(f(r1)(Data_List_Types.Nil.value));
                    };
                    if (r instanceof Data_Maybe.Just) {
                        return pure(f(r1)(r.value0));
                    };
                    throw new Error("Failed pattern match at Parsing.Indent (line 101, column 3 - line 103, column 30): " + [ r.constructor.name ]);
                });
            }));
        };
    };
};
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
