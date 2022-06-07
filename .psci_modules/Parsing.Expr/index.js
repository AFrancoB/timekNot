// | This module is a port of the Haskell
// | [__Text.Parsec.Expr__](https://hackage.haskell.org/package/parsec/docs/Text-Parsec-Expr.html)
// | module.
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Category from "../Control.Category/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
var AssocNone = /* #__PURE__ */ (function () {
    function AssocNone() {

    };
    AssocNone.value = new AssocNone();
    return AssocNone;
})();
var AssocLeft = /* #__PURE__ */ (function () {
    function AssocLeft() {

    };
    AssocLeft.value = new AssocLeft();
    return AssocLeft;
})();
var AssocRight = /* #__PURE__ */ (function () {
    function AssocRight() {

    };
    AssocRight.value = new AssocRight();
    return AssocRight;
})();
var Infix = /* #__PURE__ */ (function () {
    function Infix(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Infix.create = function (value0) {
        return function (value1) {
            return new Infix(value0, value1);
        };
    };
    return Infix;
})();
var Prefix = /* #__PURE__ */ (function () {
    function Prefix(value0) {
        this.value0 = value0;
    };
    Prefix.create = function (value0) {
        return new Prefix(value0);
    };
    return Prefix;
})();
var Postfix = /* #__PURE__ */ (function () {
    function Postfix(value0) {
        this.value0 = value0;
    };
    Postfix.create = function (value0) {
        return new Postfix(value0);
    };
    return Postfix;
})();
var termP = function (prefixP) {
    return function (term) {
        return function (postfixP) {
            return Control_Bind.bind(Parsing.bindParserT)(prefixP)(function (pre) {
                return Control_Bind.bind(Parsing.bindParserT)(term)(function (x) {
                    return Control_Bind.bind(Parsing.bindParserT)(postfixP)(function (post) {
                        return Control_Applicative.pure(Parsing.applicativeParserT)(post(pre(x)));
                    });
                });
            });
        };
    };
};
var splitOp = function (v) {
    return function (accum) {
        if (v instanceof Infix && v.value1 instanceof AssocNone) {
            return {
                rassoc: accum.rassoc,
                lassoc: accum.lassoc,
                nassoc: new Data_List_Types.Cons(v.value0, accum.nassoc),
                prefix: accum.prefix,
                postfix: accum.postfix
            };
        };
        if (v instanceof Infix && v.value1 instanceof AssocLeft) {
            return {
                rassoc: accum.rassoc,
                lassoc: new Data_List_Types.Cons(v.value0, accum.lassoc),
                nassoc: accum.nassoc,
                prefix: accum.prefix,
                postfix: accum.postfix
            };
        };
        if (v instanceof Infix && v.value1 instanceof AssocRight) {
            return {
                rassoc: new Data_List_Types.Cons(v.value0, accum.rassoc),
                lassoc: accum.lassoc,
                nassoc: accum.nassoc,
                prefix: accum.prefix,
                postfix: accum.postfix
            };
        };
        if (v instanceof Prefix) {
            return {
                rassoc: accum.rassoc,
                lassoc: accum.lassoc,
                nassoc: accum.nassoc,
                prefix: new Data_List_Types.Cons(v.value0, accum.prefix),
                postfix: accum.postfix
            };
        };
        if (v instanceof Postfix) {
            return {
                rassoc: accum.rassoc,
                lassoc: accum.lassoc,
                nassoc: accum.nassoc,
                prefix: accum.prefix,
                postfix: new Data_List_Types.Cons(v.value0, accum.postfix)
            };
        };
        throw new Error("Failed pattern match at Parsing.Expr (line 78, column 1 - line 78, column 80): " + [ v.constructor.name, accum.constructor.name ]);
    };
};
var rassocP1 = function (x) {
    return function (rassocOp) {
        return function (prefixP) {
            return function (term) {
                return function (postfixP) {
                    return Control_Alt.alt(Parsing.altParserT)(rassocP(x)(rassocOp)(prefixP)(term)(postfixP))(Control_Applicative.pure(Parsing.applicativeParserT)(x));
                };
            };
        };
    };
};
var rassocP = function (x) {
    return function (rassocOp) {
        return function (prefixP) {
            return function (term) {
                return function (postfixP) {
                    return Control_Bind.bind(Parsing.bindParserT)(rassocOp)(function (f) {
                        return Control_Bind.bind(Parsing.bindParserT)(Control_Bind.bind(Parsing.bindParserT)(termP(prefixP)(term)(postfixP))(function (z) {
                            return rassocP1(z)(rassocOp)(prefixP)(term)(postfixP);
                        }))(function (y) {
                            return Control_Applicative.pure(Parsing.applicativeParserT)(f(x)(y));
                        });
                    });
                };
            };
        };
    };
};
var nassocP = function (x) {
    return function (nassocOp) {
        return function (prefixP) {
            return function (term) {
                return function (postfixP) {
                    return Control_Bind.bind(Parsing.bindParserT)(nassocOp)(function (f) {
                        return Control_Bind.bind(Parsing.bindParserT)(termP(prefixP)(term)(postfixP))(function (y) {
                            return Control_Applicative.pure(Parsing.applicativeParserT)(f(x)(y));
                        });
                    });
                };
            };
        };
    };
};
var lassocP1 = function (x) {
    return function (lassocOp) {
        return function (prefixP) {
            return function (term) {
                return function (postfixP) {
                    return Control_Alt.alt(Parsing.altParserT)(lassocP(x)(lassocOp)(prefixP)(term)(postfixP))(Control_Applicative.pure(Parsing.applicativeParserT)(x));
                };
            };
        };
    };
};
var lassocP = function (x) {
    return function (lassocOp) {
        return function (prefixP) {
            return function (term) {
                return function (postfixP) {
                    return Control_Bind.bind(Parsing.bindParserT)(lassocOp)(function (f) {
                        return Control_Bind.bind(Parsing.bindParserT)(termP(prefixP)(term)(postfixP))(function (y) {
                            return lassocP1(f(x)(y))(lassocOp)(prefixP)(term)(postfixP);
                        });
                    });
                };
            };
        };
    };
};
var makeParser = function (term) {
    return function (ops) {
        var accum = Data_Foldable.foldr(Data_Foldable.foldableArray)(splitOp)({
            rassoc: Data_List_Types.Nil.value,
            lassoc: Data_List_Types.Nil.value,
            nassoc: Data_List_Types.Nil.value,
            prefix: Data_List_Types.Nil.value,
            postfix: Data_List_Types.Nil.value
        })(ops);
        var lassocOp = Parsing_Combinators.choice(Data_List_Types.foldableList)(accum.lassoc);
        var nassocOp = Parsing_Combinators.choice(Data_List_Types.foldableList)(accum.nassoc);
        var postfixOp = Parsing_Combinators.withErrorMessage(Parsing_Combinators.choice(Data_List_Types.foldableList)(accum.postfix))("");
        var postfixP = Control_Alt.alt(Parsing.altParserT)(postfixOp)(Control_Applicative.pure(Parsing.applicativeParserT)(Control_Category.identity(Control_Category.categoryFn)));
        var prefixOp = Parsing_Combinators.withErrorMessage(Parsing_Combinators.choice(Data_List_Types.foldableList)(accum.prefix))("");
        var prefixP = Control_Alt.alt(Parsing.altParserT)(prefixOp)(Control_Applicative.pure(Parsing.applicativeParserT)(Control_Category.identity(Control_Category.categoryFn)));
        var rassocOp = Parsing_Combinators.choice(Data_List_Types.foldableList)(accum.rassoc);
        return Control_Bind.bind(Parsing.bindParserT)(termP(prefixP)(term)(postfixP))(function (x) {
            return Control_Alt.alt(Parsing.altParserT)(rassocP(x)(rassocOp)(prefixP)(term)(postfixP))(Control_Alt.alt(Parsing.altParserT)(lassocP(x)(lassocOp)(prefixP)(term)(postfixP))(Control_Alt.alt(Parsing.altParserT)(nassocP(x)(nassocOp)(prefixP)(term)(postfixP))(Parsing_Combinators.withErrorMessage(Control_Applicative.pure(Parsing.applicativeParserT)(x))("operator"))));
        });
    };
};

// | Build a parser from an `OperatorTable`.
// |
// | For example:
// |
// | ```purescript
// | buildExprParser [ [ Infix (string "/" $> div) AssocRight ]
// |                 , [ Infix (string "*" $> mul) AssocRight ]
// |                 , [ Infix (string "-" $> sub) AssocRight ]
// |                 , [ Infix (string "+" $> add) AssocRight ]
// |                 ] digit
// | ```
var buildExprParser = function (operators) {
    return function (simpleExpr) {
        return Data_Foldable.foldl(Data_Foldable.foldableArray)(makeParser)(simpleExpr)(operators);
    };
};
export {
    AssocNone,
    AssocLeft,
    AssocRight,
    Infix,
    Prefix,
    Postfix,
    buildExprParser
};
