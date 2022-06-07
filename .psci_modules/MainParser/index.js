import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Foldable from "../Data.Foldable/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_Language from "../Parsing.Language/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as Parsing_Token from "../Parsing.Token/index.js";
var Proportion = /* #__PURE__ */ (function () {
    function Proportion(value0) {
        this.value0 = value0;
    };
    Proportion.create = function (value0) {
        return new Proportion(value0);
    };
    return Proportion;
})();
var BPM = /* #__PURE__ */ (function () {
    function BPM(value0) {
        this.value0 = value0;
    };
    BPM.create = function (value0) {
        return new BPM(value0);
    };
    return BPM;
})();
var CPS = /* #__PURE__ */ (function () {
    function CPS(value0) {
        this.value0 = value0;
    };
    CPS.create = function (value0) {
        return new CPS(value0);
    };
    return CPS;
})();
var Passage = /* #__PURE__ */ (function () {
    function Passage() {

    };
    Passage.value = new Passage();
    return Passage;
})();
var Origin = /* #__PURE__ */ (function () {
    function Origin() {

    };
    Origin.value = new Origin();
    return Origin;
})();
var Eval = /* #__PURE__ */ (function () {
    function Eval() {

    };
    Eval.value = new Eval();
    return Eval;
})();

//  show (Metric x y) = "metric"
// converge needs two points of contact the self and above, so Tuplet Self Above
var Diverge = /* #__PURE__ */ (function () {
    function Diverge() {

    };
    Diverge.value = new Diverge();
    return Diverge;
})();

//  show (Metric x y) = "metric"
// converge needs two points of contact the self and above, so Tuplet Self Above
var ConvergeFromOrigin = /* #__PURE__ */ (function () {
    function ConvergeFromOrigin(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ConvergeFromOrigin.create = function (value0) {
        return function (value1) {
            return new ConvergeFromOrigin(value0, value1);
        };
    };
    return ConvergeFromOrigin;
})();

//  show (Metric x y) = "metric"
// converge needs two points of contact the self and above, so Tuplet Self Above
var ConvergeFromEval = /* #__PURE__ */ (function () {
    function ConvergeFromEval(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ConvergeFromEval.create = function (value0) {
        return function (value1) {
            return new ConvergeFromEval(value0, value1);
        };
    };
    return ConvergeFromEval;
})();
var Polytemporal = /* #__PURE__ */ (function () {
    function Polytemporal(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    Polytemporal.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new Polytemporal(value0, value1, value2);
            };
        };
    };
    return Polytemporal;
})();

// bpm (120,1/4) cps [2,1,3,2.2]
// ratios/bpms/[cps] -- converge Double// eval// origin -- quant
// canonise (ratios [1,2,3,4]) $ origin $ converge 20 
// este programa tiene 4 voces todas contando desde un origen comun (que es cuando la proporcion 1 esta en el instante 0) y convergen (o pudieron haber convergido) en el evento 20
// "xxox oxx[xx] ox(3,8) !x#4" $ dur (reciprocal [2,4,8,8])//(len 12)//(len [12,6,3,1.5])//structure 12 (reciprocal [2,4,8,8]) $ inf/fin $ cp 25
// onset notation: -- dur substitutes onset notation if no onset given
// xo ---- recursive!! -- repeat notation
// euclidean
// if no dur or len given, it is one beat of the tempo, or one whole cycle
// structure recieves a length (12 beats of the tempo unit or 12 cycles) and a subdivision
// chord [0,4,7] (seq "xoo")
// arpeggio [0,4,7] 0.25 (seq' "xooo")
// razgado [0,4,7] (0.05,(-0.1)) (bjork "xooxoxo")
// heterophony [0,4,7] [0,1,1] ("ooxoooxo")
// event (func blabla) "xooxo"
// atIndex 4 (func blabla)
// atIndexMod 4 (func blabla) "xoooxoo"
// funcs: note, speed, vowel, lpf, hpf, begin, end, crush, 
// up here tengo q separar el canonic y el [passage] 
// aki va Program Canonic (Array Passage)
var Program = /* #__PURE__ */ (function () {
    function Program(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Program.create = function (value0) {
        return function (value1) {
            return new Program(value0, value1);
        };
    };
    return Program;
})();
var voicingShowInstance = {
    show: function (v) {
        if (v instanceof Proportion) {
            return Data_Show.show(Data_Show.showNumber)(v.value0);
        };
        if (v instanceof BPM) {
            return Data_Show.show(Data_Tuple.showTuple(Data_Show.showNumber)(Data_Show.showNumber))(v.value0);
        };
        if (v instanceof CPS) {
            return Data_Show.show(Data_Show.showNumber)(v.value0);
        };
        throw new Error("Failed pattern match at MainParser (line 87, column 1 - line 90, column 24): " + [ v.constructor.name ]);
    }
};

// toRat:: (Either Int Number) -> Rational
// toRat (Left x) = fromInt x
// toRat (Right x) = Ratio x
// GenLanguageDef :: Type -> (Type -> Type) -> Type
// LanguageDef { caseSensitive :: Boolean, commentEnd :: String, commentLine :: String, commentStart :: String, identLetter :: ParserT s m Char, identStart :: ParserT s m Char, nestedComments :: Boolean, opLetter :: ParserT s m Char, opStart :: ParserT s m Char, reservedNames :: Array String, reservedOpNames :: Array String }
var tokenParser = /* #__PURE__ */ Parsing_Token.makeTokenParser(Parsing_Language.haskellStyle);
var whitespace = /* #__PURE__ */ (function () {
    return tokenParser.whiteSpace;
})();
var semi = /* #__PURE__ */ (function () {
    return tokenParser.semi;
})();
var reserved = /* #__PURE__ */ (function () {
    return tokenParser.reserved;
})();
var passageShowInstance = {
    show: function (v) {
        return "passage";
    }
};
var parseOrigin = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Control_Alt.alt(Parsing.altParserT)(/* #__PURE__ */ Parsing_String.string("origin"))(/* #__PURE__ */ Parsing_String.string("o")))(function () {
    return Control_Applicative.pure(Parsing.applicativeParserT)(Origin.value);
});
var parseEval = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Control_Alt.alt(Parsing.altParserT)(/* #__PURE__ */ Parsing_String.string("eval"))(/* #__PURE__ */ Parsing_String.string("e")))(function () {
    return Control_Applicative.pure(Parsing.applicativeParserT)(Eval.value);
});
var parseDiv = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Control_Alt.alt(Parsing.altParserT)(/* #__PURE__ */ Parsing_String.string("diverge"))(/* #__PURE__ */ Parsing_String.string("d")))(function () {
    return Control_Applicative.pure(Parsing.applicativeParserT)(Diverge.value);
});
var parens = /* #__PURE__ */ (function () {
    return tokenParser.parens;
})();
var noseShowInstance = {
    show: function (v) {
        if (v instanceof Origin) {
            return "origin";
        };
        if (v instanceof Eval) {
            return "eval";
        };
        throw new Error("Failed pattern match at MainParser (line 94, column 1 - line 96, column 21): " + [ v.constructor.name ]);
    }
};
var nfToNum = function (v) {
    if (v instanceof Data_Either.Left) {
        return Data_Int.toNumber(v.value0);
    };
    if (v instanceof Data_Either.Right) {
        return v.value0;
    };
    throw new Error("Failed pattern match at MainParser (line 272, column 1 - line 272, column 40): " + [ v.constructor.name ]);
};
var naturalOrFloat = /* #__PURE__ */ (function () {
    return tokenParser.naturalOrFloat;
})();
var parseCPSMark = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(naturalOrFloat)(function (x) {
    return Control_Applicative.pure(Parsing.applicativeParserT)(nfToNum(x));
});
var parseConvE = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Control_Alt.alt(Parsing.altParserT)(/* #__PURE__ */ Parsing_String.string("convergeFromEval"))(/* #__PURE__ */ Parsing_String.string("c-e")))(function () {
    return Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT)(whitespace)(function () {
        return Control_Bind.bind(Parsing.bindParserT)(naturalOrFloat)(function (x) {
            return Control_Bind.bind(Parsing.bindParserT)(naturalOrFloat)(function (y) {
                return Control_Applicative.pure(Parsing.applicativeParserT)(new ConvergeFromEval(nfToNum(x), nfToNum(y)));
            });
        });
    });
});
var parseConvO = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Control_Alt.alt(Parsing.altParserT)(/* #__PURE__ */ Parsing_String.string("convergeFromOrigin"))(/* #__PURE__ */ Parsing_String.string("c-o")))(function () {
    return Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT)(whitespace)(function () {
        return Control_Bind.bind(Parsing.bindParserT)(naturalOrFloat)(function (x) {
            return Control_Bind.bind(Parsing.bindParserT)(naturalOrFloat)(function (y) {
                return Control_Applicative.pure(Parsing.applicativeParserT)(new ConvergeFromOrigin(nfToNum(x), nfToNum(y)));
            });
        });
    });
});
var parseSingleCPS = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(naturalOrFloat)(function (x) {
    return Control_Bind.bind(Parsing.bindParserT)(Control_Alt.alt(Parsing.altParserT)(Parsing_String.string("cycle"))(Control_Alt.alt(Parsing.altParserT)(Parsing_String.string("cps"))(Parsing_String.string("c"))))(function () {
        return Control_Applicative.pure(Parsing.applicativeParserT)(new CPS(nfToNum(x)));
    });
});
var parseSingleProp = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(naturalOrFloat)(function (x) {
    return Control_Bind.bind(Parsing.bindParserT)(Control_Alt.alt(Parsing.altParserT)(Parsing_String.string("proportion"))(Control_Alt.alt(Parsing.altParserT)(Parsing_String.string("prop"))(Parsing_String.string("p"))))(function () {
        return Control_Applicative.pure(Parsing.applicativeParserT)(new Proportion(nfToNum(x)));
    });
});
var parseSingleTempo = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(naturalOrFloat)(function (x) {
    return Control_Bind.bind(Parsing.bindParserT)(naturalOrFloat)(function (y) {
        return Control_Bind.bind(Parsing.bindParserT)(Control_Alt.alt(Parsing.altParserT)(Parsing_String.string("tempo"))(Control_Alt.alt(Parsing.altParserT)(Parsing_String.string("bpm"))(Parsing_String.string("t"))))(function () {
            return Control_Applicative.pure(Parsing.applicativeParserT)(new BPM(new Data_Tuple.Tuple(nfToNum(x), nfToNum(y))));
        });
    });
});
var parseTempoMark = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(naturalOrFloat)(function (x) {
    return Control_Bind.bind(Parsing.bindParserT)(naturalOrFloat)(function (y) {
        return Control_Applicative.pure(Parsing.applicativeParserT)(new Data_Tuple.Tuple(nfToNum(x), nfToNum(y)));
    });
});
var integer = /* #__PURE__ */ (function () {
    return tokenParser.integer;
})();
var identifier = /* #__PURE__ */ (function () {
    return tokenParser.identifier;
})();
var $$float = /* #__PURE__ */ (function () {
    return tokenParser["float"];
})();

//---
// passage :: P Program
// passage = do
//   x <- string "passage"
//   pure Passage
var discardA = function (dictMonad) {
    return function (m) {
        return function (k) {
            return Control_Bind.bind(dictMonad.Bind1())(m)(function (v) {
                return k;
            });
        };
    };
};
var defaultTempo = /* #__PURE__ */ (function () {
    return new Data_Tuple.Tuple(120.0, 0.25);
})();
var parseDefTempo = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Parsing_String.string("global"))(function (x) {
    return Control_Applicative.pure(Parsing.applicativeParserT)(defaultTempo);
});
var defaultCPS = 0.5;
var parseDefCPS = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Parsing_String.string("global"))(function (x) {
    return Control_Applicative.pure(Parsing.applicativeParserT)(defaultCPS);
});
var convergeShowInstance = {
    show: function (v) {
        if (v instanceof Diverge) {
            return "diverge";
        };
        if (v instanceof ConvergeFromOrigin) {
            return "convergeOrigin" + (Data_Show.show(Data_Show.showNumber)(v.value0) + (" " + Data_Show.show(Data_Show.showNumber)(v.value1)));
        };
        if (v instanceof ConvergeFromEval) {
            return "convergeEval" + (Data_Show.show(Data_Show.showNumber)(v.value0) + (" " + Data_Show.show(Data_Show.showNumber)(v.value1)));
        };
        throw new Error("Failed pattern match at MainParser (line 102, column 1 - line 105, column 76): " + [ v.constructor.name ]);
    }
};
var polytemporalShowInstance = {
    show: function (v) {
        return "poly " + (Data_Show.show(Data_List_Types.showList(voicingShowInstance))(v.value0) + (" " + (Data_Show.show(Data_List_Types.showList(noseShowInstance))(v.value1) + (" " + Data_Show.show(Data_List_Types.showList(convergeShowInstance))(v.value2)))));
    }
};
var programShowInstance = {
    show: function (v) {
        return "poly " + (Data_Show.show(polytemporalShowInstance)(v.value0) + (" " + ("pass " + Data_Show.show(Data_List_Types.showList(passageShowInstance))(v.value1))));
    }
};
var comma = /* #__PURE__ */ (function () {
    return tokenParser.comma;
})();
var parseByVoice = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Parsing_Combinators.sepBy(/* #__PURE__ */ Parsing_Combinators.choice(Data_Foldable.foldableArray)([ /* #__PURE__ */ Parsing_Combinators["try"](parseSingleProp), /* #__PURE__ */ Parsing_Combinators["try"](parseSingleCPS), /* #__PURE__ */ Parsing_Combinators["try"](parseSingleTempo) ]))(comma))(function (x) {
    return Control_Applicative.pure(Parsing.applicativeParserT)(x);
});

//-- Converge parser
var parseConverge = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT)(whitespace)(function () {
    return Control_Bind.bind(Parsing.bindParserT)(Parsing_Combinators.sepBy(Parsing_Combinators.choice(Data_Foldable.foldableArray)([ parseDiv, parseConvO, parseConvE ]))(comma))(function (x) {
        return Control_Applicative.pure(Parsing.applicativeParserT)(x);
    });
});

//--- Head Parser
var parseNose = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Parsing_Combinators.sepBy(/* #__PURE__ */ Parsing_Combinators.choice(Data_Foldable.foldableArray)([ parseOrigin, parseEval ]))(comma))(function (x) {
    return Control_Applicative.pure(Parsing.applicativeParserT)(x);
});
var parsebpms = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT)(whitespace)(function () {
    return Control_Bind.bind(Parsing.bindParserT)(Control_Alt.alt(Parsing.altParserT)(Parsing_String.string("tempi"))(Control_Alt.alt(Parsing.altParserT)(Parsing_String.string("tempos"))(Parsing_String.string("bpms"))))(function () {
        return Control_Bind.bind(Parsing.bindParserT)(parens(Parsing_Combinators.sepBy(Parsing_Combinators.choice(Data_Foldable.foldableArray)([ parseTempoMark, parseDefTempo ]))(comma)))(function (x) {
            return Control_Applicative.pure(Parsing.applicativeParserT)(Data_Functor.map(Data_List_Types.functorList)(BPM.create)(x));
        });
    });
});
var parsecpss = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Parsing_String.string("cycles"))(function () {
    return Control_Bind.bind(Parsing.bindParserT)(parens(Parsing_Combinators.sepBy(Parsing_Combinators.choice(Data_Foldable.foldableArray)([ parseCPSMark, parseDefCPS ]))(comma)))(function (x) {
        return Control_Applicative.pure(Parsing.applicativeParserT)(Data_Functor.map(Data_List_Types.functorList)(CPS.create)(x));
    });
});
var parseproportions = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT)(/* #__PURE__ */ Parsing_String.string("proportions"))(function () {
    return Control_Bind.bind(Parsing.bindParserT)(parens(Parsing_Combinators.sepBy(naturalOrFloat)(comma)))(function (x) {
        return Control_Applicative.pure(Parsing.applicativeParserT)(Data_Functor.map(Data_List_Types.functorList)(function (x$prime) {
            return new Proportion(nfToNum(x$prime));
        })(x));
    });
});

// parseMetric:: P Nose 
// parseMetric = do
//   _ <- string "metric" <|> string "m"
//   whitespace
//   x <- naturalOrFloat
//   whitespace
//   y <- naturalOrFloat
//   pure $ Metric (nfToNum x) (nfToNum y)
//--- voicing parser
var parsevoicings = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT)(whitespace)(function () {
    return Control_Bind.bind(Parsing.bindParserT)(Parsing_Combinators.choice(Data_Foldable.foldableArray)([ parens(parseByVoice), Parsing_Combinators["try"](parsebpms), Parsing_Combinators["try"](parseproportions), Parsing_Combinators["try"](parsecpss) ]))(function (x) {
        return Control_Applicative.pure(Parsing.applicativeParserT)(x);
    });
});
var colon = /* #__PURE__ */ (function () {
    return tokenParser.colon;
})();
var canonise = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT)(whitespace)(function () {
    return Control_Bind.bind(Parsing.bindParserT)(Parsing_String.string("canonise "))(function () {
        return Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT)(whitespace)(function () {
            return Control_Bind.bind(Parsing.bindParserT)(parsevoicings)(function (x) {
                return Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT)(whitespace)(function () {
                    return Control_Bind.bind(Parsing.bindParserT)(parens(parseNose))(function (y) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT)(whitespace)(function () {
                            return Control_Bind.bind(Parsing.bindParserT)(parens(parseConverge))(function (z) {
                                return Control_Applicative.pure(Parsing.applicativeParserT)(new Polytemporal(x, y, z));
                            });
                        });
                    });
                });
            });
        });
    });
});
var polytemporal = /* #__PURE__ */ Parsing_Combinators.choice(Data_Foldable.foldableArray)([ canonise ]);
var brackets = /* #__PURE__ */ (function () {
    return tokenParser.brackets;
})();
var braces = /* #__PURE__ */ (function () {
    return tokenParser.braces;
})();
export {
    Program,
    Passage,
    Polytemporal,
    Proportion,
    BPM,
    CPS,
    Origin,
    Eval,
    Diverge,
    ConvergeFromOrigin,
    ConvergeFromEval,
    polytemporal,
    canonise,
    parseConverge,
    parseDiv,
    parseConvO,
    parseConvE,
    parseNose,
    parseOrigin,
    parseEval,
    parsevoicings,
    parseByVoice,
    parseSingleProp,
    parseSingleTempo,
    parseSingleCPS,
    parseproportions,
    parsebpms,
    parseDefTempo,
    parseTempoMark,
    defaultTempo,
    parsecpss,
    parseDefCPS,
    defaultCPS,
    parseCPSMark,
    discardA,
    nfToNum,
    tokenParser,
    parens,
    braces,
    identifier,
    reserved,
    naturalOrFloat,
    $$float as float,
    whitespace,
    colon,
    brackets,
    comma,
    semi,
    integer,
    programShowInstance,
    passageShowInstance,
    polytemporalShowInstance,
    voicingShowInstance,
    noseShowInstance,
    convergeShowInstance
};
