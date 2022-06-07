// | This module defines the reader-writer-state monad transformer, `RWST`.
import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Apply from "../Control.Apply/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Monad_Error_Class from "../Control.Monad.Error.Class/index.js";
import * as Control_Monad_Rec_Class from "../Control.Monad.Rec.Class/index.js";
import * as Control_Monad_Trans_Class from "../Control.Monad.Trans.Class/index.js";
import * as Control_Plus from "../Control.Plus/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_Monoid from "../Data.Monoid/index.js";
import * as Data_Semigroup from "../Data.Semigroup/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
import * as Effect_Class from "../Effect.Class/index.js";
var RWSResult = /* #__PURE__ */ (function () {
    function RWSResult(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    RWSResult.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new RWSResult(value0, value1, value2);
            };
        };
    };
    return RWSResult;
})();

// | The reader-writer-state monad transformer, which combines the operations
// | of `ReaderT`, `WriterT` and `StateT` into a single monad transformer.
var RWST = function (x) {
    return x;
};

// | Change the context type in a `RWST` monad action.
var withRWST = function (f) {
    return function (m) {
        return function (r) {
            return function (s) {
                return Data_Tuple.uncurry(m)(f(r)(s));
            };
        };
    };
};

// | Run a computation in the `RWST` monad.
var runRWST = function (v) {
    return v;
};
var newtypeRWST = {
    Coercible0: function () {
        return undefined;
    }
};
var monadTransRWST = function (dictMonoid) {
    return {
        lift: function (dictMonad) {
            return function (m) {
                return function (v) {
                    return function (s) {
                        return Control_Bind.bind(dictMonad.Bind1())(m)(function (a) {
                            return Control_Applicative.pure(dictMonad.Applicative0())(new RWSResult(s, a, Data_Monoid.mempty(dictMonoid)));
                        });
                    };
                };
            };
        }
    };
};

// | Change the result and accumulator types in a `RWST` monad action.
var mapRWST = function (f) {
    return function (v) {
        return function (r) {
            return function (s) {
                return f(v(r)(s));
            };
        };
    };
};
var lazyRWST = {
    defer: function (f) {
        return function (r) {
            return function (s) {
                var v = f(Data_Unit.unit);
                return v(r)(s);
            };
        };
    }
};
var functorRWST = function (dictFunctor) {
    return {
        map: function (f) {
            return function (v) {
                return function (r) {
                    return function (s) {
                        return Data_Functor.map(dictFunctor)(function (v1) {
                            return new RWSResult(v1.value0, f(v1.value1), v1.value2);
                        })(v(r)(s));
                    };
                };
            };
        }
    };
};

// | Run a computation in the `RWST` monad, discarding the result.
var execRWST = function (dictMonad) {
    return function (v) {
        return function (r) {
            return function (s) {
                return Control_Bind.bind(dictMonad.Bind1())(v(r)(s))(function (v1) {
                    return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(v1.value0, v1.value2));
                });
            };
        };
    };
};

// | Run a computation in the `RWST` monad, discarding the final state.
var evalRWST = function (dictMonad) {
    return function (v) {
        return function (r) {
            return function (s) {
                return Control_Bind.bind(dictMonad.Bind1())(v(r)(s))(function (v1) {
                    return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(v1.value1, v1.value2));
                });
            };
        };
    };
};
var applyRWST = function (dictBind) {
    return function (dictMonoid) {
        return {
            apply: function (v) {
                return function (v1) {
                    return function (r) {
                        return function (s) {
                            return Control_Bind.bind(dictBind)(v(r)(s))(function (v2) {
                                return Data_Functor.mapFlipped((dictBind.Apply0()).Functor0())(v1(r)(v2.value0))(function (v3) {
                                    return new RWSResult(v3.value0, v2.value1(v3.value1), Data_Semigroup.append(dictMonoid.Semigroup0())(v2.value2)(v3.value2));
                                });
                            });
                        };
                    };
                };
            },
            Functor0: function () {
                return functorRWST((dictBind.Apply0()).Functor0());
            }
        };
    };
};
var bindRWST = function (dictBind) {
    return function (dictMonoid) {
        return {
            bind: function (v) {
                return function (f) {
                    return function (r) {
                        return function (s) {
                            return Control_Bind.bind(dictBind)(v(r)(s))(function (v1) {
                                var v2 = f(v1.value1);
                                return Data_Functor.mapFlipped((dictBind.Apply0()).Functor0())(v2(r)(v1.value0))(function (v3) {
                                    return new RWSResult(v3.value0, v3.value1, Data_Semigroup.append(dictMonoid.Semigroup0())(v1.value2)(v3.value2));
                                });
                            });
                        };
                    };
                };
            },
            Apply0: function () {
                return applyRWST(dictBind)(dictMonoid);
            }
        };
    };
};
var semigroupRWST = function (dictBind) {
    return function (dictMonoid) {
        return function (dictSemigroup) {
            return {
                append: Control_Apply.lift2(applyRWST(dictBind)(dictMonoid))(Data_Semigroup.append(dictSemigroup))
            };
        };
    };
};
var applicativeRWST = function (dictMonad) {
    return function (dictMonoid) {
        return {
            pure: function (a) {
                return function (v) {
                    return function (s) {
                        return Control_Applicative.pure(dictMonad.Applicative0())(new RWSResult(s, a, Data_Monoid.mempty(dictMonoid)));
                    };
                };
            },
            Apply0: function () {
                return applyRWST(dictMonad.Bind1())(dictMonoid);
            }
        };
    };
};
var monadRWST = function (dictMonad) {
    return function (dictMonoid) {
        return {
            Applicative0: function () {
                return applicativeRWST(dictMonad)(dictMonoid);
            },
            Bind1: function () {
                return bindRWST(dictMonad.Bind1())(dictMonoid);
            }
        };
    };
};
var monadAskRWST = function (dictMonad) {
    return function (dictMonoid) {
        return {
            ask: function (r) {
                return function (s) {
                    return Control_Applicative.pure(dictMonad.Applicative0())(new RWSResult(s, r, Data_Monoid.mempty(dictMonoid)));
                };
            },
            Monad0: function () {
                return monadRWST(dictMonad)(dictMonoid);
            }
        };
    };
};
var monadReaderRWST = function (dictMonad) {
    return function (dictMonoid) {
        return {
            local: function (f) {
                return function (m) {
                    return function (r) {
                        return function (s) {
                            return m(f(r))(s);
                        };
                    };
                };
            },
            MonadAsk0: function () {
                return monadAskRWST(dictMonad)(dictMonoid);
            }
        };
    };
};
var monadEffectRWS = function (dictMonoid) {
    return function (dictMonadEffect) {
        return {
            liftEffect: (function () {
                var $158 = Control_Monad_Trans_Class.lift(monadTransRWST(dictMonoid))(dictMonadEffect.Monad0());
                var $159 = Effect_Class.liftEffect(dictMonadEffect);
                return function ($160) {
                    return $158($159($160));
                };
            })(),
            Monad0: function () {
                return monadRWST(dictMonadEffect.Monad0())(dictMonoid);
            }
        };
    };
};
var monadRecRWST = function (dictMonadRec) {
    return function (dictMonoid) {
        return {
            tailRecM: function (k) {
                return function (a) {
                    var k$prime = function (r) {
                        return function (v) {
                            var v1 = k(v.value1);
                            return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())(v1(r)(v.value0))(function (v2) {
                                return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())((function () {
                                    if (v2.value1 instanceof Control_Monad_Rec_Class.Loop) {
                                        return new Control_Monad_Rec_Class.Loop(new RWSResult(v2.value0, v2.value1.value0, Data_Semigroup.append(dictMonoid.Semigroup0())(v.value2)(v2.value2)));
                                    };
                                    if (v2.value1 instanceof Control_Monad_Rec_Class.Done) {
                                        return new Control_Monad_Rec_Class.Done(new RWSResult(v2.value0, v2.value1.value0, Data_Semigroup.append(dictMonoid.Semigroup0())(v.value2)(v2.value2)));
                                    };
                                    throw new Error("Failed pattern match at Control.Monad.RWS.Trans (line 128, column 16 - line 130, column 68): " + [ v2.value1.constructor.name ]);
                                })());
                            });
                        };
                    };
                    return function (r) {
                        return function (s) {
                            return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(k$prime(r))(new RWSResult(s, a, Data_Monoid.mempty(dictMonoid)));
                        };
                    };
                };
            },
            Monad0: function () {
                return monadRWST(dictMonadRec.Monad0())(dictMonoid);
            }
        };
    };
};
var monadStateRWST = function (dictMonad) {
    return function (dictMonoid) {
        return {
            state: function (f) {
                return function (v) {
                    return function (s) {
                        var v1 = f(s);
                        return Control_Applicative.pure(dictMonad.Applicative0())(new RWSResult(v1.value1, v1.value0, Data_Monoid.mempty(dictMonoid)));
                    };
                };
            },
            Monad0: function () {
                return monadRWST(dictMonad)(dictMonoid);
            }
        };
    };
};
var monadTellRWST = function (dictMonad) {
    return function (dictMonoid) {
        return {
            tell: function (w) {
                return function (v) {
                    return function (s) {
                        return Control_Applicative.pure(dictMonad.Applicative0())(new RWSResult(s, Data_Unit.unit, w));
                    };
                };
            },
            Semigroup0: dictMonoid.Semigroup0,
            Monad1: function () {
                return monadRWST(dictMonad)(dictMonoid);
            }
        };
    };
};
var monadWriterRWST = function (dictMonad) {
    return function (dictMonoid) {
        return {
            listen: function (m) {
                return function (r) {
                    return function (s) {
                        return Control_Bind.bind(dictMonad.Bind1())(m(r)(s))(function (v) {
                            return Control_Applicative.pure(dictMonad.Applicative0())(new RWSResult(v.value0, new Data_Tuple.Tuple(v.value1, v.value2), v.value2));
                        });
                    };
                };
            },
            pass: function (m) {
                return function (r) {
                    return function (s) {
                        return Control_Bind.bind(dictMonad.Bind1())(m(r)(s))(function (v) {
                            return Control_Applicative.pure(dictMonad.Applicative0())(new RWSResult(v.value0, v.value1.value0, v.value1.value1(v.value2)));
                        });
                    };
                };
            },
            Monoid0: function () {
                return dictMonoid;
            },
            MonadTell1: function () {
                return monadTellRWST(dictMonad)(dictMonoid);
            }
        };
    };
};
var monadThrowRWST = function (dictMonadThrow) {
    return function (dictMonoid) {
        return {
            throwError: function (e) {
                return Control_Monad_Trans_Class.lift(monadTransRWST(dictMonoid))(dictMonadThrow.Monad0())(Control_Monad_Error_Class.throwError(dictMonadThrow)(e));
            },
            Monad0: function () {
                return monadRWST(dictMonadThrow.Monad0())(dictMonoid);
            }
        };
    };
};
var monadErrorRWST = function (dictMonadError) {
    return function (dictMonoid) {
        return {
            catchError: function (m) {
                return function (h) {
                    return function (r) {
                        return function (s) {
                            return Control_Monad_Error_Class.catchError(dictMonadError)(m(r)(s))(function (e) {
                                var v = h(e);
                                return v(r)(s);
                            });
                        };
                    };
                };
            },
            MonadThrow0: function () {
                return monadThrowRWST(dictMonadError.MonadThrow0())(dictMonoid);
            }
        };
    };
};
var monoidRWST = function (dictMonad) {
    return function (dictMonoid) {
        return function (dictMonoid1) {
            return {
                mempty: Control_Applicative.pure(applicativeRWST(dictMonad)(dictMonoid))(Data_Monoid.mempty(dictMonoid1)),
                Semigroup0: function () {
                    return semigroupRWST(dictMonad.Bind1())(dictMonoid)(dictMonoid1.Semigroup0());
                }
            };
        };
    };
};
var altRWST = function (dictAlt) {
    return {
        alt: function (v) {
            return function (v1) {
                return function (r) {
                    return function (s) {
                        return Control_Alt.alt(dictAlt)(v(r)(s))(v1(r)(s));
                    };
                };
            };
        },
        Functor0: function () {
            return functorRWST(dictAlt.Functor0());
        }
    };
};
var plusRWST = function (dictPlus) {
    return {
        empty: function (v) {
            return function (v1) {
                return Control_Plus.empty(dictPlus);
            };
        },
        Alt0: function () {
            return altRWST(dictPlus.Alt0());
        }
    };
};
var alternativeRWST = function (dictMonoid) {
    return function (dictAlternative) {
        return function (dictMonad) {
            return {
                Applicative0: function () {
                    return applicativeRWST(dictMonad)(dictMonoid);
                },
                Plus1: function () {
                    return plusRWST(dictAlternative.Plus1());
                }
            };
        };
    };
};
export {
    RWSResult,
    RWST,
    runRWST,
    evalRWST,
    execRWST,
    mapRWST,
    withRWST,
    newtypeRWST,
    functorRWST,
    applyRWST,
    altRWST,
    alternativeRWST,
    bindRWST,
    applicativeRWST,
    monadRWST,
    monadTransRWST,
    lazyRWST,
    monadEffectRWS,
    monadAskRWST,
    monadReaderRWST,
    monadStateRWST,
    monadTellRWST,
    monadWriterRWST,
    monadThrowRWST,
    monadErrorRWST,
    monadRecRWST,
    plusRWST,
    semigroupRWST,
    monoidRWST
};
export {
    lift
} from "../Control.Monad.Trans.Class/index.js";
