// Generated by purs version 0.15.15
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Unit from "../Data.Unit/index.js";
var uncurry9 = function (f$prime) {
    return function (v) {
        return f$prime(v.value0)(v.value1.value0)(v.value1.value1.value0)(v.value1.value1.value1.value0)(v.value1.value1.value1.value1.value0)(v.value1.value1.value1.value1.value1.value0)(v.value1.value1.value1.value1.value1.value1.value0)(v.value1.value1.value1.value1.value1.value1.value1.value0)(v.value1.value1.value1.value1.value1.value1.value1.value1.value0);
    };
};
var uncurry8 = function (f$prime) {
    return function (v) {
        return f$prime(v.value0)(v.value1.value0)(v.value1.value1.value0)(v.value1.value1.value1.value0)(v.value1.value1.value1.value1.value0)(v.value1.value1.value1.value1.value1.value0)(v.value1.value1.value1.value1.value1.value1.value0)(v.value1.value1.value1.value1.value1.value1.value1.value0);
    };
};
var uncurry7 = function (f$prime) {
    return function (v) {
        return f$prime(v.value0)(v.value1.value0)(v.value1.value1.value0)(v.value1.value1.value1.value0)(v.value1.value1.value1.value1.value0)(v.value1.value1.value1.value1.value1.value0)(v.value1.value1.value1.value1.value1.value1.value0);
    };
};
var uncurry6 = function (f$prime) {
    return function (v) {
        return f$prime(v.value0)(v.value1.value0)(v.value1.value1.value0)(v.value1.value1.value1.value0)(v.value1.value1.value1.value1.value0)(v.value1.value1.value1.value1.value1.value0);
    };
};
var uncurry5 = function (f) {
    return function (v) {
        return f(v.value0)(v.value1.value0)(v.value1.value1.value0)(v.value1.value1.value1.value0)(v.value1.value1.value1.value1.value0);
    };
};
var uncurry4 = function (f) {
    return function (v) {
        return f(v.value0)(v.value1.value0)(v.value1.value1.value0)(v.value1.value1.value1.value0);
    };
};
var uncurry3 = function (f) {
    return function (v) {
        return f(v.value0)(v.value1.value0)(v.value1.value1.value0);
    };
};
var uncurry2 = function (f) {
    return function (v) {
        return f(v.value0)(v.value1.value0);
    };
};
var uncurry10 = function (f$prime) {
    return function (v) {
        return f$prime(v.value0)(v.value1.value0)(v.value1.value1.value0)(v.value1.value1.value1.value0)(v.value1.value1.value1.value1.value0)(v.value1.value1.value1.value1.value1.value0)(v.value1.value1.value1.value1.value1.value1.value0)(v.value1.value1.value1.value1.value1.value1.value1.value0)(v.value1.value1.value1.value1.value1.value1.value1.value1.value0)(v.value1.value1.value1.value1.value1.value1.value1.value1.value1.value0);
    };
};
var uncurry1 = function (f) {
    return function (v) {
        return f(v.value0);
    };
};
var tuple9 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return function (g) {
                            return function (h) {
                                return function (i) {
                                    return new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, new Data_Tuple.Tuple(d, new Data_Tuple.Tuple(e, new Data_Tuple.Tuple(f, new Data_Tuple.Tuple(g, new Data_Tuple.Tuple(h, new Data_Tuple.Tuple(i, Data_Unit.unit)))))))));
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};
var tuple8 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return function (g) {
                            return function (h) {
                                return new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, new Data_Tuple.Tuple(d, new Data_Tuple.Tuple(e, new Data_Tuple.Tuple(f, new Data_Tuple.Tuple(g, new Data_Tuple.Tuple(h, Data_Unit.unit))))))));
                            };
                        };
                    };
                };
            };
        };
    };
};
var tuple7 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return function (g) {
                            return new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, new Data_Tuple.Tuple(d, new Data_Tuple.Tuple(e, new Data_Tuple.Tuple(f, new Data_Tuple.Tuple(g, Data_Unit.unit)))))));
                        };
                    };
                };
            };
        };
    };
};
var tuple6 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, new Data_Tuple.Tuple(d, new Data_Tuple.Tuple(e, new Data_Tuple.Tuple(f, Data_Unit.unit))))));
                    };
                };
            };
        };
    };
};
var tuple5 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, new Data_Tuple.Tuple(d, new Data_Tuple.Tuple(e, Data_Unit.unit)))));
                };
            };
        };
    };
};
var tuple4 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, new Data_Tuple.Tuple(d, Data_Unit.unit))));
            };
        };
    };
};
var tuple3 = function (a) {
    return function (b) {
        return function (c) {
            return new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, Data_Unit.unit)));
        };
    };
};
var tuple2 = function (a) {
    return function (b) {
        return new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, Data_Unit.unit));
    };
};
var tuple10 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return function (g) {
                            return function (h) {
                                return function (i) {
                                    return function (j) {
                                        return new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, new Data_Tuple.Tuple(d, new Data_Tuple.Tuple(e, new Data_Tuple.Tuple(f, new Data_Tuple.Tuple(g, new Data_Tuple.Tuple(h, new Data_Tuple.Tuple(i, new Data_Tuple.Tuple(j, Data_Unit.unit))))))))));
                                    };
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};
var tuple1 = function (a) {
    return new Data_Tuple.Tuple(a, Data_Unit.unit);
};
var over9 = function (o) {
    return function (v) {
        return new Data_Tuple.Tuple(v.value0, new Data_Tuple.Tuple(v.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value1.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value1.value1.value1.value1.value0, new Data_Tuple.Tuple(o(v.value1.value1.value1.value1.value1.value1.value1.value1.value0), v.value1.value1.value1.value1.value1.value1.value1.value1.value1)))))))));
    };
};
var over8 = function (o) {
    return function (v) {
        return new Data_Tuple.Tuple(v.value0, new Data_Tuple.Tuple(v.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value1.value1.value1.value0, new Data_Tuple.Tuple(o(v.value1.value1.value1.value1.value1.value1.value1.value0), v.value1.value1.value1.value1.value1.value1.value1.value1))))))));
    };
};
var over7 = function (o) {
    return function (v) {
        return new Data_Tuple.Tuple(v.value0, new Data_Tuple.Tuple(v.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value1.value1.value0, new Data_Tuple.Tuple(o(v.value1.value1.value1.value1.value1.value1.value0), v.value1.value1.value1.value1.value1.value1.value1)))))));
    };
};
var over6 = function (o) {
    return function (v) {
        return new Data_Tuple.Tuple(v.value0, new Data_Tuple.Tuple(v.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value1.value0, new Data_Tuple.Tuple(o(v.value1.value1.value1.value1.value1.value0), v.value1.value1.value1.value1.value1.value1))))));
    };
};
var over5 = function (o) {
    return function (v) {
        return new Data_Tuple.Tuple(v.value0, new Data_Tuple.Tuple(v.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value0, new Data_Tuple.Tuple(o(v.value1.value1.value1.value1.value0), v.value1.value1.value1.value1.value1)))));
    };
};
var over4 = function (o) {
    return function (v) {
        return new Data_Tuple.Tuple(v.value0, new Data_Tuple.Tuple(v.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value0, new Data_Tuple.Tuple(o(v.value1.value1.value1.value0), v.value1.value1.value1.value1))));
    };
};
var over3 = function (o) {
    return function (v) {
        return new Data_Tuple.Tuple(v.value0, new Data_Tuple.Tuple(v.value1.value0, new Data_Tuple.Tuple(o(v.value1.value1.value0), v.value1.value1.value1)));
    };
};
var over2 = function (o) {
    return function (v) {
        return new Data_Tuple.Tuple(v.value0, new Data_Tuple.Tuple(o(v.value1.value0), v.value1.value1));
    };
};
var over10 = function (o) {
    return function (v) {
        return new Data_Tuple.Tuple(v.value0, new Data_Tuple.Tuple(v.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value1.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value1.value1.value1.value1.value0, new Data_Tuple.Tuple(v.value1.value1.value1.value1.value1.value1.value1.value1.value0, new Data_Tuple.Tuple(o(v.value1.value1.value1.value1.value1.value1.value1.value1.value1.value0), v.value1.value1.value1.value1.value1.value1.value1.value1.value1.value1))))))))));
    };
};
var over1 = function (o) {
    return function (v) {
        return new Data_Tuple.Tuple(o(v.value0), v.value1);
    };
};
var get9 = function (v) {
    return v.value1.value1.value1.value1.value1.value1.value1.value1.value0;
};
var get8 = function (v) {
    return v.value1.value1.value1.value1.value1.value1.value1.value0;
};
var get7 = function (v) {
    return v.value1.value1.value1.value1.value1.value1.value0;
};
var get6 = function (v) {
    return v.value1.value1.value1.value1.value1.value0;
};
var get5 = function (v) {
    return v.value1.value1.value1.value1.value0;
};
var get4 = function (v) {
    return v.value1.value1.value1.value0;
};
var get3 = function (v) {
    return v.value1.value1.value0;
};
var get2 = function (v) {
    return v.value1.value0;
};
var get10 = function (v) {
    return v.value1.value1.value1.value1.value1.value1.value1.value1.value1.value0;
};
var get1 = function (v) {
    return v.value0;
};
var curry9 = function (z) {
    return function (f$prime) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return function (f) {
                                return function (g) {
                                    return function (h) {
                                        return function (i) {
                                            return f$prime(new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, new Data_Tuple.Tuple(d, new Data_Tuple.Tuple(e, new Data_Tuple.Tuple(f, new Data_Tuple.Tuple(g, new Data_Tuple.Tuple(h, new Data_Tuple.Tuple(i, z))))))))));
                                        };
                                    };
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};
var curry8 = function (z) {
    return function (f$prime) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return function (f) {
                                return function (g) {
                                    return function (h) {
                                        return f$prime(new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, new Data_Tuple.Tuple(d, new Data_Tuple.Tuple(e, new Data_Tuple.Tuple(f, new Data_Tuple.Tuple(g, new Data_Tuple.Tuple(h, z)))))))));
                                    };
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};
var curry7 = function (z) {
    return function (f$prime) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return function (f) {
                                return function (g) {
                                    return f$prime(new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, new Data_Tuple.Tuple(d, new Data_Tuple.Tuple(e, new Data_Tuple.Tuple(f, new Data_Tuple.Tuple(g, z))))))));
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};
var curry6 = function (z) {
    return function (f$prime) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return function (f) {
                                return f$prime(new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, new Data_Tuple.Tuple(d, new Data_Tuple.Tuple(e, new Data_Tuple.Tuple(f, z)))))));
                            };
                        };
                    };
                };
            };
        };
    };
};
var curry5 = function (z) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return f(new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, new Data_Tuple.Tuple(d, new Data_Tuple.Tuple(e, z))))));
                        };
                    };
                };
            };
        };
    };
};
var curry4 = function (z) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return f(new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, new Data_Tuple.Tuple(d, z)))));
                    };
                };
            };
        };
    };
};
var curry3 = function (z) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return f(new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, z))));
                };
            };
        };
    };
};
var curry2 = function (z) {
    return function (f) {
        return function (a) {
            return function (b) {
                return f(new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, z)));
            };
        };
    };
};
var curry10 = function (z) {
    return function (f$prime) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return function (e) {
                            return function (f) {
                                return function (g) {
                                    return function (h) {
                                        return function (i) {
                                            return function (j) {
                                                return f$prime(new Data_Tuple.Tuple(a, new Data_Tuple.Tuple(b, new Data_Tuple.Tuple(c, new Data_Tuple.Tuple(d, new Data_Tuple.Tuple(e, new Data_Tuple.Tuple(f, new Data_Tuple.Tuple(g, new Data_Tuple.Tuple(h, new Data_Tuple.Tuple(i, new Data_Tuple.Tuple(j, z)))))))))));
                                            };
                                        };
                                    };
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};
var curry1 = function (z) {
    return function (f) {
        return function (a) {
            return f(new Data_Tuple.Tuple(a, z));
        };
    };
};
export {
    tuple1,
    tuple2,
    tuple3,
    tuple4,
    tuple5,
    tuple6,
    tuple7,
    tuple8,
    tuple9,
    tuple10,
    get1,
    get2,
    get3,
    get4,
    get5,
    get6,
    get7,
    get8,
    get9,
    get10,
    over1,
    over2,
    over3,
    over4,
    over5,
    over6,
    over7,
    over8,
    over9,
    over10,
    uncurry1,
    uncurry2,
    uncurry3,
    uncurry4,
    uncurry5,
    uncurry6,
    uncurry7,
    uncurry8,
    uncurry9,
    uncurry10,
    curry1,
    curry2,
    curry3,
    curry4,
    curry5,
    curry6,
    curry7,
    curry8,
    curry9,
    curry10
};
