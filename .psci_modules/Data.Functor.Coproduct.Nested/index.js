import * as Data_Either from "../Data.Either/index.js";
import * as Data_Functor_Coproduct from "../Data.Functor.Coproduct/index.js";
import * as Data_Newtype from "../Data.Newtype/index.js";
import * as Data_Void from "../Data.Void/index.js";
var in9 = function (v) {
    return Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.left(v)))))))));
};
var in8 = function (v) {
    return Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.left(v))))))));
};
var in7 = function (v) {
    return Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.left(v)))))));
};
var in6 = function (v) {
    return Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.left(v))))));
};
var in5 = function (v) {
    return Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.left(v)))));
};
var in4 = function (v) {
    return Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.left(v))));
};
var in3 = function (v) {
    return Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.left(v)));
};
var in2 = function (v) {
    return Data_Functor_Coproduct.right(Data_Functor_Coproduct.left(v));
};
var in10 = function (v) {
    return Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.right(Data_Functor_Coproduct.left(v))))))))));
};
var in1 = Data_Functor_Coproduct.left;
var coproduct9 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return function (g) {
                            return function (h) {
                                return function (i) {
                                    return function (y) {
                                        if (y instanceof Data_Either.Left) {
                                            return a(y.value0);
                                        };
                                        if (y instanceof Data_Either.Right) {
                                            if (y.value0 instanceof Data_Either.Left) {
                                                return b(y.value0.value0);
                                            };
                                            if (y.value0 instanceof Data_Either.Right) {
                                                if (y.value0.value0 instanceof Data_Either.Left) {
                                                    return c(y.value0.value0.value0);
                                                };
                                                if (y.value0.value0 instanceof Data_Either.Right) {
                                                    if (y.value0.value0.value0 instanceof Data_Either.Left) {
                                                        return d(y.value0.value0.value0.value0);
                                                    };
                                                    if (y.value0.value0.value0 instanceof Data_Either.Right) {
                                                        if (y.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                            return e(y.value0.value0.value0.value0.value0);
                                                        };
                                                        if (y.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                            if (y.value0.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                                return f(y.value0.value0.value0.value0.value0.value0);
                                                            };
                                                            if (y.value0.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                                if (y.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                                    return g(y.value0.value0.value0.value0.value0.value0.value0);
                                                                };
                                                                if (y.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                                    if (y.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                                        return h(y.value0.value0.value0.value0.value0.value0.value0.value0);
                                                                    };
                                                                    if (y.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                                        if (y.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                                            return i(y.value0.value0.value0.value0.value0.value0.value0.value0.value0);
                                                                        };
                                                                        if (y.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                                            return Data_Void.absurd(Data_Newtype.unwrap()(y.value0.value0.value0.value0.value0.value0.value0.value0.value0));
                                                                        };
                                                                        throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 248, column 41 - line 250, column 61): " + [ y.value0.value0.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                                    };
                                                                    throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 246, column 39 - line 250, column 61): " + [ y.value0.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                                };
                                                                throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 244, column 37 - line 250, column 61): " + [ y.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                            };
                                                            throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 242, column 35 - line 250, column 61): " + [ y.value0.value0.value0.value0.value0.constructor.name ]);
                                                        };
                                                        throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 240, column 33 - line 250, column 61): " + [ y.value0.value0.value0.value0.constructor.name ]);
                                                    };
                                                    throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 238, column 31 - line 250, column 61): " + [ y.value0.value0.value0.constructor.name ]);
                                                };
                                                throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 236, column 29 - line 250, column 61): " + [ y.value0.value0.constructor.name ]);
                                            };
                                            throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 234, column 27 - line 250, column 61): " + [ y.value0.constructor.name ]);
                                        };
                                        throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 232, column 34 - line 250, column 61): " + [ y.constructor.name ]);
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
var coproduct8 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return function (g) {
                            return function (h) {
                                return function (y) {
                                    if (y instanceof Data_Either.Left) {
                                        return a(y.value0);
                                    };
                                    if (y instanceof Data_Either.Right) {
                                        if (y.value0 instanceof Data_Either.Left) {
                                            return b(y.value0.value0);
                                        };
                                        if (y.value0 instanceof Data_Either.Right) {
                                            if (y.value0.value0 instanceof Data_Either.Left) {
                                                return c(y.value0.value0.value0);
                                            };
                                            if (y.value0.value0 instanceof Data_Either.Right) {
                                                if (y.value0.value0.value0 instanceof Data_Either.Left) {
                                                    return d(y.value0.value0.value0.value0);
                                                };
                                                if (y.value0.value0.value0 instanceof Data_Either.Right) {
                                                    if (y.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                        return e(y.value0.value0.value0.value0.value0);
                                                    };
                                                    if (y.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                        if (y.value0.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                            return f(y.value0.value0.value0.value0.value0.value0);
                                                        };
                                                        if (y.value0.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                            if (y.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                                return g(y.value0.value0.value0.value0.value0.value0.value0);
                                                            };
                                                            if (y.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                                if (y.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                                    return h(y.value0.value0.value0.value0.value0.value0.value0.value0);
                                                                };
                                                                if (y.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                                    return Data_Void.absurd(Data_Newtype.unwrap()(y.value0.value0.value0.value0.value0.value0.value0.value0));
                                                                };
                                                                throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 227, column 39 - line 229, column 59): " + [ y.value0.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                            };
                                                            throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 225, column 37 - line 229, column 59): " + [ y.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                        };
                                                        throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 223, column 35 - line 229, column 59): " + [ y.value0.value0.value0.value0.value0.constructor.name ]);
                                                    };
                                                    throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 221, column 33 - line 229, column 59): " + [ y.value0.value0.value0.value0.constructor.name ]);
                                                };
                                                throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 219, column 31 - line 229, column 59): " + [ y.value0.value0.value0.constructor.name ]);
                                            };
                                            throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 217, column 29 - line 229, column 59): " + [ y.value0.value0.constructor.name ]);
                                        };
                                        throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 215, column 27 - line 229, column 59): " + [ y.value0.constructor.name ]);
                                    };
                                    throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 213, column 32 - line 229, column 59): " + [ y.constructor.name ]);
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};
var coproduct7 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return function (g) {
                            return function (y) {
                                if (y instanceof Data_Either.Left) {
                                    return a(y.value0);
                                };
                                if (y instanceof Data_Either.Right) {
                                    if (y.value0 instanceof Data_Either.Left) {
                                        return b(y.value0.value0);
                                    };
                                    if (y.value0 instanceof Data_Either.Right) {
                                        if (y.value0.value0 instanceof Data_Either.Left) {
                                            return c(y.value0.value0.value0);
                                        };
                                        if (y.value0.value0 instanceof Data_Either.Right) {
                                            if (y.value0.value0.value0 instanceof Data_Either.Left) {
                                                return d(y.value0.value0.value0.value0);
                                            };
                                            if (y.value0.value0.value0 instanceof Data_Either.Right) {
                                                if (y.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                    return e(y.value0.value0.value0.value0.value0);
                                                };
                                                if (y.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                    if (y.value0.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                        return f(y.value0.value0.value0.value0.value0.value0);
                                                    };
                                                    if (y.value0.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                        if (y.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                            return g(y.value0.value0.value0.value0.value0.value0.value0);
                                                        };
                                                        if (y.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                            return Data_Void.absurd(Data_Newtype.unwrap()(y.value0.value0.value0.value0.value0.value0.value0));
                                                        };
                                                        throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 208, column 37 - line 210, column 57): " + [ y.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                    };
                                                    throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 206, column 35 - line 210, column 57): " + [ y.value0.value0.value0.value0.value0.constructor.name ]);
                                                };
                                                throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 204, column 33 - line 210, column 57): " + [ y.value0.value0.value0.value0.constructor.name ]);
                                            };
                                            throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 202, column 31 - line 210, column 57): " + [ y.value0.value0.value0.constructor.name ]);
                                        };
                                        throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 200, column 29 - line 210, column 57): " + [ y.value0.value0.constructor.name ]);
                                    };
                                    throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 198, column 27 - line 210, column 57): " + [ y.value0.constructor.name ]);
                                };
                                throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 196, column 30 - line 210, column 57): " + [ y.constructor.name ]);
                            };
                        };
                    };
                };
            };
        };
    };
};
var coproduct6 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return function (y) {
                            if (y instanceof Data_Either.Left) {
                                return a(y.value0);
                            };
                            if (y instanceof Data_Either.Right) {
                                if (y.value0 instanceof Data_Either.Left) {
                                    return b(y.value0.value0);
                                };
                                if (y.value0 instanceof Data_Either.Right) {
                                    if (y.value0.value0 instanceof Data_Either.Left) {
                                        return c(y.value0.value0.value0);
                                    };
                                    if (y.value0.value0 instanceof Data_Either.Right) {
                                        if (y.value0.value0.value0 instanceof Data_Either.Left) {
                                            return d(y.value0.value0.value0.value0);
                                        };
                                        if (y.value0.value0.value0 instanceof Data_Either.Right) {
                                            if (y.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                return e(y.value0.value0.value0.value0.value0);
                                            };
                                            if (y.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                if (y.value0.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                    return f(y.value0.value0.value0.value0.value0.value0);
                                                };
                                                if (y.value0.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                    return Data_Void.absurd(Data_Newtype.unwrap()(y.value0.value0.value0.value0.value0.value0));
                                                };
                                                throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 191, column 35 - line 193, column 55): " + [ y.value0.value0.value0.value0.value0.constructor.name ]);
                                            };
                                            throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 189, column 33 - line 193, column 55): " + [ y.value0.value0.value0.value0.constructor.name ]);
                                        };
                                        throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 187, column 31 - line 193, column 55): " + [ y.value0.value0.value0.constructor.name ]);
                                    };
                                    throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 185, column 29 - line 193, column 55): " + [ y.value0.value0.constructor.name ]);
                                };
                                throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 183, column 27 - line 193, column 55): " + [ y.value0.constructor.name ]);
                            };
                            throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 181, column 28 - line 193, column 55): " + [ y.constructor.name ]);
                        };
                    };
                };
            };
        };
    };
};
var coproduct5 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (y) {
                        if (y instanceof Data_Either.Left) {
                            return a(y.value0);
                        };
                        if (y instanceof Data_Either.Right) {
                            if (y.value0 instanceof Data_Either.Left) {
                                return b(y.value0.value0);
                            };
                            if (y.value0 instanceof Data_Either.Right) {
                                if (y.value0.value0 instanceof Data_Either.Left) {
                                    return c(y.value0.value0.value0);
                                };
                                if (y.value0.value0 instanceof Data_Either.Right) {
                                    if (y.value0.value0.value0 instanceof Data_Either.Left) {
                                        return d(y.value0.value0.value0.value0);
                                    };
                                    if (y.value0.value0.value0 instanceof Data_Either.Right) {
                                        if (y.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                            return e(y.value0.value0.value0.value0.value0);
                                        };
                                        if (y.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                            return Data_Void.absurd(Data_Newtype.unwrap()(y.value0.value0.value0.value0.value0));
                                        };
                                        throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 176, column 33 - line 178, column 53): " + [ y.value0.value0.value0.value0.constructor.name ]);
                                    };
                                    throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 174, column 31 - line 178, column 53): " + [ y.value0.value0.value0.constructor.name ]);
                                };
                                throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 172, column 29 - line 178, column 53): " + [ y.value0.value0.constructor.name ]);
                            };
                            throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 170, column 27 - line 178, column 53): " + [ y.value0.constructor.name ]);
                        };
                        throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 168, column 26 - line 178, column 53): " + [ y.constructor.name ]);
                    };
                };
            };
        };
    };
};
var coproduct4 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (y) {
                    if (y instanceof Data_Either.Left) {
                        return a(y.value0);
                    };
                    if (y instanceof Data_Either.Right) {
                        if (y.value0 instanceof Data_Either.Left) {
                            return b(y.value0.value0);
                        };
                        if (y.value0 instanceof Data_Either.Right) {
                            if (y.value0.value0 instanceof Data_Either.Left) {
                                return c(y.value0.value0.value0);
                            };
                            if (y.value0.value0 instanceof Data_Either.Right) {
                                if (y.value0.value0.value0 instanceof Data_Either.Left) {
                                    return d(y.value0.value0.value0.value0);
                                };
                                if (y.value0.value0.value0 instanceof Data_Either.Right) {
                                    return Data_Void.absurd(Data_Newtype.unwrap()(y.value0.value0.value0.value0));
                                };
                                throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 163, column 31 - line 165, column 51): " + [ y.value0.value0.value0.constructor.name ]);
                            };
                            throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 161, column 29 - line 165, column 51): " + [ y.value0.value0.constructor.name ]);
                        };
                        throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 159, column 27 - line 165, column 51): " + [ y.value0.constructor.name ]);
                    };
                    throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 157, column 24 - line 165, column 51): " + [ y.constructor.name ]);
                };
            };
        };
    };
};
var coproduct3 = function (a) {
    return function (b) {
        return function (c) {
            return function (y) {
                if (y instanceof Data_Either.Left) {
                    return a(y.value0);
                };
                if (y instanceof Data_Either.Right) {
                    if (y.value0 instanceof Data_Either.Left) {
                        return b(y.value0.value0);
                    };
                    if (y.value0 instanceof Data_Either.Right) {
                        if (y.value0.value0 instanceof Data_Either.Left) {
                            return c(y.value0.value0.value0);
                        };
                        if (y.value0.value0 instanceof Data_Either.Right) {
                            return Data_Void.absurd(Data_Newtype.unwrap()(y.value0.value0.value0));
                        };
                        throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 152, column 29 - line 154, column 49): " + [ y.value0.value0.constructor.name ]);
                    };
                    throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 150, column 27 - line 154, column 49): " + [ y.value0.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 148, column 22 - line 154, column 49): " + [ y.constructor.name ]);
            };
        };
    };
};
var coproduct2 = function (a) {
    return function (b) {
        return function (y) {
            if (y instanceof Data_Either.Left) {
                return a(y.value0);
            };
            if (y instanceof Data_Either.Right) {
                if (y.value0 instanceof Data_Either.Left) {
                    return b(y.value0.value0);
                };
                if (y.value0 instanceof Data_Either.Right) {
                    return Data_Void.absurd(Data_Newtype.unwrap()(y.value0.value0));
                };
                throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 143, column 27 - line 145, column 47): " + [ y.value0.constructor.name ]);
            };
            throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 141, column 20 - line 145, column 47): " + [ y.constructor.name ]);
        };
    };
};
var coproduct10 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return function (g) {
                            return function (h) {
                                return function (i) {
                                    return function (j) {
                                        return function (y) {
                                            if (y instanceof Data_Either.Left) {
                                                return a(y.value0);
                                            };
                                            if (y instanceof Data_Either.Right) {
                                                if (y.value0 instanceof Data_Either.Left) {
                                                    return b(y.value0.value0);
                                                };
                                                if (y.value0 instanceof Data_Either.Right) {
                                                    if (y.value0.value0 instanceof Data_Either.Left) {
                                                        return c(y.value0.value0.value0);
                                                    };
                                                    if (y.value0.value0 instanceof Data_Either.Right) {
                                                        if (y.value0.value0.value0 instanceof Data_Either.Left) {
                                                            return d(y.value0.value0.value0.value0);
                                                        };
                                                        if (y.value0.value0.value0 instanceof Data_Either.Right) {
                                                            if (y.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                                return e(y.value0.value0.value0.value0.value0);
                                                            };
                                                            if (y.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                                if (y.value0.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                                    return f(y.value0.value0.value0.value0.value0.value0);
                                                                };
                                                                if (y.value0.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                                    if (y.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                                        return g(y.value0.value0.value0.value0.value0.value0.value0);
                                                                    };
                                                                    if (y.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                                        if (y.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                                            return h(y.value0.value0.value0.value0.value0.value0.value0.value0);
                                                                        };
                                                                        if (y.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                                            if (y.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                                                return i(y.value0.value0.value0.value0.value0.value0.value0.value0.value0);
                                                                            };
                                                                            if (y.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                                                if (y.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Left) {
                                                                                    return j(y.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0);
                                                                                };
                                                                                if (y.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right) {
                                                                                    return Data_Void.absurd(Data_Newtype.unwrap()(y.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0));
                                                                                };
                                                                                throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 271, column 43 - line 273, column 65): " + [ y.value0.value0.value0.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                                            };
                                                                            throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 269, column 41 - line 273, column 65): " + [ y.value0.value0.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                                        };
                                                                        throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 267, column 39 - line 273, column 65): " + [ y.value0.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                                    };
                                                                    throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 265, column 37 - line 273, column 65): " + [ y.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                                };
                                                                throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 263, column 35 - line 273, column 65): " + [ y.value0.value0.value0.value0.value0.constructor.name ]);
                                                            };
                                                            throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 261, column 33 - line 273, column 65): " + [ y.value0.value0.value0.value0.constructor.name ]);
                                                        };
                                                        throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 259, column 31 - line 273, column 65): " + [ y.value0.value0.value0.constructor.name ]);
                                                    };
                                                    throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 257, column 29 - line 273, column 65): " + [ y.value0.value0.constructor.name ]);
                                                };
                                                throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 255, column 27 - line 273, column 65): " + [ y.value0.constructor.name ]);
                                            };
                                            throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 253, column 37 - line 273, column 65): " + [ y.constructor.name ]);
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
var coproduct1 = function (y) {
    if (y instanceof Data_Either.Left) {
        return y.value0;
    };
    if (y instanceof Data_Either.Right) {
        return Data_Void.absurd(Data_Newtype.unwrap()(y.value0));
    };
    throw new Error("Failed pattern match at Data.Functor.Coproduct.Nested (line 136, column 16 - line 138, column 45): " + [ y.constructor.name ]);
};
var at9 = function (b) {
    return function (f) {
        return function (y) {
            if (y instanceof Data_Either.Right && (y.value0 instanceof Data_Either.Right && (y.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right && y.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Left)))))))) {
                return f(y.value0.value0.value0.value0.value0.value0.value0.value0.value0);
            };
            return b;
        };
    };
};
var at8 = function (b) {
    return function (f) {
        return function (y) {
            if (y instanceof Data_Either.Right && (y.value0 instanceof Data_Either.Right && (y.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right && y.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Left))))))) {
                return f(y.value0.value0.value0.value0.value0.value0.value0.value0);
            };
            return b;
        };
    };
};
var at7 = function (b) {
    return function (f) {
        return function (y) {
            if (y instanceof Data_Either.Right && (y.value0 instanceof Data_Either.Right && (y.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0.value0.value0 instanceof Data_Either.Right && y.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Left)))))) {
                return f(y.value0.value0.value0.value0.value0.value0.value0);
            };
            return b;
        };
    };
};
var at6 = function (b) {
    return function (f) {
        return function (y) {
            if (y instanceof Data_Either.Right && (y.value0 instanceof Data_Either.Right && (y.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0.value0 instanceof Data_Either.Right && y.value0.value0.value0.value0.value0 instanceof Data_Either.Left))))) {
                return f(y.value0.value0.value0.value0.value0.value0);
            };
            return b;
        };
    };
};
var at5 = function (b) {
    return function (f) {
        return function (y) {
            if (y instanceof Data_Either.Right && (y.value0 instanceof Data_Either.Right && (y.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0 instanceof Data_Either.Right && y.value0.value0.value0.value0 instanceof Data_Either.Left)))) {
                return f(y.value0.value0.value0.value0.value0);
            };
            return b;
        };
    };
};
var at4 = function (b) {
    return function (f) {
        return function (y) {
            if (y instanceof Data_Either.Right && (y.value0 instanceof Data_Either.Right && (y.value0.value0 instanceof Data_Either.Right && y.value0.value0.value0 instanceof Data_Either.Left))) {
                return f(y.value0.value0.value0.value0);
            };
            return b;
        };
    };
};
var at3 = function (b) {
    return function (f) {
        return function (y) {
            if (y instanceof Data_Either.Right && (y.value0 instanceof Data_Either.Right && y.value0.value0 instanceof Data_Either.Left)) {
                return f(y.value0.value0.value0);
            };
            return b;
        };
    };
};
var at2 = function (b) {
    return function (f) {
        return function (y) {
            if (y instanceof Data_Either.Right && y.value0 instanceof Data_Either.Left) {
                return f(y.value0.value0);
            };
            return b;
        };
    };
};
var at10 = function (b) {
    return function (f) {
        return function (y) {
            if (y instanceof Data_Either.Right && (y.value0 instanceof Data_Either.Right && (y.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right && (y.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Right && y.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Either.Left))))))))) {
                return f(y.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0);
            };
            return b;
        };
    };
};
var at1 = function (b) {
    return function (f) {
        return function (y) {
            if (y instanceof Data_Either.Left) {
                return f(y.value0);
            };
            return b;
        };
    };
};
export {
    in1,
    in2,
    in3,
    in4,
    in5,
    in6,
    in7,
    in8,
    in9,
    in10,
    at1,
    at2,
    at3,
    at4,
    at5,
    at6,
    at7,
    at8,
    at9,
    at10,
    coproduct1,
    coproduct2,
    coproduct3,
    coproduct4,
    coproduct5,
    coproduct6,
    coproduct7,
    coproduct8,
    coproduct9,
    coproduct10
};
