// | Utilities for n-eithers: sums types with more than two terms built from nested eithers.
// |
// | Nested eithers arise naturally in sum combinators. You shouldn't
// | represent sum data using nested eithers, but if combinators you're working with
// | create them, utilities in this module will allow to to more easily work
// | with them, including translating to and from more traditional sum types.
// |
// | ```purescript
// | data Color = Red Number | Green Number | Blue Number
// |
// | fromEither3 :: Either3 Number Number Number -> Color
// | fromEither3 = either3 Red Green Blue
// |
// | toEither3 :: Color -> Either3 Number Number Number
// | toEither3 (Red   v) = in1 v
// | toEither3 (Green v) = in2 v
// | toEither3 (Blue  v) = in3 v
// | ```
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Void from "../Data.Void/index.js";
var in9 = function (v) {
    return new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Left(v)))))))));
};
var in8 = function (v) {
    return new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Left(v))))))));
};
var in7 = function (v) {
    return new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Left(v)))))));
};
var in6 = function (v) {
    return new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Left(v))))));
};
var in5 = function (v) {
    return new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Left(v)))));
};
var in4 = function (v) {
    return new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Left(v))));
};
var in3 = function (v) {
    return new Data_Either.Right(new Data_Either.Right(new Data_Either.Left(v)));
};
var in2 = function (v) {
    return new Data_Either.Right(new Data_Either.Left(v));
};
var in10 = function (v) {
    return new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Right(new Data_Either.Left(v))))))))));
};
var in1 = /* #__PURE__ */ (function () {
    return Data_Either.Left.create;
})();
var either9 = function (a) {
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
                                                                            return Data_Void.absurd(y.value0.value0.value0.value0.value0.value0.value0.value0.value0);
                                                                        };
                                                                        throw new Error("Failed pattern match at Data.Either.Nested (line 253, column 29 - line 255, column 40): " + [ y.value0.value0.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                                    };
                                                                    throw new Error("Failed pattern match at Data.Either.Nested (line 251, column 27 - line 255, column 40): " + [ y.value0.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                                };
                                                                throw new Error("Failed pattern match at Data.Either.Nested (line 249, column 25 - line 255, column 40): " + [ y.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                            };
                                                            throw new Error("Failed pattern match at Data.Either.Nested (line 247, column 23 - line 255, column 40): " + [ y.value0.value0.value0.value0.value0.constructor.name ]);
                                                        };
                                                        throw new Error("Failed pattern match at Data.Either.Nested (line 245, column 21 - line 255, column 40): " + [ y.value0.value0.value0.value0.constructor.name ]);
                                                    };
                                                    throw new Error("Failed pattern match at Data.Either.Nested (line 243, column 19 - line 255, column 40): " + [ y.value0.value0.value0.constructor.name ]);
                                                };
                                                throw new Error("Failed pattern match at Data.Either.Nested (line 241, column 17 - line 255, column 40): " + [ y.value0.value0.constructor.name ]);
                                            };
                                            throw new Error("Failed pattern match at Data.Either.Nested (line 239, column 15 - line 255, column 40): " + [ y.value0.constructor.name ]);
                                        };
                                        throw new Error("Failed pattern match at Data.Either.Nested (line 237, column 31 - line 255, column 40): " + [ y.constructor.name ]);
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
var either8 = function (a) {
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
                                                                    return Data_Void.absurd(y.value0.value0.value0.value0.value0.value0.value0.value0);
                                                                };
                                                                throw new Error("Failed pattern match at Data.Either.Nested (line 232, column 27 - line 234, column 38): " + [ y.value0.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                            };
                                                            throw new Error("Failed pattern match at Data.Either.Nested (line 230, column 25 - line 234, column 38): " + [ y.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                        };
                                                        throw new Error("Failed pattern match at Data.Either.Nested (line 228, column 23 - line 234, column 38): " + [ y.value0.value0.value0.value0.value0.constructor.name ]);
                                                    };
                                                    throw new Error("Failed pattern match at Data.Either.Nested (line 226, column 21 - line 234, column 38): " + [ y.value0.value0.value0.value0.constructor.name ]);
                                                };
                                                throw new Error("Failed pattern match at Data.Either.Nested (line 224, column 19 - line 234, column 38): " + [ y.value0.value0.value0.constructor.name ]);
                                            };
                                            throw new Error("Failed pattern match at Data.Either.Nested (line 222, column 17 - line 234, column 38): " + [ y.value0.value0.constructor.name ]);
                                        };
                                        throw new Error("Failed pattern match at Data.Either.Nested (line 220, column 15 - line 234, column 38): " + [ y.value0.constructor.name ]);
                                    };
                                    throw new Error("Failed pattern match at Data.Either.Nested (line 218, column 29 - line 234, column 38): " + [ y.constructor.name ]);
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};
var either7 = function (a) {
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
                                                            return Data_Void.absurd(y.value0.value0.value0.value0.value0.value0.value0);
                                                        };
                                                        throw new Error("Failed pattern match at Data.Either.Nested (line 213, column 25 - line 215, column 36): " + [ y.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                    };
                                                    throw new Error("Failed pattern match at Data.Either.Nested (line 211, column 23 - line 215, column 36): " + [ y.value0.value0.value0.value0.value0.constructor.name ]);
                                                };
                                                throw new Error("Failed pattern match at Data.Either.Nested (line 209, column 21 - line 215, column 36): " + [ y.value0.value0.value0.value0.constructor.name ]);
                                            };
                                            throw new Error("Failed pattern match at Data.Either.Nested (line 207, column 19 - line 215, column 36): " + [ y.value0.value0.value0.constructor.name ]);
                                        };
                                        throw new Error("Failed pattern match at Data.Either.Nested (line 205, column 17 - line 215, column 36): " + [ y.value0.value0.constructor.name ]);
                                    };
                                    throw new Error("Failed pattern match at Data.Either.Nested (line 203, column 15 - line 215, column 36): " + [ y.value0.constructor.name ]);
                                };
                                throw new Error("Failed pattern match at Data.Either.Nested (line 201, column 27 - line 215, column 36): " + [ y.constructor.name ]);
                            };
                        };
                    };
                };
            };
        };
    };
};
var either6 = function (a) {
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
                                                    return Data_Void.absurd(y.value0.value0.value0.value0.value0.value0);
                                                };
                                                throw new Error("Failed pattern match at Data.Either.Nested (line 196, column 23 - line 198, column 34): " + [ y.value0.value0.value0.value0.value0.constructor.name ]);
                                            };
                                            throw new Error("Failed pattern match at Data.Either.Nested (line 194, column 21 - line 198, column 34): " + [ y.value0.value0.value0.value0.constructor.name ]);
                                        };
                                        throw new Error("Failed pattern match at Data.Either.Nested (line 192, column 19 - line 198, column 34): " + [ y.value0.value0.value0.constructor.name ]);
                                    };
                                    throw new Error("Failed pattern match at Data.Either.Nested (line 190, column 17 - line 198, column 34): " + [ y.value0.value0.constructor.name ]);
                                };
                                throw new Error("Failed pattern match at Data.Either.Nested (line 188, column 15 - line 198, column 34): " + [ y.value0.constructor.name ]);
                            };
                            throw new Error("Failed pattern match at Data.Either.Nested (line 186, column 25 - line 198, column 34): " + [ y.constructor.name ]);
                        };
                    };
                };
            };
        };
    };
};
var either5 = function (a) {
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
                                            return Data_Void.absurd(y.value0.value0.value0.value0.value0);
                                        };
                                        throw new Error("Failed pattern match at Data.Either.Nested (line 181, column 21 - line 183, column 32): " + [ y.value0.value0.value0.value0.constructor.name ]);
                                    };
                                    throw new Error("Failed pattern match at Data.Either.Nested (line 179, column 19 - line 183, column 32): " + [ y.value0.value0.value0.constructor.name ]);
                                };
                                throw new Error("Failed pattern match at Data.Either.Nested (line 177, column 17 - line 183, column 32): " + [ y.value0.value0.constructor.name ]);
                            };
                            throw new Error("Failed pattern match at Data.Either.Nested (line 175, column 15 - line 183, column 32): " + [ y.value0.constructor.name ]);
                        };
                        throw new Error("Failed pattern match at Data.Either.Nested (line 173, column 23 - line 183, column 32): " + [ y.constructor.name ]);
                    };
                };
            };
        };
    };
};
var either4 = function (a) {
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
                                    return Data_Void.absurd(y.value0.value0.value0.value0);
                                };
                                throw new Error("Failed pattern match at Data.Either.Nested (line 168, column 19 - line 170, column 30): " + [ y.value0.value0.value0.constructor.name ]);
                            };
                            throw new Error("Failed pattern match at Data.Either.Nested (line 166, column 17 - line 170, column 30): " + [ y.value0.value0.constructor.name ]);
                        };
                        throw new Error("Failed pattern match at Data.Either.Nested (line 164, column 15 - line 170, column 30): " + [ y.value0.constructor.name ]);
                    };
                    throw new Error("Failed pattern match at Data.Either.Nested (line 162, column 21 - line 170, column 30): " + [ y.constructor.name ]);
                };
            };
        };
    };
};
var either3 = function (a) {
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
                            return Data_Void.absurd(y.value0.value0.value0);
                        };
                        throw new Error("Failed pattern match at Data.Either.Nested (line 157, column 17 - line 159, column 28): " + [ y.value0.value0.constructor.name ]);
                    };
                    throw new Error("Failed pattern match at Data.Either.Nested (line 155, column 15 - line 159, column 28): " + [ y.value0.constructor.name ]);
                };
                throw new Error("Failed pattern match at Data.Either.Nested (line 153, column 19 - line 159, column 28): " + [ y.constructor.name ]);
            };
        };
    };
};
var either2 = function (a) {
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
                    return Data_Void.absurd(y.value0.value0);
                };
                throw new Error("Failed pattern match at Data.Either.Nested (line 148, column 15 - line 150, column 26): " + [ y.value0.constructor.name ]);
            };
            throw new Error("Failed pattern match at Data.Either.Nested (line 146, column 17 - line 150, column 26): " + [ y.constructor.name ]);
        };
    };
};
var either10 = function (a) {
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
                                                                                    return Data_Void.absurd(y.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0);
                                                                                };
                                                                                throw new Error("Failed pattern match at Data.Either.Nested (line 276, column 31 - line 278, column 44): " + [ y.value0.value0.value0.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                                            };
                                                                            throw new Error("Failed pattern match at Data.Either.Nested (line 274, column 29 - line 278, column 44): " + [ y.value0.value0.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                                        };
                                                                        throw new Error("Failed pattern match at Data.Either.Nested (line 272, column 27 - line 278, column 44): " + [ y.value0.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                                    };
                                                                    throw new Error("Failed pattern match at Data.Either.Nested (line 270, column 25 - line 278, column 44): " + [ y.value0.value0.value0.value0.value0.value0.constructor.name ]);
                                                                };
                                                                throw new Error("Failed pattern match at Data.Either.Nested (line 268, column 23 - line 278, column 44): " + [ y.value0.value0.value0.value0.value0.constructor.name ]);
                                                            };
                                                            throw new Error("Failed pattern match at Data.Either.Nested (line 266, column 21 - line 278, column 44): " + [ y.value0.value0.value0.value0.constructor.name ]);
                                                        };
                                                        throw new Error("Failed pattern match at Data.Either.Nested (line 264, column 19 - line 278, column 44): " + [ y.value0.value0.value0.constructor.name ]);
                                                    };
                                                    throw new Error("Failed pattern match at Data.Either.Nested (line 262, column 17 - line 278, column 44): " + [ y.value0.value0.constructor.name ]);
                                                };
                                                throw new Error("Failed pattern match at Data.Either.Nested (line 260, column 15 - line 278, column 44): " + [ y.value0.constructor.name ]);
                                            };
                                            throw new Error("Failed pattern match at Data.Either.Nested (line 258, column 34 - line 278, column 44): " + [ y.constructor.name ]);
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
var either1 = function (y) {
    if (y instanceof Data_Either.Left) {
        return y.value0;
    };
    if (y instanceof Data_Either.Right) {
        return Data_Void.absurd(y.value0);
    };
    throw new Error("Failed pattern match at Data.Either.Nested (line 141, column 13 - line 143, column 24): " + [ y.constructor.name ]);
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
    either1,
    either2,
    either3,
    either4,
    either5,
    either6,
    either7,
    either8,
    either9,
    either10
};
