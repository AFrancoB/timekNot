var semigroupoidFn = {
    compose: function (f) {
        return function (g) {
            return function (x) {
                return f(g(x));
            };
        };
    }
};
var compose = function (dict) {
    return dict.compose;
};

// | Forwards composition, or `compose` with its arguments reversed.
var composeFlipped = function (dictSemigroupoid) {
    return function (f) {
        return function (g) {
            return compose(dictSemigroupoid)(g)(f);
        };
    };
};
export {
    compose,
    composeFlipped,
    semigroupoidFn
};
