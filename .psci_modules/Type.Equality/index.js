var To = function (x) {
    return x;
};
var From = function (x) {
    return x;
};
var refl = {
    proof: function (a) {
        return a;
    },
    Coercible0: function () {
        return undefined;
    }
};
var proof = function (dict) {
    return dict.proof;
};
var to = function (dictTypeEquals) {
    var v = proof(dictTypeEquals)(function (a) {
        return a;
    });
    return v;
};
var from = function (dictTypeEquals) {
    var v = proof(dictTypeEquals)(function (a) {
        return a;
    });
    return v;
};
export {
    proof,
    to,
    from,
    refl
};
