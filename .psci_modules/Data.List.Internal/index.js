import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Ordering from "../Data.Ordering/index.js";
var Leaf = /* #__PURE__ */ (function () {
    function Leaf() {

    };
    Leaf.value = new Leaf();
    return Leaf;
})();
var Two = /* #__PURE__ */ (function () {
    function Two(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    Two.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new Two(value0, value1, value2);
            };
        };
    };
    return Two;
})();
var Three = /* #__PURE__ */ (function () {
    function Three(value0, value1, value2, value3, value4) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
    };
    Three.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return new Three(value0, value1, value2, value3, value4);
                    };
                };
            };
        };
    };
    return Three;
})();
var TwoLeft = /* #__PURE__ */ (function () {
    function TwoLeft(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TwoLeft.create = function (value0) {
        return function (value1) {
            return new TwoLeft(value0, value1);
        };
    };
    return TwoLeft;
})();
var TwoRight = /* #__PURE__ */ (function () {
    function TwoRight(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TwoRight.create = function (value0) {
        return function (value1) {
            return new TwoRight(value0, value1);
        };
    };
    return TwoRight;
})();
var ThreeLeft = /* #__PURE__ */ (function () {
    function ThreeLeft(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    ThreeLeft.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new ThreeLeft(value0, value1, value2, value3);
                };
            };
        };
    };
    return ThreeLeft;
})();
var ThreeMiddle = /* #__PURE__ */ (function () {
    function ThreeMiddle(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    ThreeMiddle.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new ThreeMiddle(value0, value1, value2, value3);
                };
            };
        };
    };
    return ThreeMiddle;
})();
var ThreeRight = /* #__PURE__ */ (function () {
    function ThreeRight(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    ThreeRight.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new ThreeRight(value0, value1, value2, value3);
                };
            };
        };
    };
    return ThreeRight;
})();
var KickUp = /* #__PURE__ */ (function () {
    function KickUp(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    KickUp.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new KickUp(value0, value1, value2);
            };
        };
    };
    return KickUp;
})();
var fromZipper = function ($copy_v) {
    return function ($copy_tree) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, tree) {
            if (v instanceof Data_List_Types.Nil) {
                $tco_done = true;
                return tree;
            };
            if (v instanceof Data_List_Types.Cons) {
                if (v.value0 instanceof TwoLeft) {
                    $tco_var_v = v.value1;
                    $copy_tree = new Two(tree, v.value0.value0, v.value0.value1);
                    return;
                };
                if (v.value0 instanceof TwoRight) {
                    $tco_var_v = v.value1;
                    $copy_tree = new Two(v.value0.value0, v.value0.value1, tree);
                    return;
                };
                if (v.value0 instanceof ThreeLeft) {
                    $tco_var_v = v.value1;
                    $copy_tree = new Three(tree, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3);
                    return;
                };
                if (v.value0 instanceof ThreeMiddle) {
                    $tco_var_v = v.value1;
                    $copy_tree = new Three(v.value0.value0, v.value0.value1, tree, v.value0.value2, v.value0.value3);
                    return;
                };
                if (v.value0 instanceof ThreeRight) {
                    $tco_var_v = v.value1;
                    $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, tree);
                    return;
                };
                throw new Error("Failed pattern match at Data.List.Internal (line 25, column 3 - line 30, column 76): " + [ v.value0.constructor.name ]);
            };
            throw new Error("Failed pattern match at Data.List.Internal (line 22, column 1 - line 22, column 63): " + [ v.constructor.name, tree.constructor.name ]);
        };
        while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_v, $copy_tree);
        };
        return $tco_result;
    };
};

// | Insert or replace a key/value pair in a map
var insertAndLookupBy = function (comp) {
    return function (k) {
        return function (orig) {
            var up = function ($copy_v) {
                return function ($copy_v1) {
                    var $tco_var_v = $copy_v;
                    var $tco_done = false;
                    var $tco_result;
                    function $tco_loop(v, v1) {
                        if (v instanceof Data_List_Types.Nil) {
                            $tco_done = true;
                            return new Two(v1.value0, v1.value1, v1.value2);
                        };
                        if (v instanceof Data_List_Types.Cons) {
                            if (v.value0 instanceof TwoLeft) {
                                $tco_done = true;
                                return fromZipper(v.value1)(new Three(v1.value0, v1.value1, v1.value2, v.value0.value0, v.value0.value1));
                            };
                            if (v.value0 instanceof TwoRight) {
                                $tco_done = true;
                                return fromZipper(v.value1)(new Three(v.value0.value0, v.value0.value1, v1.value0, v1.value1, v1.value2));
                            };
                            if (v.value0 instanceof ThreeLeft) {
                                $tco_var_v = v.value1;
                                $copy_v1 = new KickUp(new Two(v1.value0, v1.value1, v1.value2), v.value0.value0, new Two(v.value0.value1, v.value0.value2, v.value0.value3));
                                return;
                            };
                            if (v.value0 instanceof ThreeMiddle) {
                                $tco_var_v = v.value1;
                                $copy_v1 = new KickUp(new Two(v.value0.value0, v.value0.value1, v1.value0), v1.value1, new Two(v1.value2, v.value0.value2, v.value0.value3));
                                return;
                            };
                            if (v.value0 instanceof ThreeRight) {
                                $tco_var_v = v.value1;
                                $copy_v1 = new KickUp(new Two(v.value0.value0, v.value0.value1, v.value0.value2), v.value0.value3, new Two(v1.value0, v1.value1, v1.value2));
                                return;
                            };
                            throw new Error("Failed pattern match at Data.List.Internal (line 58, column 5 - line 63, column 90): " + [ v.value0.constructor.name, v1.constructor.name ]);
                        };
                        throw new Error("Failed pattern match at Data.List.Internal (line 55, column 3 - line 55, column 50): " + [ v.constructor.name, v1.constructor.name ]);
                    };
                    while (!$tco_done) {
                        $tco_result = $tco_loop($tco_var_v, $copy_v1);
                    };
                    return $tco_result;
                };
            };
            var down = function ($copy_ctx) {
                return function ($copy_v) {
                    var $tco_var_ctx = $copy_ctx;
                    var $tco_done1 = false;
                    var $tco_result;
                    function $tco_loop(ctx, v) {
                        if (v instanceof Leaf) {
                            $tco_done1 = true;
                            return {
                                found: false,
                                result: up(ctx)(new KickUp(Leaf.value, k, Leaf.value))
                            };
                        };
                        if (v instanceof Two) {
                            var v1 = comp(k)(v.value1);
                            if (v1 instanceof Data_Ordering.EQ) {
                                $tco_done1 = true;
                                return {
                                    found: true,
                                    result: orig
                                };
                            };
                            if (v1 instanceof Data_Ordering.LT) {
                                $tco_var_ctx = new Data_List_Types.Cons(new TwoLeft(v.value1, v.value2), ctx);
                                $copy_v = v.value0;
                                return;
                            };
                            $tco_var_ctx = new Data_List_Types.Cons(new TwoRight(v.value0, v.value1), ctx);
                            $copy_v = v.value2;
                            return;
                        };
                        if (v instanceof Three) {
                            var v1 = comp(k)(v.value1);
                            if (v1 instanceof Data_Ordering.EQ) {
                                $tco_done1 = true;
                                return {
                                    found: true,
                                    result: orig
                                };
                            };
                            var v2 = comp(k)(v.value3);
                            if (v2 instanceof Data_Ordering.EQ) {
                                $tco_done1 = true;
                                return {
                                    found: true,
                                    result: orig
                                };
                            };
                            if (v1 instanceof Data_Ordering.LT) {
                                $tco_var_ctx = new Data_List_Types.Cons(new ThreeLeft(v.value1, v.value2, v.value3, v.value4), ctx);
                                $copy_v = v.value0;
                                return;
                            };
                            if (v1 instanceof Data_Ordering.GT && v2 instanceof Data_Ordering.LT) {
                                $tco_var_ctx = new Data_List_Types.Cons(new ThreeMiddle(v.value0, v.value1, v.value3, v.value4), ctx);
                                $copy_v = v.value2;
                                return;
                            };
                            $tco_var_ctx = new Data_List_Types.Cons(new ThreeRight(v.value0, v.value1, v.value2, v.value3), ctx);
                            $copy_v = v.value4;
                            return;
                        };
                        throw new Error("Failed pattern match at Data.List.Internal (line 38, column 3 - line 38, column 81): " + [ ctx.constructor.name, v.constructor.name ]);
                    };
                    while (!$tco_done1) {
                        $tco_result = $tco_loop($tco_var_ctx, $copy_v);
                    };
                    return $tco_result;
                };
            };
            return down(Data_List_Types.Nil.value)(orig);
        };
    };
};
var emptySet = /* #__PURE__ */ (function () {
    return Leaf.value;
})();
export {
    emptySet,
    insertAndLookupBy
};
