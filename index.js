// output/Control.Semigroupoid/index.js
var semigroupoidFn = {
  compose: function(f) {
    return function(g) {
      return function(x) {
        return f(g(x));
      };
    };
  }
};

// output/Control.Category/index.js
var identity = function(dict) {
  return dict.identity;
};
var categoryFn = {
  identity: function(x) {
    return x;
  },
  Semigroupoid0: function() {
    return semigroupoidFn;
  }
};

// output/Data.Boolean/index.js
var otherwise = true;

// output/Data.Function/index.js
var flip = function(f) {
  return function(b) {
    return function(a) {
      return f(a)(b);
    };
  };
};
var $$const = function(a) {
  return function(v) {
    return a;
  };
};

// output/Data.Functor/foreign.js
var arrayMap = function(f) {
  return function(arr) {
    var l = arr.length;
    var result = new Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(arr[i]);
    }
    return result;
  };
};

// output/Data.Unit/foreign.js
var unit = void 0;

// output/Type.Proxy/index.js
var $$Proxy = /* @__PURE__ */ function() {
  function $$Proxy2() {
  }
  ;
  $$Proxy2.value = new $$Proxy2();
  return $$Proxy2;
}();

// output/Data.Functor/index.js
var map = function(dict) {
  return dict.map;
};
var voidLeft = function(dictFunctor) {
  return function(f) {
    return function(x) {
      return map(dictFunctor)($$const(x))(f);
    };
  };
};
var functorArray = {
  map: arrayMap
};

// output/Control.Apply/index.js
var apply = function(dict) {
  return dict.apply;
};
var applySecond = function(dictApply) {
  return function(a) {
    return function(b) {
      return apply(dictApply)(map(dictApply.Functor0())($$const(identity(categoryFn)))(a))(b);
    };
  };
};

// output/Control.Applicative/index.js
var pure = function(dict) {
  return dict.pure;
};
var liftA1 = function(dictApplicative) {
  return function(f) {
    return function(a) {
      return apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
    };
  };
};

// output/Control.Bind/index.js
var discard = function(dict) {
  return dict.discard;
};
var bind = function(dict) {
  return dict.bind;
};
var bindFlipped = function(dictBind) {
  return flip(bind(dictBind));
};
var discardUnit = {
  discard: function(dictBind) {
    return bind(dictBind);
  }
};
var join = function(dictBind) {
  return function(m) {
    return bind(dictBind)(m)(identity(categoryFn));
  };
};

// output/Data.DateTime/foreign.js
var createUTC = function(y, mo, d, h, m, s, ms) {
  var date2 = new Date(Date.UTC(y, mo, d, h, m, s, ms));
  if (y >= 0 && y < 100) {
    date2.setUTCFullYear(y);
  }
  return date2.getTime();
};
function calcDiff(rec1, rec2) {
  var msUTC1 = createUTC(rec1.year, rec1.month - 1, rec1.day, rec1.hour, rec1.minute, rec1.second, rec1.millisecond);
  var msUTC2 = createUTC(rec2.year, rec2.month - 1, rec2.day, rec2.hour, rec2.minute, rec2.second, rec2.millisecond);
  return msUTC1 - msUTC2;
}
function adjustImpl(just) {
  return function(nothing) {
    return function(offset) {
      return function(rec) {
        var msUTC = createUTC(rec.year, rec.month - 1, rec.day, rec.hour, rec.minute, rec.second, rec.millisecond);
        var dt = new Date(msUTC + offset);
        return isNaN(dt.getTime()) ? nothing : just({
          year: dt.getUTCFullYear(),
          month: dt.getUTCMonth() + 1,
          day: dt.getUTCDate(),
          hour: dt.getUTCHours(),
          minute: dt.getUTCMinutes(),
          second: dt.getUTCSeconds(),
          millisecond: dt.getUTCMilliseconds()
        });
      };
    };
  };
}

// output/Data.Bounded/foreign.js
var topInt = 2147483647;
var bottomInt = -2147483648;
var topChar = String.fromCharCode(65535);
var bottomChar = String.fromCharCode(0);
var topNumber = Number.POSITIVE_INFINITY;
var bottomNumber = Number.NEGATIVE_INFINITY;

// output/Data.Ord/foreign.js
var unsafeCompareImpl = function(lt) {
  return function(eq2) {
    return function(gt) {
      return function(x) {
        return function(y) {
          return x < y ? lt : x === y ? eq2 : gt;
        };
      };
    };
  };
};
var ordIntImpl = unsafeCompareImpl;
var ordCharImpl = unsafeCompareImpl;

// output/Data.Eq/foreign.js
var refEq = function(r1) {
  return function(r2) {
    return r1 === r2;
  };
};
var eqIntImpl = refEq;
var eqNumberImpl = refEq;
var eqCharImpl = refEq;

// output/Data.Symbol/index.js
var reflectSymbol = function(dict) {
  return dict.reflectSymbol;
};

// output/Record.Unsafe/foreign.js
var unsafeGet = function(label) {
  return function(rec) {
    return rec[label];
  };
};

// output/Data.Eq/index.js
var eqNumber = {
  eq: eqNumberImpl
};
var eqInt = {
  eq: eqIntImpl
};
var eqChar = {
  eq: eqCharImpl
};
var eq1 = function(dict) {
  return dict.eq1;
};
var eq = function(dict) {
  return dict.eq;
};

// output/Data.Ordering/index.js
var LT = /* @__PURE__ */ function() {
  function LT2() {
  }
  ;
  LT2.value = new LT2();
  return LT2;
}();
var GT = /* @__PURE__ */ function() {
  function GT2() {
  }
  ;
  GT2.value = new GT2();
  return GT2;
}();
var EQ = /* @__PURE__ */ function() {
  function EQ2() {
  }
  ;
  EQ2.value = new EQ2();
  return EQ2;
}();

// output/Data.Ring/foreign.js
var intSub = function(x) {
  return function(y) {
    return x - y | 0;
  };
};

// output/Data.Semiring/foreign.js
var intAdd = function(x) {
  return function(y) {
    return x + y | 0;
  };
};
var intMul = function(x) {
  return function(y) {
    return x * y | 0;
  };
};

// output/Data.Semiring/index.js
var zero = function(dict) {
  return dict.zero;
};
var semiringInt = {
  add: intAdd,
  zero: 0,
  mul: intMul,
  one: 1
};
var one = function(dict) {
  return dict.one;
};
var mul = function(dict) {
  return dict.mul;
};
var add = function(dict) {
  return dict.add;
};

// output/Data.Ring/index.js
var sub = function(dict) {
  return dict.sub;
};
var ringInt = {
  sub: intSub,
  Semiring0: function() {
    return semiringInt;
  }
};
var negate = function(dictRing) {
  return function(a) {
    return sub(dictRing)(zero(dictRing.Semiring0()))(a);
  };
};

// output/Data.Ord/index.js
var ordInt = /* @__PURE__ */ function() {
  return {
    compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqInt;
    }
  };
}();
var ordChar = /* @__PURE__ */ function() {
  return {
    compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqChar;
    }
  };
}();
var compare = function(dict) {
  return dict.compare;
};
var greaterThan = function(dictOrd) {
  return function(a1) {
    return function(a2) {
      var v = compare(dictOrd)(a1)(a2);
      if (v instanceof GT) {
        return true;
      }
      ;
      return false;
    };
  };
};
var greaterThanOrEq = function(dictOrd) {
  return function(a1) {
    return function(a2) {
      var v = compare(dictOrd)(a1)(a2);
      if (v instanceof LT) {
        return false;
      }
      ;
      return true;
    };
  };
};
var lessThan = function(dictOrd) {
  return function(a1) {
    return function(a2) {
      var v = compare(dictOrd)(a1)(a2);
      if (v instanceof LT) {
        return true;
      }
      ;
      return false;
    };
  };
};
var signum = function(dictOrd) {
  return function(dictRing) {
    return function(x) {
      var $47 = lessThan(dictOrd)(x)(zero(dictRing.Semiring0()));
      if ($47) {
        return negate(dictRing)(one(dictRing.Semiring0()));
      }
      ;
      var $48 = greaterThan(dictOrd)(x)(zero(dictRing.Semiring0()));
      if ($48) {
        return one(dictRing.Semiring0());
      }
      ;
      return x;
    };
  };
};
var abs = function(dictOrd) {
  return function(dictRing) {
    return function(x) {
      var $57 = greaterThanOrEq(dictOrd)(x)(zero(dictRing.Semiring0()));
      if ($57) {
        return x;
      }
      ;
      return negate(dictRing)(x);
    };
  };
};

// output/Data.Bounded/index.js
var top = function(dict) {
  return dict.top;
};
var boundedInt = {
  top: topInt,
  bottom: bottomInt,
  Ord0: function() {
    return ordInt;
  }
};
var boundedChar = {
  top: topChar,
  bottom: bottomChar,
  Ord0: function() {
    return ordChar;
  }
};
var bottom = function(dict) {
  return dict.bottom;
};

// output/Data.Date/foreign.js
var createDate = function(y, m, d) {
  var date2 = new Date(Date.UTC(y, m, d));
  if (y >= 0 && y < 100) {
    date2.setUTCFullYear(y);
  }
  return date2;
};
function canonicalDateImpl(ctor, y, m, d) {
  var date2 = createDate(y, m - 1, d);
  return ctor(date2.getUTCFullYear())(date2.getUTCMonth() + 1)(date2.getUTCDate());
}

// output/Data.Enum/foreign.js
function toCharCode(c) {
  return c.charCodeAt(0);
}
function fromCharCode(c) {
  return String.fromCharCode(c);
}

// output/Data.Semigroup/index.js
var append = function(dict) {
  return dict.append;
};

// output/Control.Alt/index.js
var alt = function(dict) {
  return dict.alt;
};

// output/Control.Plus/index.js
var empty = function(dict) {
  return dict.empty;
};

// output/Data.Show/foreign.js
var showIntImpl = function(n) {
  return n.toString();
};
var showNumberImpl = function(n) {
  var str = n.toString();
  return isNaN(str + ".0") ? str : str + ".0";
};
var showCharImpl = function(c) {
  var code = c.charCodeAt(0);
  if (code < 32 || code === 127) {
    switch (c) {
      case "\x07":
        return "'\\a'";
      case "\b":
        return "'\\b'";
      case "\f":
        return "'\\f'";
      case "\n":
        return "'\\n'";
      case "\r":
        return "'\\r'";
      case "	":
        return "'\\t'";
      case "\v":
        return "'\\v'";
    }
    return "'\\" + code.toString(10) + "'";
  }
  return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
};
var showStringImpl = function(s) {
  var l = s.length;
  return '"' + s.replace(/[\0-\x1F\x7F"\\]/g, function(c, i) {
    switch (c) {
      case '"':
      case "\\":
        return "\\" + c;
      case "\x07":
        return "\\a";
      case "\b":
        return "\\b";
      case "\f":
        return "\\f";
      case "\n":
        return "\\n";
      case "\r":
        return "\\r";
      case "	":
        return "\\t";
      case "\v":
        return "\\v";
    }
    var k = i + 1;
    var empty3 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
    return "\\" + c.charCodeAt(0).toString(10) + empty3;
  }) + '"';
};
var showArrayImpl = function(f) {
  return function(xs) {
    var ss = [];
    for (var i = 0, l = xs.length; i < l; i++) {
      ss[i] = f(xs[i]);
    }
    return "[" + ss.join(",") + "]";
  };
};
var cons = function(head4) {
  return function(tail2) {
    return [head4].concat(tail2);
  };
};
var intercalate = function(separator) {
  return function(xs) {
    return xs.join(separator);
  };
};

// output/Data.Show/index.js
var showString = {
  show: showStringImpl
};
var showRecordFieldsNil = {
  showRecordFields: function(v) {
    return function(v1) {
      return [];
    };
  }
};
var showRecordFields = function(dict) {
  return dict.showRecordFields;
};
var showRecord = function() {
  return function() {
    return function(dictShowRecordFields) {
      return {
        show: function(record) {
          var v = showRecordFields(dictShowRecordFields)($$Proxy.value)(record);
          if (v.length === 0) {
            return "{}";
          }
          ;
          return intercalate(" ")(["{", intercalate(", ")(v), "}"]);
        }
      };
    };
  };
};
var showNumber = {
  show: showNumberImpl
};
var showInt = {
  show: showIntImpl
};
var showChar = {
  show: showCharImpl
};
var showBoolean = {
  show: function(v) {
    if (v) {
      return "true";
    }
    ;
    if (!v) {
      return "false";
    }
    ;
    throw new Error("Failed pattern match at Data.Show (line 23, column 1 - line 25, column 23): " + [v.constructor.name]);
  }
};
var show = function(dict) {
  return dict.show;
};
var showArray = function(dictShow) {
  return {
    show: showArrayImpl(show(dictShow))
  };
};
var showRecordFieldsCons = function(dictIsSymbol) {
  return function(dictShowRecordFields) {
    return function(dictShow) {
      return {
        showRecordFields: function(v) {
          return function(record) {
            var tail2 = showRecordFields(dictShowRecordFields)($$Proxy.value)(record);
            var key = reflectSymbol(dictIsSymbol)($$Proxy.value);
            var focus = unsafeGet(key)(record);
            return cons(intercalate(": ")([key, show(dictShow)(focus)]))(tail2);
          };
        }
      };
    };
  };
};

// output/Data.Maybe/index.js
var Nothing = /* @__PURE__ */ function() {
  function Nothing2() {
  }
  ;
  Nothing2.value = new Nothing2();
  return Nothing2;
}();
var Just = /* @__PURE__ */ function() {
  function Just2(value0) {
    this.value0 = value0;
  }
  ;
  Just2.create = function(value0) {
    return new Just2(value0);
  };
  return Just2;
}();
var maybe = function(v) {
  return function(v1) {
    return function(v2) {
      if (v2 instanceof Nothing) {
        return v;
      }
      ;
      if (v2 instanceof Just) {
        return v1(v2.value0);
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
    };
  };
};
var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
var functorMaybe = {
  map: function(v) {
    return function(v1) {
      if (v1 instanceof Just) {
        return new Just(v(v1.value0));
      }
      ;
      return Nothing.value;
    };
  }
};
var fromMaybe = function(a) {
  return maybe(a)(identity(categoryFn));
};
var fromJust = function() {
  return function(v) {
    if (v instanceof Just) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
  };
};
var applyMaybe = {
  apply: function(v) {
    return function(v1) {
      if (v instanceof Just) {
        return map(functorMaybe)(v.value0)(v1);
      }
      ;
      if (v instanceof Nothing) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Functor0: function() {
    return functorMaybe;
  }
};
var bindMaybe = {
  bind: function(v) {
    return function(v1) {
      if (v instanceof Just) {
        return v1(v.value0);
      }
      ;
      if (v instanceof Nothing) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Apply0: function() {
    return applyMaybe;
  }
};

// output/Data.Either/index.js
var Left = /* @__PURE__ */ function() {
  function Left2(value0) {
    this.value0 = value0;
  }
  ;
  Left2.create = function(value0) {
    return new Left2(value0);
  };
  return Left2;
}();
var Right = /* @__PURE__ */ function() {
  function Right2(value0) {
    this.value0 = value0;
  }
  ;
  Right2.create = function(value0) {
    return new Right2(value0);
  };
  return Right2;
}();

// output/Control.Lazy/index.js
var $runtime_lazy = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var defer = function(dict) {
  return dict.defer;
};
var fix = function(dictLazy) {
  return function(f) {
    var $lazy_go = $runtime_lazy("go", "Control.Lazy", function() {
      return defer(dictLazy)(function(v) {
        return f($lazy_go(25));
      });
    });
    var go = $lazy_go(25);
    return go;
  };
};

// output/Data.EuclideanRing/foreign.js
var intDegree = function(x) {
  return Math.min(Math.abs(x), 2147483647);
};
var intDiv = function(x) {
  return function(y) {
    if (y === 0)
      return 0;
    return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
  };
};
var intMod = function(x) {
  return function(y) {
    if (y === 0)
      return 0;
    var yy = Math.abs(y);
    return (x % yy + yy) % yy;
  };
};

// output/Data.CommutativeRing/index.js
var commutativeRingInt = {
  Ring0: function() {
    return ringInt;
  }
};

// output/Data.EuclideanRing/index.js
var mod = function(dict) {
  return dict.mod;
};
var gcd = function($copy_dictEq) {
  return function($copy_dictEuclideanRing) {
    return function($copy_a) {
      return function($copy_b) {
        var $tco_var_dictEq = $copy_dictEq;
        var $tco_var_dictEuclideanRing = $copy_dictEuclideanRing;
        var $tco_var_a = $copy_a;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(dictEq, dictEuclideanRing, a, b) {
          var $8 = eq(dictEq)(b)(zero(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0()));
          if ($8) {
            $tco_done = true;
            return a;
          }
          ;
          $tco_var_dictEq = dictEq;
          $tco_var_dictEuclideanRing = dictEuclideanRing;
          $tco_var_a = b;
          $copy_b = mod(dictEuclideanRing)(a)(b);
          return;
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictEq, $tco_var_dictEuclideanRing, $tco_var_a, $copy_b);
        }
        ;
        return $tco_result;
      };
    };
  };
};
var euclideanRingInt = {
  degree: intDegree,
  div: intDiv,
  mod: intMod,
  CommutativeRing0: function() {
    return commutativeRingInt;
  }
};
var div = function(dict) {
  return dict.div;
};

// output/Data.Monoid/index.js
var mempty = function(dict) {
  return dict.mempty;
};

// output/Data.Tuple/index.js
var Tuple = /* @__PURE__ */ function() {
  function Tuple2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Tuple2.create = function(value0) {
    return function(value1) {
      return new Tuple2(value0, value1);
    };
  };
  return Tuple2;
}();
var snd = function(v) {
  return v.value1;
};
var fst = function(v) {
  return v.value0;
};

// output/Data.Unfoldable/foreign.js
var unfoldrArrayImpl = function(isNothing2) {
  return function(fromJust2) {
    return function(fst2) {
      return function(snd2) {
        return function(f) {
          return function(b) {
            var result = [];
            var value = b;
            while (true) {
              var maybe2 = f(value);
              if (isNothing2(maybe2))
                return result;
              var tuple = fromJust2(maybe2);
              result.push(fst2(tuple));
              value = snd2(tuple);
            }
          };
        };
      };
    };
  };
};

// output/Data.Traversable/foreign.js
var traverseArrayImpl = function() {
  function array1(a) {
    return [a];
  }
  function array2(a) {
    return function(b) {
      return [a, b];
    };
  }
  function array3(a) {
    return function(b) {
      return function(c) {
        return [a, b, c];
      };
    };
  }
  function concat22(xs) {
    return function(ys) {
      return xs.concat(ys);
    };
  }
  return function(apply2) {
    return function(map2) {
      return function(pure2) {
        return function(f) {
          return function(array) {
            function go(bot, top2) {
              switch (top2 - bot) {
                case 0:
                  return pure2([]);
                case 1:
                  return map2(array1)(f(array[bot]));
                case 2:
                  return apply2(map2(array2)(f(array[bot])))(f(array[bot + 1]));
                case 3:
                  return apply2(apply2(map2(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                default:
                  var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                  return apply2(map2(concat22)(go(bot, pivot)))(go(pivot, top2));
              }
            }
            return go(0, array.length);
          };
        };
      };
    };
  };
}();

// output/Data.Foldable/foreign.js
var foldrArray = function(f) {
  return function(init3) {
    return function(xs) {
      var acc = init3;
      var len = xs.length;
      for (var i = len - 1; i >= 0; i--) {
        acc = f(xs[i])(acc);
      }
      return acc;
    };
  };
};
var foldlArray = function(f) {
  return function(init3) {
    return function(xs) {
      var acc = init3;
      var len = xs.length;
      for (var i = 0; i < len; i++) {
        acc = f(acc)(xs[i]);
      }
      return acc;
    };
  };
};

// output/Data.Bifunctor/index.js
var bimap = function(dict) {
  return dict.bimap;
};

// output/Unsafe.Coerce/foreign.js
var unsafeCoerce2 = function(x) {
  return x;
};

// output/Safe.Coerce/index.js
var coerce = function() {
  return unsafeCoerce2;
};

// output/Data.Newtype/index.js
var unwrap = coerce;
var over = function() {
  return function() {
    return function(v) {
      return coerce();
    };
  };
};

// output/Data.Foldable/index.js
var foldr = function(dict) {
  return dict.foldr;
};
var foldl = function(dict) {
  return dict.foldl;
};
var foldMapDefaultR = function(dictFoldable) {
  return function(dictMonoid) {
    return function(f) {
      return foldr(dictFoldable)(function(x) {
        return function(acc) {
          return append(dictMonoid.Semigroup0())(f(x))(acc);
        };
      })(mempty(dictMonoid));
    };
  };
};
var foldableArray = {
  foldr: foldrArray,
  foldl: foldlArray,
  foldMap: function(dictMonoid) {
    return foldMapDefaultR(foldableArray)(dictMonoid);
  }
};
var foldMap = function(dict) {
  return dict.foldMap;
};

// output/Data.Identity/index.js
var Identity = function(x) {
  return x;
};
var functorIdentity = {
  map: function(f) {
    return function(m) {
      return f(m);
    };
  }
};
var applyIdentity = {
  apply: function(v) {
    return function(v1) {
      return v(v1);
    };
  },
  Functor0: function() {
    return functorIdentity;
  }
};
var bindIdentity = {
  bind: function(v) {
    return function(f) {
      return f(v);
    };
  },
  Apply0: function() {
    return applyIdentity;
  }
};
var applicativeIdentity = {
  pure: Identity,
  Apply0: function() {
    return applyIdentity;
  }
};
var monadIdentity = {
  Applicative0: function() {
    return applicativeIdentity;
  },
  Bind1: function() {
    return bindIdentity;
  }
};

// output/Data.Unfoldable1/foreign.js
var unfoldr1ArrayImpl = function(isNothing2) {
  return function(fromJust2) {
    return function(fst2) {
      return function(snd2) {
        return function(f) {
          return function(b) {
            var result = [];
            var value = b;
            while (true) {
              var tuple = f(value);
              result.push(fst2(tuple));
              var maybe2 = snd2(tuple);
              if (isNothing2(maybe2))
                return result;
              value = fromJust2(maybe2);
            }
          };
        };
      };
    };
  };
};

// output/Data.Unfoldable1/index.js
var unfoldable1Array = {
  unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(/* @__PURE__ */ fromJust())(fst)(snd)
};

// output/Data.Unfoldable/index.js
var unfoldr = function(dict) {
  return dict.unfoldr;
};
var unfoldableArray = {
  unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(/* @__PURE__ */ fromJust())(fst)(snd),
  Unfoldable10: function() {
    return unfoldable1Array;
  }
};

// output/Data.Enum/index.js
var toEnum = function(dict) {
  return dict.toEnum;
};
var fromEnum = function(dict) {
  return dict.fromEnum;
};
var defaultSucc = function(toEnum$prime) {
  return function(fromEnum$prime) {
    return function(a) {
      return toEnum$prime(fromEnum$prime(a) + 1 | 0);
    };
  };
};
var defaultPred = function(toEnum$prime) {
  return function(fromEnum$prime) {
    return function(a) {
      return toEnum$prime(fromEnum$prime(a) - 1 | 0);
    };
  };
};
var charToEnum = function(v) {
  if (v >= bottom(boundedInt) && v <= top(boundedInt)) {
    return new Just(fromCharCode(v));
  }
  ;
  return Nothing.value;
};
var enumChar = {
  succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
  pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
  Ord0: function() {
    return ordChar;
  }
};
var boundedEnumChar = /* @__PURE__ */ function() {
  return {
    cardinality: toCharCode(top(boundedChar)) - toCharCode(bottom(boundedChar)) | 0,
    toEnum: charToEnum,
    fromEnum: toCharCode,
    Bounded0: function() {
      return boundedChar;
    },
    Enum1: function() {
      return enumChar;
    }
  };
}();

// output/Data.Date.Component/index.js
var $runtime_lazy2 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var January = /* @__PURE__ */ function() {
  function January2() {
  }
  ;
  January2.value = new January2();
  return January2;
}();
var February = /* @__PURE__ */ function() {
  function February2() {
  }
  ;
  February2.value = new February2();
  return February2;
}();
var March = /* @__PURE__ */ function() {
  function March2() {
  }
  ;
  March2.value = new March2();
  return March2;
}();
var April = /* @__PURE__ */ function() {
  function April2() {
  }
  ;
  April2.value = new April2();
  return April2;
}();
var May = /* @__PURE__ */ function() {
  function May2() {
  }
  ;
  May2.value = new May2();
  return May2;
}();
var June = /* @__PURE__ */ function() {
  function June2() {
  }
  ;
  June2.value = new June2();
  return June2;
}();
var July = /* @__PURE__ */ function() {
  function July2() {
  }
  ;
  July2.value = new July2();
  return July2;
}();
var August = /* @__PURE__ */ function() {
  function August2() {
  }
  ;
  August2.value = new August2();
  return August2;
}();
var September = /* @__PURE__ */ function() {
  function September2() {
  }
  ;
  September2.value = new September2();
  return September2;
}();
var October = /* @__PURE__ */ function() {
  function October2() {
  }
  ;
  October2.value = new October2();
  return October2;
}();
var November = /* @__PURE__ */ function() {
  function November2() {
  }
  ;
  November2.value = new November2();
  return November2;
}();
var December = /* @__PURE__ */ function() {
  function December2() {
  }
  ;
  December2.value = new December2();
  return December2;
}();
var showYear = {
  show: function(v) {
    return "(Year " + (show(showInt)(v) + ")");
  }
};
var showMonth = {
  show: function(v) {
    if (v instanceof January) {
      return "January";
    }
    ;
    if (v instanceof February) {
      return "February";
    }
    ;
    if (v instanceof March) {
      return "March";
    }
    ;
    if (v instanceof April) {
      return "April";
    }
    ;
    if (v instanceof May) {
      return "May";
    }
    ;
    if (v instanceof June) {
      return "June";
    }
    ;
    if (v instanceof July) {
      return "July";
    }
    ;
    if (v instanceof August) {
      return "August";
    }
    ;
    if (v instanceof September) {
      return "September";
    }
    ;
    if (v instanceof October) {
      return "October";
    }
    ;
    if (v instanceof November) {
      return "November";
    }
    ;
    if (v instanceof December) {
      return "December";
    }
    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 101, column 1 - line 113, column 29): " + [v.constructor.name]);
  }
};
var showDay = {
  show: function(v) {
    return "(Day " + (show(showInt)(v) + ")");
  }
};
var ordYear = ordInt;
var ordDay = ordInt;
var eqYear = eqInt;
var eqMonth = {
  eq: function(x) {
    return function(y) {
      if (x instanceof January && y instanceof January) {
        return true;
      }
      ;
      if (x instanceof February && y instanceof February) {
        return true;
      }
      ;
      if (x instanceof March && y instanceof March) {
        return true;
      }
      ;
      if (x instanceof April && y instanceof April) {
        return true;
      }
      ;
      if (x instanceof May && y instanceof May) {
        return true;
      }
      ;
      if (x instanceof June && y instanceof June) {
        return true;
      }
      ;
      if (x instanceof July && y instanceof July) {
        return true;
      }
      ;
      if (x instanceof August && y instanceof August) {
        return true;
      }
      ;
      if (x instanceof September && y instanceof September) {
        return true;
      }
      ;
      if (x instanceof October && y instanceof October) {
        return true;
      }
      ;
      if (x instanceof November && y instanceof November) {
        return true;
      }
      ;
      if (x instanceof December && y instanceof December) {
        return true;
      }
      ;
      return false;
    };
  }
};
var ordMonth = {
  compare: function(x) {
    return function(y) {
      if (x instanceof January && y instanceof January) {
        return EQ.value;
      }
      ;
      if (x instanceof January) {
        return LT.value;
      }
      ;
      if (y instanceof January) {
        return GT.value;
      }
      ;
      if (x instanceof February && y instanceof February) {
        return EQ.value;
      }
      ;
      if (x instanceof February) {
        return LT.value;
      }
      ;
      if (y instanceof February) {
        return GT.value;
      }
      ;
      if (x instanceof March && y instanceof March) {
        return EQ.value;
      }
      ;
      if (x instanceof March) {
        return LT.value;
      }
      ;
      if (y instanceof March) {
        return GT.value;
      }
      ;
      if (x instanceof April && y instanceof April) {
        return EQ.value;
      }
      ;
      if (x instanceof April) {
        return LT.value;
      }
      ;
      if (y instanceof April) {
        return GT.value;
      }
      ;
      if (x instanceof May && y instanceof May) {
        return EQ.value;
      }
      ;
      if (x instanceof May) {
        return LT.value;
      }
      ;
      if (y instanceof May) {
        return GT.value;
      }
      ;
      if (x instanceof June && y instanceof June) {
        return EQ.value;
      }
      ;
      if (x instanceof June) {
        return LT.value;
      }
      ;
      if (y instanceof June) {
        return GT.value;
      }
      ;
      if (x instanceof July && y instanceof July) {
        return EQ.value;
      }
      ;
      if (x instanceof July) {
        return LT.value;
      }
      ;
      if (y instanceof July) {
        return GT.value;
      }
      ;
      if (x instanceof August && y instanceof August) {
        return EQ.value;
      }
      ;
      if (x instanceof August) {
        return LT.value;
      }
      ;
      if (y instanceof August) {
        return GT.value;
      }
      ;
      if (x instanceof September && y instanceof September) {
        return EQ.value;
      }
      ;
      if (x instanceof September) {
        return LT.value;
      }
      ;
      if (y instanceof September) {
        return GT.value;
      }
      ;
      if (x instanceof October && y instanceof October) {
        return EQ.value;
      }
      ;
      if (x instanceof October) {
        return LT.value;
      }
      ;
      if (y instanceof October) {
        return GT.value;
      }
      ;
      if (x instanceof November && y instanceof November) {
        return EQ.value;
      }
      ;
      if (x instanceof November) {
        return LT.value;
      }
      ;
      if (y instanceof November) {
        return GT.value;
      }
      ;
      if (x instanceof December && y instanceof December) {
        return EQ.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
    };
  },
  Eq0: function() {
    return eqMonth;
  }
};
var eqDay = eqInt;
var boundedYear = /* @__PURE__ */ function() {
  return {
    bottom: -271820 | 0,
    top: 275759,
    Ord0: function() {
      return ordYear;
    }
  };
}();
var boundedMonth = /* @__PURE__ */ function() {
  return {
    bottom: January.value,
    top: December.value,
    Ord0: function() {
      return ordMonth;
    }
  };
}();
var boundedEnumYear = {
  cardinality: 547580,
  toEnum: function(n) {
    if (n >= (-271820 | 0) && n <= 275759) {
      return new Just(n);
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 35, column 1 - line 40, column 24): " + [n.constructor.name]);
  },
  fromEnum: function(v) {
    return v;
  },
  Bounded0: function() {
    return boundedYear;
  },
  Enum1: function() {
    return $lazy_enumYear(0);
  }
};
var $lazy_enumYear = /* @__PURE__ */ $runtime_lazy2("enumYear", "Data.Date.Component", function() {
  return {
    succ: function() {
      var $46 = toEnum(boundedEnumYear);
      var $47 = fromEnum(boundedEnumYear);
      return function($48) {
        return $46(function(v) {
          return v + 1 | 0;
        }($47($48)));
      };
    }(),
    pred: function() {
      var $49 = toEnum(boundedEnumYear);
      var $50 = fromEnum(boundedEnumYear);
      return function($51) {
        return $49(function(v) {
          return v - 1 | 0;
        }($50($51)));
      };
    }(),
    Ord0: function() {
      return ordYear;
    }
  };
});
var boundedEnumMonth = {
  cardinality: 12,
  toEnum: function(v) {
    if (v === 1) {
      return new Just(January.value);
    }
    ;
    if (v === 2) {
      return new Just(February.value);
    }
    ;
    if (v === 3) {
      return new Just(March.value);
    }
    ;
    if (v === 4) {
      return new Just(April.value);
    }
    ;
    if (v === 5) {
      return new Just(May.value);
    }
    ;
    if (v === 6) {
      return new Just(June.value);
    }
    ;
    if (v === 7) {
      return new Just(July.value);
    }
    ;
    if (v === 8) {
      return new Just(August.value);
    }
    ;
    if (v === 9) {
      return new Just(September.value);
    }
    ;
    if (v === 10) {
      return new Just(October.value);
    }
    ;
    if (v === 11) {
      return new Just(November.value);
    }
    ;
    if (v === 12) {
      return new Just(December.value);
    }
    ;
    return Nothing.value;
  },
  fromEnum: function(v) {
    if (v instanceof January) {
      return 1;
    }
    ;
    if (v instanceof February) {
      return 2;
    }
    ;
    if (v instanceof March) {
      return 3;
    }
    ;
    if (v instanceof April) {
      return 4;
    }
    ;
    if (v instanceof May) {
      return 5;
    }
    ;
    if (v instanceof June) {
      return 6;
    }
    ;
    if (v instanceof July) {
      return 7;
    }
    ;
    if (v instanceof August) {
      return 8;
    }
    ;
    if (v instanceof September) {
      return 9;
    }
    ;
    if (v instanceof October) {
      return 10;
    }
    ;
    if (v instanceof November) {
      return 11;
    }
    ;
    if (v instanceof December) {
      return 12;
    }
    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 87, column 14 - line 99, column 19): " + [v.constructor.name]);
  },
  Bounded0: function() {
    return boundedMonth;
  },
  Enum1: function() {
    return $lazy_enumMonth(0);
  }
};
var $lazy_enumMonth = /* @__PURE__ */ $runtime_lazy2("enumMonth", "Data.Date.Component", function() {
  return {
    succ: function() {
      var $58 = toEnum(boundedEnumMonth);
      var $59 = fromEnum(boundedEnumMonth);
      return function($60) {
        return $58(function(v) {
          return v + 1 | 0;
        }($59($60)));
      };
    }(),
    pred: function() {
      var $61 = toEnum(boundedEnumMonth);
      var $62 = fromEnum(boundedEnumMonth);
      return function($63) {
        return $61(function(v) {
          return v - 1 | 0;
        }($62($63)));
      };
    }(),
    Ord0: function() {
      return ordMonth;
    }
  };
});
var boundedDay = {
  bottom: 1,
  top: 31,
  Ord0: function() {
    return ordDay;
  }
};
var boundedEnumDay = {
  cardinality: 31,
  toEnum: function(n) {
    if (n >= 1 && n <= 31) {
      return new Just(n);
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.Date.Component (line 133, column 1 - line 138, column 23): " + [n.constructor.name]);
  },
  fromEnum: function(v) {
    return v;
  },
  Bounded0: function() {
    return boundedDay;
  },
  Enum1: function() {
    return $lazy_enumDay(0);
  }
};
var $lazy_enumDay = /* @__PURE__ */ $runtime_lazy2("enumDay", "Data.Date.Component", function() {
  return {
    succ: function() {
      var $64 = toEnum(boundedEnumDay);
      var $65 = fromEnum(boundedEnumDay);
      return function($66) {
        return $64(function(v) {
          return v + 1 | 0;
        }($65($66)));
      };
    }(),
    pred: function() {
      var $67 = toEnum(boundedEnumDay);
      var $68 = fromEnum(boundedEnumDay);
      return function($69) {
        return $67(function(v) {
          return v - 1 | 0;
        }($68($69)));
      };
    }(),
    Ord0: function() {
      return ordDay;
    }
  };
});

// output/Data.Int/foreign.js
var fromNumberImpl = function(just) {
  return function(nothing) {
    return function(n) {
      return (n | 0) === n ? just(n) : nothing;
    };
  };
};
var toNumber = function(n) {
  return n;
};

// output/Data.Number/foreign.js
var isFiniteImpl = isFinite;
var floor = Math.floor;
var round = Math.round;

// output/Data.Int/index.js
var fromNumber = /* @__PURE__ */ function() {
  return fromNumberImpl(Just.create)(Nothing.value);
}();
var unsafeClamp = function(x) {
  if (!isFiniteImpl(x)) {
    return 0;
  }
  ;
  if (x >= toNumber(top(boundedInt))) {
    return top(boundedInt);
  }
  ;
  if (x <= toNumber(bottom(boundedInt))) {
    return bottom(boundedInt);
  }
  ;
  if (otherwise) {
    return fromMaybe(0)(fromNumber(x));
  }
  ;
  throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
};
var round2 = function($23) {
  return unsafeClamp(round($23));
};
var floor2 = function($25) {
  return unsafeClamp(floor($25));
};

// output/Data.Time.Duration/index.js
var Seconds = function(x) {
  return x;
};
var Milliseconds = function(x) {
  return x;
};
var toDuration = function(dict) {
  return dict.toDuration;
};
var fromDuration = function(dict) {
  return dict.fromDuration;
};
var durationSeconds = {
  fromDuration: /* @__PURE__ */ over()()(Seconds)(function(v) {
    return v * 1e3;
  }),
  toDuration: /* @__PURE__ */ over()()(Milliseconds)(function(v) {
    return v / 1e3;
  })
};
var durationMilliseconds = {
  fromDuration: /* @__PURE__ */ identity(categoryFn),
  toDuration: /* @__PURE__ */ identity(categoryFn)
};

// output/Data.Date/index.js
var $$Date = /* @__PURE__ */ function() {
  function $$Date2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  $$Date2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new $$Date2(value0, value1, value2);
      };
    };
  };
  return $$Date2;
}();
var year = function(v) {
  return v.value0;
};
var showDate = {
  show: function(v) {
    return "(Date " + (show(showYear)(v.value0) + (" " + (show(showMonth)(v.value1) + (" " + (show(showDay)(v.value2) + ")")))));
  }
};
var month = function(v) {
  return v.value1;
};
var eqDate = {
  eq: function(x) {
    return function(y) {
      return eq(eqYear)(x.value0)(y.value0) && eq(eqMonth)(x.value1)(y.value1) && eq(eqDay)(x.value2)(y.value2);
    };
  }
};
var day = function(v) {
  return v.value2;
};
var canonicalDate = function(y) {
  return function(m) {
    return function(d) {
      var mkDate = function(y$prime) {
        return function(m$prime) {
          return function(d$prime) {
            return new $$Date(y$prime, fromJust()(toEnum(boundedEnumMonth)(m$prime)), d$prime);
          };
        };
      };
      return canonicalDateImpl(mkDate, y, fromEnum(boundedEnumMonth)(m), d);
    };
  };
};
var exactDate = function(y) {
  return function(m) {
    return function(d) {
      var dt = new $$Date(y, m, d);
      var $94 = eq(eqDate)(canonicalDate(y)(m)(d))(dt);
      if ($94) {
        return new Just(dt);
      }
      ;
      return Nothing.value;
    };
  };
};

// output/Data.Time.Component/index.js
var $runtime_lazy3 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var showSecond = {
  show: function(v) {
    return "(Second " + (show(showInt)(v) + ")");
  }
};
var showMinute = {
  show: function(v) {
    return "(Minute " + (show(showInt)(v) + ")");
  }
};
var showMillisecond = {
  show: function(v) {
    return "(Millisecond " + (show(showInt)(v) + ")");
  }
};
var showHour = {
  show: function(v) {
    return "(Hour " + (show(showInt)(v) + ")");
  }
};
var ordSecond = ordInt;
var ordMinute = ordInt;
var ordMillisecond = ordInt;
var ordHour = ordInt;
var boundedSecond = {
  bottom: 0,
  top: 59,
  Ord0: function() {
    return ordSecond;
  }
};
var boundedMinute = {
  bottom: 0,
  top: 59,
  Ord0: function() {
    return ordMinute;
  }
};
var boundedMillisecond = {
  bottom: 0,
  top: 999,
  Ord0: function() {
    return ordMillisecond;
  }
};
var boundedHour = {
  bottom: 0,
  top: 23,
  Ord0: function() {
    return ordHour;
  }
};
var boundedEnumSecond = {
  cardinality: 60,
  toEnum: function(n) {
    if (n >= 0 && n <= 59) {
      return new Just(n);
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 90, column 1 - line 95, column 26): " + [n.constructor.name]);
  },
  fromEnum: function(v) {
    return v;
  },
  Bounded0: function() {
    return boundedSecond;
  },
  Enum1: function() {
    return $lazy_enumSecond(0);
  }
};
var $lazy_enumSecond = /* @__PURE__ */ $runtime_lazy3("enumSecond", "Data.Time.Component", function() {
  return {
    succ: function() {
      var $28 = toEnum(boundedEnumSecond);
      var $29 = fromEnum(boundedEnumSecond);
      return function($30) {
        return $28(function(v) {
          return v + 1 | 0;
        }($29($30)));
      };
    }(),
    pred: function() {
      var $31 = toEnum(boundedEnumSecond);
      var $32 = fromEnum(boundedEnumSecond);
      return function($33) {
        return $31(function(v) {
          return v - 1 | 0;
        }($32($33)));
      };
    }(),
    Ord0: function() {
      return ordSecond;
    }
  };
});
var boundedEnumMinute = {
  cardinality: 60,
  toEnum: function(n) {
    if (n >= 0 && n <= 59) {
      return new Just(n);
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 61, column 1 - line 66, column 26): " + [n.constructor.name]);
  },
  fromEnum: function(v) {
    return v;
  },
  Bounded0: function() {
    return boundedMinute;
  },
  Enum1: function() {
    return $lazy_enumMinute(0);
  }
};
var $lazy_enumMinute = /* @__PURE__ */ $runtime_lazy3("enumMinute", "Data.Time.Component", function() {
  return {
    succ: function() {
      var $34 = toEnum(boundedEnumMinute);
      var $35 = fromEnum(boundedEnumMinute);
      return function($36) {
        return $34(function(v) {
          return v + 1 | 0;
        }($35($36)));
      };
    }(),
    pred: function() {
      var $37 = toEnum(boundedEnumMinute);
      var $38 = fromEnum(boundedEnumMinute);
      return function($39) {
        return $37(function(v) {
          return v - 1 | 0;
        }($38($39)));
      };
    }(),
    Ord0: function() {
      return ordMinute;
    }
  };
});
var boundedEnumMillisecond = {
  cardinality: 1e3,
  toEnum: function(n) {
    if (n >= 0 && n <= 999) {
      return new Just(n);
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 120, column 1 - line 125, column 31): " + [n.constructor.name]);
  },
  fromEnum: function(v) {
    return v;
  },
  Bounded0: function() {
    return boundedMillisecond;
  },
  Enum1: function() {
    return $lazy_enumMillisecond(0);
  }
};
var $lazy_enumMillisecond = /* @__PURE__ */ $runtime_lazy3("enumMillisecond", "Data.Time.Component", function() {
  return {
    succ: function() {
      var $40 = toEnum(boundedEnumMillisecond);
      var $41 = fromEnum(boundedEnumMillisecond);
      return function($42) {
        return $40(function(v) {
          return v + 1 | 0;
        }($41($42)));
      };
    }(),
    pred: function() {
      var $43 = toEnum(boundedEnumMillisecond);
      var $44 = fromEnum(boundedEnumMillisecond);
      return function($45) {
        return $43(function(v) {
          return v - 1 | 0;
        }($44($45)));
      };
    }(),
    Ord0: function() {
      return ordMillisecond;
    }
  };
});
var boundedEnumHour = {
  cardinality: 24,
  toEnum: function(n) {
    if (n >= 0 && n <= 23) {
      return new Just(n);
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.Time.Component (line 32, column 1 - line 37, column 24): " + [n.constructor.name]);
  },
  fromEnum: function(v) {
    return v;
  },
  Bounded0: function() {
    return boundedHour;
  },
  Enum1: function() {
    return $lazy_enumHour(0);
  }
};
var $lazy_enumHour = /* @__PURE__ */ $runtime_lazy3("enumHour", "Data.Time.Component", function() {
  return {
    succ: function() {
      var $46 = toEnum(boundedEnumHour);
      var $47 = fromEnum(boundedEnumHour);
      return function($48) {
        return $46(function(v) {
          return v + 1 | 0;
        }($47($48)));
      };
    }(),
    pred: function() {
      var $49 = toEnum(boundedEnumHour);
      var $50 = fromEnum(boundedEnumHour);
      return function($51) {
        return $49(function(v) {
          return v - 1 | 0;
        }($50($51)));
      };
    }(),
    Ord0: function() {
      return ordHour;
    }
  };
});

// output/Data.Time/index.js
var Time = /* @__PURE__ */ function() {
  function Time2(value0, value1, value2, value3) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
    this.value3 = value3;
  }
  ;
  Time2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return function(value3) {
          return new Time2(value0, value1, value2, value3);
        };
      };
    };
  };
  return Time2;
}();
var showTime = {
  show: function(v) {
    return "(Time " + (show(showHour)(v.value0) + (" " + (show(showMinute)(v.value1) + (" " + (show(showSecond)(v.value2) + (" " + (show(showMillisecond)(v.value3) + ")")))))));
  }
};
var second = function(v) {
  return v.value2;
};
var minute = function(v) {
  return v.value1;
};
var millisecond = function(v) {
  return v.value3;
};
var hour = function(v) {
  return v.value0;
};

// output/Data.DateTime/index.js
var DateTime = /* @__PURE__ */ function() {
  function DateTime2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  DateTime2.create = function(value0) {
    return function(value1) {
      return new DateTime2(value0, value1);
    };
  };
  return DateTime2;
}();
var toRecord = function(v) {
  return {
    year: fromEnum(boundedEnumYear)(year(v.value0)),
    month: fromEnum(boundedEnumMonth)(month(v.value0)),
    day: fromEnum(boundedEnumDay)(day(v.value0)),
    hour: fromEnum(boundedEnumHour)(hour(v.value1)),
    minute: fromEnum(boundedEnumMinute)(minute(v.value1)),
    second: fromEnum(boundedEnumSecond)(second(v.value1)),
    millisecond: fromEnum(boundedEnumMillisecond)(millisecond(v.value1))
  };
};
var showDateTime = {
  show: function(v) {
    return "(DateTime " + (show(showDate)(v.value0) + (" " + (show(showTime)(v.value1) + ")")));
  }
};
var diff = function(dictDuration) {
  return function(dt1) {
    return function(dt2) {
      return toDuration(dictDuration)(calcDiff(toRecord(dt1), toRecord(dt2)));
    };
  };
};
var adjust = function(dictDuration) {
  return function(d) {
    return function(dt) {
      return bind(bindMaybe)(adjustImpl(Just.create)(Nothing.value)(fromDuration(dictDuration)(d))(toRecord(dt)))(function(rec) {
        return apply(applyMaybe)(map(functorMaybe)(DateTime.create)(join(bindMaybe)(apply(applyMaybe)(apply(applyMaybe)(map(functorMaybe)(exactDate)(toEnum(boundedEnumYear)(rec.year)))(toEnum(boundedEnumMonth)(rec.month)))(toEnum(boundedEnumDay)(rec.day)))))(apply(applyMaybe)(apply(applyMaybe)(apply(applyMaybe)(map(functorMaybe)(Time.create)(toEnum(boundedEnumHour)(rec.hour)))(toEnum(boundedEnumMinute)(rec.minute)))(toEnum(boundedEnumSecond)(rec.second)))(toEnum(boundedEnumMillisecond)(rec.millisecond)));
      });
    };
  };
};

// output/Data.DateTime.Instant/foreign.js
var createDateTime = function(y, m, d, h, mi, s, ms) {
  var dateTime = new Date(Date.UTC(y, m, d, h, mi, s, ms));
  if (y >= 0 && y < 100) {
    dateTime.setUTCFullYear(y);
  }
  return dateTime;
};
function fromDateTimeImpl(y, mo, d, h, mi, s, ms) {
  return createDateTime(y, mo - 1, d, h, mi, s, ms).getTime();
}
function toDateTimeImpl(ctor) {
  return function(instant2) {
    var dt = new Date(instant2);
    return ctor(dt.getUTCFullYear())(dt.getUTCMonth() + 1)(dt.getUTCDate())(dt.getUTCHours())(dt.getUTCMinutes())(dt.getUTCSeconds())(dt.getUTCMilliseconds());
  };
}

// output/Data.DateTime.Instant/index.js
var unInstant = function(v) {
  return v;
};
var toDateTime = /* @__PURE__ */ function() {
  var mkDateTime = function(y) {
    return function(mo) {
      return function(d) {
        return function(h) {
          return function(mi) {
            return function(s) {
              return function(ms) {
                return new DateTime(canonicalDate(y)(fromJust()(toEnum(boundedEnumMonth)(mo)))(d), new Time(h, mi, s, ms));
              };
            };
          };
        };
      };
    };
  };
  return toDateTimeImpl(mkDateTime);
}();
var instant = function(v) {
  if (v >= -86399778816e5 && v <= 8639977881599999) {
    return new Just(v);
  }
  ;
  if (otherwise) {
    return Nothing.value;
  }
  ;
  throw new Error("Failed pattern match at Data.DateTime.Instant (line 44, column 1 - line 44, column 41): " + [v.constructor.name]);
};
var fromDateTime = function(v) {
  return fromDateTimeImpl(year(v.value0), fromEnum(boundedEnumMonth)(month(v.value0)), day(v.value0), hour(v.value1), minute(v.value1), second(v.value1), millisecond(v.value1));
};

// output/Control.Monad/index.js
var ap = function(dictMonad) {
  return function(f) {
    return function(a) {
      return bind(dictMonad.Bind1())(f)(function(f$prime) {
        return bind(dictMonad.Bind1())(a)(function(a$prime) {
          return pure(dictMonad.Applicative0())(f$prime(a$prime));
        });
      });
    };
  };
};

// output/Effect/foreign.js
var pureE = function(a) {
  return function() {
    return a;
  };
};
var bindE = function(a) {
  return function(f) {
    return function() {
      return f(a())();
    };
  };
};

// output/Effect/index.js
var $runtime_lazy4 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var monadEffect = {
  Applicative0: function() {
    return applicativeEffect;
  },
  Bind1: function() {
    return bindEffect;
  }
};
var bindEffect = {
  bind: bindE,
  Apply0: function() {
    return $lazy_applyEffect(0);
  }
};
var applicativeEffect = {
  pure: pureE,
  Apply0: function() {
    return $lazy_applyEffect(0);
  }
};
var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy4("functorEffect", "Effect", function() {
  return {
    map: liftA1(applicativeEffect)
  };
});
var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy4("applyEffect", "Effect", function() {
  return {
    apply: ap(monadEffect),
    Functor0: function() {
      return $lazy_functorEffect(0);
    }
  };
});
var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

// output/Effect.Ref/foreign.js
var _new = function(val) {
  return function() {
    return { value: val };
  };
};
var read = function(ref) {
  return function() {
    return ref.value;
  };
};
var write = function(val) {
  return function(ref) {
    return function() {
      ref.value = val;
    };
  };
};

// output/Effect.Ref/index.js
var $$new = _new;

// output/Control.Monad.Rec.Class/index.js
var Loop = /* @__PURE__ */ function() {
  function Loop2(value0) {
    this.value0 = value0;
  }
  ;
  Loop2.create = function(value0) {
    return new Loop2(value0);
  };
  return Loop2;
}();
var Done = /* @__PURE__ */ function() {
  function Done2(value0) {
    this.value0 = value0;
  }
  ;
  Done2.create = function(value0) {
    return new Done2(value0);
  };
  return Done2;
}();
var tailRecM = function(dict) {
  return dict.tailRecM;
};
var tailRec = function(f) {
  var go = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v instanceof Loop) {
        $copy_v = f(v.value0);
        return;
      }
      ;
      if (v instanceof Done) {
        $tco_done = true;
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 93, column 3 - line 93, column 25): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  return function($55) {
    return go(f($55));
  };
};
var monadRecIdentity = {
  tailRecM: function(f) {
    var runIdentity = function(v) {
      return v;
    };
    var $56 = tailRec(function($58) {
      return runIdentity(f($58));
    });
    return function($57) {
      return Identity($56($57));
    };
  },
  Monad0: function() {
    return monadIdentity;
  }
};
var bifunctorStep = {
  bimap: function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Loop) {
          return new Loop(v(v2.value0));
        }
        ;
        if (v2 instanceof Done) {
          return new Done(v1(v2.value0));
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 29, column 1 - line 31, column 34): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  }
};

// output/Data.FoldableWithIndex/index.js
var foldlWithIndex = function(dict) {
  return dict.foldlWithIndex;
};

// output/Data.List.Types/index.js
var Nil = /* @__PURE__ */ function() {
  function Nil3() {
  }
  ;
  Nil3.value = new Nil3();
  return Nil3;
}();
var Cons = /* @__PURE__ */ function() {
  function Cons3(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Cons3.create = function(value0) {
    return function(value1) {
      return new Cons3(value0, value1);
    };
  };
  return Cons3;
}();
var foldableList = {
  foldr: function(f) {
    return function(b) {
      var rev = function() {
        var go = function($copy_acc) {
          return function($copy_v) {
            var $tco_var_acc = $copy_acc;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(acc, v) {
              if (v instanceof Nil) {
                $tco_done = true;
                return acc;
              }
              ;
              if (v instanceof Cons) {
                $tco_var_acc = new Cons(v.value0, acc);
                $copy_v = v.value1;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [acc.constructor.name, v.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_acc, $copy_v);
            }
            ;
            return $tco_result;
          };
        };
        return go(Nil.value);
      }();
      var $205 = foldl(foldableList)(flip(f))(b);
      return function($206) {
        return $205(rev($206));
      };
    };
  },
  foldl: function(f) {
    var go = function($copy_b) {
      return function($copy_v) {
        var $tco_var_b = $copy_b;
        var $tco_done1 = false;
        var $tco_result;
        function $tco_loop(b, v) {
          if (v instanceof Nil) {
            $tco_done1 = true;
            return b;
          }
          ;
          if (v instanceof Cons) {
            $tco_var_b = f(b)(v.value0);
            $copy_v = v.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done1) {
          $tco_result = $tco_loop($tco_var_b, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return go;
  },
  foldMap: function(dictMonoid) {
    return function(f) {
      return foldl(foldableList)(function(acc) {
        var $207 = append(dictMonoid.Semigroup0())(acc);
        return function($208) {
          return $207(f($208));
        };
      })(mempty(dictMonoid));
    };
  }
};

// output/Data.List/index.js
var uncons = function(v) {
  if (v instanceof Nil) {
    return Nothing.value;
  }
  ;
  if (v instanceof Cons) {
    return new Just({
      head: v.value0,
      tail: v.value1
    });
  }
  ;
  throw new Error("Failed pattern match at Data.List (line 259, column 1 - line 259, column 66): " + [v.constructor.name]);
};
var toUnfoldable = function(dictUnfoldable) {
  return unfoldr(dictUnfoldable)(function(xs) {
    return map(functorMaybe)(function(rec) {
      return new Tuple(rec.head, rec.tail);
    })(uncons(xs));
  });
};
var reverse = /* @__PURE__ */ function() {
  var go = function($copy_acc) {
    return function($copy_v) {
      var $tco_var_acc = $copy_acc;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(acc, v) {
        if (v instanceof Nil) {
          $tco_done = true;
          return acc;
        }
        ;
        if (v instanceof Cons) {
          $tco_var_acc = new Cons(v.value0, acc);
          $copy_v = v.value1;
          return;
        }
        ;
        throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [acc.constructor.name, v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_acc, $copy_v);
      }
      ;
      return $tco_result;
    };
  };
  return go(Nil.value);
}();
var manyRec = function(dictMonadRec) {
  return function(dictAlternative) {
    return function(p) {
      var go = function(acc) {
        return bind(dictMonadRec.Monad0().Bind1())(alt(dictAlternative.Plus1().Alt0())(map(dictAlternative.Plus1().Alt0().Functor0())(Loop.create)(p))(pure(dictAlternative.Applicative0())(new Done(unit))))(function(aa) {
          return pure(dictAlternative.Applicative0())(bimap(bifunctorStep)(function(v) {
            return new Cons(v, acc);
          })(function(v) {
            return reverse(acc);
          })(aa));
        });
      };
      return tailRecM(dictMonadRec)(go)(Nil.value);
    };
  };
};

// output/Data.Lazy/foreign.js
var defer2 = function(thunk) {
  var v = null;
  return function() {
    if (thunk === void 0)
      return v;
    v = thunk();
    thunk = void 0;
    return v;
  };
};
var force = function(l) {
  return l();
};

// output/Data.Lazy/index.js
var functorLazy = {
  map: function(f) {
    return function(l) {
      return defer2(function(v) {
        return f(force(l));
      });
    };
  }
};
var applyLazy = {
  apply: function(f) {
    return function(x) {
      return defer2(function(v) {
        return force(f)(force(x));
      });
    };
  },
  Functor0: function() {
    return functorLazy;
  }
};

// output/Data.List.Lazy.Types/index.js
var $runtime_lazy5 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var List = function(x) {
  return x;
};
var Nil2 = /* @__PURE__ */ function() {
  function Nil3() {
  }
  ;
  Nil3.value = new Nil3();
  return Nil3;
}();
var Cons2 = /* @__PURE__ */ function() {
  function Cons3(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Cons3.create = function(value0) {
    return function(value1) {
      return new Cons3(value0, value1);
    };
  };
  return Cons3;
}();
var nil = /* @__PURE__ */ defer2(function(v) {
  return Nil2.value;
});
var step = /* @__PURE__ */ function() {
  var $225 = unwrap();
  return function($226) {
    return force($225($226));
  };
}();
var semigroupList2 = {
  append: function(xs) {
    return function(ys) {
      var go = function(v) {
        if (v instanceof Nil2) {
          return step(ys);
        }
        ;
        if (v instanceof Cons2) {
          return new Cons2(v.value0, append(semigroupList2)(v.value1)(ys));
        }
        ;
        throw new Error("Failed pattern match at Data.List.Lazy.Types (line 103, column 5 - line 103, column 21): " + [v.constructor.name]);
      };
      return map(functorLazy)(go)(unwrap()(xs));
    };
  }
};
var lazyList = {
  defer: function(f) {
    return defer2(function($227) {
      return step(f($227));
    });
  }
};
var functorList = {
  map: function(f) {
    return function(xs) {
      var go = function(v) {
        if (v instanceof Nil2) {
          return Nil2.value;
        }
        ;
        if (v instanceof Cons2) {
          return new Cons2(f(v.value0), map(functorList)(f)(v.value1));
        }
        ;
        throw new Error("Failed pattern match at Data.List.Lazy.Types (line 112, column 5 - line 112, column 17): " + [v.constructor.name]);
      };
      return map(functorLazy)(go)(unwrap()(xs));
    };
  }
};
var eq1List = {
  eq1: function(dictEq) {
    return function(xs) {
      return function(ys) {
        var go = function($copy_v) {
          return function($copy_v1) {
            var $tco_var_v = $copy_v;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v, v1) {
              if (v instanceof Nil2 && v1 instanceof Nil2) {
                $tco_done = true;
                return true;
              }
              ;
              if (v instanceof Cons2 && (v1 instanceof Cons2 && eq(dictEq)(v.value0)(v1.value0))) {
                $tco_var_v = step(v.value1);
                $copy_v1 = step(v1.value1);
                return;
              }
              ;
              $tco_done = true;
              return false;
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v, $copy_v1);
            }
            ;
            return $tco_result;
          };
        };
        return go(step(xs))(step(ys));
      };
    };
  }
};
var eqList2 = function(dictEq) {
  return {
    eq: eq1(eq1List)(dictEq)
  };
};
var cons2 = function(x) {
  return function(xs) {
    return defer2(function(v) {
      return new Cons2(x, xs);
    });
  };
};
var foldableList2 = {
  foldr: function(op) {
    return function(z) {
      return function(xs) {
        var rev = foldl(foldableList2)(flip(cons2))(nil);
        return foldl(foldableList2)(flip(op))(z)(rev(xs));
      };
    };
  },
  foldl: function(op) {
    var go = function($copy_b) {
      return function($copy_xs) {
        var $tco_var_b = $copy_b;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(b, xs) {
          var v = step(xs);
          if (v instanceof Nil2) {
            $tco_done = true;
            return b;
          }
          ;
          if (v instanceof Cons2) {
            $tco_var_b = op(b)(v.value0);
            $copy_xs = v.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List.Lazy.Types (line 127, column 7 - line 129, column 40): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_b, $copy_xs);
        }
        ;
        return $tco_result;
      };
    };
    return go;
  },
  foldMap: function(dictMonoid) {
    return function(f) {
      return foldl(foldableList2)(function(b) {
        return function(a) {
          return append(dictMonoid.Semigroup0())(b)(f(a));
        };
      })(mempty(dictMonoid));
    };
  }
};
var showList2 = function(dictShow) {
  return {
    show: function(xs) {
      return "(fromFoldable [" + (function() {
        var v = step(xs);
        if (v instanceof Nil2) {
          return "";
        }
        ;
        if (v instanceof Cons2) {
          return show(dictShow)(v.value0) + foldl(foldableList2)(function(shown) {
            return function(x$prime) {
              return shown + ("," + show(dictShow)(x$prime));
            };
          })("")(v.value1);
        }
        ;
        throw new Error("Failed pattern match at Data.List.Lazy.Types (line 66, column 13 - line 69, column 78): " + [v.constructor.name]);
      }() + "])");
    }
  };
};
var foldableWithIndexList = {
  foldrWithIndex: function(f) {
    return function(b) {
      return function(xs) {
        var v = function() {
          var rev = foldl(foldableList2)(function(v1) {
            return function(a) {
              return new Tuple(v1.value0 + 1 | 0, cons2(a)(v1.value1));
            };
          });
          return rev(new Tuple(0, nil))(xs);
        }();
        return snd(foldl(foldableList2)(function(v1) {
          return function(a) {
            return new Tuple(v1.value0 - 1 | 0, f(v1.value0 - 1 | 0)(a)(v1.value1));
          };
        })(new Tuple(v.value0, b))(v.value1));
      };
    };
  },
  foldlWithIndex: function(f) {
    return function(acc) {
      var $228 = foldl(foldableList2)(function(v) {
        return function(a) {
          return new Tuple(v.value0 + 1 | 0, f(v.value0)(v.value1)(a));
        };
      })(new Tuple(0, acc));
      return function($229) {
        return snd($228($229));
      };
    };
  },
  foldMapWithIndex: function(dictMonoid) {
    return function(f) {
      return foldlWithIndex(foldableWithIndexList)(function(i) {
        return function(acc) {
          var $230 = append(dictMonoid.Semigroup0())(acc);
          var $231 = f(i);
          return function($232) {
            return $230($231($232));
          };
        };
      })(mempty(dictMonoid));
    };
  },
  Foldable0: function() {
    return foldableList2;
  }
};
var unfoldable1List = {
  unfoldr1: /* @__PURE__ */ function() {
    var go = function(f) {
      return function(b) {
        return defer(lazyList)(function(v) {
          var v1 = f(b);
          if (v1.value1 instanceof Just) {
            return cons2(v1.value0)(go(f)(v1.value1.value0));
          }
          ;
          if (v1.value1 instanceof Nothing) {
            return cons2(v1.value0)(nil);
          }
          ;
          throw new Error("Failed pattern match at Data.List.Lazy.Types (line 151, column 28 - line 153, column 33): " + [v1.constructor.name]);
        });
      };
    };
    return go;
  }()
};
var unfoldableList = {
  unfoldr: /* @__PURE__ */ function() {
    var go = function(f) {
      return function(b) {
        return defer(lazyList)(function(v) {
          var v1 = f(b);
          if (v1 instanceof Nothing) {
            return nil;
          }
          ;
          if (v1 instanceof Just) {
            return cons2(v1.value0.value0)(go(f)(v1.value0.value1));
          }
          ;
          throw new Error("Failed pattern match at Data.List.Lazy.Types (line 157, column 28 - line 159, column 39): " + [v1.constructor.name]);
        });
      };
    };
    return go;
  }(),
  Unfoldable10: function() {
    return unfoldable1List;
  }
};
var monadList = {
  Applicative0: function() {
    return applicativeList;
  },
  Bind1: function() {
    return bindList2;
  }
};
var bindList2 = {
  bind: function(xs) {
    return function(f) {
      var go = function(v) {
        if (v instanceof Nil2) {
          return Nil2.value;
        }
        ;
        if (v instanceof Cons2) {
          return step(append(semigroupList2)(f(v.value0))(bind(bindList2)(v.value1)(f)));
        }
        ;
        throw new Error("Failed pattern match at Data.List.Lazy.Types (line 180, column 5 - line 180, column 17): " + [v.constructor.name]);
      };
      return map(functorLazy)(go)(unwrap()(xs));
    };
  },
  Apply0: function() {
    return $lazy_applyList(0);
  }
};
var applicativeList = {
  pure: function(a) {
    return cons2(a)(nil);
  },
  Apply0: function() {
    return $lazy_applyList(0);
  }
};
var $lazy_applyList = /* @__PURE__ */ $runtime_lazy5("applyList", "Data.List.Lazy.Types", function() {
  return {
    apply: ap(monadList),
    Functor0: function() {
      return functorList;
    }
  };
});

// output/Data.List.Lazy/index.js
var zipWith = function(f) {
  return function(xs) {
    return function(ys) {
      var go = function(v) {
        return function(v1) {
          if (v instanceof Nil2) {
            return Nil2.value;
          }
          ;
          if (v1 instanceof Nil2) {
            return Nil2.value;
          }
          ;
          if (v instanceof Cons2 && v1 instanceof Cons2) {
            return new Cons2(f(v.value0)(v1.value0), zipWith(f)(v.value1)(v1.value1));
          }
          ;
          throw new Error("Failed pattern match at Data.List.Lazy (line 705, column 3 - line 705, column 35): " + [v.constructor.name, v1.constructor.name]);
        };
      };
      return apply(applyLazy)(map(functorLazy)(go)(unwrap()(xs)))(unwrap()(ys));
    };
  };
};
var zip = /* @__PURE__ */ function() {
  return zipWith(Tuple.create);
}();
var take = function(n) {
  var go = function(v) {
    return function(v1) {
      if (v1 instanceof Nil2) {
        return Nil2.value;
      }
      ;
      if (v1 instanceof Cons2) {
        return new Cons2(v1.value0, take(v - 1 | 0)(v1.value1));
      }
      ;
      throw new Error("Failed pattern match at Data.List.Lazy (line 505, column 3 - line 505, column 32): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var $122 = n <= 0;
  if ($122) {
    return $$const(nil);
  }
  ;
  var $254 = map(functorLazy)(go(n));
  var $255 = unwrap();
  return function($256) {
    return List($254($255($256)));
  };
};
var snoc = function(xs) {
  return function(x) {
    return foldr(foldableList2)(cons2)(cons2(x)(nil))(xs);
  };
};
var repeat = function(x) {
  return fix(lazyList)(function(xs) {
    return cons2(x)(xs);
  });
};
var replicate = function(i) {
  return function(xs) {
    return take(i)(repeat(xs));
  };
};
var range2 = function(start) {
  return function(end) {
    if (start > end) {
      var g = function(x) {
        if (x >= end) {
          return new Just(new Tuple(x, x - 1 | 0));
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.List.Lazy (line 151, column 13 - line 152, column 38): " + [x.constructor.name]);
      };
      return unfoldr(unfoldableList)(g)(start);
    }
    ;
    if (otherwise) {
      var f = function(x) {
        if (x <= end) {
          return new Just(new Tuple(x, x + 1 | 0));
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.List.Lazy (line 156, column 5 - line 157, column 30): " + [x.constructor.name]);
      };
      return unfoldr(unfoldableList)(f)(start);
    }
    ;
    throw new Error("Failed pattern match at Data.List.Lazy (line 148, column 1 - line 148, column 32): " + [start.constructor.name, end.constructor.name]);
  };
};
var length = /* @__PURE__ */ foldl(foldableList2)(function(l) {
  return function(v) {
    return l + 1 | 0;
  };
})(0);
var fromFoldable = function(dictFoldable) {
  return foldr(dictFoldable)(cons2)(nil);
};
var filter = function(p) {
  var go = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v instanceof Nil2) {
        $tco_done = true;
        return Nil2.value;
      }
      ;
      if (v instanceof Cons2) {
        if (p(v.value0)) {
          $tco_done = true;
          return new Cons2(v.value0, filter(p)(v.value1));
        }
        ;
        if (otherwise) {
          $copy_v = step(v.value1);
          return;
        }
        ;
      }
      ;
      throw new Error("Failed pattern match at Data.List.Lazy (line 416, column 3 - line 416, column 15): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var $268 = map(functorLazy)(go);
  var $269 = unwrap();
  return function($270) {
    return List($268($269($270)));
  };
};
var cycle = function(xs) {
  return fix(lazyList)(function(ys) {
    return append(semigroupList2)(xs)(ys);
  });
};
var concat = function(v) {
  return bind(bindList2)(v)(identity(categoryFn));
};

// output/Data.Map.Internal/index.js
var Leaf = /* @__PURE__ */ function() {
  function Leaf2() {
  }
  ;
  Leaf2.value = new Leaf2();
  return Leaf2;
}();
var Two = /* @__PURE__ */ function() {
  function Two2(value0, value1, value2, value3) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
    this.value3 = value3;
  }
  ;
  Two2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return function(value3) {
          return new Two2(value0, value1, value2, value3);
        };
      };
    };
  };
  return Two2;
}();
var Three = /* @__PURE__ */ function() {
  function Three2(value0, value1, value2, value3, value4, value5, value6) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
    this.value3 = value3;
    this.value4 = value4;
    this.value5 = value5;
    this.value6 = value6;
  }
  ;
  Three2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return function(value3) {
          return function(value4) {
            return function(value5) {
              return function(value6) {
                return new Three2(value0, value1, value2, value3, value4, value5, value6);
              };
            };
          };
        };
      };
    };
  };
  return Three2;
}();
var TwoLeft = /* @__PURE__ */ function() {
  function TwoLeft2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  TwoLeft2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new TwoLeft2(value0, value1, value2);
      };
    };
  };
  return TwoLeft2;
}();
var TwoRight = /* @__PURE__ */ function() {
  function TwoRight2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  TwoRight2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new TwoRight2(value0, value1, value2);
      };
    };
  };
  return TwoRight2;
}();
var ThreeLeft = /* @__PURE__ */ function() {
  function ThreeLeft2(value0, value1, value2, value3, value4, value5) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
    this.value3 = value3;
    this.value4 = value4;
    this.value5 = value5;
  }
  ;
  ThreeLeft2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return function(value3) {
          return function(value4) {
            return function(value5) {
              return new ThreeLeft2(value0, value1, value2, value3, value4, value5);
            };
          };
        };
      };
    };
  };
  return ThreeLeft2;
}();
var ThreeMiddle = /* @__PURE__ */ function() {
  function ThreeMiddle2(value0, value1, value2, value3, value4, value5) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
    this.value3 = value3;
    this.value4 = value4;
    this.value5 = value5;
  }
  ;
  ThreeMiddle2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return function(value3) {
          return function(value4) {
            return function(value5) {
              return new ThreeMiddle2(value0, value1, value2, value3, value4, value5);
            };
          };
        };
      };
    };
  };
  return ThreeMiddle2;
}();
var ThreeRight = /* @__PURE__ */ function() {
  function ThreeRight2(value0, value1, value2, value3, value4, value5) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
    this.value3 = value3;
    this.value4 = value4;
    this.value5 = value5;
  }
  ;
  ThreeRight2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return function(value3) {
          return function(value4) {
            return function(value5) {
              return new ThreeRight2(value0, value1, value2, value3, value4, value5);
            };
          };
        };
      };
    };
  };
  return ThreeRight2;
}();
var KickUp = /* @__PURE__ */ function() {
  function KickUp2(value0, value1, value2, value3) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
    this.value3 = value3;
  }
  ;
  KickUp2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return function(value3) {
          return new KickUp2(value0, value1, value2, value3);
        };
      };
    };
  };
  return KickUp2;
}();
var fromZipper = function($copy_dictOrd) {
  return function($copy_v) {
    return function($copy_tree) {
      var $tco_var_dictOrd = $copy_dictOrd;
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(dictOrd, v, tree) {
        if (v instanceof Nil) {
          $tco_done = true;
          return tree;
        }
        ;
        if (v instanceof Cons) {
          if (v.value0 instanceof TwoLeft) {
            $tco_var_dictOrd = dictOrd;
            $tco_var_v = v.value1;
            $copy_tree = new Two(tree, v.value0.value0, v.value0.value1, v.value0.value2);
            return;
          }
          ;
          if (v.value0 instanceof TwoRight) {
            $tco_var_dictOrd = dictOrd;
            $tco_var_v = v.value1;
            $copy_tree = new Two(v.value0.value0, v.value0.value1, v.value0.value2, tree);
            return;
          }
          ;
          if (v.value0 instanceof ThreeLeft) {
            $tco_var_dictOrd = dictOrd;
            $tco_var_v = v.value1;
            $copy_tree = new Three(tree, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
            return;
          }
          ;
          if (v.value0 instanceof ThreeMiddle) {
            $tco_var_dictOrd = dictOrd;
            $tco_var_v = v.value1;
            $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, tree, v.value0.value3, v.value0.value4, v.value0.value5);
            return;
          }
          ;
          if (v.value0 instanceof ThreeRight) {
            $tco_var_dictOrd = dictOrd;
            $tco_var_v = v.value1;
            $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, tree);
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): " + [v.value0.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): " + [v.constructor.name, tree.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_tree);
      }
      ;
      return $tco_result;
    };
  };
};
var insert = function(dictOrd) {
  return function(k) {
    return function(v) {
      var up = function($copy_v1) {
        return function($copy_v2) {
          var $tco_var_v1 = $copy_v1;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v1, v2) {
            if (v1 instanceof Nil) {
              $tco_done = true;
              return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
            }
            ;
            if (v1 instanceof Cons) {
              if (v1.value0 instanceof TwoLeft) {
                $tco_done = true;
                return fromZipper(dictOrd)(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
              }
              ;
              if (v1.value0 instanceof TwoRight) {
                $tco_done = true;
                return fromZipper(dictOrd)(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
              }
              ;
              if (v1.value0 instanceof ThreeLeft) {
                $tco_var_v1 = v1.value1;
                $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                return;
              }
              ;
              if (v1.value0 instanceof ThreeMiddle) {
                $tco_var_v1 = v1.value1;
                $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                return;
              }
              ;
              if (v1.value0 instanceof ThreeRight) {
                $tco_var_v1 = v1.value1;
                $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): " + [v1.value0.constructor.name, v2.constructor.name]);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): " + [v1.constructor.name, v2.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_v1, $copy_v2);
          }
          ;
          return $tco_result;
        };
      };
      var comp = compare(dictOrd);
      var down = function($copy_ctx) {
        return function($copy_v1) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(ctx, v1) {
            if (v1 instanceof Leaf) {
              $tco_done1 = true;
              return up(ctx)(new KickUp(Leaf.value, k, v, Leaf.value));
            }
            ;
            if (v1 instanceof Two) {
              var v2 = comp(k)(v1.value1);
              if (v2 instanceof EQ) {
                $tco_done1 = true;
                return fromZipper(dictOrd)(ctx)(new Two(v1.value0, k, v, v1.value3));
              }
              ;
              if (v2 instanceof LT) {
                $tco_var_ctx = new Cons(new TwoLeft(v1.value1, v1.value2, v1.value3), ctx);
                $copy_v1 = v1.value0;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new TwoRight(v1.value0, v1.value1, v1.value2), ctx);
              $copy_v1 = v1.value3;
              return;
            }
            ;
            if (v1 instanceof Three) {
              var v3 = comp(k)(v1.value1);
              if (v3 instanceof EQ) {
                $tco_done1 = true;
                return fromZipper(dictOrd)(ctx)(new Three(v1.value0, k, v, v1.value3, v1.value4, v1.value5, v1.value6));
              }
              ;
              var v4 = comp(k)(v1.value4);
              if (v4 instanceof EQ) {
                $tco_done1 = true;
                return fromZipper(dictOrd)(ctx)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, k, v, v1.value6));
              }
              ;
              if (v3 instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeLeft(v1.value1, v1.value2, v1.value3, v1.value4, v1.value5, v1.value6), ctx);
                $copy_v1 = v1.value0;
                return;
              }
              ;
              if (v3 instanceof GT && v4 instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, v1.value4, v1.value5, v1.value6), ctx);
                $copy_v1 = v1.value3;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
              $copy_v1 = v1.value6;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): " + [ctx.constructor.name, v1.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_v1);
          }
          ;
          return $tco_result;
        };
      };
      return down(Nil.value);
    };
  };
};
var foldableMap = {
  foldr: function(f) {
    return function(z) {
      return function(m) {
        if (m instanceof Leaf) {
          return z;
        }
        ;
        if (m instanceof Two) {
          return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(z)(m.value3)))(m.value0);
        }
        ;
        if (m instanceof Three) {
          return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(f(m.value5)(foldr(foldableMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): " + [m.constructor.name]);
      };
    };
  },
  foldl: function(f) {
    return function(z) {
      return function(m) {
        if (m instanceof Leaf) {
          return z;
        }
        ;
        if (m instanceof Two) {
          return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3);
        }
        ;
        if (m instanceof Three) {
          return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): " + [m.constructor.name]);
      };
    };
  },
  foldMap: function(dictMonoid) {
    return function(f) {
      return function(m) {
        if (m instanceof Leaf) {
          return mempty(dictMonoid);
        }
        ;
        if (m instanceof Two) {
          return append(dictMonoid.Semigroup0())(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append(dictMonoid.Semigroup0())(f(m.value2))(foldMap(foldableMap)(dictMonoid)(f)(m.value3)));
        }
        ;
        if (m instanceof Three) {
          return append(dictMonoid.Semigroup0())(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append(dictMonoid.Semigroup0())(f(m.value2))(append(dictMonoid.Semigroup0())(foldMap(foldableMap)(dictMonoid)(f)(m.value3))(append(dictMonoid.Semigroup0())(f(m.value5))(foldMap(foldableMap)(dictMonoid)(f)(m.value6)))));
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): " + [m.constructor.name]);
      };
    };
  }
};
var values = /* @__PURE__ */ function() {
  return foldr(foldableMap)(Cons.create)(Nil.value);
}();
var empty2 = /* @__PURE__ */ function() {
  return Leaf.value;
}();
var fromFoldableWithIndex = function(dictOrd) {
  return function(dictFoldableWithIndex) {
    return foldlWithIndex(dictFoldableWithIndex)(function(k) {
      return function(m) {
        return function(v) {
          return insert(dictOrd)(k)(v)(m);
        };
      };
    })(empty2);
  };
};

// output/Data.Ratio/index.js
var Ratio = /* @__PURE__ */ function() {
  function Ratio2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Ratio2.create = function(value0) {
    return function(value1) {
      return new Ratio2(value0, value1);
    };
  };
  return Ratio2;
}();
var reduce = function(dictOrd) {
  return function(dictEuclideanRing) {
    return function(n) {
      return function(d) {
        var g = gcd(dictOrd.Eq0())(dictEuclideanRing)(n)(d);
        var d$prime = div(dictEuclideanRing)(d)(g);
        return new Ratio(mul(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0())(div(dictEuclideanRing)(n)(g))(signum(dictOrd)(dictEuclideanRing.CommutativeRing0().Ring0())(d$prime)), abs(dictOrd)(dictEuclideanRing.CommutativeRing0().Ring0())(d$prime));
      };
    };
  };
};
var semiringRatio = function(dictOrd) {
  return function(dictEuclideanRing) {
    return {
      one: new Ratio(one(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0()), one(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0())),
      mul: function(v) {
        return function(v1) {
          return reduce(dictOrd)(dictEuclideanRing)(mul(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0())(v.value0)(v1.value0))(mul(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0())(v.value1)(v1.value1));
        };
      },
      zero: new Ratio(zero(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0()), one(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0())),
      add: function(v) {
        return function(v1) {
          return reduce(dictOrd)(dictEuclideanRing)(add(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0())(mul(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0())(v.value0)(v1.value1))(mul(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0())(v.value1)(v1.value0)))(mul(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0())(v.value1)(v1.value1));
        };
      }
    };
  };
};
var ringRatio = function(dictOrd) {
  return function(dictEuclideanRing) {
    return {
      sub: function(v) {
        return function(v1) {
          return reduce(dictOrd)(dictEuclideanRing)(sub(dictEuclideanRing.CommutativeRing0().Ring0())(mul(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0())(v.value0)(v1.value1))(mul(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0())(v.value1)(v1.value0)))(mul(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0())(v.value1)(v1.value1));
        };
      },
      Semiring0: function() {
        return semiringRatio(dictOrd)(dictEuclideanRing);
      }
    };
  };
};
var numerator = function(v) {
  return v.value0;
};
var denominator = function(v) {
  return v.value1;
};
var commutativeRingRatio = function(dictOrd) {
  return function(dictEuclideanRing) {
    return {
      Ring0: function() {
        return ringRatio(dictOrd)(dictEuclideanRing);
      }
    };
  };
};
var euclideanRingRatio = function(dictOrd) {
  return function(dictEuclideanRing) {
    return {
      degree: function(v) {
        return 1;
      },
      div: function(v) {
        return function(v1) {
          return reduce(dictOrd)(dictEuclideanRing)(mul(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0())(v.value0)(v1.value1))(mul(dictEuclideanRing.CommutativeRing0().Ring0().Semiring0())(v.value1)(v1.value0));
        };
      },
      mod: function(v) {
        return function(v1) {
          return zero(semiringRatio(dictOrd)(dictEuclideanRing));
        };
      },
      CommutativeRing0: function() {
        return commutativeRingRatio(dictOrd)(dictEuclideanRing);
      }
    };
  };
};

// output/Data.Rational/index.js
var toNumber2 = function(x) {
  return toNumber(numerator(x)) / toNumber(denominator(x));
};
var fromInt = function(i) {
  return reduce(ordInt)(euclideanRingInt)(i)(1);
};

// output/Effect.Now/foreign.js
function now() {
  return Date.now();
}

// output/Effect.Now/index.js
var nowDateTime = /* @__PURE__ */ map(functorEffect)(toDateTime)(now);

// output/Data.Tempo/index.js
var timeToCountNumber = function(x) {
  return function(t) {
    var timeDiff = unwrap()(diff(durationMilliseconds)(t)(x.time));
    var df = timeDiff * toNumber2(x.freq) / 1e3;
    return df + toNumber2(x.count);
  };
};
var origin = function(x) {
  var increment = div(euclideanRingRatio(ordInt)(euclideanRingInt))(mul(semiringRatio(ordInt)(euclideanRingInt))(x.count)(fromInt(-1e3 | 0)))(x.freq);
  return maybe(x.time)(identity(categoryFn))(adjust(durationMilliseconds)(toNumber2(increment))(x.time));
};
var newTempo = function(freq) {
  return function __do2() {
    var time3 = nowDateTime();
    return {
      freq,
      time: time3,
      count: fromInt(0)
    };
  };
};
var fromForeignTempo = function(x) {
  var time3 = toDateTime(fromJust()(instant(x.time)));
  var freq = reduce(ordInt)(euclideanRingInt)(x.freqNumerator)(x.freqDenominator);
  var count = reduce(ordInt)(euclideanRingInt)(x.countNumerator)(x.countDenominator);
  return {
    freq,
    time: time3,
    count
  };
};
var countToTime = function(x) {
  return function(c) {
    return maybe(x.time)(identity(categoryFn))(adjust(durationSeconds)(toNumber2(div(euclideanRingRatio(ordInt)(euclideanRingInt))(c)(x.freq)))(origin(x)));
  };
};

// output/Effect.Console/foreign.js
var log2 = function(s) {
  return function() {
    console.log(s);
  };
};

// output/Foreign/foreign.js
var isArray = Array.isArray || function(value) {
  return Object.prototype.toString.call(value) === "[object Array]";
};

// output/Control.Monad.Error.Class/index.js
var throwError = function(dict) {
  return dict.throwError;
};

// output/Data.String.CodeUnits/foreign.js
var length3 = function(s) {
  return s.length;
};
var drop2 = function(n) {
  return function(s) {
    return s.substring(n);
  };
};

// output/Data.String.Unsafe/foreign.js
var charAt = function(i) {
  return function(s) {
    if (i >= 0 && i < s.length)
      return s.charAt(i);
    throw new Error("Data.String.Unsafe.charAt: Invalid index.");
  };
};

// output/Foreign/index.js
var unsafeToForeign = unsafeCoerce2;

// output/Parsing/index.js
var $runtime_lazy6 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var ParseState = /* @__PURE__ */ function() {
  function ParseState2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  ParseState2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new ParseState2(value0, value1, value2);
      };
    };
  };
  return ParseState2;
}();
var ParseError = /* @__PURE__ */ function() {
  function ParseError2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  ParseError2.create = function(value0) {
    return function(value1) {
      return new ParseError2(value0, value1);
    };
  };
  return ParseError2;
}();
var More = /* @__PURE__ */ function() {
  function More2(value0) {
    this.value0 = value0;
  }
  ;
  More2.create = function(value0) {
    return new More2(value0);
  };
  return More2;
}();
var Lift = /* @__PURE__ */ function() {
  function Lift2(value0) {
    this.value0 = value0;
  }
  ;
  Lift2.create = function(value0) {
    return new Lift2(value0);
  };
  return Lift2;
}();
var Stop = /* @__PURE__ */ function() {
  function Stop2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Stop2.create = function(value0) {
    return function(value1) {
      return new Stop2(value0, value1);
    };
  };
  return Stop2;
}();
var lazyParserT = {
  defer: function(f) {
    var m = defer2(f);
    return function(state1, more, lift3, $$throw, done) {
      var v = force(m);
      return v(state1, more, lift3, $$throw, done);
    };
  }
};
var functorParserT = {
  map: function(f) {
    return function(v) {
      return function(state1, more, lift3, $$throw, done) {
        return more(function(v1) {
          return v(state1, more, lift3, $$throw, function(state2, a) {
            return more(function(v2) {
              return done(state2, f(a));
            });
          });
        });
      };
    };
  }
};
var applyParserT = {
  apply: function(v) {
    return function(v1) {
      return function(state1, more, lift3, $$throw, done) {
        return more(function(v2) {
          return v(state1, more, lift3, $$throw, function(state2, f) {
            return more(function(v3) {
              return v1(state2, more, lift3, $$throw, function(state3, a) {
                return more(function(v4) {
                  return done(state3, f(a));
                });
              });
            });
          });
        });
      };
    };
  },
  Functor0: function() {
    return functorParserT;
  }
};
var bindParserT = {
  bind: function(v) {
    return function(next) {
      return function(state1, more, lift3, $$throw, done) {
        return more(function(v1) {
          return v(state1, more, lift3, $$throw, function(state2, a) {
            return more(function(v2) {
              var v3 = next(a);
              return v3(state2, more, lift3, $$throw, done);
            });
          });
        });
      };
    };
  },
  Apply0: function() {
    return applyParserT;
  }
};
var applicativeParserT = {
  pure: function(a) {
    return function(state1, v, v1, v2, done) {
      return done(state1, a);
    };
  },
  Apply0: function() {
    return applyParserT;
  }
};
var monadParserT = {
  Applicative0: function() {
    return applicativeParserT;
  },
  Bind1: function() {
    return bindParserT;
  }
};
var monadRecParserT = {
  tailRecM: function(next) {
    return function(initArg) {
      return function(state1, more, lift3, $$throw, done) {
        var $lazy_loop = $runtime_lazy6("loop", "Parsing", function() {
          return function(state2, arg, gas) {
            var v = next(arg);
            return v(state2, more, lift3, $$throw, function(state3, step2) {
              if (step2 instanceof Loop) {
                var $120 = gas === 0;
                if ($120) {
                  return more(function(v1) {
                    return $lazy_loop(269)(state3, step2.value0, 30);
                  });
                }
                ;
                return $lazy_loop(271)(state3, step2.value0, gas - 1 | 0);
              }
              ;
              if (step2 instanceof Done) {
                return done(state3, step2.value0);
              }
              ;
              throw new Error("Failed pattern match at Parsing (line 265, column 39 - line 273, column 43): " + [step2.constructor.name]);
            });
          };
        });
        var loop = $lazy_loop(262);
        return loop(state1, initArg, 30);
      };
    };
  },
  Monad0: function() {
    return monadParserT;
  }
};
var monadThrowParseErrorParse = {
  throwError: function(err) {
    return function(state1, v, v1, $$throw, v2) {
      return $$throw(state1, err);
    };
  },
  Monad0: function() {
    return monadParserT;
  }
};
var altParserT = {
  alt: function(v) {
    return function(v1) {
      return function(v2, more, lift3, $$throw, done) {
        return more(function(v3) {
          return v(new ParseState(v2.value0, v2.value1, false), more, lift3, function(v4, err) {
            return more(function(v5) {
              if (v4.value2) {
                return $$throw(v4, err);
              }
              ;
              return v1(v2, more, lift3, $$throw, done);
            });
          }, done);
        });
      };
    };
  },
  Functor0: function() {
    return functorParserT;
  }
};
var stateParserT = function(k) {
  return function(state1, v, v1, v2, done) {
    var v3 = k(state1);
    return done(v3.value1, v3.value0);
  };
};
var runParserT$prime = function(dictMonadRec) {
  return function(state1) {
    return function(v) {
      var go = function($copy_step) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(step2) {
          var v1 = step2(unit);
          if (v1 instanceof More) {
            $copy_step = v1.value0;
            return;
          }
          ;
          if (v1 instanceof Lift) {
            $tco_done = true;
            return map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(Loop.create)(v1.value0);
          }
          ;
          if (v1 instanceof Stop) {
            $tco_done = true;
            return pure(dictMonadRec.Monad0().Applicative0())(new Done(new Tuple(v1.value1, v1.value0)));
          }
          ;
          throw new Error("Failed pattern match at Parsing (line 144, column 13 - line 150, column 32): " + [v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_step);
        }
        ;
        return $tco_result;
      };
      return tailRecM(dictMonadRec)(go)(function(v1) {
        return v(state1, More.create, Lift.create, function(state2, err) {
          return new Stop(state2, new Left(err));
        }, function(state2, res) {
          return new Stop(state2, new Right(res));
        });
      });
    };
  };
};
var position = /* @__PURE__ */ stateParserT(function(v) {
  return new Tuple(v.value1, v);
});
var parseErrorMessage = function(v) {
  return v.value0;
};
var initialPos = {
  index: 0,
  line: 1,
  column: 1
};
var runParserT = function(dictMonadRec) {
  return function(s) {
    return function(p) {
      var initialState = new ParseState(s, initialPos, false);
      return map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(fst)(runParserT$prime(dictMonadRec)(initialState)(p));
    };
  };
};
var runParser = function(s) {
  var $185 = unwrap();
  var $186 = runParserT(monadRecIdentity)(s);
  return function($187) {
    return $185($186($187));
  };
};
var failWithPosition = function(message2) {
  return function(pos) {
    return throwError(monadThrowParseErrorParse)(new ParseError(message2, pos));
  };
};
var fail = function(message2) {
  return bindFlipped(bindParserT)(failWithPosition(message2))(position);
};
var plusParserT = {
  empty: /* @__PURE__ */ fail("No alternative"),
  Alt0: function() {
    return altParserT;
  }
};
var alternativeParserT = {
  Applicative0: function() {
    return applicativeParserT;
  },
  Plus1: function() {
    return plusParserT;
  }
};

// output/Parsing.Combinators/index.js
var withLazyErrorMessage = function(p) {
  return function(msg) {
    return alt(altParserT)(p)(defer(lazyParserT)(function(v) {
      return fail("Expected " + msg(unit));
    }));
  };
};
var withErrorMessage = function(p) {
  return function(msg) {
    return alt(altParserT)(p)(fail("Expected " + msg));
  };
};
var $$try = function(v) {
  return function(v1, more, lift3, $$throw, done) {
    return v(v1, more, lift3, function(v2, err) {
      return $$throw(new ParseState(v2.value0, v2.value1, v1.value2), err);
    }, done);
  };
};
var many = /* @__PURE__ */ manyRec(monadRecParserT)(alternativeParserT);
var choice = function(dictFoldable) {
  var go = function(p1) {
    return function(v) {
      if (v instanceof Nothing) {
        return new Just(p1);
      }
      ;
      if (v instanceof Just) {
        return new Just(alt(altParserT)(p1)(v.value0));
      }
      ;
      throw new Error("Failed pattern match at Parsing.Combinators (line 357, column 11 - line 359, column 32): " + [v.constructor.name]);
    };
  };
  var $68 = fromMaybe(empty(plusParserT));
  var $69 = foldr(dictFoldable)(go)(Nothing.value);
  return function($70) {
    return $68($69($70));
  };
};
var chainl1 = function(p) {
  return function(f) {
    var go = function(a) {
      return alt(altParserT)(bind(bindParserT)(f)(function(op) {
        return bind(bindParserT)(p)(function(a$prime) {
          return pure(applicativeParserT)(new Loop(op(a)(a$prime)));
        });
      }))(pure(applicativeParserT)(new Done(a)));
    };
    return bind(bindParserT)(p)(function(a) {
      return tailRecM(monadRecParserT)(go)(a);
    });
  };
};

// output/Data.Array/foreign.js
var replicateFill = function(count) {
  return function(value) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value);
  };
};
var replicatePolyfill = function(count) {
  return function(value) {
    var result = [];
    var n = 0;
    for (var i = 0; i < count; i++) {
      result[n++] = value;
    }
    return result;
  };
};
var replicate2 = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
var fromFoldableImpl = function() {
  function Cons3(head4, tail2) {
    this.head = head4;
    this.tail = tail2;
  }
  var emptyList = {};
  function curryCons(head4) {
    return function(tail2) {
      return new Cons3(head4, tail2);
    };
  }
  function listToArray(list) {
    var result = [];
    var count = 0;
    var xs = list;
    while (xs !== emptyList) {
      result[count++] = xs.head;
      xs = xs.tail;
    }
    return result;
  }
  return function(foldr2) {
    return function(xs) {
      return listToArray(foldr2(curryCons)(emptyList)(xs));
    };
  };
}();
var findIndexImpl = function(just) {
  return function(nothing) {
    return function(f) {
      return function(xs) {
        for (var i = 0, l = xs.length; i < l; i++) {
          if (f(xs[i]))
            return just(i);
        }
        return nothing;
      };
    };
  };
};
var sortByImpl = function() {
  function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to) {
    var mid;
    var i;
    var j;
    var k;
    var x;
    var y;
    var c;
    mid = from2 + (to - from2 >> 1);
    if (mid - from2 > 1)
      mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
    if (to - mid > 1)
      mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
    i = from2;
    j = mid;
    k = from2;
    while (i < mid && j < to) {
      x = xs2[i];
      y = xs2[j];
      c = fromOrdering(compare2(x)(y));
      if (c > 0) {
        xs1[k++] = y;
        ++j;
      } else {
        xs1[k++] = x;
        ++i;
      }
    }
    while (i < mid) {
      xs1[k++] = xs2[i++];
    }
    while (j < to) {
      xs1[k++] = xs2[j++];
    }
  }
  return function(compare2) {
    return function(fromOrdering) {
      return function(xs) {
        var out;
        if (xs.length < 2)
          return xs;
        out = xs.slice(0);
        mergeFromTo(compare2, fromOrdering, out, xs.slice(0), 0, xs.length);
        return out;
      };
    };
  };
}();

// output/Data.Array.ST/foreign.js
var sortByImpl2 = function() {
  function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to) {
    var mid;
    var i;
    var j;
    var k;
    var x;
    var y;
    var c;
    mid = from2 + (to - from2 >> 1);
    if (mid - from2 > 1)
      mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
    if (to - mid > 1)
      mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
    i = from2;
    j = mid;
    k = from2;
    while (i < mid && j < to) {
      x = xs2[i];
      y = xs2[j];
      c = fromOrdering(compare2(x)(y));
      if (c > 0) {
        xs1[k++] = y;
        ++j;
      } else {
        xs1[k++] = x;
        ++i;
      }
    }
    while (i < mid) {
      xs1[k++] = xs2[i++];
    }
    while (j < to) {
      xs1[k++] = xs2[j++];
    }
  }
  return function(compare2) {
    return function(fromOrdering) {
      return function(xs) {
        return function() {
          if (xs.length < 2)
            return xs;
          mergeFromTo(compare2, fromOrdering, xs, xs.slice(0), 0, xs.length);
          return xs;
        };
      };
    };
  };
}();

// output/Data.Array/index.js
var findIndex2 = /* @__PURE__ */ function() {
  return findIndexImpl(Just.create)(Nothing.value);
}();
var elemIndex = function(dictEq) {
  return function(x) {
    return findIndex2(function(v) {
      return eq(dictEq)(v)(x);
    });
  };
};
var elem2 = function(dictEq) {
  return function(a) {
    return function(arr) {
      return isJust(elemIndex(dictEq)(a)(arr));
    };
  };
};

// output/Data.Array.NonEmpty.Internal/foreign.js
var traverse1Impl = function() {
  function Cont(fn) {
    this.fn = fn;
  }
  var emptyList = {};
  var ConsCell = function(head4, tail2) {
    this.head = head4;
    this.tail = tail2;
  };
  function finalCell(head4) {
    return new ConsCell(head4, emptyList);
  }
  function consList(x) {
    return function(xs) {
      return new ConsCell(x, xs);
    };
  }
  function listToArray(list) {
    var arr = [];
    var xs = list;
    while (xs !== emptyList) {
      arr.push(xs.head);
      xs = xs.tail;
    }
    return arr;
  }
  return function(apply2) {
    return function(map2) {
      return function(f) {
        var buildFrom = function(x, ys) {
          return apply2(map2(consList)(f(x)))(ys);
        };
        var go = function(acc, currentLen, xs) {
          if (currentLen === 0) {
            return acc;
          } else {
            var last3 = xs[currentLen - 1];
            return new Cont(function() {
              var built = go(buildFrom(last3, acc), currentLen - 1, xs);
              return built;
            });
          }
        };
        return function(array) {
          var acc = map2(finalCell)(f(array[array.length - 1]));
          var result = go(acc, array.length - 1, array);
          while (result instanceof Cont) {
            result = result.fn();
          }
          return map2(listToArray)(result);
        };
      };
    };
  };
}();

// output/Data.Function.Uncurried/foreign.js
var mkFn5 = function(fn) {
  return function(a, b, c, d, e) {
    return fn(a)(b)(c)(d)(e);
  };
};

// output/Data.String.CodePoints/foreign.js
var hasArrayFrom = typeof Array.from === "function";
var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
var hasCodePointAt = typeof String.prototype.codePointAt === "function";
var _unsafeCodePointAt0 = function(fallback) {
  return hasCodePointAt ? function(str) {
    return str.codePointAt(0);
  } : fallback;
};
var _codePointAt = function(fallback) {
  return function(Just2) {
    return function(Nothing2) {
      return function(unsafeCodePointAt02) {
        return function(index3) {
          return function(str) {
            var length6 = str.length;
            if (index3 < 0 || index3 >= length6)
              return Nothing2;
            if (hasStringIterator) {
              var iter = str[Symbol.iterator]();
              for (var i = index3; ; --i) {
                var o = iter.next();
                if (o.done)
                  return Nothing2;
                if (i === 0)
                  return Just2(unsafeCodePointAt02(o.value));
              }
            }
            return fallback(index3)(str);
          };
        };
      };
    };
  };
};

// output/Data.String.Common/index.js
var $$null = function(s) {
  return s === "";
};

// output/Data.String.CodePoints/index.js
var $runtime_lazy7 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var unsurrogate = function(lead) {
  return function(trail) {
    return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
  };
};
var isTrail = function(cu) {
  return 56320 <= cu && cu <= 57343;
};
var isLead = function(cu) {
  return 55296 <= cu && cu <= 56319;
};
var uncons3 = function(s) {
  var v = length3(s);
  if (v === 0) {
    return Nothing.value;
  }
  ;
  if (v === 1) {
    return new Just({
      head: fromEnum(boundedEnumChar)(charAt(0)(s)),
      tail: ""
    });
  }
  ;
  var cu1 = fromEnum(boundedEnumChar)(charAt(1)(s));
  var cu0 = fromEnum(boundedEnumChar)(charAt(0)(s));
  var $21 = isLead(cu0) && isTrail(cu1);
  if ($21) {
    return new Just({
      head: unsurrogate(cu0)(cu1),
      tail: drop2(2)(s)
    });
  }
  ;
  return new Just({
    head: cu0,
    tail: drop2(1)(s)
  });
};
var unsafeCodePointAt0Fallback = function(s) {
  var cu0 = fromEnum(boundedEnumChar)(charAt(0)(s));
  var $25 = isLead(cu0) && length3(s) > 1;
  if ($25) {
    var cu1 = fromEnum(boundedEnumChar)(charAt(1)(s));
    var $26 = isTrail(cu1);
    if ($26) {
      return unsurrogate(cu0)(cu1);
    }
    ;
    return cu0;
  }
  ;
  return cu0;
};
var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
var eqCodePoint = {
  eq: function(x) {
    return function(y) {
      return x === y;
    };
  }
};
var ordCodePoint = {
  compare: function(x) {
    return function(y) {
      return compare(ordInt)(x)(y);
    };
  },
  Eq0: function() {
    return eqCodePoint;
  }
};
var codePointAtFallback = function($copy_n) {
  return function($copy_s) {
    var $tco_var_n = $copy_n;
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(n, s) {
      var v = uncons3(s);
      if (v instanceof Just) {
        var $44 = n === 0;
        if ($44) {
          $tco_done = true;
          return new Just(v.value0.head);
        }
        ;
        $tco_var_n = n - 1 | 0;
        $copy_s = v.value0.tail;
        return;
      }
      ;
      $tco_done = true;
      return Nothing.value;
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($tco_var_n, $copy_s);
    }
    ;
    return $tco_result;
  };
};
var codePointAt = function(v) {
  return function(v1) {
    if (v < 0) {
      return Nothing.value;
    }
    ;
    if (v === 0 && v1 === "") {
      return Nothing.value;
    }
    ;
    if (v === 0) {
      return new Just(unsafeCodePointAt0(v1));
    }
    ;
    return _codePointAt(codePointAtFallback)(Just.create)(Nothing.value)(unsafeCodePointAt0)(v)(v1);
  };
};
var boundedCodePoint = {
  bottom: 0,
  top: 1114111,
  Ord0: function() {
    return ordCodePoint;
  }
};
var boundedEnumCodePoint = /* @__PURE__ */ function() {
  return {
    cardinality: 1114111 + 1 | 0,
    fromEnum: function(v) {
      return v;
    },
    toEnum: function(n) {
      if (n >= 0 && n <= 1114111) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.String.CodePoints (line 63, column 1 - line 68, column 26): " + [n.constructor.name]);
    },
    Bounded0: function() {
      return boundedCodePoint;
    },
    Enum1: function() {
      return $lazy_enumCodePoint(0);
    }
  };
}();
var $lazy_enumCodePoint = /* @__PURE__ */ $runtime_lazy7("enumCodePoint", "Data.String.CodePoints", function() {
  return {
    succ: defaultSucc(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
    pred: defaultPred(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
    Ord0: function() {
      return ordCodePoint;
    }
  };
});

// output/Parsing.String/index.js
var updatePosSingle = function(v) {
  return function(cp) {
    return function(after) {
      var v1 = fromEnum(boundedEnumCodePoint)(cp);
      if (v1 === 10) {
        return {
          index: v.index + 1 | 0,
          line: v.line + 1 | 0,
          column: 1
        };
      }
      ;
      if (v1 === 13) {
        var v2 = codePointAt(0)(after);
        if (v2 instanceof Just && fromEnum(boundedEnumCodePoint)(v2.value0) === 10) {
          return {
            index: v.index + 1 | 0,
            line: v.line,
            column: v.column
          };
        }
        ;
        return {
          index: v.index + 1 | 0,
          line: v.line + 1 | 0,
          column: 1
        };
      }
      ;
      if (v1 === 9) {
        return {
          index: v.index + 1 | 0,
          line: v.line,
          column: (v.column + 8 | 0) - mod(euclideanRingInt)(v.column - 1 | 0)(8) | 0
        };
      }
      ;
      return {
        index: v.index + 1 | 0,
        line: v.line,
        column: v.column + 1 | 0
      };
    };
  };
};
var satisfy = function(f) {
  return mkFn5(function(v) {
    return function(v1) {
      return function(v2) {
        return function($$throw) {
          return function(done) {
            var v3 = uncons3(v.value0);
            if (v3 instanceof Nothing) {
              return $$throw(v, new ParseError("Unexpected EOF", v.value1));
            }
            ;
            if (v3 instanceof Just) {
              var cp = fromEnum(boundedEnumCodePoint)(v3.value0.head);
              var $53 = cp < 0 || cp > 65535;
              if ($53) {
                return $$throw(v, new ParseError("Expected Char", v.value1));
              }
              ;
              var ch = fromJust()(toEnum(boundedEnumChar)(cp));
              var $54 = f(ch);
              if ($54) {
                return done(new ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), ch);
              }
              ;
              return $$throw(v, new ParseError("Predicate unsatisfied", v.value1));
            }
            ;
            throw new Error("Failed pattern match at Parsing.String (line 109, column 7 - line 124, column 75): " + [v3.constructor.name]);
          };
        };
      };
    };
  });
};
var eof = /* @__PURE__ */ mkFn5(function(v) {
  return function(v1) {
    return function(v2) {
      return function($$throw) {
        return function(done) {
          var $70 = $$null(v.value0);
          if ($70) {
            return done(new ParseState(v.value0, v.value1, true), unit);
          }
          ;
          return $$throw(v, new ParseError("Expected EOF", v.value1));
        };
      };
    };
  };
});
var $$char = function(c) {
  return withErrorMessage(satisfy(function(v) {
    return v === c;
  }))(show(showChar)(c));
};

// output/Parsing.String.Basic/index.js
var oneOf2 = function(ss) {
  return withLazyErrorMessage(satisfy(flip(elem2(eqChar))(ss)))(function(v) {
    return "one of " + show(showArray(showChar))(ss);
  });
};

// output/Rhythmic/index.js
var Full = /* @__PURE__ */ function() {
  function Full2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Full2.create = function(value0) {
    return function(value1) {
      return new Full2(value0, value1);
    };
  };
  return Full2;
}();
var K = /* @__PURE__ */ function() {
  function K2(value0) {
    this.value0 = value0;
  }
  ;
  K2.create = function(value0) {
    return new K2(value0);
  };
  return K2;
}();
var InverseK = /* @__PURE__ */ function() {
  function InverseK2(value0) {
    this.value0 = value0;
  }
  ;
  InverseK2.create = function(value0) {
    return new InverseK2(value0);
  };
  return InverseK2;
}();
var Onsets = /* @__PURE__ */ function() {
  function Onsets2(value0) {
    this.value0 = value0;
  }
  ;
  Onsets2.create = function(value0) {
    return new Onsets2(value0);
  };
  return Onsets2;
}();
var Pattern = /* @__PURE__ */ function() {
  function Pattern2(value0) {
    this.value0 = value0;
  }
  ;
  Pattern2.create = function(value0) {
    return new Pattern2(value0);
  };
  return Pattern2;
}();
var Subdivision = /* @__PURE__ */ function() {
  function Subdivision2(value0) {
    this.value0 = value0;
  }
  ;
  Subdivision2.create = function(value0) {
    return new Subdivision2(value0);
  };
  return Subdivision2;
}();
var Euclidean = /* @__PURE__ */ function() {
  function Euclidean2(value0, value1, value2, value3) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
    this.value3 = value3;
  }
  ;
  Euclidean2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return function(value3) {
          return new Euclidean2(value0, value1, value2, value3);
        };
      };
    };
  };
  return Euclidean2;
}();
var Repetition = /* @__PURE__ */ function() {
  function Repetition2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Repetition2.create = function(value0) {
    return function(value1) {
      return new Repetition2(value0, value1);
    };
  };
  return Repetition2;
}();
var onset = /* @__PURE__ */ bind(bindParserT)(/* @__PURE__ */ choice(foldableArray)([/* @__PURE__ */ applySecond(applyParserT)(/* @__PURE__ */ oneOf2(["x"]))(/* @__PURE__ */ pure(applicativeParserT)(true)), /* @__PURE__ */ applySecond(applyParserT)(/* @__PURE__ */ oneOf2(["o"]))(/* @__PURE__ */ pure(applicativeParserT)(false))]))(function(x) {
  return bind(bindParserT)(pure(applicativeParserT)(1))(function() {
    return pure(applicativeParserT)(x);
  });
});
var onsets = /* @__PURE__ */ bind(bindParserT)(/* @__PURE__ */ many(onset))(function(xs) {
  return bind(bindParserT)(pure(applicativeParserT)(1))(function() {
    return pure(applicativeParserT)(new Onsets(fromFoldable(foldableList)(xs)));
  });
});
var rhythmicShowInstance = {
  show: function(v) {
    if (v instanceof Onsets) {
      return "onsets " + show(showList2(showBoolean))(v.value0);
    }
    ;
    if (v instanceof Pattern) {
      return "pattern " + show(showList2(rhythmicShowInstance))(v.value0);
    }
    ;
    if (v instanceof Subdivision) {
      return "subdivision: " + show(showList2(rhythmicShowInstance))(v.value0);
    }
    ;
    if (v instanceof Euclidean) {
      return show(euclideanShowInstance)(v.value0) + (" euclidean " + (show(showInt)(v.value1) + ("," + (show(showInt)(v.value2) + ("," + show(showInt)(v.value3))))));
    }
    ;
    if (v instanceof Repetition) {
      return show(rhythmicShowInstance)(v.value0) + (" times " + show(showInt)(v.value1));
    }
    ;
    throw new Error("Failed pattern match at Rhythmic (line 91, column 1 - line 96, column 60): " + [v.constructor.name]);
  }
};
var euclideanShowInstance = {
  show: function(v) {
    if (v instanceof Full) {
      return show(rhythmicShowInstance)(v.value0) + ("on ks and not on ks " + show(rhythmicShowInstance)(v.value1));
    }
    ;
    if (v instanceof K) {
      return show(rhythmicShowInstance)(v.value0);
    }
    ;
    if (v instanceof InverseK) {
      return show(rhythmicShowInstance)(v.value0);
    }
    ;
    throw new Error("Failed pattern match at Rhythmic (line 86, column 1 - line 89, column 29): " + [v.constructor.name]);
  }
};
var chainRhythms = function(x) {
  return function(y) {
    return new Pattern(snoc(fromFoldable(foldableArray)([x]))(y));
  };
};
var patterns = /* @__PURE__ */ bind(bindParserT)(/* @__PURE__ */ chainl1(/* @__PURE__ */ choice(foldableArray)([/* @__PURE__ */ $$try(onsets)]))(/* @__PURE__ */ voidLeft(functorParserT)(/* @__PURE__ */ $$char(" "))(chainRhythms)))(function(x) {
  return bind(bindParserT)(pure(applicativeParserT)(1))(function() {
    return pure(applicativeParserT)(x);
  });
});
var topRhythmic = /* @__PURE__ */ bind(bindParserT)(/* @__PURE__ */ choice(foldableArray)([patterns]))(function(r) {
  return discard(discardUnit)(bindParserT)(eof)(function() {
    return pure(applicativeParserT)(r);
  });
});

// output/Motor/index.js
var Coord = /* @__PURE__ */ function() {
  function Coord2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  Coord2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new Coord2(value0, value1, value2);
      };
    };
  };
  return Coord2;
}();
var toL = function(dictFoldable) {
  return function(x) {
    return fromFoldable(dictFoldable)(x);
  };
};
var iToN = function(x) {
  return toNumber(x);
};
var multiplePatternW = function(indexAtPhrase) {
  return function(mo) {
    return function(first) {
      return function(o1) {
        return function(last$prime) {
          var o$prime = concat(replicate(mo - 1 | 0)(o1));
          var lenO = length(o1);
          var middle = map(functorList)(function(x) {
            return fst(x) + snd(x);
          })(zip(o$prime)(map(functorList)(iToN)(concat(toL(foldableList2)(map(functorList)(function(x) {
            return replicate(lenO)(x);
          })(toL(foldableList2)(range2(1)(mo - 1 | 0))))))));
          var last3 = function() {
            var $11 = eq(eqList2(eqNumber))(last$prime)(toL(foldableArray)([]));
            if ($11) {
              return toL(foldableArray)([]);
            }
            ;
            return map(functorList)(function(v) {
              return v + iToN(mo);
            })(last$prime);
          }();
          return map(functorList)(function(v) {
            return v + iToN(indexAtPhrase);
          })(concat(toL(foldableArray)([first, middle, last3])));
        };
      };
    };
  };
};
var twoPatternW = function(indexPhrase) {
  return function(first) {
    return function(last$prime) {
      var last3 = function() {
        var $12 = eq(eqList2(eqNumber))(last$prime)(toL(foldableArray)([]));
        if ($12) {
          return toL(foldableArray)([]);
        }
        ;
        return map(functorList)(function(v) {
          return v + 1;
        })(toL(foldableList2)(last$prime));
      }();
      return map(functorList)(function(v) {
        return v + iToN(indexPhrase);
      })(concat(toL(foldableArray)([first, last3])));
    };
  };
};
var getIndexSimple = function(start) {
  return function(end) {
    return function(o1) {
      var between2 = filter(function(x) {
        return x > start && end >= x;
      })(o1);
      var before = length(filter(function(x) {
        return x < start;
      })(o1));
      var $13 = before === 0;
      if ($13) {
        return range2(0)(length(between2));
      }
      ;
      return range2(before)(length(between2));
    };
  };
};
var getIndexOfMiddleList = function(middleLen) {
  return function(o1) {
    var x = toL(foldableList2)(range2(0)(length(o1)));
    return take(middleLen)(cycle(x));
  };
};
var getIndexOfLastList = function(x) {
  return function(o1) {
    if (eq(eqList2(eqNumber))(x)(toL(foldableArray)([]))) {
      return toL(foldableArray)([]);
    }
    ;
    if (otherwise) {
      return range2(0)(length(x) - 1 | 0);
    }
    ;
    throw new Error("Failed pattern match at Motor (line 161, column 1 - line 161, column 60): " + [x.constructor.name, o1.constructor.name]);
  };
};
var getIndexOfFirstList = function(x) {
  return function(o1) {
    if (eq(eqList2(eqNumber))(x)(toL(foldableArray)([]))) {
      return toL(foldableArray)([]);
    }
    ;
    if (otherwise) {
      return range2(length(o1) - length(x) | 0)(length(o1) - 1 | 0);
    }
    ;
    throw new Error("Failed pattern match at Motor (line 152, column 1 - line 152, column 61): " + [x.constructor.name, o1.constructor.name]);
  };
};
var fromPatternToList = function(v) {
  if (v instanceof Onsets) {
    return v.value0;
  }
  ;
  return fromFoldable(foldableArray)([false]);
};
var fromRhythmicToList = function(v) {
  if (v instanceof Onsets) {
    return v.value0;
  }
  ;
  if (v instanceof Pattern) {
    return concat(map(functorList)(fromPatternToList)(v.value0));
  }
  ;
  return fromFoldable(foldableArray)([false]);
};
var floor3 = function(x) {
  return floor2(x);
};
var toRat = function(x) {
  var floored = floor3(x);
  var fract = x - iToN(floored);
  var fract$prime = round2(fract * iToN(1e6));
  return add(semiringRatio(ordInt)(euclideanRingInt))(reduce(ordInt)(euclideanRingInt)(floored)(1))(reduce(ordInt)(euclideanRingInt)(fract$prime)(1e6));
};
var positionToTime = function(t1) {
  return function(lenPasaje) {
    return function(v) {
      var posInTempo = mul(semiringRatio(ordInt)(euclideanRingInt))(toRat(v.value0))(lenPasaje);
      var countInTime = countToTime(t1)(posInTempo);
      return new Coord(unwrap()(unInstant(fromDateTime(countInTime))), floor3(v.value0), v.value1);
    };
  };
};
var filterEvents = function(nPassages) {
  return function(start) {
    return function(end) {
      return function(passageAtStart) {
        return function(o1) {
          if (nPassages === 0) {
            var x = map(functorList)(function(v) {
              return v + iToN(floor3(passageAtStart));
            })(filter(function(x1) {
              return x1 >= start && x1 < end;
            })(o1));
            return zip(x)(getIndexSimple(start)(end)(o1));
          }
          ;
          if (nPassages === 1) {
            var lastList = filter(function(x2) {
              return x2 >= 0 && x2 < end;
            })(o1);
            var indexLast = getIndexOfLastList(lastList)(o1);
            var firstList = filter(function(x2) {
              return x2 >= start && x2 < 1;
            })(o1);
            var indexFst = getIndexOfFirstList(firstList)(o1);
            var listOfIndexes = concat(toL(foldableArray)([indexFst, indexLast]));
            var listOfEvents = twoPatternW(floor3(passageAtStart))(firstList)(lastList);
            return zip(listOfEvents)(listOfIndexes);
          }
          ;
          if (otherwise) {
            var middleList = take(floor3(iToN(length(o1)) * (iToN(nPassages) - 1)))(cycle(o1));
            var middleIndex = getIndexOfMiddleList(length(middleList))(o1);
            var lastList = filter(function(x2) {
              return x2 >= 0 && x2 < end;
            })(o1);
            var lastIndex = getIndexOfLastList(lastList)(o1);
            var firstList = filter(function(x2) {
              return x2 >= start && x2 < 1;
            })(o1);
            var fstIndex = getIndexOfFirstList(firstList)(o1);
            var listOfIndexes = concat(toL(foldableArray)([fstIndex, middleIndex, lastIndex]));
            var listOfEvents = multiplePatternW(floor3(passageAtStart))(nPassages)(firstList)(o1)(lastList);
            return zip(listOfEvents)(listOfIndexes);
          }
          ;
          throw new Error("Failed pattern match at Motor (line 123, column 1 - line 123, column 92): " + [nPassages.constructor.name, start.constructor.name, end.constructor.name, passageAtStart.constructor.name, o1.constructor.name]);
        };
      };
    };
  };
};
var passagePosition = function(o1) {
  return function(lenPasaje) {
    return function(t1) {
      return function(ws1) {
        return function(we1) {
          return function(eval1) {
            var countAtStart = timeToCountNumber(t1)(ws1);
            var passageAtStart = countAtStart / toNumber2(lenPasaje);
            var percentAtStart = passageAtStart - iToN(floor3(passageAtStart));
            var countAtEnd = timeToCountNumber(t1)(we1);
            var passageAtEnd = countAtEnd / toNumber2(lenPasaje);
            var nPassages = floor3(passageAtEnd) - floor3(passageAtStart) | 0;
            var percentAtEnd = passageAtEnd - iToN(floor3(passageAtEnd));
            var filtrado = filterEvents(nPassages)(percentAtStart)(percentAtEnd)(passageAtStart)(o1);
            var posToTime = map(functorList)(function(x) {
              return positionToTime(t1)(lenPasaje)(x);
            })(filtrado);
            return fromFoldableWithIndex(ordInt)(foldableWithIndexList)(posToTime);
          };
        };
      };
    };
  };
};
var fromPassageToCoord = function(rhy) {
  return function(t1) {
    return function(ws1) {
      return function(we1) {
        return function(eval1) {
          var x = fromRhythmicToList(rhy);
          var passageLength = fromInt(length(x));
          var onsets2 = map(functorList)(function($37) {
            return fromInt(snd($37));
          })(filter(function(x1) {
            return fst(x1) === true;
          })(zip(x)(range2(0)(length(x)))));
          var oPercen = map(functorList)(function($38) {
            return toNumber2(function(v) {
              return div(euclideanRingRatio(ordInt)(euclideanRingInt))(v)(passageLength);
            }($38));
          })(onsets2);
          return passagePosition(oPercen)(passageLength)(t1)(ws1)(we1)(eval1);
        };
      };
    };
  };
};

// output/Main/index.js
var unsafeMaybeMilliseconds = function($copy_v) {
  var $tco_done = false;
  var $tco_result;
  function $tco_loop(v) {
    if (v instanceof Just) {
      $tco_done = true;
      return v.value0;
    }
    ;
    if (v instanceof Nothing) {
      $copy_v = instant(0);
      return;
    }
    ;
    throw new Error("Failed pattern match at Main (line 108, column 1 - line 108, column 51): " + [v.constructor.name]);
  }
  ;
  while (!$tco_done) {
    $tco_result = $tco_loop($copy_v);
  }
  ;
  return $tco_result;
};
var testMaybeInstant = function(x) {
  return instant(x);
};
var setTempo = function(timekNot) {
  return function(t) {
    return write(fromForeignTempo(t))(timekNot.tempo);
  };
};
var pErrorToString = function(v) {
  if (v instanceof Left) {
    return new Left(parseErrorMessage(v.value0));
  }
  ;
  if (v instanceof Right) {
    return new Right(v.value0);
  }
  ;
  throw new Error("Failed pattern match at Main (line 82, column 1 - line 82, column 70): " + [v.constructor.name]);
};
var numToDateTime = function(x) {
  var asMaybeInstant = instant(x);
  var asInstant = unsafeMaybeMilliseconds(asMaybeInstant);
  return toDateTime(asInstant);
};
var launch = function __do() {
  log2("timekNot-CU: launch")();
  var ast = $$new(new Onsets(fromFoldable(foldableArray)([false])))();
  var tempo = bind(bindEffect)(newTempo(reduce(ordInt)(euclideanRingInt)(4)(1)))($$new)();
  var $$eval = bind(bindEffect)(nowDateTime)($$new)();
  return {
    ast,
    tempo,
    "eval": $$eval
  };
};
var evaluate = function(timekNot) {
  return function(str) {
    return function __do2() {
      log2("timekNot-CU: evaluate")();
      var rhythmic = read(timekNot.ast)();
      var $$eval = nowDateTime();
      var pr = pErrorToString(runParser(str)(topRhythmic));
      if (pr instanceof Left) {
        return {
          success: false,
          error: pr.value0
        };
      }
      ;
      if (pr instanceof Right) {
        write($$eval)(timekNot["eval"])();
        write(pr.value0)(timekNot.ast)();
        return {
          success: true,
          error: ""
        };
      }
      ;
      throw new Error("Failed pattern match at Main (line 75, column 3 - line 80, column 42): " + [pr.constructor.name]);
    };
  };
};
var debugging = function(a) {
  return function __do2() {
    log2(show(showNumber)(a))();
    return unit;
  };
};
var coordToEvent = function(v) {
  return {
    whenPosix: v.value0,
    s: "cp",
    n: 0
  };
};
var fromCoordenateToArray = function(x) {
  return function(t) {
    return function(ws) {
      return function(we) {
        return function($$eval) {
          var coords = fromPassageToCoord(x)(t)(ws)(we)($$eval);
          var coordsfromMapToArray = toUnfoldable(unfoldableArray)(values(coords));
          var events = map(functorArray)(coordToEvent)(coordsfromMapToArray);
          return events;
        };
      };
    };
  };
};
var timekNotToEvents = function(tk) {
  return function(ws) {
    return function(we) {
      return function __do2() {
        var rhy = read(tk.ast)();
        var t = read(tk.tempo)();
        var $$eval = read(tk["eval"])();
        log2(show(rhythmicShowInstance)(rhy))();
        log2(show(showDateTime)(ws))();
        log2(show(showDateTime)(we))();
        var events = fromCoordenateToArray(rhy)(t)(ws)(we)($$eval);
        log2(show(showArray(showRecord()()(showRecordFieldsCons({
          reflectSymbol: function() {
            return "n";
          }
        })(showRecordFieldsCons({
          reflectSymbol: function() {
            return "s";
          }
        })(showRecordFieldsCons({
          reflectSymbol: function() {
            return "whenPosix";
          }
        })(showRecordFieldsNil)(showNumber))(showString))(showInt))))(events))();
        return map(functorArray)(unsafeToForeign)(events);
      };
    };
  };
};
var scheduleNoteEvents = function(tk) {
  return function(ws) {
    return function(we) {
      var d2 = debugging(we);
      var d1 = debugging(ws);
      return timekNotToEvents(tk)(numToDateTime(ws))(numToDateTime(we));
    };
  };
};
export {
  coordToEvent,
  debugging,
  evaluate,
  fromCoordenateToArray,
  launch,
  numToDateTime,
  pErrorToString,
  scheduleNoteEvents,
  setTempo,
  testMaybeInstant,
  timekNotToEvents,
  unsafeMaybeMilliseconds
};
