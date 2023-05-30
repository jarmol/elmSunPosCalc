(() => {
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

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };

  // output/Control.Apply/index.js
  var apply = function(dict) {
    return dict.apply;
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var liftA1 = function(dictApplicative) {
    var apply2 = apply(dictApplicative.Apply0());
    var pure1 = pure(dictApplicative);
    return function(f) {
      return function(a) {
        return apply2(pure1(f))(a);
      };
    };
  };

  // output/Control.Bind/index.js
  var bind = function(dict) {
    return dict.bind;
  };

  // output/Data.Bounded/foreign.js
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

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqIntImpl = refEq;

  // output/Data.Eq/index.js
  var eqInt = {
    eq: eqIntImpl
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
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };

  // output/Data.Ring/index.js
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
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
  var compare = function(dict) {
    return dict.compare;
  };
  var max = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(x) {
      return function(y) {
        var v = compare3(x)(y);
        if (v instanceof LT) {
          return y;
        }
        ;
        if (v instanceof EQ) {
          return x;
        }
        ;
        if (v instanceof GT) {
          return x;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): " + [v.constructor.name]);
      };
    };
  };
  var min = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(x) {
      return function(y) {
        var v = compare3(x)(y);
        if (v instanceof LT) {
          return x;
        }
        ;
        if (v instanceof EQ) {
          return x;
        }
        ;
        if (v instanceof GT) {
          return y;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 172, column 3 - line 175, column 12): " + [v.constructor.name]);
      };
    };
  };
  var clamp = function(dictOrd) {
    var min1 = min(dictOrd);
    var max1 = max(dictOrd);
    return function(low) {
      return function(hi) {
        return function(x) {
          return min1(hi)(max1(low)(x));
        };
      };
    };
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };

  // output/Data.Show/index.js
  var showInt = {
    show: showIntImpl
  };
  var show = function(dict) {
    return dict.show;
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
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
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
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply2) {
      return function(map3) {
        return function(pure2) {
          return function(f) {
            return function(array) {
              function go(bot, top2) {
                switch (top2 - bot) {
                  case 0:
                    return pure2([]);
                  case 1:
                    return map3(array1)(f(array[bot]));
                  case 2:
                    return apply2(map3(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply2(apply2(map3(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                    return apply2(map3(concat2)(go(bot, pivot)))(go(pivot, top2));
                }
              }
              return go(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Enum/index.js
  var toEnum = function(dict) {
    return dict.toEnum;
  };
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };

  // output/Data.Date.Component/index.js
  var $runtime_lazy = function(name, moduleName, init) {
    var state = 0;
    var val;
    return function(lineNumber) {
      if (state === 2)
        return val;
      if (state === 1)
        throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state = 1;
      val = init();
      state = 2;
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
  var ordYear = ordInt;
  var ordDay = ordInt;
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
  var $lazy_enumYear = /* @__PURE__ */ $runtime_lazy("enumYear", "Data.Date.Component", function() {
    return {
      succ: function() {
        var $55 = toEnum(boundedEnumYear);
        var $56 = fromEnum(boundedEnumYear);
        return function($57) {
          return $55(function(v) {
            return v + 1 | 0;
          }($56($57)));
        };
      }(),
      pred: function() {
        var $58 = toEnum(boundedEnumYear);
        var $59 = fromEnum(boundedEnumYear);
        return function($60) {
          return $58(function(v) {
            return v - 1 | 0;
          }($59($60)));
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
  var $lazy_enumMonth = /* @__PURE__ */ $runtime_lazy("enumMonth", "Data.Date.Component", function() {
    return {
      succ: function() {
        var $67 = toEnum(boundedEnumMonth);
        var $68 = fromEnum(boundedEnumMonth);
        return function($69) {
          return $67(function(v) {
            return v + 1 | 0;
          }($68($69)));
        };
      }(),
      pred: function() {
        var $70 = toEnum(boundedEnumMonth);
        var $71 = fromEnum(boundedEnumMonth);
        return function($72) {
          return $70(function(v) {
            return v - 1 | 0;
          }($71($72)));
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
  var $lazy_enumDay = /* @__PURE__ */ $runtime_lazy("enumDay", "Data.Date.Component", function() {
    return {
      succ: function() {
        var $73 = toEnum(boundedEnumDay);
        var $74 = fromEnum(boundedEnumDay);
        return function($75) {
          return $73(function(v) {
            return v + 1 | 0;
          }($74($75)));
        };
      }(),
      pred: function() {
        var $76 = toEnum(boundedEnumDay);
        var $77 = fromEnum(boundedEnumDay);
        return function($78) {
          return $76(function(v) {
            return v - 1 | 0;
          }($77($78)));
        };
      }(),
      Ord0: function() {
        return ordDay;
      }
    };
  });

  // output/Data.Int/foreign.js
  var toNumber = function(n) {
    return n;
  };

  // output/Data.Number/foreign.js
  var acos = Math.acos;
  var asin = Math.asin;
  var cos = Math.cos;
  var floor = Math.floor;
  var pow = function(n) {
    return function(p) {
      return Math.pow(n, p);
    };
  };
  var remainder = function(n) {
    return function(m) {
      return n % m;
    };
  };
  var round = Math.round;
  var sin = Math.sin;
  var tan = Math.tan;

  // output/Data.Number/index.js
  var pi = 3.141592653589793;

  // output/Data.Date/index.js
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumMonth);
  var fromJust2 = /* @__PURE__ */ fromJust();
  var toEnum2 = /* @__PURE__ */ toEnum(boundedEnumMonth);
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
  var month = function(v) {
    return v.value1;
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
              return new $$Date(y$prime, fromJust2(toEnum2(m$prime)), d$prime);
            };
          };
        };
        return canonicalDateImpl(mkDate, y, fromEnum2(m), d);
      };
    };
  };

  // output/Data.Number.Format/foreign.js
  function wrap(method) {
    return function(d) {
      return function(num) {
        return method.apply(num, [d]);
      };
    };
  }
  var toPrecisionNative = wrap(Number.prototype.toPrecision);
  var toFixedNative = wrap(Number.prototype.toFixed);
  var toExponentialNative = wrap(Number.prototype.toExponential);
  function toString(num) {
    return num.toString();
  }

  // output/Data.Number.Format/index.js
  var clamp2 = /* @__PURE__ */ clamp(ordInt);
  var Precision = /* @__PURE__ */ function() {
    function Precision2(value0) {
      this.value0 = value0;
    }
    ;
    Precision2.create = function(value0) {
      return new Precision2(value0);
    };
    return Precision2;
  }();
  var Fixed = /* @__PURE__ */ function() {
    function Fixed2(value0) {
      this.value0 = value0;
    }
    ;
    Fixed2.create = function(value0) {
      return new Fixed2(value0);
    };
    return Fixed2;
  }();
  var Exponential = /* @__PURE__ */ function() {
    function Exponential2(value0) {
      this.value0 = value0;
    }
    ;
    Exponential2.create = function(value0) {
      return new Exponential2(value0);
    };
    return Exponential2;
  }();
  var toStringWith = function(v) {
    if (v instanceof Precision) {
      return toPrecisionNative(v.value0);
    }
    ;
    if (v instanceof Fixed) {
      return toFixedNative(v.value0);
    }
    ;
    if (v instanceof Exponential) {
      return toExponentialNative(v.value0);
    }
    ;
    throw new Error("Failed pattern match at Data.Number.Format (line 59, column 1 - line 59, column 43): " + [v.constructor.name]);
  };
  var fixed = /* @__PURE__ */ function() {
    var $9 = clamp2(0)(20);
    return function($10) {
      return Fixed.create($9($10));
    };
  }();

  // output/Data.Time.Component/index.js
  var $runtime_lazy2 = function(name, moduleName, init) {
    var state = 0;
    var val;
    return function(lineNumber) {
      if (state === 2)
        return val;
      if (state === 1)
        throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state = 1;
      val = init();
      state = 2;
      return val;
    };
  };
  var ordSecond = ordInt;
  var ordMinute = ordInt;
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
  var $lazy_enumSecond = /* @__PURE__ */ $runtime_lazy2("enumSecond", "Data.Time.Component", function() {
    return {
      succ: function() {
        var $36 = toEnum(boundedEnumSecond);
        var $37 = fromEnum(boundedEnumSecond);
        return function($38) {
          return $36(function(v) {
            return v + 1 | 0;
          }($37($38)));
        };
      }(),
      pred: function() {
        var $39 = toEnum(boundedEnumSecond);
        var $40 = fromEnum(boundedEnumSecond);
        return function($41) {
          return $39(function(v) {
            return v - 1 | 0;
          }($40($41)));
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
  var $lazy_enumMinute = /* @__PURE__ */ $runtime_lazy2("enumMinute", "Data.Time.Component", function() {
    return {
      succ: function() {
        var $42 = toEnum(boundedEnumMinute);
        var $43 = fromEnum(boundedEnumMinute);
        return function($44) {
          return $42(function(v) {
            return v + 1 | 0;
          }($43($44)));
        };
      }(),
      pred: function() {
        var $45 = toEnum(boundedEnumMinute);
        var $46 = fromEnum(boundedEnumMinute);
        return function($47) {
          return $45(function(v) {
            return v - 1 | 0;
          }($46($47)));
        };
      }(),
      Ord0: function() {
        return ordMinute;
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
  var $lazy_enumHour = /* @__PURE__ */ $runtime_lazy2("enumHour", "Data.Time.Component", function() {
    return {
      succ: function() {
        var $54 = toEnum(boundedEnumHour);
        var $55 = fromEnum(boundedEnumHour);
        return function($56) {
          return $54(function(v) {
            return v + 1 | 0;
          }($55($56)));
        };
      }(),
      pred: function() {
        var $57 = toEnum(boundedEnumHour);
        var $58 = fromEnum(boundedEnumHour);
        return function($59) {
          return $57(function(v) {
            return v - 1 | 0;
          }($58($59)));
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
  var second = function(v) {
    return v.value2;
  };
  var minute = function(v) {
    return v.value1;
  };
  var hour = function(v) {
    return v.value0;
  };

  // output/Effect.Console/foreign.js
  var log2 = function(s) {
    return function() {
      console.log(s);
    };
  };

  // output/Effect.Now/foreign.js
  function now() {
    return Date.now();
  }

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
  var time2 = function(v) {
    return v.value1;
  };
  var date = function(v) {
    return v.value0;
  };

  // output/Data.DateTime.Instant/foreign.js
  function toDateTimeImpl(ctor) {
    return function(instant) {
      var dt = new Date(instant);
      return ctor(dt.getUTCFullYear())(dt.getUTCMonth() + 1)(dt.getUTCDate())(dt.getUTCHours())(dt.getUTCMinutes())(dt.getUTCSeconds())(dt.getUTCMilliseconds());
    };
  }

  // output/Data.DateTime.Instant/index.js
  var fromJust3 = /* @__PURE__ */ fromJust();
  var toEnum3 = /* @__PURE__ */ toEnum(boundedEnumMonth);
  var toDateTime = /* @__PURE__ */ function() {
    var mkDateTime = function(y) {
      return function(mo) {
        return function(d) {
          return function(h) {
            return function(mi) {
              return function(s) {
                return function(ms) {
                  return new DateTime(canonicalDate(y)(fromJust3(toEnum3(mo)))(d), new Time(h, mi, s, ms));
                };
              };
            };
          };
        };
      };
    };
    return toDateTimeImpl(mkDateTime);
  }();

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

  // output/Control.Monad/index.js
  var ap = function(dictMonad) {
    var bind2 = bind(dictMonad.Bind1());
    var pure2 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a) {
        return bind2(f)(function(f$prime) {
          return bind2(a)(function(a$prime) {
            return pure2(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy3 = function(name, moduleName, init) {
    var state = 0;
    var val;
    return function(lineNumber) {
      if (state === 2)
        return val;
      if (state === 1)
        throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state = 1;
      val = init();
      state = 2;
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
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy3("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy3("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

  // output/Effect.Now/index.js
  var map2 = /* @__PURE__ */ map(functorEffect);
  var nowTime = /* @__PURE__ */ map2(function($2) {
    return time2(toDateTime($2));
  })(now);
  var nowDate = /* @__PURE__ */ map2(function($3) {
    return date(toDateTime($3));
  })(now);

  // output/Main/index.js
  var show2 = /* @__PURE__ */ show(showInt);
  var div1 = /* @__PURE__ */ div(euclideanRingInt);
  var fromEnum3 = /* @__PURE__ */ fromEnum(boundedEnumYear);
  var fromEnum1 = /* @__PURE__ */ fromEnum(boundedEnumMonth);
  var fromEnum22 = /* @__PURE__ */ fromEnum(boundedEnumDay);
  var fromEnum32 = /* @__PURE__ */ fromEnum(boundedEnumSecond);
  var fromEnum4 = /* @__PURE__ */ fromEnum(boundedEnumMinute);
  var fromEnum5 = /* @__PURE__ */ fromEnum(boundedEnumHour);
  var toDeg = function(beta) {
    return 180 * beta / pi;
  };
  var tanDeg = function(x) {
    return tan(pi * x / 180);
  };
  var stringA = function(j) {
    return function(m) {
      return function(t) {
        var preZero = function(n) {
          var $25 = n < 10;
          if ($25) {
            return "0";
          }
          ;
          return "";
        };
        return show2(j) + ("-" + (preZero(m) + (show2(m) + ("-" + (preZero(t) + show2(t))))));
      };
    };
  };
  var sinDeg = function(x) {
    return sin(pi * x / 180);
  };
  var negdiv = function(n) {
    return function(m) {
      var $26 = n < 0;
      if ($26) {
        return -div1(-n | 0)(m) | 0;
      }
      ;
      return div1(n)(m);
    };
  };
  var mnsToHrMnSc = function(mns) {
    var sc = round(remainder(mns * 60)(60));
    var pre0 = function(t) {
      var $27 = t < 10;
      if ($27) {
        return "0";
      }
      ;
      return "";
    };
    var sc0 = pre0(sc);
    var hr = floor(mns / 60);
    var hr0 = pre0(hr);
    var mn = floor(mns - 60 * hr);
    var mn0 = pre0(mn);
    return hr0 + (toString(hr) + (":" + (mn0 + (toString(mn) + (":" + (sc0 + toString(sc)))))));
  };
  var meanObliqEclip = function(cnt) {
    return 23 + (26 + (21.448 - cnt * (46.815 + cnt * (59e-5 - cnt * 1813e-6))) / 60) / 60;
  };
  var meanAnomalSun = function(cnt) {
    return 357.52911 + cnt * (35999.05029 + cnt * 1537e-7);
  };
  var sunEqCntr = function(cnt) {
    var anom = meanAnomalSun(cnt);
    return sinDeg(anom) * (1.914602 - cnt * (4817e-6 + 14e-6 * cnt)) + sinDeg(2 * anom) * (0.019993 - 101e-6 * cnt) + sinDeg(3 * anom) * 289e-6;
  };
  var jdnGr = function(y) {
    return function(m) {
      return function(d) {
        return (((negdiv(1461 * ((y + 4800 | 0) + negdiv(m - 14 | 0)(12) | 0) | 0)(4) + negdiv(367 * ((m - 2 | 0) - (12 * negdiv(m - 14 | 0)(12) | 0) | 0) | 0)(12) | 0) - (3 * negdiv(negdiv((y + 4900 | 0) + negdiv(m - 14 | 0)(12) | 0)(100))(4) | 0) | 0) + d | 0) - 32075 | 0;
      };
    };
  };
  var jdateGr = function(y) {
    return function(m) {
      return function(d) {
        return function(hr) {
          return function(mn) {
            return function(sc) {
              var jdn = jdnGr(y)(m)(d);
              return toNumber(jdn) + toNumber(hr - 12 | 0) / toNumber(24) + toNumber(mn) / toNumber(1440) + toNumber(sc) / 86400;
            };
          };
        };
      };
    };
  };
  var getCent = function(jd) {
    var jD2000 = toNumber(2451545);
    return (jd - jD2000) / 36525;
  };
  var eccentEarthOrbit = function(cnt) {
    return 0.016708634 - cnt * (42037e-9 + 1267e-10 * cnt);
  };
  var decmod = function($copy_x) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(x) {
      var $28 = x < 360;
      if ($28) {
        $tco_done = true;
        return x;
      }
      ;
      $copy_x = x - 360;
      return;
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_x);
    }
    ;
    return $tco_result;
  };
  var solAzimuth = function(preAz) {
    return function(hrAngle) {
      var $29 = hrAngle > 0;
      if ($29) {
        return decmod(preAz + 180);
      }
      ;
      return decmod(540 - preAz);
    };
  };
  var cosDeg = function(x) {
    return cos(pi * x / 180);
  };
  var obliqCorr = function(cnt) {
    return meanObliqEclip(cnt) + 256e-5 * cosDeg(125.04 - 1934.136 * cnt);
  };
  var variableY = function(cnt) {
    var x = tanDeg(obliqCorr(cnt) / 2);
    return x * x;
  };
  var calcSunML = function(cnt) {
    return 280.46646 + cnt * (36000.76983 + cnt * 3032e-7);
  };
  var equatTime = function(cnt) {
    var variaY = variableY(cnt);
    var meanLongS = calcSunML(cnt);
    var meanAnomS = meanAnomalSun(cnt);
    var eOrbitEx = eccentEarthOrbit(cnt);
    return toDeg(variaY * sinDeg(2 * meanLongS) - 2 * eOrbitEx * sinDeg(meanAnomS) + 4 * eOrbitEx * variaY * sinDeg(meanAnomS) * cosDeg(2 * meanLongS) - 0.5 * variaY * variaY * sinDeg(4 * meanLongS) - 1.25 * eOrbitEx * eOrbitEx * sinDeg(2 * meanAnomS)) * 4;
  };
  var getNoon = function(cnt) {
    return function(geoLong) {
      return function(timeZone) {
        var eqTime2 = equatTime(cnt);
        return 720 - 4 * geoLong - eqTime2 + timeZone * 60;
      };
    };
  };
  var trueSolTime = function(cnt) {
    return function(hr) {
      return function(mn) {
        return function(sc) {
          return function(tz) {
            return function(longit) {
              var v2 = equatTime(cnt);
              var e2 = 60 * (toNumber(hr) + tz) + toNumber(mn) + toNumber(sc) / 60;
              return e2 + v2 + 4 * longit - 60 * tz;
            };
          };
        };
      };
    };
  };
  var hourAngle = function(cnt) {
    return function(hr) {
      return function(mn) {
        return function(sc) {
          return function(tz) {
            return function(longit) {
              var tSt = trueSolTime(cnt)(hr)(mn)(sc)(tz)(longit);
              var $30 = tSt > 0;
              if ($30) {
                return 0.25 * tSt - 180;
              }
              ;
              return 0.25 * tSt + 180;
            };
          };
        };
      };
    };
  };
  var trueLongSun = function(cnt) {
    return calcSunML(cnt) + sunEqCntr(cnt);
  };
  var atmosRefract = function(solElev) {
    var $31 = solElev > 85;
    if ($31) {
      return 0;
    }
    ;
    var $32 = solElev > 5;
    if ($32) {
      return (58.1 / tanDeg(solElev) - 0.07 / pow(tanDeg(solElev))(3) + 86e-6 / pow(tanDeg(solElev))(5)) / 3600;
    }
    ;
    var $33 = solElev > -0.575;
    if ($33) {
      return (1735 + solElev * (-518.2 + solElev * (103.4 + solElev * (-12.79 + solElev * 0.711)))) / 3600;
    }
    ;
    return -20.772 / tanDeg(solElev) / 3600;
  };
  var asinDeg = function(x) {
    return toDeg(asin(x));
  };
  var appLongSun = function(cnt) {
    return trueLongSun(cnt) - 569e-5 - 478e-5 * sinDeg(125.04 - 1934.136 * cnt);
  };
  var sunDeclination = function(cnt) {
    var oblC = sinDeg(obliqCorr(cnt));
    var appLS = sinDeg(appLongSun(cnt));
    return asinDeg(oblC * appLS);
  };
  var acosDeg = function(x) {
    return toDeg(acos(x));
  };
  var preAzimuth = function(lat) {
    return function(zenith) {
      return function(decl) {
        return acosDeg((sinDeg(lat) * cosDeg(zenith) - sinDeg(decl)) / (cosDeg(lat) * sinDeg(zenith)));
      };
    };
  };
  var srHA = function(cnt) {
    return function(zenith) {
      var declination = sunDeclination(cnt);
      var x = cosDeg(zenith) / (cosDeg(65.85) * cosDeg(declination)) - tanDeg(65.85) * tanDeg(declination);
      var $34 = x > 0.99999 && declination < 0;
      if ($34) {
        return 0;
      }
      ;
      var $35 = x < -0.99999 && declination > 0;
      if ($35) {
        return 180;
      }
      ;
      return acosDeg(x);
    };
  };
  var risetMns = function(cnt) {
    return function(geoLong) {
      return function(timeZone) {
        return function(rsOption) {
          var noon = getNoon(cnt)(geoLong)(timeZone);
          var getHA = srHA(cnt)(90.833);
          var c = function() {
            if (rsOption) {
              return 1;
            }
            ;
            return -1;
          }();
          return noon + 4 * c * getHA;
        };
      };
    };
  };
  var sunRise = function(cnt) {
    return function(geoLong) {
      return function(timeZone) {
        return risetMns(cnt)(geoLong)(timeZone)(false);
      };
    };
  };
  var sunSet = function(cnt) {
    return function(geoLong) {
      return function(timeZone) {
        return risetMns(cnt)(geoLong)(timeZone)(true);
      };
    };
  };
  var getDate = function __do() {
    var currDate = nowDate();
    var currYear = fromEnum3(year(currDate));
    var currMonth = fromEnum1(month(currDate));
    var currDay = fromEnum22(day(currDate));
    var currentTime = nowTime();
    var cSecond = fromEnum32(second(currentTime));
    var cMinute = fromEnum4(minute(currentTime));
    var cHour = fromEnum5(hour(currentTime));
    var cent1 = getCent(jdateGr(currYear)(currMonth)(currDay)(cHour)(cMinute)(cSecond));
    var decl = sunDeclination(cent1);
    var declinationSun = toStringWith(fixed(5))(decl);
    var minutesNoon = getNoon(cent1)(24.18)(2);
    var sunriseHA = srHA(cent1)(90.833);
    var dayLength = sunriseHA * 8;
    var sunriseTornio = sunRise(cent1)(24.18)(2);
    var sunsetTornio = sunSet(cent1)(24.18)(2);
    var t2 = sunDeclination(cent1);
    var timeEquat = toStringWith(fixed(5))(equatTime(cent1));
    var hourAngle1 = hourAngle(cent1)(cHour)(cMinute)(cSecond)(2)(24.18);
    var trueSolarTime = trueSolTime(cent1)(cHour)(cMinute)(cSecond)(2)(24.18);
    var solarZenith = acosDeg(sinDeg(65.85) * sinDeg(t2) + cosDeg(65.85) * cosDeg(t2) * cosDeg(hourAngle1));
    var preAzim = preAzimuth(65.85)(solarZenith)(decl);
    var solAzim = solAzimuth(preAzim)(hourAngle1);
    var solarElevation = 90 - solarZenith;
    var atmosphericRefraction = atmosRefract(solarElevation);
    var refractCorrectAltitude = solarElevation + atmosphericRefraction;
    return log2("Date " + (stringA(currYear)(currMonth)(currDay) + (" Time UTC " + (show2(cHour) + (":" + (show2(cMinute) + (":" + (show2(cSecond) + ("\nCurrent Julian day JD = " + (toStringWith(fixed(6))(jdateGr(currYear)(currMonth)(currDay)(cHour)(cMinute)(cSecond)) + ("\nCentury " + (toStringWith(fixed(9))(cent1) + ("\nSun declination " + (declinationSun + ("\xB0" + ("\nTime Equation " + (timeEquat + (" minutes" + ("\nSunrise HA " + (toStringWith(fixed(5))(sunriseHA) + ("\nNoon time " + (mnsToHrMnSc(minutesNoon) + (" UTC+2h" + ("\nSunrise time " + (mnsToHrMnSc(sunriseTornio) + (" UTC+2h" + ("\nSunset time " + (mnsToHrMnSc(sunsetTornio) + (" UTC+2h" + ("\nDaylength " + (mnsToHrMnSc(dayLength) + ("\nTrue solar time " + (toStringWith(fixed(4))(trueSolarTime) + ("\nHour angle 2 " + (toStringWith(fixed(5))(hourAngle1) + ("\nSolar Zenith " + (toStringWith(fixed(4))(solarZenith) + ("\nSolar elevation " + (toStringWith(fixed(4))(solarElevation) + ("\xB0" + ("\nAtmospheric refraction " + (toStringWith(fixed(4))(atmosphericRefraction) + ("\xB0" + ("\nRefraction corrected elevation " + (toStringWith(fixed(4))(refractCorrectAltitude) + ("\xB0" + ("\nSolar azimuth " + (toStringWith(fixed(4))(solAzim) + "\xB0"))))))))))))))))))))))))))))))))))))))))))))))))();
  };
  var main = function __do2() {
    log2("SOLAR CALCULATOR")();
    getDate();
    return log2("\nJarmo Lammi \xA9 2023")();
  };

  // <stdin>
  main();
})();
