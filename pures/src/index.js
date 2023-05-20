(() => {
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

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };

  // output/Control.Apply/index.js
  var apply = function(dict) {
    return dict.apply;
  };

  // output/Control.Bind/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var bind = function(dict) {
    return dict.bind;
  };
  var join = function(dictBind) {
    var bind1 = bind(dictBind);
    return function(m) {
      return bind1(m)(identity2);
    };
  };

  // output/Data.Date/foreign.js
  var createDate = function(y, m, d) {
    var date = new Date(Date.UTC(y, m, d));
    if (y >= 0 && y < 100) {
      date.setUTCFullYear(y);
    }
    return date;
  };
  function canonicalDateImpl(ctor, y, m, d) {
    var date = createDate(y, m - 1, d);
    return ctor(date.getUTCFullYear())(date.getUTCMonth() + 1)(date.getUTCDate());
  }

  // output/Data.Bounded/foreign.js
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq5) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq5 : gt;
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

  // output/Data.Ord/index.js
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showNumberImpl = function(n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };

  // output/Data.Show/index.js
  var showNumber = {
    show: showNumberImpl
  };
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
  var showMaybe = function(dictShow) {
    var show9 = show(dictShow);
    return {
      show: function(v) {
        if (v instanceof Just) {
          return "(Just " + (show9(v.value0) + ")");
        }
        ;
        if (v instanceof Nothing) {
          return "Nothing";
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 223, column 1 - line 225, column 28): " + [v.constructor.name]);
      }
    };
  };
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
  var map2 = /* @__PURE__ */ map(functorMaybe);
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
          return map2(v.value0)(v1);
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
    return function(apply4) {
      return function(map5) {
        return function(pure2) {
          return function(f) {
            return function(array) {
              function go(bot, top2) {
                switch (top2 - bot) {
                  case 0:
                    return pure2([]);
                  case 1:
                    return map5(array1)(f(array[bot]));
                  case 2:
                    return apply4(map5(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply4(apply4(map5(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                    return apply4(map5(concat2)(go(bot, pivot)))(go(pivot, top2));
                }
              }
              return go(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var over = function() {
    return function() {
      return function(v) {
        return coerce2;
      };
    };
  };

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
  var show2 = /* @__PURE__ */ show(showInt);
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
      return "(Year " + (show2(v) + ")");
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
      return "(Day " + (show2(v) + ")");
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

  // output/Data.Time.Duration/index.js
  var show3 = /* @__PURE__ */ show(showNumber);
  var over2 = /* @__PURE__ */ over()();
  var Seconds = function(x) {
    return x;
  };
  var Milliseconds = function(x) {
    return x;
  };
  var Hours = function(x) {
    return x;
  };
  var Days = function(x) {
    return x;
  };
  var toDuration = function(dict) {
    return dict.toDuration;
  };
  var showDays = {
    show: function(v) {
      return "(Days " + (show3(v) + ")");
    }
  };
  var fromDuration = function(dict) {
    return dict.fromDuration;
  };
  var durationSeconds = {
    fromDuration: /* @__PURE__ */ over2(Seconds)(function(v) {
      return v * 1e3;
    }),
    toDuration: /* @__PURE__ */ over2(Milliseconds)(function(v) {
      return v / 1e3;
    })
  };
  var durationHours = {
    fromDuration: /* @__PURE__ */ over2(Hours)(function(v) {
      return v * 36e5;
    }),
    toDuration: /* @__PURE__ */ over2(Milliseconds)(function(v) {
      return v / 36e5;
    })
  };
  var durationDays = {
    fromDuration: /* @__PURE__ */ over2(Days)(function(v) {
      return v * 864e5;
    }),
    toDuration: /* @__PURE__ */ over2(Milliseconds)(function(v) {
      return v / 864e5;
    })
  };

  // output/Data.Date/index.js
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumMonth);
  var fromJust2 = /* @__PURE__ */ fromJust();
  var show4 = /* @__PURE__ */ show(showYear);
  var show1 = /* @__PURE__ */ show(showMonth);
  var show22 = /* @__PURE__ */ show(showDay);
  var eq12 = /* @__PURE__ */ eq(eqYear);
  var eq2 = /* @__PURE__ */ eq(eqMonth);
  var eq3 = /* @__PURE__ */ eq(eqDay);
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
  var showDate = {
    show: function(v) {
      return "(Date " + (show4(v.value0) + (" " + (show1(v.value1) + (" " + (show22(v.value2) + ")")))));
    }
  };
  var month = function(v) {
    return v.value1;
  };
  var eqDate = {
    eq: function(x) {
      return function(y) {
        return eq12(x.value0)(y.value0) && eq2(x.value1)(y.value1) && eq3(x.value2)(y.value2);
      };
    }
  };
  var eq4 = /* @__PURE__ */ eq(eqDate);
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
  var exactDate = function(y) {
    return function(m) {
      return function(d) {
        var dt = new $$Date(y, m, d);
        var $144 = eq4(canonicalDate(y)(m)(d))(dt);
        if ($144) {
          return new Just(dt);
        }
        ;
        return Nothing.value;
      };
    };
  };

  // output/Data.DateTime/foreign.js
  var createUTC = function(y, mo, d, h, m, s, ms) {
    var date = new Date(Date.UTC(y, mo, d, h, m, s, ms));
    if (y >= 0 && y < 100) {
      date.setUTCFullYear(y);
    }
    return date.getTime();
  };
  function calcDiff2(rec1, rec2) {
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
  var show5 = /* @__PURE__ */ show(showInt);
  var showSecond = {
    show: function(v) {
      return "(Second " + (show5(v) + ")");
    }
  };
  var showMinute = {
    show: function(v) {
      return "(Minute " + (show5(v) + ")");
    }
  };
  var showMillisecond = {
    show: function(v) {
      return "(Millisecond " + (show5(v) + ")");
    }
  };
  var showHour = {
    show: function(v) {
      return "(Hour " + (show5(v) + ")");
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
  var $lazy_enumMillisecond = /* @__PURE__ */ $runtime_lazy2("enumMillisecond", "Data.Time.Component", function() {
    return {
      succ: function() {
        var $48 = toEnum(boundedEnumMillisecond);
        var $49 = fromEnum(boundedEnumMillisecond);
        return function($50) {
          return $48(function(v) {
            return v + 1 | 0;
          }($49($50)));
        };
      }(),
      pred: function() {
        var $51 = toEnum(boundedEnumMillisecond);
        var $52 = fromEnum(boundedEnumMillisecond);
        return function($53) {
          return $51(function(v) {
            return v - 1 | 0;
          }($52($53)));
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
  var show6 = /* @__PURE__ */ show(showHour);
  var show12 = /* @__PURE__ */ show(showMinute);
  var show23 = /* @__PURE__ */ show(showSecond);
  var show32 = /* @__PURE__ */ show(showMillisecond);
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
      return "(Time " + (show6(v.value0) + (" " + (show12(v.value1) + (" " + (show23(v.value2) + (" " + (show32(v.value3) + ")")))))));
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
  var fromEnum3 = /* @__PURE__ */ fromEnum(boundedEnumYear);
  var fromEnum1 = /* @__PURE__ */ fromEnum(boundedEnumMonth);
  var fromEnum22 = /* @__PURE__ */ fromEnum(boundedEnumDay);
  var fromEnum32 = /* @__PURE__ */ fromEnum(boundedEnumHour);
  var fromEnum4 = /* @__PURE__ */ fromEnum(boundedEnumMinute);
  var fromEnum5 = /* @__PURE__ */ fromEnum(boundedEnumSecond);
  var fromEnum6 = /* @__PURE__ */ fromEnum(boundedEnumMillisecond);
  var show7 = /* @__PURE__ */ show(showDate);
  var show13 = /* @__PURE__ */ show(showTime);
  var bind2 = /* @__PURE__ */ bind(bindMaybe);
  var apply2 = /* @__PURE__ */ apply(applyMaybe);
  var map3 = /* @__PURE__ */ map(functorMaybe);
  var join2 = /* @__PURE__ */ join(bindMaybe);
  var toEnum3 = /* @__PURE__ */ toEnum(boundedEnumYear);
  var toEnum1 = /* @__PURE__ */ toEnum(boundedEnumMonth);
  var toEnum22 = /* @__PURE__ */ toEnum(boundedEnumDay);
  var toEnum32 = /* @__PURE__ */ toEnum(boundedEnumHour);
  var toEnum4 = /* @__PURE__ */ toEnum(boundedEnumMinute);
  var toEnum5 = /* @__PURE__ */ toEnum(boundedEnumSecond);
  var toEnum6 = /* @__PURE__ */ toEnum(boundedEnumMillisecond);
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
      year: fromEnum3(year(v.value0)),
      month: fromEnum1(month(v.value0)),
      day: fromEnum22(day(v.value0)),
      hour: fromEnum32(hour(v.value1)),
      minute: fromEnum4(minute(v.value1)),
      second: fromEnum5(second(v.value1)),
      millisecond: fromEnum6(millisecond(v.value1))
    };
  };
  var showDateTime = {
    show: function(v) {
      return "(DateTime " + (show7(v.value0) + (" " + (show13(v.value1) + ")")));
    }
  };
  var diff = function(dictDuration) {
    var toDuration2 = toDuration(dictDuration);
    return function(dt1) {
      return function(dt2) {
        return toDuration2(calcDiff2(toRecord(dt1), toRecord(dt2)));
      };
    };
  };
  var adjust = function(dictDuration) {
    var fromDuration2 = fromDuration(dictDuration);
    return function(d) {
      return function(dt) {
        return bind2(adjustImpl(Just.create)(Nothing.value)(fromDuration2(d))(toRecord(dt)))(function(rec) {
          return apply2(map3(DateTime.create)(join2(apply2(apply2(map3(exactDate)(toEnum3(rec.year)))(toEnum1(rec.month)))(toEnum22(rec.day)))))(apply2(apply2(apply2(map3(Time.create)(toEnum32(rec.hour)))(toEnum4(rec.minute)))(toEnum5(rec.second)))(toEnum6(rec.millisecond)));
        });
      };
    };
  };

  // output/Effect.Console/foreign.js
  var log2 = function(s) {
    return function() {
      console.log(s);
    };
  };

  // output/Main/index.js
  var show8 = /* @__PURE__ */ show(/* @__PURE__ */ showMaybe(showMinute));
  var toEnum7 = /* @__PURE__ */ toEnum(boundedEnumMinute);
  var show14 = /* @__PURE__ */ show(/* @__PURE__ */ showMaybe(showHour));
  var toEnum12 = /* @__PURE__ */ toEnum(boundedEnumHour);
  var apply3 = /* @__PURE__ */ apply(applyMaybe);
  var map4 = /* @__PURE__ */ map(functorMaybe);
  var toEnum23 = /* @__PURE__ */ toEnum(boundedEnumSecond);
  var toEnum33 = /* @__PURE__ */ toEnum(boundedEnumMillisecond);
  var show24 = /* @__PURE__ */ show(/* @__PURE__ */ showMaybe(showTime));
  var bind3 = /* @__PURE__ */ bind(bindMaybe);
  var toEnum42 = /* @__PURE__ */ toEnum(boundedEnumYear);
  var toEnum52 = /* @__PURE__ */ toEnum(boundedEnumDay);
  var show33 = /* @__PURE__ */ show(/* @__PURE__ */ showMaybe(showDate));
  var show42 = /* @__PURE__ */ show(/* @__PURE__ */ showMaybe(showDateTime));
  var fromJust3 = /* @__PURE__ */ fromJust();
  var show52 = /* @__PURE__ */ show(showDateTime);
  var adjust2 = /* @__PURE__ */ adjust(durationSeconds);
  var adjust1 = /* @__PURE__ */ adjust(durationDays);
  var adjust22 = /* @__PURE__ */ adjust(durationHours);
  var diff2 = /* @__PURE__ */ diff(durationDays);
  var show62 = /* @__PURE__ */ show(showDays);
  var main = function __do() {
    log2("\n    To create a specific part of a `Time` value, you must use\n    the `toEnum` function. This ensures that the value you provide\n    is within the correct bounds.\n  \n    For example, a Minute must be between 0 and 59. If you happen\n    to pass it -1, what should it's value be? Should it be clamped to 0?\n    Or should it be set to 59? Rather than deciding for you, it forces\n    you to do the checking by using `toEnum` to return a `Maybe Minute`.\n    ")();
    log2("toEnum 4: " + show8(toEnum7(4)))();
    log2("toEnum -8: " + show8(toEnum7(-8 | 0)))();
    log2("toEnum 69: " + show8(toEnum7(69)))();
    log2("\n    The same goes for Hour (0 - 23), Second (0 - 59), and\n    Millisecond (0 - 999).\n    ")();
    log2("toEnum 4: " + show14(toEnum12(4)))();
    log2("toEnum -8: " + show14(toEnum12(-8 | 0)))();
    log2("toEnum 69: " + show14(toEnum12(69)))();
    log2("\n    To create a `Time` value, you use the `Maybe` monad and do notation:\n    ")();
    var mkTime = function(h) {
      return function(m) {
        return function(s) {
          return function(ms) {
            return apply3(apply3(apply3(map4(Time.create)(toEnum12(h)))(toEnum7(m)))(toEnum23(s)))(toEnum33(ms));
          };
        };
      };
    };
    log2("mkTime 14 24 05 100 : " + show24(mkTime(14)(24)(5)(100)))();
    log2("mkTime 23 33 30 0 " + show24(mkTime(23)(33)(30)(0)))();
    log2("\n      To create a `Date` value, we use the `Maybe` monad again:\n      ")();
    var mkExactDate = function(year2) {
      return function(month2) {
        return function(day2) {
          return bind3(toEnum42(year2))(function(year$prime) {
            return bind3(toEnum52(day2))(function(day$prime) {
              return exactDate(year$prime)(month2)(day$prime);
            });
          });
        };
      };
    };
    var mkCanonicalDate = function(year2) {
      return function(month2) {
        return function(day2) {
          return apply3(map4(function(v) {
            return function(v1) {
              return canonicalDate(v)(month2)(v1);
            };
          })(toEnum42(year2)))(toEnum52(day2));
        };
      };
    };
    log2("mkCanonicalDate 2023 May 17: " + show33(mkCanonicalDate(2023)(May.value)(17)))();
    log2("mkExactDate 2016 Feb 31: " + show33(mkExactDate(2016)(February.value)(31)))();
    log2("mkExactDate 2016 Feb 27: " + show33(mkExactDate(2016)(February.value)(27)))();
    var mkDateTime = function(y) {
      return function(month2) {
        return function(d) {
          return function(h) {
            return function(m) {
              return function(s) {
                return function(ms) {
                  return apply3(map4(function(v) {
                    return function(v1) {
                      return new DateTime(v, v1);
                    };
                  })(mkCanonicalDate(y)(month2)(d)))(mkTime(h)(m)(s)(ms));
                };
              };
            };
          };
        };
      };
    };
    log2("mkDateTime 2016 Feb 27 @ 11:04:14:423: " + show42(mkDateTime(2016)(February.value)(27)(11)(4)(14)(423)))();
    var dateTimeValue = fromJust3(mkDateTime(2016)(February.value)(27)(11)(4)(14)(423));
    log2("Original DateTime: " + show52(dateTimeValue))();
    log2("Add five seconds: " + show42(adjust2(5)(dateTimeValue)))();
    log2("Add two days: " + show42(adjust1(2)(dateTimeValue)))();
    log2("Add four hours: " + show42(adjust22(4)(dateTimeValue)))();
    var oneDayDifference = fromJust3(adjust1(1)(dateTimeValue));
    var result = diff2(oneDayDifference)(dateTimeValue);
    return log2("Number of Days between two dateTimes: " + show62(result))();
  };

  // <stdin>
  main();
})();