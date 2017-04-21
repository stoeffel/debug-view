//import Native.Utils //
var _stoeffel$debug_view$Native_Debug_View = (function() {
  var log = {};
  var clickHandlers = {};
  function inspect(a, id) {
    if (!log[id]) {
      log[id] = [];
    }
    if (!clickHandlers[id]) {
      function addClickHandlers() {
        var entryId = "elm-debug-view-entry-" + id;
        var closeId = "elm-debug-view-close-button-" + id;
        var counterId = "elm-debug-view-counter-" + id;
        var counterEl = document.getElementById(counterId);
        var entryEl = document.getElementById(entryId);
        var closeEl = document.getElementById(closeId);
        if (!counterEl || !entryEl) {
          return setTimeout(addClickHandlers, 500);
        }
        clickHandlers[id] = {
          counter: counterEl.addEventListener("click", function() {
            entryEl.style.display = "flex";
            counterEl.style.display = "none";
          }),
          close: closeEl.addEventListener("click", function() {
            counterEl.style.display = "block";
            entryEl.style.display = "none";
          })
        };
      }
      requestAnimationFrame(addClickHandlers);
    }
    var entry = toString(a, 0);
    if (JSON.stringify(log[id][log[id].length - 1]) === JSON.stringify(entry)) {
      return _elm_lang$core$Native_List.fromArray(log[id]);
    }
    log[id].push(entry);
    return _elm_lang$core$Native_List.fromArray(log[id]);
  }
  function inspect2(a, b, id) {
    return inspect([a, b], id);
  }
  return {
    inspect: F2(inspect),
    inspect2: F3(inspect2)
  };

  // This is a modified version of toString from elm core: https://github.com/elm-lang/core/blob/3.0.0/src/Native/Utils.js#L358
  function toString(v) {
    var type = typeof v;
    if (type === "function") {
      var name = v.func ? v.func.name : v.name;
      return {
        ctor: "ElmFunction",
        _0: name
      };
    } else if (type === "boolean") {
      return {
        ctor: "ElmBoolean",
        _0: v
      };
    } else if (type === "number") {
      return {
        ctor: "ElmNumber",
        _0: "" + v
      };
    } else if (v instanceof String && v.isChar) {
      return {
        ctor: "ElmChar",
        _0: v
      };
    } else if (type === "string") {
      return {
        ctor: "ElmString",
        _0: v
      };
    } else if (type === "object" && "ctor" in v) {
      if (v.ctor.substring(0, 6) === "_Tuple") {
        var output = [];
        for (var k in v) {
          if (k === "ctor") continue;
          output.push(toString(v[k]));
        }
        return {
          ctor: "ElmTuple",
          _0: _elm_lang$core$Native_List.fromArray(output)
        };
      } else if (v.ctor === "_Array") {
        var list = _elm_lang$core$Array.toList(v);
        return {
          ctor: "ElmArray",
          _0: listToString(list)
        };
      } else if (v.ctor === "::") {
        return {
          ctor: "ElmList",
          _0: listToString(v)
        };
      } else if (v.ctor === "[]") {
        return {
          ctor: "ElmList",
          _0: listToString(v)
        };
      } else if (
        v.ctor === "RBNode_elm_builtin" ||
        v.ctor === "RBEmpty_elm_builtin" ||
        v.ctor === "Set_elm_builtin"
      ) {
        var list;
        var name;
        if (v.ctor === "Set_elm_builtin") {
          name = "Set";
          list = A2(
            _elm_lang$core$List.map,
            function(x) {
              return x._0;
            },
            _elm_lang$core$Dict$toList(v._0)
          );
        } else {
          name = "Dict";
          list = _elm_lang$core$Dict$toList(v);
        }
        return {
          ctor: "Elm" + name,
          _0: listToString(list)
        };
      } else if (v.ctor.slice(0, 5) === "Text:") {
        return {
          ctor: "ElmCustom",
          _0: "<text>"
        };
      } else if (v.ctor === "Element_elm_builtin") {
        return {
          ctor: "ElmCustom",
          _0: "<element>"
        };
      } else if (v.ctor === "Form_elm_builtin") {
        return {
          ctor: "ElmCustom",
          _0: "<form>"
        };
      } else {
        var output = "";
        for (var i in v) {
          if (i === "ctor") continue;
          var str = toString(v[i]);
          var parenless = str[0] === "{" ||
            str[0] === "<" ||
            str.indexOf(" ") < 0;
          output += " " + (parenless ? str : "(" + str + ")");
        }
        return {
          ctor: "ElmCustom",
          _0: v.ctor + output
        };
      }
    } else if (type === "object" && "notify" in v && "id" in v) {
      return {
        ctor: "ElmCustom",
        _0: "<signal>"
      };
    } else if (type === "object") {
      var output = [];
      for (var k in v) {
        output.push(_elm_lang$core$Native_Utils.Tuple2(k, toString(v[k])));
      }
      return {
        ctor: "ElmRecord",
        _0: _elm_lang$core$Native_List.fromArray(output)
      };
    }
    return {
      ctor: "ElmCustom",
      _0: "<internal structure>"
    };
  }

  function listToString(v) {
    var output = [];
    while (v.ctor === "::") {
      output.push(toString(v._0));
      v = v._1;
    }
    return _elm_lang$core$Native_List.fromArray(output);
  }

  function addSlashes(str, isChar) {
    var s = str
      .replace(/\\/g, "\\\\")
      .replace(/\n/g, "\\n")
      .replace(/\t/g, "\\t")
      .replace(/\r/g, "\\r")
      .replace(/\v/g, "\\v")
      .replace(/\0/g, "\\0");
    if (isChar) {
      return s.replace(/\'/g, "\\'");
    } else {
      return s.replace(/\"/g, '\\"');
    }
  }
})();
window._elmRenderVisualizerToggleCollapse = function(e) {
  var cls = "elm-debug-view-collapsed";
  toggleClassElmRenderVisualizer(e, cls);
  window.event.stopPropagation();
};
function toggleClassElmRenderVisualizer(element, className) {
  if (!element || !className) {
    return;
  }

  var classString = element.className,
    nameIndex = classString.indexOf(className);
  if (nameIndex == -1) {
    classString += " " + className;
  } else {
    classString = classString.substr(0, nameIndex) +
      classString.substr(nameIndex + className.length);
  }
  element.className = classString;
}
