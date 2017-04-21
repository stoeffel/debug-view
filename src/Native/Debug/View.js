//import Native.Utils //
var _stoeffel$debug_view$Native_Debug = (function() {
  var log = {};
  var clickHandlers = {};
  function inspect(a, id) {
    if (!log[id]) {
      log[id] = [];
    }
    if (!clickHandlers[id]) {
      function addClickHandlers() {
        var entryId = "elm-render-visualizer-entry-" + id;
        var closeId = "elm-render-visualizer-close-button-" + id;
        var counterId = "elm-render-visualizer-counter-" + id;
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
    var entry = toString(a);
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
      return "<function" + (name === "" ? "" : ": ") + name + ">";
    } else if (type === "boolean") {
      return v ? "True" : "False";
    } else if (type === "number") {
      return v + "";
    } else if (v instanceof String && v.isChar) {
      return "'" + addSlashes(v, true) + "'";
    } else if (type === "string") {
      return '"' + addSlashes(v, false) + '"';
    } else if (type === "object" && "ctor" in v) {
      if (v.ctor.substring(0, 6) === "_Tuple") {
        var output = [];
        for (var k in v) {
          if (k === "ctor") continue;
          output.push(toString(v[k]));
        }
        return "(" + output.join(",") + ")";
      } else if (v.ctor === "_Array") {
        var list = _elm_lang$core$Array.toList(v);
        return "Array.fromList " + toString(list);
      } else if (v.ctor === "::") {
        var output = "[" + toString(v._0);
        v = v._1;
        while (v.ctor === "::") {
          output += "," + toString(v._0);
          v = v._1;
        }
        return output + "]";
      } else if (v.ctor === "[]") {
        return "[]";
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
            _elm_lang$core$Dict.toList(v._0)
          );
        } else {
          name = "Dict";
          list = _elm_lang$core$Dict.toList(v);
        }
        return name + ".fromList " + toString(list);
      } else if (v.ctor.slice(0, 5) === "Text:") {
        return "<text>";
      } else if (v.ctor === "Element_elm_builtin") {
        return "<element>";
      } else if (v.ctor === "Form_elm_builtin") {
        return "<form>";
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
        return v.ctor + output;
      }
    } else if (type === "object" && "notify" in v && "id" in v) {
      return "<signal>";
    } else if (type === "object") {
      var output = [];
      for (var k in v) {
        output.push(k + " = " + toString(v[k]));
      }
      if (output.length === 0) {
        return "{}";
      }
      return "{ " + output.join("{%NEWLINE%}, ") + "{%NEWLINE%}}";
    }
    return "<internal structure>";
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
