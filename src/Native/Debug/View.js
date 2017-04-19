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
            entryEl.style.display = "block";
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
    var entry = _elm_lang$core$Native_Utils.toString(a);
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
})();
