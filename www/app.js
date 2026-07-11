(function () {
  "use strict";

  window.kkaiSelectResultsSection = function (inputId, section) {
    if (!window.Shiny || !inputId || !section) return;

    window.Shiny.setInputValue(
      inputId,
      {
        section: section,
        scrollY: window.scrollY || window.pageYOffset || 0,
        token: Date.now()
      },
      { priority: "event" }
    );
  };

  var registerScrollHandler = function () {
    if (!window.Shiny) {
      window.setTimeout(registerScrollHandler, 25);
      return;
    }
    if (window.__kkaiScrollHandlerRegistered) return;
    window.__kkaiScrollHandlerRegistered = true;

    window.Shiny.addCustomMessageHandler("kkai-restore-scroll", function (message) {
      var y = Number(message && message.y);
      if (!Number.isFinite(y)) return;

      var attempts = 0;
      var restore = function () {
        window.scrollTo({ top: y, left: window.scrollX || 0, behavior: "auto" });
        attempts += 1;
        if (attempts < 3) window.setTimeout(restore, 40 * attempts);
      };

      window.requestAnimationFrame(function () {
        window.requestAnimationFrame(restore);
      });
    });
  };

  registerScrollHandler();
})();
