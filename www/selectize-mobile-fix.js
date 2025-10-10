// www/selectize-mobile-fix.js  (v2 – patch per selectize-instans)
(function () {
  function isIOS() {
    return /iPad|iPhone|iPod/.test(navigator.userAgent) ||
           (navigator.userAgent.includes("Mac") && "ontouchend" in document);
  }

  function dismissKeyboardIOS() {
    // 1) normal blur af aktivt element
    if (document.activeElement && document.activeElement.blur) {
      try { document.activeElement.blur(); } catch (_) {}
    }
    // 2) iOS fallback: fokusér kort et skjult input og blur det igen
    if (isIOS()) {
      var tmp = document.createElement("input");
      tmp.type = "text";
      tmp.style.position = "fixed";
      tmp.style.opacity = "0";
      tmp.style.pointerEvents = "none";
      tmp.style.height = "0";
      tmp.style.width = "0";
      tmp.style.top = "-10000px";
      document.body.appendChild(tmp);
      try { tmp.focus({ preventScroll: true }); } catch (_) {}
      setTimeout(function () {
        try { tmp.blur(); } catch (_) {}
        if (tmp.parentNode) tmp.parentNode.removeChild(tmp);
      }, 0);
    }
  }

  function patchSelectize(selectEl) {
    if (!selectEl || !selectEl.selectize || selectEl._gaPatched) return;
    var s = selectEl.selectize;

    // Sikre mobilvenlig adfærd
    s.settings.openOnFocus = true;
    if (s.settings.maxItems == null) s.settings.maxItems = (selectEl.multiple ? 9999 : 1);
    if (s.settings.maxItems === 1) s.settings.closeAfterSelect = true;

    // Åbn dropdown når der fokuseres/trykkes i feltet
    s.on('focus', function () {
      try { s.open(); } catch (_) {}
    });

    // Når en option ER valgt (tap/enter) -> luk pænt og skjul keyboard
    s.on('item_add', function () {
      if (s.settings.maxItems === 1) {
        setTimeout(function () {
          try { s.close(); } catch (_) {}
          try { s.blur(); } catch (_) {}
          try { s.control_input && s.control_input.blur(); } catch (_) {}
          dismissKeyboardIOS();
        }, 0);
      }
    });

    // Safety: hvis dropdown lukker mens feltet stadig har fokus
    s.on('dropdown_close', function () {
      if (s.settings.maxItems === 1) {
        setTimeout(function () {
          try { s.blur(); } catch (_) {}
          try { s.control_input && s.control_input.blur(); } catch (_) {}
          dismissKeyboardIOS();
        }, 0);
      }
    });

    selectEl._gaPatched = true;
  }

  // Patch alle eksisterende selectize-kontroller
  function scan() {
    document.querySelectorAll('select.selectized').forEach(patchSelectize);
  }

  if (document.readyState !== 'loading') scan();
  else document.addEventListener('DOMContentLoaded', scan);

  // Patch også kontroller, der tilføjes senere (Shiny re-render)
  new MutationObserver(function (muts) {
    for (var i = 0; i < muts.length; i++) {
      if (muts[i].addedNodes && muts[i].addedNodes.length) { scan(); break; }
    }
  }).observe(document.documentElement, { childList: true, subtree: true });

  // Hjælp dropdown med at åbne på tap i shinyMobile uden at forstyrre valg
  document.addEventListener('click', function (e) {
    var ctl = e.target.closest('.selectize-control');
    if (!ctl) return;
    var sel = ctl.previousElementSibling;
    if (sel && sel.selectize) {
      setTimeout(function () {
        if (!sel.selectize.isOpen) sel.selectize.open();
      }, 0);
    }
  }, true);
})();
