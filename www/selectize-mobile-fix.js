// www/selectize-mobile-fix.js
(function() {
  document.addEventListener('click', function(e) {
    var ctl = e.target.closest('.selectize-control');
    if (!ctl) return;
    var sel = ctl.previousElementSibling; // <select> før .selectize-control
    if (sel && sel.selectize) {
      sel.selectize.settings.openOnFocus = true;
      if (!sel.selectize.isOpen) sel.selectize.open();
    }
  }, true);

  document.addEventListener('touchstart', function(e) {
    var ctl = e.target.closest('.selectize-control, .selectize-dropdown');
    if (!ctl) return;
    e.stopPropagation(); // undgå at Framework7 sluger eventet
  }, { capture: true });

  document.addEventListener('focusin', function(e) {
    var ctl = e.target.closest('.selectize-control');
    if (!ctl) return;
    var sel = ctl.previousElementSibling;
    if (sel && sel.selectize) {
      sel.selectize.settings.openOnFocus = true;
      if (!sel.selectize.isOpen) sel.selectize.open();
    }
  }, true);
})();


// Luk tastatur når der er valgt en option (især i iOS/Android)
document.addEventListener('change', function(e) {
  var sel = e.target;
  if (!sel || !sel.selectize) return;

  var s = sel.selectize;

  // Kun for single-select (ændr/udkommenter, hvis du også vil gøre det for multiple)
  if (s.settings.maxItems && s.settings.maxItems > 1) return;

  // Luk dropdown og fjern fokus => tastatur forsvinder
  s.close();
  // blur selve selectize (kontrol) og den interne input
  if (s.blur) s.blur();
  if (s.control_input && s.control_input.blur) s.control_input.blur();

  // iOS/Safari er nogle gange stædige – “double blur” hjælper
  setTimeout(function() {
    if (document.activeElement && document.activeElement.blur) {
      document.activeElement.blur();
    }
  }, 0);
}, true);


// --- iOS keyboard dismiss workaround ---
(function() {
  function isIOS() {
    return /iPad|iPhone|iPod/.test(navigator.userAgent) ||
           (navigator.userAgent.includes("Mac") && "ontouchend" in document);
  }

  function dismissKeyboardIOS() {
    if (!isIOS()) return;

    // 1) Forsøg normal blur først
    if (document.activeElement && document.activeElement.blur) {
      document.activeElement.blur();
    }

    // 2) iOS fallback: fokusér et skjult input kortvarigt, så blur lukker keyboardet
    var tmp = document.createElement("input");
    tmp.setAttribute("type", "text");
    // gør den usynlig og ikke-klikbar
    tmp.style.position = "fixed";
    tmp.style.opacity = "0";
    tmp.style.pointerEvents = "none";
    tmp.style.height = "0";
    tmp.style.width = "0";
    tmp.style.top = "-10000px";
    document.body.appendChild(tmp);
    tmp.focus({ preventScroll: true });
    setTimeout(function() {
      tmp.blur();
      document.body.removeChild(tmp);
    }, 0);
  }

  // Når der vælges en option i selectize (tap/click)
  document.addEventListener("mousedown", onCommit, true);
  document.addEventListener("touchend", onCommit, true);
  document.addEventListener("click", onCommit, true);

  function onCommit(e) {
    var isOption = e.target.closest(".selectize-dropdown .option");
    if (!isOption) return;

    // lille timeout så selectize når at opdatere value og lukke dropdown
    setTimeout(function() {
      // luk evt. aktiv selectize og blur dens indre input
      var ctl = document.querySelector(".selectize-control .selectize-input.input-active");
      if (ctl) {
        var sel = ctl.parentElement && ctl.parentElement.previousElementSibling;
        if (sel && sel.selectize) {
          try { sel.selectize.close(); } catch(_) {}
          try { sel.selectize.blur(); } catch(_) {}
          try { sel.selectize.control_input && sel.selectize.control_input.blur(); } catch(_) {}
        }
      }
      dismissKeyboardIOS();
    }, 0);
  }

  // Sikring: hvis værdi ændres via tastatur/Enter
  document.addEventListener("change", function(e) {
    var sel = e.target;
    if (!sel || !sel.selectize) return;

    // Kun single-select; fjern gerne kommentaren hvis du også vil lukke på multiple
    if (sel.selectize.settings.maxItems && sel.selectize.settings.maxItems > 1) return;

    setTimeout(function() {
      try { sel.selectize.close(); } catch(_) {}
      try { sel.selectize.blur(); } catch(_) {}
      try { sel.selectize.control_input && sel.selectize.control_input.blur(); } catch(_) {}
      dismissKeyboardIOS();
    }, 0);
  }, true);
})();
