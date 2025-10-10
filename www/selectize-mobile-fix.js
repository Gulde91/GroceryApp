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
