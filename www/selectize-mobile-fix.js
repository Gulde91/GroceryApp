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
