// www/button-press.js
(function () {
  function addRipple(e, el) {
    try {
      const rect = el.getBoundingClientRect();
      const x = (e.touches?.[0]?.clientX ?? e.clientX) - rect.left;
      const y = (e.touches?.[0]?.clientY ?? e.clientY) - rect.top;

      const ripple = document.createElement('span');
      ripple.className = 'ga-ripple';
      ripple.style.left = x + 'px';
      ripple.style.top  = y + 'px';
      el.appendChild(ripple);

      ripple.addEventListener('animationend', () => {
        ripple.remove();
      }, { once: true });
    } catch (_) { /* no-op */ }
  }

  function pressStart(e) {
    const btn = e.target.closest('.button');
    if (!btn) return;
    btn.classList.add('is-pressed');
    addRipple(e, btn);
  }

  function pressEnd(e) {
    document.querySelectorAll('.button.is-pressed').forEach(b => b.classList.remove('is-pressed'));
  }

  // Touch først (capture) så F7 ikke sluger vores event
  document.addEventListener('touchstart', pressStart, { passive: true, capture: true });
  document.addEventListener('mousedown',  pressStart, true);

  ['touchend','touchcancel','mouseup','mouseleave','dragend','blur'].forEach(evt => {
    document.addEventListener(evt, pressEnd, true);
  });

  // Ryd op hvis siden mister fokus
  window.addEventListener('blur', pressEnd, true);
})();
