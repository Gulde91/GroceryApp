(function () {
  var viewportMeta = document.querySelector('meta[name="viewport"]');

  function inStandaloneMode() {
    return window.matchMedia('(display-mode: standalone)').matches || window.navigator.standalone === true;
  }

  function setStandaloneClass() {
    document.documentElement.classList.toggle('ga-standalone', inStandaloneMode());
  }

  function setViewportUnits() {
    var vh = window.innerHeight * 0.01;
    var vw = window.innerWidth * 0.01;

    if (window.visualViewport) {
      vh = window.visualViewport.height * 0.01;
      vw = window.visualViewport.width * 0.01;
    }

    document.documentElement.style.setProperty('--ga-vh', vh + 'px');
    document.documentElement.style.setProperty('--ga-vw', vw + 'px');
  }

  function syncViewportMeta() {
    if (!viewportMeta) return;

    if (inStandaloneMode()) {
      viewportMeta.setAttribute(
        'content',
        'width=device-width, initial-scale=1, maximum-scale=1, viewport-fit=cover'
      );
      return;
    }

    viewportMeta.setAttribute(
      'content',
      'width=device-width, initial-scale=1, viewport-fit=cover'
    );
  }

  setStandaloneClass();
  setViewportUnits();
  syncViewportMeta();

  window.addEventListener('resize', function () {
    setStandaloneClass();
    setViewportUnits();
    syncViewportMeta();
  });

  window.addEventListener('orientationchange', function () {
    setStandaloneClass();
    setViewportUnits();
    syncViewportMeta();
  });

  if (window.visualViewport) {
    window.visualViewport.addEventListener('resize', setViewportUnits);
  }
})();
