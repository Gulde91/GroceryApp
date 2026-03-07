(function () {
  function inStandaloneMode() {
    return window.matchMedia('(display-mode: standalone)').matches || window.navigator.standalone === true;
  }

  function setStandaloneClass() {
    document.documentElement.classList.toggle('ga-standalone', inStandaloneMode());
  }

  function setViewportUnit() {
    var viewportHeight = window.visualViewport ? window.visualViewport.height : window.innerHeight;
    var vh = viewportHeight * 0.01;
    document.documentElement.style.setProperty('--ga-vh', vh + 'px');
    document.documentElement.style.setProperty('--ga-app-height', viewportHeight + 'px');
  }

  function refreshLayoutFlags() {
    setStandaloneClass();
    setViewportUnit();
  }

  refreshLayoutFlags();

  window.addEventListener('resize', refreshLayoutFlags);
  window.addEventListener('orientationchange', refreshLayoutFlags);

  if (window.visualViewport) {
    window.visualViewport.addEventListener('resize', refreshLayoutFlags);
  }
})();
