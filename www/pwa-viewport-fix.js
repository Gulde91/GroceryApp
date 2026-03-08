(function () {
  function inStandaloneMode() {
    return window.matchMedia('(display-mode: standalone)').matches || window.navigator.standalone === true;
  }

  function setStandaloneClass() {
    document.documentElement.classList.toggle('ga-standalone', inStandaloneMode());
  }

  function setViewportUnits() {
    var viewportHeight = window.innerHeight;

    if (window.visualViewport && window.visualViewport.height) {
      viewportHeight = window.visualViewport.height;
    }

    document.documentElement.style.setProperty('--ga-vh', (viewportHeight * 0.01) + 'px');
  }

  function refreshViewportState() {
    setStandaloneClass();
    setViewportUnits();
  }

  refreshViewportState();

  window.addEventListener('resize', refreshViewportState);
  window.addEventListener('orientationchange', function () {
    window.setTimeout(refreshViewportState, 50);
    window.setTimeout(refreshViewportState, 250);
  });

  window.addEventListener('focus', refreshViewportState);

  if (window.visualViewport) {
    window.visualViewport.addEventListener('resize', refreshViewportState);
  }
})();
