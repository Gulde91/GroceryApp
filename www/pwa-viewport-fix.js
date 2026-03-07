(function () {
  function inStandaloneMode() {
    return window.matchMedia('(display-mode: standalone)').matches || window.navigator.standalone === true;
  }

  function setStandaloneClass() {
    document.documentElement.classList.toggle('ga-standalone', inStandaloneMode());
  }

  function setViewportUnit() {
    var vh = window.innerHeight * 0.01;
    document.documentElement.style.setProperty('--ga-vh', vh + 'px');
  }

  setStandaloneClass();
  setViewportUnit();

  window.addEventListener('resize', function () {
    setStandaloneClass();
    setViewportUnit();
  });

  window.addEventListener('orientationchange', function () {
    setStandaloneClass();
    setViewportUnit();
  });
})();
