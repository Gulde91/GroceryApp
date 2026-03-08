(function () {
  var DIAG_QUERY_KEY = 'gaDebugViewport';
  var DIAG_STORAGE_KEY = 'ga-debug-viewport';

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

  function diagnosticsEnabled() {
    var params = new URLSearchParams(window.location.search);
    var queryValue = params.get(DIAG_QUERY_KEY);

    if (queryValue === '1') {
      window.localStorage.setItem(DIAG_STORAGE_KEY, '1');
      return true;
    }

    if (queryValue === '0') {
      window.localStorage.removeItem(DIAG_STORAGE_KEY);
      return false;
    }

    return window.localStorage.getItem(DIAG_STORAGE_KEY) === '1';
  }

  function getSafeAreaValues() {
    var styles = window.getComputedStyle(document.documentElement);
    return {
      top: styles.getPropertyValue('--ga-safe-top').trim(),
      right: styles.getPropertyValue('--ga-safe-right').trim(),
      bottom: styles.getPropertyValue('--ga-safe-bottom').trim(),
      left: styles.getPropertyValue('--ga-safe-left').trim()
    };
  }

  function buildDiagnosticsText() {
    var vv = window.visualViewport;
    var viewMain = document.querySelector('.view-main');
    var page = document.querySelector('.page');
    var safe = getSafeAreaValues();

    var lines = [
      'timestamp=' + new Date().toISOString(),
      'url=' + window.location.href,
      'standalone=' + String(inStandaloneMode()),
      'display-mode-standalone=' + String(window.matchMedia('(display-mode: standalone)').matches),
      'navigator.standalone=' + String(window.navigator.standalone === true),
      'orientation=' + (window.screen.orientation ? window.screen.orientation.type : 'n/a'),
      'screen=' + window.screen.width + 'x' + window.screen.height,
      'inner=' + window.innerWidth + 'x' + window.innerHeight,
      'outer=' + window.outerWidth + 'x' + window.outerHeight,
      'docClient=' + document.documentElement.clientWidth + 'x' + document.documentElement.clientHeight,
      'bodyClient=' + document.body.clientWidth + 'x' + document.body.clientHeight,
      'css --ga-vh=' + window.getComputedStyle(document.documentElement).getPropertyValue('--ga-vh').trim(),
      'safe-area top/right/bottom/left=' + safe.top + '/' + safe.right + '/' + safe.bottom + '/' + safe.left,
      'visualViewport=' + (vv ? (vv.width + 'x' + vv.height + ' scale=' + vv.scale + ' offset=' + vv.offsetLeft + ',' + vv.offsetTop) : 'n/a'),
      'view-main width=' + (viewMain ? Math.round(viewMain.getBoundingClientRect().width) : 'n/a'),
      'page width=' + (page ? Math.round(page.getBoundingClientRect().width) : 'n/a'),
      'userAgent=' + window.navigator.userAgent
    ];

    return lines.join('\n');
  }

  function ensureDiagnosticsPanel() {
    var panel = document.getElementById('ga-viewport-diagnostics');

    if (panel) return panel;

    panel = document.createElement('div');
    panel.id = 'ga-viewport-diagnostics';
    panel.innerHTML = '<div class="ga-diag-header">Viewport diagnostics</div>' +
      '<pre class="ga-diag-content"></pre>' +
      '<button type="button" class="ga-diag-copy">Kopiér data</button>';

    document.body.appendChild(panel);

    panel.querySelector('.ga-diag-copy').addEventListener('click', function () {
      var text = panel.querySelector('.ga-diag-content').textContent;
      if (window.navigator.clipboard && window.navigator.clipboard.writeText) {
        window.navigator.clipboard.writeText(text);
      }
    });

    return panel;
  }

  function updateDiagnosticsPanel() {
    if (!diagnosticsEnabled()) return;

    document.documentElement.classList.add('ga-debug-viewport');

    var panel = ensureDiagnosticsPanel();
    panel.querySelector('.ga-diag-content').textContent = buildDiagnosticsText();
  }

  function refreshViewportState() {
    setStandaloneClass();
    setViewportUnits();
    updateDiagnosticsPanel();
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
    window.visualViewport.addEventListener('scroll', updateDiagnosticsPanel);
  }
})();
