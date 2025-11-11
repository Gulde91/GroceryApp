// www/copy-helper.js
function tryCopy(msg) {
  // 1) Moderne clipboard (kræver HTTPS/localhost)
  if (navigator.clipboard && window.isSecureContext) {
    return navigator.clipboard.writeText(msg);
  }
  // 2) Fallback: execCommand
  return new Promise(function (resolve, reject) {
    try {
      var ta = document.createElement('textarea');
      ta.value = msg;
      ta.setAttribute('readonly', '');
      ta.style.position = 'absolute';
      ta.style.left = '-9999px';
      document.body.appendChild(ta);
      ta.select();
      ta.setSelectionRange(0, ta.value.length);
      var ok = document.execCommand('copy');
      document.body.removeChild(ta);
      ok ? resolve() : reject(new Error('execCommand failed'));
    } catch (e) {
      reject(e);
    }
  });
}

Shiny.addCustomMessageHandler('copy_text', function (msg) {
  tryCopy(msg).then(function () {
    Shiny.setInputValue('copy_done', Date.now(), { priority: 'event' });
  }).catch(function () {
    // 3) iOS over HTTP: vis manuel kopi-dialog i appen
    Shiny.setInputValue('copy_show_manual', msg, { priority: 'event' });
  });
});
