// www/copy-helper.js
Shiny.addCustomMessageHandler('copy_text', function (msg) {
  try {
    var el = document.createElement('textarea');
    el.value = msg;
    document.body.appendChild(el);
    el.select();
    document.execCommand('copy');
    document.body.removeChild(el);
  } catch (e) {
    alert('Kunne ikke kopiere automatisk.\n\n' + msg);
  }
});