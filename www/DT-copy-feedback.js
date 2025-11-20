// www/copy-feedback.js

// Gør funktionen global, så DT::JS("copyWithFeedback") kan finde den
window.copyWithFeedback = function (e, dt, node, config) {
  // 1) Kør standard copy-aktion (brug copyHtml5-varianten)
  var copyAction = $.fn.dataTable.ext.buttons.copyHtml5
    ? $.fn.dataTable.ext.buttons.copyHtml5.action
    : null;

  if (copyAction) {
    copyAction.call(this, e, dt, node, config);
  } else {
    console.warn("copyHtml5 action ikke fundet – tjek DataTables Buttons setup.");
  }

  // 2) Lille push-effekt på knappen
  var $btn = $(node);
  $btn.addClass("copy-btn-pushed");
  setTimeout(function () {
    $btn.removeClass("copy-btn-pushed");
  }, 150);

  // 3) Lille toast-notifikation
  var toast = document.getElementById("copy-toast");
  if (!toast) {
    toast = document.createElement("div");
    toast.id = "copy-toast";
    toast.className = "copy-toast";
    toast.innerText = "Indkøbsliste kopieret ✔";
    document.body.appendChild(toast);
  }

  toast.classList.add("show");

  // Sørg for at flere klik bare forlænger samme toast
  clearTimeout(window.copyToastTimeout);
  window.copyToastTimeout = setTimeout(function () {
    toast.classList.remove("show");
  }, 2000);
};
