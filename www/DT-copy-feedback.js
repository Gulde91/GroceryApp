// www/copy-feedback.js

window.showCopyToast = function (message, tone) {
  var toast = document.getElementById("copy-toast");
  if (!toast) {
    toast = document.createElement("div");
    toast.id = "copy-toast";
    toast.className = "copy-toast";
    document.body.appendChild(toast);
  }

  toast.innerText = message || "Indkøbsliste kopieret ✔";

  var toastTone = tone || "green";
  toast.classList.remove("toast-green", "toast-blue");
  toast.classList.add(toastTone === "blue" ? "toast-blue" : "toast-green");
  toast.classList.add("show");

  // Sørg for at flere klik bare forlænger samme toast
  clearTimeout(window.copyToastTimeout);
  window.copyToastTimeout = setTimeout(function () {
    toast.classList.remove("show");
  }, 2000);
}

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
  showCopyToast("Indkøbsliste kopieret ✔", "green");
};

if (window.Shiny && window.Shiny.addCustomMessageHandler) {
  window.Shiny.addCustomMessageHandler("show_toast", function (msg) {
    var text = msg && msg.text ? msg.text : "Udført ✔";
    var tone = msg && msg.tone ? msg.tone : "green";
    showCopyToast(text, tone);
  });
}
