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

  if (!copyAction) {
    console.warn("copyHtml5 action ikke fundet – tjek DataTables Buttons setup.");
    showCopyToast("Indkøbslisten kunne ikke kopieres", "blue");
    return;
  }

  try {
    copyAction.call(this, e, dt, node, config);
  } catch (error) {
    console.warn("Kopiering af indkøbslisten mislykkedes.", error);
    showCopyToast("Indkøbslisten kunne ikke kopieres", "blue");
    return;
  }

  // 2) Lille push-effekt på knappen
  var $btn = $(node);
  $btn.addClass("copy-btn-pushed");
  setTimeout(function () {
    $btn.removeClass("copy-btn-pushed");
  }, 150);

  // 3) DataTables viser et textarea, hvis browseren kræver manuel Ctrl+C.
  //    Uden textarea er execCommand-kopieringen allerede lykkedes.
  var info = document.getElementById("datatables_buttons_info");
  var manualTextarea = info ? info.querySelector("textarea") : null;

  if (!info) {
    showCopyToast("Kopieringen kunne ikke bekræftes – sedlen er bevaret", "blue");
    return;
  }

  function notifyCopyCompleted() {
    showCopyToast("Indkøbsliste kopieret ✔", "green");
    if (
      config &&
      typeof config.copyInputId === "string" &&
      typeof config.copyRequestId === "string" &&
      window.Shiny &&
      window.Shiny.setInputValue
    ) {
      window.Shiny.setInputValue(
        config.copyInputId,
        {
          request_id: config.copyRequestId,
          nonce: Date.now()
        },
        { priority: "event" }
      );
    }
  }

  if (!manualTextarea) {
    notifyCopyCompleted();
    return;
  }

  // Ved manuel fallback ryddes sedlen først, når browseren faktisk udsender
  // copy/cut-eventet. Klik udenfor, Escape eller timeout bevarer sedlen.
  var cleanupTimer = null;
  var completed = false;
  var cleanup = function () {
    manualTextarea.removeEventListener("copy", completeManualCopy);
    manualTextarea.removeEventListener("cut", completeManualCopy);
    if (info) info.removeEventListener("click", cleanup);
    document.removeEventListener("keydown", cancelOnEscape);
    if (cleanupTimer) clearTimeout(cleanupTimer);
  };
  var completeManualCopy = function () {
    if (completed) return;
    completed = true;
    cleanup();
    notifyCopyCompleted();
  };
  var cancelOnEscape = function (event) {
    if (event.key === "Escape" || event.keyCode === 27) cleanup();
  };

  manualTextarea.addEventListener("copy", completeManualCopy);
  manualTextarea.addEventListener("cut", completeManualCopy);
  if (info) info.addEventListener("click", cleanup);
  document.addEventListener("keydown", cancelOnEscape);
  cleanupTimer = setTimeout(cleanup, 60000);
};

if (window.Shiny && window.Shiny.addCustomMessageHandler) {
  window.Shiny.addCustomMessageHandler("show_toast", function (msg) {
    var text = msg && msg.text ? msg.text : "Udført ✔";
    var tone = msg && msg.tone ? msg.tone : "green";
    showCopyToast(text, tone);
  });
}
