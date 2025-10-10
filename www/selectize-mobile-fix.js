/**
 * selectize-mobile-fix.js
 * ----------------------------------------------------------------------
 * Purpose
 *  - Gør selectize-baserede selectInput-felter mobilvenlige i shinyMobile
 *    (Framework7): dropdown åbner på tap/fokus, valg registreres korrekt,
 *    og mobil-tastatur (iOS/Android) lukker efter valg.
 *
 * Hvad den gør (kort):
 *  1) Finder alle selectize-instanser og patcher dem én for én.
 *  2) Åbner dropdown når feltet får fokus (tap).
 *  3) Når et valg ER registreret (event: `item_add`), lukkes dropdown,
 *     fokus fjernes (blur), og tastaturet lukkes (inkl. iOS fallback).
 *  4) Rører kun single-selects som standard (maxItems = 1). Multiple kan
 *     nemt aktiveres, se TOGGLE længere nede.
 *
 * Hvorfor er det nødvendigt?
 *  - Framework7 (shinyMobile) kan “sluge” click/touch-events, hvilket gør,
 *    at selectize ikke altid åbner/lukker som forventet på mobil.
 *  - iOS holder tastaturet åbent, hvis et (skjult) tekstinput stadig har fokus.
 *    Selectize bruger netop sådan et input – derfor skal vi blur’e aktivt.
 *
 * Krav:
 *  - Indlæs filen EFTER shinyMobile/F7 og selectize (brug fx htmltools::singleton).
 *  - Tilføj funktionel CSS så dropdown kan modtage klik og ikke “klippes”:
 *
 *      .selectize-dropdown { z-index: 20000 !important; pointer-events: auto; }
 *      .selectize-control  { pointer-events: auto; }
 *
 * Anbefalet inkludering i R:
 *  tags$head(
 *    includeCSS("www/styles.css"),
 *    htmltools::singleton(tags$script(src = "selectize-mobile-fix.js"))
 *  )
 *
 * Testet på:
 *  - iOS (Safari) – PWA og Safari.
 *  - Android (Chrome).
 *  - Desktop (Chrome/Firefox/Safari/Edge).
 *
 * Kendte begrænsninger:
 *  - Hvis du har “eksotisk” markup/CSS med overlay/layers over dropdown,
 *    kan z-index/pointer-events skulle justeres lokalt.
 *  - Hvis du har egne selectize handlers, så undgå at lukke/blur’e for tidligt.
 *
 * Changelog:
 *  v2 – Patch per selectize-instans, luk/blur efter `item_add`, iOS keyboard-fallback.
 */

/* =======================
 * Konfiguration / Toggles
 * ======================= */

/**
 * Skal tastaturet automatisk lukkes på single-select efter valg?
 * (På iOS anbefales TRUE, fordi keyboard ellers “hænger” ofte.)
 */
var CLOSE_KEYBOARD_ON_SINGLE_SELECT = true;

/**
 * Skal vi også forsøge at lukke keyboard ved multiple-select?
 * (Typisk FALSE – brugeren vil ofte vælge flere ting i træk.)
 */
var CLOSE_KEYBOARD_ON_MULTI_SELECT = false;

/**
 * Åbn dropdown automatisk når feltet får fokus?
 * (God mobil-UX: tap i feltet => dropdown synlig med det samme.)
 */
var OPEN_ON_FOCUS = true;


/* =======================
 * Hjælpefunktioner
 * ======================= */

/**
 * Detektion af iOS (inkl. iPadOS med touch “Mac” UA).
 */
function isIOS() {
  return /iPad|iPhone|iPod/.test(navigator.userAgent) ||
         (navigator.userAgent.includes("Mac") && "ontouchend" in document);
}

/**
 * Tving tastatur til at lukke på iOS:
 * 1) Blur aktivt element, 2) fokusér kortvarigt et skjult input, blur igen.
 */
function dismissKeyboardIOS() {
  try {
    if (document.activeElement && document.activeElement.blur) {
      document.activeElement.blur();
    }
  } catch (_) {}

  if (!isIOS()) return;

  var tmp = document.createElement("input");
  tmp.type = "text";
  tmp.style.position = "fixed";
  tmp.style.opacity = "0";
  tmp.style.pointerEvents = "none";
  tmp.style.height = "0";
  tmp.style.width = "0";
  tmp.style.top = "-10000px";
  document.body.appendChild(tmp);

  try { tmp.focus({ preventScroll: true }); } catch (_) {}
  setTimeout(function () {
    try { tmp.blur(); } catch (_) {}
    if (tmp.parentNode) tmp.parentNode.removeChild(tmp);
  }, 0);
}

/**
 * Skal vi lukke/blur’e efter valg for en given instans?
 * - Som standard: kun single-select (maxItems === 1).
 */
function shouldCloseAfterSelect(selectizeInstance) {
  var maxItems = selectizeInstance && selectizeInstance.settings && selectizeInstance.settings.maxItems;
  if (maxItems === 1) return CLOSE_KEYBOARD_ON_SINGLE_SELECT;
  return CLOSE_KEYBOARD_ON_MULTI_SELECT;
}


/* =======================
 * Kerne-patch
 * ======================= */

/**
 * Patch en enkelt <select> som allerede er “selectized”.
 * Tilføjer handlers for fokus/valg/luk og iOS keyboard-dismiss.
 */
function patchSelectize(selectEl) {
  if (!selectEl || !selectEl.selectize || selectEl._gaPatched) return;

  var s = selectEl.selectize;

  // Sikre mobilvenlig standardadfærd
  s.settings.openOnFocus = !!OPEN_ON_FOCUS;
  if (s.settings.maxItems == null) {
    // Hvis select har multiple-attribut => tillad mange, ellers kun 1
    s.settings.maxItems = (selectEl.multiple ? 9999 : 1);
  }
  if (s.settings.maxItems === 1) {
    s.settings.closeAfterSelect = true; // single-select: auto-luk
  }

  // 1) Åbn dropdown ved fokus (tap/klik i feltet)
  s.on('focus', function () {
    if (OPEN_ON_FOCUS) {
      try { s.open(); } catch (_) {}
    }
  });

  // 2) Når selectize HAR tilføjet et valgt item
  s.on('item_add', function () {
    if (!shouldCloseAfterSelect(s)) return;

    // Vent en tik, så selectize når at opdatere internt (værdi/DOM)
    setTimeout(function () {
      try { s.close(); } catch (_) {}
      try { s.blur(); } catch (_) {}
      try { s.control_input && s.control_input.blur(); } catch (_) {}
      dismissKeyboardIOS();
    }, 0);
  });

  // 3) Hvis dropdown lukkes uden item_add (fx klik uden for)
  s.on('dropdown_close', function () {
    if (!shouldCloseAfterSelect(s)) return;

    setTimeout(function () {
      try { s.blur(); } catch (_) {}
      try { s.control_input && s.control_input.blur(); } catch (_) {}
      dismissKeyboardIOS();
    }, 0);
  });

  // Markér som patchet, så vi ikke dobbeltbinder events
  selectEl._gaPatched = true;
}

/**
 * Find og patch alle eksisterende selectize-kontroller.
 */
function scanAndPatch() {
  document.querySelectorAll('select.selectized').forEach(patchSelectize);
}


/* =======================
 * Init & Observers
 * ======================= */

// Patch nu (når DOM er klar)
if (document.readyState !== 'loading') scanAndPatch();
else document.addEventListener('DOMContentLoaded', scanAndPatch);

// Patch også kontroller der dukker op senere (Shiny re-render / moduler)
new MutationObserver(function (muts) {
  for (var i = 0; i < muts.length; i++) {
    if (muts[i].addedNodes && muts[i].addedNodes.length) {
      scanAndPatch();
      break;
    }
  }
}).observe(document.documentElement, { childList: true, subtree: true });

// Hjælp dropdown med at åbne på tap i shinyMobile uden at forstyrre valget.
// (Vi åbner *efter* event-bobling så F7 når sit – men kun hvis stadig lukket.)
document.addEventListener('click', function (e) {
  var ctl = e.target.closest('.selectize-control');
  if (!ctl) return;
  var sel = ctl.previousElementSibling; // original <select> før .selectize-control
  if (sel && sel.selectize) {
    setTimeout(function () {
      if (OPEN_ON_FOCUS && !sel.selectize.isOpen) sel.selectize.open();
    }, 0);
  }
}, true);
