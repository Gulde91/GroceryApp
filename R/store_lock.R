# Fælles OS-understøttet lås til appens fillagre ----------------------------
#
# Denne fil giver fillagrene én fælles måde at serialisere læsning, recovery
# og gemning på. En åben eksklusiv SQLite-transaktion holder operativsystemets
# fillås, som automatisk frigives ved processtop og derfor ikke kræver usikker
# overtagelse af gamle låsemapper.

library(DBI, exclude = "show")
library(RSQLite, exclude = "show")

#' Gør et lagernavn klar til starten af en fejlbesked
#'
#' @param store_label Lagerets navn skrevet med lille begyndelsesbogstav.
#'
#' @return Lagernavnet med stort begyndelsesbogstav.
#' @keywords internal
store_lock_sentence_label <- function(store_label) {
  paste0(
    toupper(substr(store_label, 1L, 1L)),
    substring(store_label, 2L)
  )
}

#' Kontrollér konteksten for en fælles lagerlås
#'
#' Lagernavnet bruges i forståelige fejlbeskeder, mens condition-klassen gør
#' det muligt for det enkelte lager at stoppe uden rollback, hvis låsen mistes.
#'
#' @param store_label Lagerets navn i bestemt form.
#' @param lock_lost_class Lagerets egen condition-klasse for mistet lås.
#'
#' @return Usynligt `TRUE`.
#' @keywords internal
store_lock_validate_context <- function(
  store_label,
  lock_lost_class
) {
  valid_label <- is.character(store_label) &&
    length(store_label) == 1L &&
    !is.na(store_label) &&
    nzchar(store_label)
  valid_class <- is.character(lock_lost_class) &&
    length(lock_lost_class) == 1L &&
    !is.na(lock_lost_class) &&
    grepl(
      "^[A-Za-z][A-Za-z0-9_.]*$",
      lock_lost_class
    )

  if (!valid_label || !valid_class) {
    stop("Konteksten for lagerlåsen er ugyldig.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Stop med lagerets genkendelige fejlklasse for mistet lås
#'
#' Den særskilte klasse sikrer, at lageret kan afbryde uden at forsøge
#' rollback i filer, som en ny låseejer kan være ved at ændre.
#'
#' @param store_label Lagerets navn i bestemt form.
#' @param lock_lost_class Lagerets egen condition-klasse for mistet lås.
#'
#' @return Funktionen returnerer ikke.
#' @keywords internal
store_lock_stop_lost <- function(
  store_label,
  lock_lost_class
) {
  store_lock_validate_context(
    store_label,
    lock_lost_class
  )
  condition <- structure(
    list(
      message = paste(
        paste0(
          "Låsen til ",
          store_label,
          " blev mistet."
        ),
        "Prøv handlingen igen."
      ),
      call = NULL
    ),
    class = c(
      lock_lost_class,
      "store_lock_lost",
      "error",
      "condition"
    )
  )
  stop(condition)
}

#' Kontrollér at et låsehåndtag stadig ejer SQLite-låsen
#'
#' @param lock_handle Den åbne SQLite-forbindelse og låsefilens sti.
#' @param store_label Lagerets navn i bestemt form.
#' @param lock_lost_class Lagerets egen condition-klasse for mistet lås.
#'
#' @return Usynligt `TRUE`.
#' @keywords internal
store_lock_assert_owner <- function(
  lock_handle,
  store_label,
  lock_lost_class
) {
  store_lock_validate_context(
    store_label,
    lock_lost_class
  )
  valid_handle <- is.list(lock_handle) &&
    identical(
      sort(names(lock_handle)),
      c("connection", "path")
    ) &&
    is.character(lock_handle$path) &&
    length(lock_handle$path) == 1L &&
    !is.na(lock_handle$path) &&
    nzchar(lock_handle$path) &&
    inherits(lock_handle$connection, "DBIConnection") &&
    isTRUE(dbIsValid(lock_handle$connection))

  if (!isTRUE(valid_handle)) {
    store_lock_stop_lost(
      store_label,
      lock_lost_class
    )
  }

  invisible(TRUE)
}

#' Kontrollér den aktive SQLite-lås mellem kritiske filtrin
#'
#' En lille forespørgsel bekræfter, at forbindelsen stadig er brugbar. Den
#' eksklusive transaktion forbliver åben, indtil låsen frigives.
#'
#' @param lock_handle Den åbne SQLite-forbindelse og låsefilens sti.
#' @param store_label Lagerets navn i bestemt form.
#' @param lock_lost_class Lagerets egen condition-klasse for mistet lås.
#'
#' @return Usynligt `TRUE`.
#' @keywords internal
store_lock_touch <- function(
  lock_handle,
  store_label,
  lock_lost_class
) {
  store_lock_assert_owner(
    lock_handle,
    store_label,
    lock_lost_class
  )
  probe <- tryCatch(
    dbGetQuery(
      lock_handle$connection,
      "SELECT 1 AS lock_is_alive"
    ),
    error = identity
  )
  if (
    inherits(probe, "error") ||
      !identical(probe$lock_is_alive, 1L)
  ) {
    store_lock_stop_lost(
      store_label,
      lock_lost_class
    )
  }

  invisible(TRUE)
}

#' Tag en eksklusiv OS-understøttet lagerlås
#'
#' SQLite serialiserer samtidige processer på både Windows og Unix. Låsen
#' bliver frigivet af operativsystemet, hvis processen stopper, så der findes
#' ingen gammel låsemappe, som en anden proces skal vurdere eller slette.
#'
#' @param lock_path Den faste sti til lagerets SQLite-låsefil.
#' @param store_label Lagerets navn i bestemt form.
#' @param lock_lost_class Lagerets egen condition-klasse for mistet lås.
#' @param wait_seconds Hvor længe der højst ventes på en aktiv lås.
#'
#' @return Et låsehåndtag med forbindelse og sti.
#' @keywords internal
store_lock_acquire <- function(
  lock_path,
  store_label,
  lock_lost_class,
  wait_seconds = 1
) {
  store_lock_validate_context(
    store_label,
    lock_lost_class
  )
  valid_path <- is.character(lock_path) &&
    length(lock_path) == 1L &&
    !is.na(lock_path) &&
    nzchar(lock_path)
  valid_wait <- is.numeric(wait_seconds) &&
    length(wait_seconds) == 1L &&
    !is.na(wait_seconds) &&
    is.finite(wait_seconds) &&
    wait_seconds >= 0 &&
    wait_seconds <= 3600
  if (!valid_path) {
    stop("Stien til lagerlåsen er ugyldig.", call. = FALSE)
  }
  if (!valid_wait) {
    stop("Ventetiden for lagerlåsen er ugyldig.", call. = FALSE)
  }

  connection <- tryCatch(
    dbConnect(
      SQLite(),
      lock_path,
      synchronous = NULL
    ),
    error = identity
  )
  if (inherits(connection, "error")) {
    stop(
      paste(
        paste0(
          "Låsedatabasen til ",
          store_label,
          " kunne ikke åbnes:"
        ),
        conditionMessage(connection)
      ),
      call. = FALSE
    )
  }

  wait_milliseconds <- as.integer(
    ceiling(wait_seconds * 1000)
  )
  lock_error <- tryCatch(
    {
      dbExecute(
        connection,
        paste0(
          "PRAGMA busy_timeout = ",
          wait_milliseconds
        )
      )
      dbExecute(
        connection,
        "BEGIN EXCLUSIVE TRANSACTION"
      )
      NULL
    },
    error = identity
  )
  if (inherits(lock_error, "error")) {
    try(dbDisconnect(connection), silent = TRUE)
    lock_message <- conditionMessage(lock_error)
    if (grepl(
      "locked|busy",
      lock_message,
      ignore.case = TRUE
    )) {
      stop(
        paste(
          paste0(
            store_lock_sentence_label(store_label),
            " er i brug af en anden handling."
          ),
          "Prøv igen om et øjeblik."
        ),
        call. = FALSE
      )
    }
    stop(
      paste(
        paste0(
          "Låsen til ",
          store_label,
          " kunne ikke oprettes:"
        ),
        lock_message
      ),
      call. = FALSE
    )
  }

  lock_handle <- list(
    connection = connection,
    path = lock_path
  )
  lock_ready <- FALSE
  on.exit(
    if (!lock_ready) {
      store_lock_release(
        lock_handle,
        store_label
      )
    },
    add = TRUE
  )
  store_lock_touch(
    lock_handle,
    store_label,
    lock_lost_class
  )
  lock_ready <- TRUE
  lock_handle
}

#' Frigiv en eksklusiv SQLite-lås
#'
#' Transaktionen rulles tilbage, fordi låsedatabasen ikke indeholder
#' forretningsdata. Forbindelsen lukkes derefter, så OS-låsen frigives straks.
#'
#' @param lock_handle Den åbne SQLite-forbindelse og låsefilens sti.
#' @param store_label Lagerets navn i bestemt form.
#'
#' @return `TRUE`, hvis forbindelsen blev lukket, ellers `FALSE`, usynligt.
#' @keywords internal
store_lock_release <- function(
  lock_handle,
  store_label
) {
  valid_label <- is.character(store_label) &&
    length(store_label) == 1L &&
    !is.na(store_label) &&
    nzchar(store_label)
  valid_handle <- is.list(lock_handle) &&
    identical(
      sort(names(lock_handle)),
      c("connection", "path")
    ) &&
    inherits(lock_handle$connection, "DBIConnection") &&
    isTRUE(dbIsValid(lock_handle$connection))
  if (!valid_label || !isTRUE(valid_handle)) {
    return(invisible(FALSE))
  }

  try(
    dbExecute(
      lock_handle$connection,
      "ROLLBACK"
    ),
    silent = TRUE
  )
  disconnect_error <- tryCatch(
    {
      dbDisconnect(lock_handle$connection)
      NULL
    },
    error = identity
  )
  if (inherits(disconnect_error, "error")) {
    warning(
      paste(
        paste0(
          "Låseforbindelsen til ",
          store_label,
          " kunne ikke lukkes:"
        ),
        conditionMessage(disconnect_error)
      ),
      call. = FALSE
    )
    return(invisible(FALSE))
  }

  invisible(TRUE)
}
