# Struktureret driftslog til systemd/journald -------------------------------
#
# Appen skriver korte logfmt-linjer til R-processens message-stream. Ved drift
# under systemd opsamler journald linjerne og står for adgang, rotation og
# retention. Loggeren må aldrig kunne afbryde en brugerhandling. Den kan
# medtage korte vare-, opskrifts- og ingrediensnavne i læsbare
# handlingsbeskeder, men logger aldrig hele indkøbssedler eller opskrifter.

app_log_runtime_state <- local({
  state <- new.env(parent = emptyenv())
  started_at <- Sys.time()
  state$runtime_id <- paste0(
    "process-",
    Sys.getpid(),
    "-",
    format(started_at, "%Y%m%dT%H%M%OS3", tz = "UTC")
  )
  state$session_sequence <- 0L
  state
})

#' Normalisér ét logfelts navn
#'
#' Kun små bogstaver, tal og underscore bevares, så den færdige loglinje kan
#' filtreres stabilt med almindelige kommandolinjeværktøjer.
#'
#' @param key Det ønskede feltnavn.
#'
#' @return Et sikkert logfmt-feltnavn.
#' @keywords internal
app_log_key <- function(key) {
  key <- if (is.null(key) || length(key) == 0L) "" else key[[1L]]
  key <- tolower(trimws(as.character(key)))
  key <- gsub("[^a-z0-9_]+", "_", key)
  key <- gsub("^_+|_+$", "", key)
  if (!nzchar(key)) "field" else key
}

#' Normalisér én logværdi til en kort enkeltlinje
#'
#' Kontroltegn fjernes, linjeskift foldes sammen, og lange værdier beskæres.
#' Funktionen er beregnet til tekniske metadata og ikke vilkårlige objekter.
#'
#' @param value Værdien, der skal skrives.
#' @param max_chars Maksimalt antal tegn i resultatet.
#'
#' @return En sikker tegnværdi.
#' @keywords internal
app_log_value <- function(value, max_chars = 500L) {
  max_chars <- suppressWarnings(as.integer(max_chars))
  if (length(max_chars) != 1L || is.na(max_chars) || max_chars < 1L) {
    max_chars <- 500L
  }

  if (is.null(value) || length(value) == 0L) {
    text <- ""
  } else if (inherits(value, "condition")) {
    text <- conditionMessage(value)
  } else if (inherits(value, "POSIXt")) {
    text <- format(value[[1L]], "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  } else if (is.logical(value)) {
    text <- if (is.na(value[[1L]])) {
      ""
    } else if (isTRUE(value[[1L]])) {
      "true"
    } else {
      "false"
    }
  } else if (is.numeric(value)) {
    text <- if (is.na(value[[1L]]) || !is.finite(value[[1L]])) {
      ""
    } else {
      format(value[[1L]], trim = TRUE, scientific = FALSE, digits = 15L)
    }
  } else {
    text <- as.character(value[[1L]])
    if (is.na(text)) text <- ""
  }

  text <- gsub("[\r\n\t]+", " ", text)
  text <- gsub("[[:cntrl:]]+", "", text)
  text <- trimws(gsub(" {2,}", " ", text))
  substr(text, 1L, max_chars)
}

#' Klassificér en fejl til en stabil teknisk kode
#'
#' De eksisterende stores bruger både typede conditions og enkelte historiske
#' fejltekster. Klassifikationen samler dem i få søgbare koder uden at ændre
#' den oprindelige condition eller brugerbesked.
#'
#' @param error En fejl-condition.
#'
#' @return En kort fejlkode.
#' @keywords internal
app_log_error_code <- function(error) {
  if (!inherits(error, "condition")) return("unknown_error")

  classes <- class(error)
  text <- tolower(conditionMessage(error))
  if (
    any(grepl("_conflict$", classes)) ||
      grepl("ændret i en anden session", text, fixed = TRUE) ||
      grepl("ændret, siden handlingen begyndte", text, fixed = TRUE)
  ) {
    return("concurrency_conflict")
  }
  if (any(grepl("_lock_lost$", classes))) return("lock_lost")
  if (
    grepl("er i brug af en anden handling", text, fixed = TRUE) ||
      grepl("database is locked", text, fixed = TRUE) ||
      grepl("database is busy", text, fixed = TRUE)
  ) {
    return("lock_busy")
  }
  if (
    grepl("låsedatabasen", text, fixed = TRUE) ||
      grepl("låsen til", text, fixed = TRUE)
  ) {
    return("lock_failure")
  }
  if (inherits(error, "shiny.error.fatal")) return("shiny_fatal")

  app_log_key(classes[[1L]])
}

#' Omsæt en condition til sikre logfelter
#'
#' Fejlklassens første navn, en stabil kode og en enkeltlinjet fejltekst er
#' nok til fejlsøgning uden at skrive stack dumps eller inputdata i loggen.
#'
#' @param error En eventuel fejl-condition.
#'
#' @return En navngivet liste, eventuelt tom.
#' @keywords internal
app_log_error_fields <- function(error) {
  if (is.null(error)) return(list())
  if (!inherits(error, "condition")) {
    return(list(
      error_code = "unknown_error",
      error_class = class(error)[[1L]],
      error_message = "En ikke-standard fejl blev modtaget."
    ))
  }

  list(
    error_code = app_log_error_code(error),
    error_class = class(error)[[1L]],
    error_message = conditionMessage(error)
  )
}

#' Rangér et logniveau
#'
#' @param level Et af niveauerne `DEBUG`, `INFO`, `WARN` eller `ERROR`.
#'
#' @return Et heltal eller `NA_integer_` for et ukendt niveau.
#' @keywords internal
app_log_level_rank <- function(level) {
  ranks <- c(DEBUG = 10L, INFO = 20L, WARN = 30L, ERROR = 40L)
  level <- toupper(app_log_value(level, max_chars = 10L))
  if (!level %in% names(ranks)) return(NA_integer_)
  unname(ranks[[level]])
}

#' Afgør om et logevent passerer det valgte niveau
#'
#' @param level Eventets niveau.
#' @param threshold Det laveste niveau, som skal skrives.
#'
#' @return `TRUE`, når eventet skal skrives.
#' @keywords internal
app_log_should_emit <- function(level, threshold) {
  event_rank <- app_log_level_rank(level)
  threshold_rank <- app_log_level_rank(threshold)
  if (is.na(threshold_rank)) threshold_rank <- app_log_level_rank("INFO")
  !is.na(event_rank) && event_rank >= threshold_rank
}

#' Formatér ét struktureret logevent
#'
#' @param level Eventets logniveau.
#' @param event Et stabilt eventnavn.
#' @param component Komponenten, som udsender eventet.
#' @param context Faste proces- og sessionsfelter.
#' @param fields Yderligere tekniske metadata.
#' @param error En eventuel condition.
#' @param timestamp Tidspunktet for eventet.
#'
#' @return Én logfmt-linje uden linjeskift.
#' @keywords internal
app_log_format <- function(
  level,
  event,
  component,
  context = list(),
  fields = list(),
  error = NULL,
  timestamp = Sys.time()
) {
  if (!is.list(context) || !is.list(fields)) {
    stop("Logkontekst og logfelter skal være lister.", call. = FALSE)
  }
  if (
    length(context) > 0L &&
      (is.null(names(context)) || any(!nzchar(names(context))))
  ) {
    stop("Alle kontekstfelter skal have navne.", call. = FALSE)
  }
  if (
    length(fields) > 0L &&
      (is.null(names(fields)) || any(!nzchar(names(fields))))
  ) {
    stop("Alle logfelter skal have navne.", call. = FALSE)
  }

  timestamp <- as.POSIXct(timestamp, tz = "UTC")
  if (length(timestamp) != 1L || is.na(timestamp)) {
    stop("Logtidspunktet er ugyldigt.", call. = FALSE)
  }

  payload <- c(
    list(
      timestamp = format(
        timestamp,
        "%Y-%m-%dT%H:%M:%OS3Z",
        tz = "UTC"
      ),
      level = toupper(app_log_value(level, max_chars = 10L)),
      event = app_log_key(event),
      component = app_log_key(component)
    ),
    context,
    fields,
    app_log_error_fields(error)
  )
  keys <- vapply(names(payload), app_log_key, "")
  keep <- !duplicated(keys)
  keys <- keys[keep]
  payload <- payload[keep]
  values <- vapply(payload, app_log_value, "")
  pairs <- paste0(
    keys,
    "=",
    vapply(values, encodeString, "", quote = '"', na.encode = FALSE)
  )

  paste(c("groceryapp", pairs), collapse = " ")
}

#' Skriv én loglinje til R-processens message-stream
#'
#' systemd opsamler denne stream i journald. Funktionen er separat, så tests
#' kan injicere en writer uden at skrive til den virkelige driftslog.
#'
#' @param line Den færdige enkeltlinje.
#'
#' @return `TRUE` usynligt.
#' @keywords internal
app_log_write <- function(line) {
  message(line)
  invisible(TRUE)
}

#' Udsend ét fejl-tolerant driftslogevent
#'
#' Alle fejl under validering, formatering eller skrivning sluges bevidst.
#' Driftslogning må aldrig kunne ændre resultatet af en brugerhandling.
#'
#' @param level Eventets logniveau.
#' @param event Et stabilt eventnavn.
#' @param component Komponenten, som udsender eventet.
#' @param fields Yderligere tekniske metadata.
#' @param error En eventuel condition.
#' @param context Faste proces- og sessionsfelter.
#' @param threshold Det laveste aktiverede logniveau.
#' @param clock Funktion, som returnerer det aktuelle tidspunkt.
#' @param writer Funktion, som skriver én færdig loglinje.
#'
#' @return `TRUE` ved skrivning og `FALSE` ved filtrering eller fejl.
#' @keywords internal
app_log_event <- function(
  level,
  event,
  component,
  fields = list(),
  error = NULL,
  context = list(),
  threshold = Sys.getenv("GROCERYAPP_LOG_LEVEL", unset = "INFO"),
  clock = Sys.time,
  writer = app_log_write
) {
  tryCatch(
    {
      if (!is.function(clock) || !is.function(writer)) {
        return(invisible(FALSE))
      }
      if (!app_log_should_emit(level, threshold)) {
        return(invisible(FALSE))
      }

      line <- app_log_format(
        level = level,
        event = event,
        component = component,
        context = context,
        fields = fields,
        error = error,
        timestamp = clock()
      )
      writer(line)
      invisible(TRUE)
    },
    error = function(error) invisible(FALSE)
  )
}

#' Hent proceskonteksten til driftsloggen
#'
#' @param session_id Et internt sessions-id eller tom tekst for proces-events.
#'
#' @return En navngivet liste uden bruger- eller netværksidentifikatorer.
#' @keywords internal
app_log_context <- function(session_id = "") {
  list(
    runtime_id = app_log_runtime_state$runtime_id,
    session_id = app_log_value(session_id, max_chars = 80L)
  )
}

#' Opret en ny anonym sessionskontekst
#'
#' Et monotont løbenummer er tilstrækkeligt sammen med processens runtime-id
#' og undgår at logge Shiny-token, IP-adresse eller user-agent.
#'
#' @return En ny proces- og sessionskontekst.
#' @keywords internal
app_log_new_session_context <- function() {
  app_log_runtime_state$session_sequence <-
    app_log_runtime_state$session_sequence + 1L
  app_log_context(sprintf(
    "session-%06d",
    app_log_runtime_state$session_sequence
  ))
}

#' Bind en fast kontekst til loggeren
#'
#' @param context Proces- og sessionskonteksten.
#'
#' @return En callback med argumenterne `level`, `event`, `component`,
#'   `fields` og `error`.
#' @keywords internal
app_log_bind <- function(context) {
  force(context)

  function(
    level,
    event,
    component,
    fields = list(),
    error = NULL
  ) {
    app_log_event(
      level = level,
      event = event,
      component = component,
      fields = fields,
      error = error,
      context = context
    )
  }
}
