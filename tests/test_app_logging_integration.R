suppressPackageStartupMessages(
  source("app.R", encoding = "UTF-8")
)

local({
  captured <- new.env(parent = emptyenv())
  captured$events <- list()
  original_logger <- app_log_event
  on.exit(
    assign("app_log_event", original_logger, envir = .GlobalEnv),
    add = TRUE
  )

  assign(
    "app_log_event",
    function(
      level,
      event,
      component,
      fields = list(),
      error = NULL,
      context = list(),
      ...
    ) {
      captured$events[[length(captured$events) + 1L]] <- list(
        level = level,
        event = event,
        component = component,
        fields = fields,
        error = error,
        context = context
      )
      invisible(TRUE)
    },
    envir = .GlobalEnv
  )

  shiny::testServer(server, {
    session$flushReact()

    started <- Filter(
      function(entry) identical(entry$event, "session_started"),
      captured$events
    )
    stopifnot(
      length(started) == 1L,
      identical(started[[1L]]$level, "INFO"),
      identical(started[[1L]]$component, "application"),
      identical(
        started[[1L]]$fields$message,
        "En brugersession blev startet."
      ),
      grepl(
        "^session-[0-9]{6}$",
        started[[1L]]$context$session_id
      )
    )

    session$unhandledError(
      simpleError("Fremprovokeret uventet sessionsfejl."),
      close = FALSE
    )
    unhandled <- Filter(
      function(entry) {
        identical(entry$event, "session_unhandled_error")
      },
      captured$events
    )
    stopifnot(
      length(unhandled) == 1L,
      identical(unhandled[[1L]]$level, "ERROR"),
      identical(unhandled[[1L]]$component, "application"),
      identical(
        unhandled[[1L]]$fields$message,
        "En uventet fejl opstod i brugersessionen."
      ),
      identical(unhandled[[1L]]$fields$fatal, FALSE),
      inherits(unhandled[[1L]]$error, "error"),
      identical(
        unhandled[[1L]]$context$session_id,
        started[[1L]]$context$session_id
      )
    )
  })

  ended <- Filter(
    function(entry) identical(entry$event, "session_ended"),
    captured$events
  )
  stopifnot(
    length(ended) == 1L,
    identical(ended[[1L]]$level, "INFO"),
    identical(ended[[1L]]$component, "application"),
    identical(
      ended[[1L]]$fields$message,
      "En brugersession blev afsluttet."
    ),
    is.numeric(ended[[1L]]$fields$duration_ms),
    ended[[1L]]$fields$duration_ms >= 0
  )
})

message(paste(
  "App-serveren logger sessionens start, slut og uventede fejl med samme",
  "anonyme sessionskontekst."
))
