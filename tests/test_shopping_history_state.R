suppressPackageStartupMessages(
  source(
    file.path("R", "shopping_history_state.R"),
    encoding = "UTF-8"
  )
)

shopping_history_state_entries <- function(
  text = "1 stk Havregryn",
  filename = "indkobsseddel_20260730.rda",
  date = as.Date("2026-07-30")
) {
  data.frame(
    filename = rep(filename, length(text)),
    date = rep(date, length(text)),
    line_number = seq_along(text),
    Indkøbsliste = text,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

shopping_history_state_fixture <- function(
  revision = "fixture-revision-1",
  entries = shopping_history_state_entries()
) {
  list(
    entries = entries,
    revision = revision
  )
}

shopping_history_state_stop_conflict <- function() {
  condition <- structure(
    list(
      message = "Indkøbshistorikken er ændret i en anden session.",
      call = NULL
    ),
    class = c(
      "shopping_history_store_conflict",
      "error",
      "condition"
    )
  )
  stop(condition)
}

shopping_history_state_harness <- function(snapshot) {
  harness <- new.env(parent = emptyenv())
  harness$disk_snapshot <- snapshot
  harness$read_calls <- character()
  harness$revision_calls <- character()
  harness$save_calls <- list()
  harness$next_save_snapshot <- NULL
  harness$fail_save_once <- FALSE
  harness$fail_read_once <- FALSE
  harness$fail_revision_once <- FALSE
  harness$invalid_read_once <- FALSE
  harness$state_reader <- NULL
  harness$visible_during_save <- NULL

  harness$store_read <- function(
    history_dir = "./data/indkobssedler"
  ) {
    harness$read_calls <- c(
      harness$read_calls,
      history_dir
    )

    if (isTRUE(harness$fail_read_once)) {
      harness$fail_read_once <- FALSE
      stop("Fremprovokeret læsefejl.", call. = FALSE)
    }
    if (isTRUE(harness$invalid_read_once)) {
      harness$invalid_read_once <- FALSE
      return(list(
        entries = "ikke en historiktabel",
        revision = NA_character_
      ))
    }

    harness$disk_snapshot
  }

  harness$store_revision <- function(
    history_dir = "./data/indkobssedler"
  ) {
    harness$revision_calls <- c(
      harness$revision_calls,
      history_dir
    )

    if (isTRUE(harness$fail_revision_once)) {
      harness$fail_revision_once <- FALSE
      stop("Fremprovokeret revisionsfejl.", call. = FALSE)
    }

    harness$disk_snapshot$revision
  }

  harness$store_save <- function(
    history_df,
    expected_revision,
    history_dir = "./data/indkobssedler",
    ...
  ) {
    harness$save_calls[[
      length(harness$save_calls) + 1L
    ]] <- list(
      history_df = history_df,
      expected_revision = expected_revision,
      history_dir = history_dir
    )

    if (is.function(harness$state_reader)) {
      harness$visible_during_save <- harness$state_reader()
    }

    if (isTRUE(harness$fail_save_once)) {
      harness$fail_save_once <- FALSE
      stop("Fremprovokeret gemmefejl.", call. = FALSE)
    }
    if (!identical(
      expected_revision,
      harness$disk_snapshot$revision
    )) {
      shopping_history_state_stop_conflict()
    }

    if (is.null(harness$next_save_snapshot)) {
      stop(
        "Testen mangler et snapshot, som lageret skal returnere.",
        call. = FALSE
      )
    }

    next_snapshot <- harness$next_save_snapshot
    harness$next_save_snapshot <- NULL
    harness$disk_snapshot <- next_snapshot
    next_snapshot
  }

  harness
}

shopping_history_state_test_server <- function(
  id,
  harness,
  history_dir = "memory-history",
  poll_interval_ms = 1000L
) {
  shiny::moduleServer(id, function(input, output, session) {
    state_api <- create_shopping_history_state(
      session = session,
      history_dir = history_dir,
      poll_interval_ms = poll_interval_ms,
      store_read = harness$store_read,
      store_revision = harness$store_revision,
      store_save = harness$store_save
    )
  })
}

local({
  initial <- shopping_history_state_fixture()
  harness <- shopping_history_state_harness(initial)

  shiny::testServer(
    shopping_history_state_test_server,
    args = list(harness = harness),
    {
      stopifnot(
        identical(harness$read_calls, "memory-history"),
        identical(names(state_api), c("read", "commit")),
        identical(
          names(state_api$read),
          c("snapshot", "entries", "revision")
        ),
        all(vapply(state_api$read, is.function, logical(1))),
        is.function(state_api$commit),
        identical(state_api$read$snapshot(), initial),
        identical(state_api$read$entries(), initial$entries),
        identical(state_api$read$revision(), initial$revision)
      )

      detached_copy <- state_api$read$snapshot()
      detached_copy$entries$Indkøbsliste[[1L]] <- "Ændret kopi"
      stopifnot(identical(state_api$read$snapshot(), initial))

      invalidations <- new.env(parent = emptyenv())
      invalidations$entries <- 0L
      invalidations$revision <- 0L

      shiny::observe({
        state_api$read$entries()
        invalidations$entries <- invalidations$entries + 1L
      })
      shiny::observe({
        state_api$read$revision()
        invalidations$revision <- invalidations$revision + 1L
      })
      session$flushReact()

      counts_before <- as.list(invalidations)
      history_df <- data.frame(
        Indkøbsliste = c("2 stk Kaffe", "1 l Mælk"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      returned_snapshot <- shopping_history_state_fixture(
        revision = "persisted-revision",
        entries = shopping_history_state_entries(
          text = history_df$Indkøbsliste,
          filename = "indkobsseddel_20260731.rda",
          date = as.Date("2026-07-31")
        )
      )
      harness$next_save_snapshot <- returned_snapshot
      harness$state_reader <- state_api$read$snapshot

      stopifnot(isTRUE(state_api$commit(history_df)))
      session$flushReact()

      save_call <- harness$save_calls[[1L]]
      stopifnot(
        identical(harness$visible_during_save, initial),
        identical(save_call$history_df, history_df),
        identical(
          save_call$expected_revision,
          initial$revision
        ),
        identical(save_call$history_dir, "memory-history"),
        identical(
          state_api$read$snapshot(),
          returned_snapshot
        ),
        identical(
          state_api$read$entries(),
          returned_snapshot$entries
        ),
        identical(
          state_api$read$revision(),
          returned_snapshot$revision
        ),
        invalidations$entries ==
          counts_before$entries + 1L,
        invalidations$revision ==
          counts_before$revision + 1L
      )

      counts_before_revision_only <- as.list(invalidations)
      revision_only_snapshot <- shopping_history_state_fixture(
        revision = "revision-only-change",
        entries = returned_snapshot$entries
      )
      harness$next_save_snapshot <- revision_only_snapshot

      stopifnot(isTRUE(state_api$commit(history_df)))
      session$flushReact()
      stopifnot(
        identical(
          invalidations$entries,
          counts_before_revision_only$entries
        ),
        invalidations$revision ==
          counts_before_revision_only$revision + 1L
      )
    }
  )
})

local({
  initial <- shopping_history_state_fixture()
  harness <- shopping_history_state_harness(initial)

  shiny::testServer(
    shopping_history_state_test_server,
    args = list(harness = harness),
    {
      history_df <- data.frame(
        Indkøbsliste = "1 stk Skal-ikke-publiceres",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      harness$state_reader <- state_api$read$snapshot
      harness$fail_save_once <- TRUE
      reads_before_error <- length(harness$read_calls)

      save_error <- tryCatch(
        state_api$commit(history_df),
        error = identity
      )

      stopifnot(
        inherits(save_error, "error"),
        grepl(
          "Fremprovokeret gemmefejl.",
          conditionMessage(save_error),
          fixed = TRUE
        ),
        identical(state_api$read$snapshot(), initial),
        identical(harness$disk_snapshot, initial),
        identical(harness$visible_during_save, initial),
        length(harness$read_calls) == reads_before_error
      )
    }
  )
})

local({
  initial <- shopping_history_state_fixture()
  harness <- shopping_history_state_harness(initial)

  shiny::testServer(
    shopping_history_state_test_server,
    args = list(harness = harness),
    {
      history_df <- data.frame(
        Indkøbsliste = "1 stk Lokal vare",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      external_snapshot <- shopping_history_state_fixture(
        revision = "external-revision",
        entries = shopping_history_state_entries(
          "1 stk Ekstern vare",
          filename = "indkobsseddel_20260731.rda",
          date = as.Date("2026-07-31")
        )
      )
      harness$disk_snapshot <- external_snapshot
      reads_before_conflict <- length(harness$read_calls)

      conflict <- tryCatch(
        state_api$commit(history_df),
        error = identity
      )

      stopifnot(
        inherits(conflict, "error"),
        grepl(
          "Historikken er nu opdateret; prøv at gemme igen.",
          conditionMessage(conflict),
          fixed = TRUE
        ),
        length(harness$read_calls) ==
          reads_before_conflict + 1L,
        identical(
          state_api$read$snapshot(),
          external_snapshot
        )
      )

      retry_snapshot <- shopping_history_state_fixture(
        revision = "retry-revision",
        entries = shopping_history_state_entries(
          c("1 stk Ekstern vare", "1 stk Lokal vare"),
          filename = "indkobsseddel_20260801.rda",
          date = as.Date("2026-08-01")
        )
      )
      harness$next_save_snapshot <- retry_snapshot
      stopifnot(isTRUE(state_api$commit(history_df)))

      retry_call <- harness$save_calls[[2L]]
      stopifnot(
        identical(
          retry_call$expected_revision,
          external_snapshot$revision
        ),
        identical(
          state_api$read$snapshot(),
          retry_snapshot
        )
      )
    }
  )
})

local({
  initial <- shopping_history_state_fixture()
  harness <- shopping_history_state_harness(initial)

  shiny::testServer(
    shopping_history_state_test_server,
    args = list(harness = harness),
    {
      history_df <- data.frame(
        Indkøbsliste = "1 stk Lokal vare",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      harness$disk_snapshot <- shopping_history_state_fixture(
        revision = "external-revision",
        entries = shopping_history_state_entries(
          "1 stk Ekstern vare"
        )
      )
      harness$fail_read_once <- TRUE

      refresh_error <- tryCatch(
        state_api$commit(history_df),
        error = identity
      )

      stopifnot(
        inherits(refresh_error, "error"),
        grepl(
          "den nyeste historik kunne ikke indlæses",
          conditionMessage(refresh_error),
          fixed = TRUE
        ),
        grepl(
          "Fremprovokeret læsefejl.",
          conditionMessage(refresh_error),
          fixed = TRUE
        ),
        identical(state_api$read$snapshot(), initial)
      )
    }
  )
})

local({
  initial <- shopping_history_state_fixture()
  harness <- shopping_history_state_harness(initial)

  shiny::testServer(
    shopping_history_state_test_server,
    args = list(
      harness = harness,
      poll_interval_ms = 1000L
    ),
    {
      session$flushReact()
      reads_before_unchanged <- length(harness$read_calls)
      revisions_before_unchanged <- length(
        harness$revision_calls
      )

      session$elapse(1001L)
      session$flushReact()
      stopifnot(
        length(harness$revision_calls) >
          revisions_before_unchanged,
        length(harness$read_calls) == reads_before_unchanged,
        identical(state_api$read$snapshot(), initial)
      )

      external_snapshot <- shopping_history_state_fixture(
        revision = "polled-revision",
        entries = shopping_history_state_entries(
          "1 stk Polling-vare",
          filename = "indkobsseddel_20260731.rda",
          date = as.Date("2026-07-31")
        )
      )
      harness$disk_snapshot <- external_snapshot
      reads_before_changed <- length(harness$read_calls)

      session$elapse(1001L)
      session$flushReact()
      stopifnot(
        length(harness$read_calls) ==
          reads_before_changed + 1L,
        identical(
          state_api$read$snapshot(),
          external_snapshot
        )
      )
    }
  )
})

local({
  initial <- shopping_history_state_fixture()
  harness <- shopping_history_state_harness(initial)

  shiny::testServer(
    shopping_history_state_test_server,
    args = list(
      harness = harness,
      poll_interval_ms = 1000L
    ),
    {
      session$flushReact()
      external_snapshot <- shopping_history_state_fixture(
        revision = "eventual-poll-revision",
        entries = shopping_history_state_entries(
          "1 stk Vare efter polling-fejl"
        )
      )
      harness$disk_snapshot <- external_snapshot
      reads_before_errors <- length(harness$read_calls)

      harness$fail_revision_once <- TRUE
      session$elapse(1001L)
      session$flushReact()
      stopifnot(
        length(harness$read_calls) == reads_before_errors,
        identical(state_api$read$snapshot(), initial)
      )

      harness$fail_read_once <- TRUE
      session$elapse(1001L)
      session$flushReact()
      stopifnot(
        length(harness$read_calls) ==
          reads_before_errors + 1L,
        identical(state_api$read$snapshot(), initial)
      )

      harness$invalid_read_once <- TRUE
      session$elapse(1001L)
      session$flushReact()
      stopifnot(
        length(harness$read_calls) ==
          reads_before_errors + 2L,
        identical(state_api$read$snapshot(), initial)
      )

      session$elapse(1001L)
      session$flushReact()
      stopifnot(
        length(harness$read_calls) ==
          reads_before_errors + 3L,
        identical(
          state_api$read$snapshot(),
          external_snapshot
        )
      )
    }
  )
})

valid_snapshot <- shopping_history_state_fixture()
stopifnot(isTRUE(
  shopping_history_state_validate_snapshot(valid_snapshot)
))

invalid_snapshots <- list(
  NULL,
  list(
    revision = "forkert rækkefølge",
    entries = valid_snapshot$entries
  ),
  list(
    entries = "ikke en historiktabel",
    revision = "revision"
  ),
  list(
    entries = data.frame(forkert = "kolonne"),
    revision = "revision"
  ),
  list(
    entries = transform(
      valid_snapshot$entries,
      filename = factor(filename)
    ),
    revision = "revision"
  ),
  list(
    entries = transform(
      valid_snapshot$entries,
      date = as.character(date)
    ),
    revision = "revision"
  ),
  list(
    entries = transform(
      valid_snapshot$entries,
      line_number = as.numeric(line_number)
    ),
    revision = "revision"
  ),
  list(
    entries = transform(
      valid_snapshot$entries,
      Indkøbsliste = factor(Indkøbsliste)
    ),
    revision = "revision"
  ),
  list(entries = valid_snapshot$entries, revision = NA_character_),
  list(entries = valid_snapshot$entries, revision = character()),
  list(entries = valid_snapshot$entries, revision = ""),
  list(entries = valid_snapshot$entries, revision = 1)
)

for (invalid_snapshot in invalid_snapshots) {
  validation_error <- tryCatch(
    {
      shopping_history_state_validate_snapshot(invalid_snapshot)
      NULL
    },
    error = identity
  )
  stopifnot(inherits(validation_error, "error"))
}

message(paste(
  "Indkøbshistorikkens state validerer og publicerer komplette snapshots,",
  "gemmer før publicering, genindlæser ved konflikt og prøver polling-fejl",
  "igen uden at ændre den kanoniske state."
))
