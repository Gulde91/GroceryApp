suppressPackageStartupMessages(
  source("basis_varer_state.R", encoding = "UTF-8")
)

basis_varer_state_row <- function(
  navn,
  enhed = "stk",
  kat_1 = "konserves",
  kat_2 = ""
) {
  data.frame(
    Indkobsliste = navn,
    maengde = 1,
    enhed = enhed,
    kat_1 = kat_1,
    kat_2 = kat_2,
    stringsAsFactors = FALSE
  )
}

basis_varer_state_fixture <- function(
  revision = "fixture-revision-1",
  varer = basis_varer_state_row("Havregryn")
) {
  list(
    varer = varer,
    revision = revision
  )
}

basis_varer_state_stop_conflict <- function() {
  condition <- structure(
    list(
      message = "Basisvarerne er ændret i en anden session.",
      call = NULL
    ),
    class = c(
      "basis_varer_store_conflict",
      "error",
      "condition"
    )
  )
  stop(condition)
}

basis_varer_state_harness <- function(snapshot) {
  harness <- new.env(parent = emptyenv())
  harness$disk_snapshot <- snapshot
  harness$read_calls <- character()
  harness$revision_calls <- character()
  harness$commit_calls <- list()
  harness$notifications <- list()
  harness$next_revision_number <- 1L
  harness$next_commit_snapshot <- NULL
  harness$fail_commit_once <- FALSE
  harness$fail_revision_once <- FALSE
  harness$fail_read_once <- FALSE
  harness$invalid_read_once <- FALSE
  harness$state_reader <- NULL
  harness$visible_during_commit <- NULL

  harness$store_read <- function(data_dir = "./data") {
    harness$read_calls <- c(harness$read_calls, data_dir)

    if (isTRUE(harness$fail_read_once)) {
      harness$fail_read_once <- FALSE
      stop("Fremprovokeret læsefejl.", call. = FALSE)
    }
    if (isTRUE(harness$invalid_read_once)) {
      harness$invalid_read_once <- FALSE
      return(list(varer = "ikke en tabel", revision = NA_character_))
    }

    harness$disk_snapshot
  }

  harness$store_revision <- function(data_dir = "./data") {
    harness$revision_calls <- c(
      harness$revision_calls,
      data_dir
    )

    if (isTRUE(harness$fail_revision_once)) {
      harness$fail_revision_once <- FALSE
      stop("Fremprovokeret revisionsfejl.", call. = FALSE)
    }

    harness$disk_snapshot$revision
  }

  harness$store_commit <- function(
    varer,
    expected_revision,
    data_dir = "./data",
    ...
  ) {
    harness$commit_calls[[
      length(harness$commit_calls) + 1L
    ]] <- list(
      varer = varer,
      expected_revision = expected_revision,
      data_dir = data_dir
    )

    if (is.function(harness$state_reader)) {
      harness$visible_during_commit <- harness$state_reader()
    }

    if (isTRUE(harness$fail_commit_once)) {
      harness$fail_commit_once <- FALSE
      stop("Fremprovokeret commitfejl.", call. = FALSE)
    }
    if (!identical(
      expected_revision,
      harness$disk_snapshot$revision
    )) {
      basis_varer_state_stop_conflict()
    }

    if (!is.null(harness$next_commit_snapshot)) {
      next_snapshot <- harness$next_commit_snapshot
      harness$next_commit_snapshot <- NULL
    } else {
      next_snapshot <- basis_varer_state_fixture(
        revision = paste0(
          "memory-revision-",
          harness$next_revision_number
        ),
        varer = varer
      )
      harness$next_revision_number <-
        harness$next_revision_number + 1L
    }

    harness$disk_snapshot <- next_snapshot
    next_snapshot
  }

  harness$notify <- function(...) {
    harness$notifications[[
      length(harness$notifications) + 1L
    ]] <- list(...)
    invisible(NULL)
  }

  harness
}

basis_varer_state_test_server <- function(
  id,
  harness,
  data_dir = "memory-basisvarer",
  poll_interval_ms = 1000L
) {
  shiny::moduleServer(id, function(input, output, session) {
    state_api <- create_basis_varer_state(
      session = session,
      data_dir = data_dir,
      poll_interval_ms = poll_interval_ms,
      store_read = harness$store_read,
      store_revision = harness$store_revision,
      store_commit = harness$store_commit,
      notify = harness$notify
    )
  })
}

local({
  initial <- basis_varer_state_fixture()
  harness <- basis_varer_state_harness(initial)

  shiny::testServer(
    basis_varer_state_test_server,
    args = list(harness = harness),
    {
      stopifnot(
        identical(harness$read_calls, "memory-basisvarer"),
        identical(names(state_api), c("read", "commit")),
        identical(
          names(state_api$read),
          c("snapshot", "varer", "revision")
        ),
        all(vapply(state_api$read, is.function, logical(1))),
        is.function(state_api$commit),
        identical(state_api$read$snapshot(), initial),
        identical(state_api$read$varer(), initial$varer),
        identical(state_api$read$revision(), initial$revision)
      )

      detached_copy <- state_api$read$snapshot()
      detached_copy$varer$Indkobsliste[[1]] <- "Ændret kopi"
      stopifnot(identical(state_api$read$snapshot(), initial))

      invalidations <- new.env(parent = emptyenv())
      invalidations$varer <- 0L
      invalidations$revision <- 0L

      shiny::observe({
        state_api$read$varer()
        invalidations$varer <- invalidations$varer + 1L
      })
      shiny::observe({
        state_api$read$revision()
        invalidations$revision <- invalidations$revision + 1L
      })
      session$flushReact()

      counts_before <- as.list(invalidations)
      candidate <- rbind(
        initial$varer,
        basis_varer_state_row("  Kaffe  ")
      )
      canonical_varer <- rbind(
        initial$varer,
        basis_varer_state_row("Kaffe")
      )
      returned_snapshot <- basis_varer_state_fixture(
        revision = "canonical-revision",
        varer = canonical_varer
      )
      harness$next_commit_snapshot <- returned_snapshot
      harness$state_reader <- state_api$read$snapshot

      stopifnot(isTRUE(state_api$commit(candidate)))
      session$flushReact()

      commit_call <- harness$commit_calls[[1L]]
      stopifnot(
        identical(harness$visible_during_commit, initial),
        identical(commit_call$varer, candidate),
        identical(commit_call$expected_revision, initial$revision),
        identical(commit_call$data_dir, "memory-basisvarer"),
        identical(state_api$read$snapshot(), returned_snapshot),
        identical(state_api$read$varer(), canonical_varer),
        identical(
          state_api$read$revision(),
          "canonical-revision"
        ),
        invalidations$varer == counts_before$varer + 1L,
        invalidations$revision ==
          counts_before$revision + 1L
      )

      counts_before_revision_only <- as.list(invalidations)
      revision_only_snapshot <- basis_varer_state_fixture(
        revision = "revision-only-change",
        varer = canonical_varer
      )
      harness$next_commit_snapshot <- revision_only_snapshot

      stopifnot(isTRUE(state_api$commit(canonical_varer)))
      session$flushReact()
      stopifnot(
        identical(
          invalidations$varer,
          counts_before_revision_only$varer
        ),
        invalidations$revision ==
          counts_before_revision_only$revision + 1L
      )
    }
  )
})

local({
  initial <- basis_varer_state_fixture()
  harness <- basis_varer_state_harness(initial)

  shiny::testServer(
    basis_varer_state_test_server,
    args = list(harness = harness),
    {
      harness$state_reader <- state_api$read$snapshot
      candidate <- rbind(
        initial$varer,
        basis_varer_state_row("Kaffe")
      )
      reads_before_error <- length(harness$read_calls)
      harness$fail_commit_once <- TRUE

      stopifnot(
        identical(
          state_api$commit(
            candidate,
            error_message = "Kunne ikke gemme testvaren."
          ),
          FALSE
        ),
        identical(state_api$read$snapshot(), initial),
        identical(harness$disk_snapshot, initial),
        identical(harness$visible_during_commit, initial),
        length(harness$read_calls) == reads_before_error,
        length(harness$notifications) == 1L
      )

      notification <- harness$notifications[[1L]]
      notification_text <- paste(
        unlist(notification),
        collapse = " "
      )
      stopifnot(
        grepl(
          "Kunne ikke gemme testvaren.",
          notification_text,
          fixed = TRUE
        ),
        grepl(
          "Fremprovokeret commitfejl.",
          notification_text,
          fixed = TRUE
        ),
        identical(notification$type, "error"),
        is.null(notification$duration)
      )
    }
  )
})

local({
  initial <- basis_varer_state_fixture()
  harness <- basis_varer_state_harness(initial)

  shiny::testServer(
    basis_varer_state_test_server,
    args = list(harness = harness),
    {
      candidate <- rbind(
        initial$varer,
        basis_varer_state_row("Kaffe")
      )
      external_snapshot <- basis_varer_state_fixture(
        revision = "external-revision",
        varer = rbind(
          initial$varer,
          basis_varer_state_row("Te")
        )
      )
      harness$disk_snapshot <- external_snapshot
      reads_before_conflict <- length(harness$read_calls)

      stopifnot(
        identical(state_api$commit(candidate), FALSE),
        length(harness$read_calls) == reads_before_conflict + 1L,
        identical(
          state_api$read$snapshot(),
          external_snapshot
        ),
        length(harness$notifications) == 1L,
        grepl(
          "Listen er nu opdateret; prøv handlingen igen.",
          paste(
            unlist(harness$notifications[[1L]]),
            collapse = " "
          ),
          fixed = TRUE
        )
      )

      retry_candidate <- rbind(
        state_api$read$varer(),
        basis_varer_state_row("Kaffe")
      )
      stopifnot(isTRUE(state_api$commit(retry_candidate)))

      retry_call <- harness$commit_calls[[2L]]
      stopifnot(
        identical(
          retry_call$expected_revision,
          external_snapshot$revision
        ),
        all(
          c("Te", "Kaffe") %in%
            state_api$read$varer()$Indkobsliste
        )
      )
    }
  )
})

local({
  initial <- basis_varer_state_fixture()
  harness <- basis_varer_state_harness(initial)

  shiny::testServer(
    basis_varer_state_test_server,
    args = list(harness = harness),
    {
      candidate <- rbind(
        initial$varer,
        basis_varer_state_row("Kaffe")
      )
      harness$disk_snapshot <- basis_varer_state_fixture(
        revision = "external-revision",
        varer = rbind(
          initial$varer,
          basis_varer_state_row("Te")
        )
      )
      harness$fail_read_once <- TRUE

      stopifnot(
        identical(state_api$commit(candidate), FALSE),
        identical(state_api$read$snapshot(), initial),
        length(harness$notifications) == 1L
      )

      notification_text <- paste(
        unlist(harness$notifications[[1L]]),
        collapse = " "
      )
      stopifnot(
        grepl(
          "Basisvarerne er ændret i en anden session.",
          notification_text,
          fixed = TRUE
        ),
        grepl(
          "Den nyeste liste kunne ikke genindlæses:",
          notification_text,
          fixed = TRUE
        ),
        grepl(
          "Fremprovokeret læsefejl.",
          notification_text,
          fixed = TRUE
        )
      )
    }
  )
})

local({
  initial <- basis_varer_state_fixture()
  harness <- basis_varer_state_harness(initial)

  shiny::testServer(
    basis_varer_state_test_server,
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

      external_snapshot <- basis_varer_state_fixture(
        revision = "polled-revision",
        varer = rbind(
          initial$varer,
          basis_varer_state_row("Polling-vare")
        )
      )
      harness$disk_snapshot <- external_snapshot
      reads_before_changed <- length(harness$read_calls)

      session$elapse(1001L)
      session$flushReact()
      stopifnot(
        length(harness$read_calls) == reads_before_changed + 1L,
        identical(
          state_api$read$snapshot(),
          external_snapshot
        )
      )
    }
  )
})

local({
  initial <- basis_varer_state_fixture()
  harness <- basis_varer_state_harness(initial)

  shiny::testServer(
    basis_varer_state_test_server,
    args = list(
      harness = harness,
      poll_interval_ms = 1000L
    ),
    {
      session$flushReact()
      external_snapshot <- basis_varer_state_fixture(
        revision = "retry-revision",
        varer = rbind(
          initial$varer,
          basis_varer_state_row("Vare efter retry")
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
        length(harness$read_calls) == reads_before_errors + 1L,
        identical(state_api$read$snapshot(), initial)
      )

      harness$invalid_read_once <- TRUE
      session$elapse(1001L)
      session$flushReact()
      stopifnot(
        length(harness$read_calls) == reads_before_errors + 2L,
        identical(state_api$read$snapshot(), initial)
      )

      session$elapse(1001L)
      session$flushReact()
      stopifnot(
        length(harness$read_calls) == reads_before_errors + 3L,
        identical(
          state_api$read$snapshot(),
          external_snapshot
        )
      )
    }
  )
})

valid_snapshot <- basis_varer_state_fixture()
stopifnot(isTRUE(
  basis_varer_state_validate_snapshot(valid_snapshot)
))

invalid_snapshots <- list(
  NULL,
  list(revision = "forkert rækkefølge", varer = data.frame()),
  list(varer = "ikke en tabel", revision = "revision"),
  list(varer = data.frame(forkert = "kolonne"), revision = "revision"),
  list(varer = data.frame(), revision = NA_character_),
  list(varer = data.frame(), revision = character()),
  list(varer = valid_snapshot$varer, revision = ""),
  list(varer = data.frame(), revision = 1)
)
for (invalid_snapshot in invalid_snapshots) {
  validation_error <- tryCatch(
    {
      basis_varer_state_validate_snapshot(invalid_snapshot)
      NULL
    },
    error = identity
  )
  stopifnot(inherits(validation_error, "error"))
}

message(paste(
  "Basisvarernes state læser og publicerer komplette snapshots, gemmer",
  "før publicering, genindlæser ved konflikt og prøver sikre polling-fejl",
  "igen uden at ændre den kanoniske state."
))
