suppressPackageStartupMessages(source("app.R", encoding = "UTF-8"))

run_shopping_history_integration_tests <- function() {
  test_history_dir <- tempfile("shopping-history-integration-")
  dir.create(test_history_dir)
  on.exit(
    unlink(test_history_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  original_read <- shopping_history_store_read
  original_revision <- shopping_history_store_revision
  original_save <- shopping_history_store_save
  on.exit(
    {
      assign(
        "shopping_history_store_read",
        original_read,
        envir = .GlobalEnv
      )
      assign(
        "shopping_history_store_revision",
        original_revision,
        envir = .GlobalEnv
      )
      assign(
        "shopping_history_store_save",
        original_save,
        envir = .GlobalEnv
      )
    },
    add = TRUE
  )

  fixed_date <- as.Date("2026-07-22")
  requested_read_dirs <- character()
  requested_revision_dirs <- character()
  requested_save_dirs <- character()
  requested_expected_revisions <- character()
  save_calls <- 0L
  fail_next_save <- FALSE
  fail_next_read <- FALSE
  returned_snapshot <- NULL
  history_column <- "Indk\u00f8bsliste"

  read_stub <- function(history_dir = "./data/indkobssedler") {
    requested_read_dirs <<- c(requested_read_dirs, history_dir)
    if (fail_next_read) {
      fail_next_read <<- FALSE
      stop(
        "Fremprovokeret fejl ved genindlæsning.",
        call. = FALSE
      )
    }
    original_read(history_dir = test_history_dir)
  }

  revision_stub <- function(history_dir = "./data/indkobssedler") {
    requested_revision_dirs <<- c(
      requested_revision_dirs,
      history_dir
    )
    original_revision(history_dir = test_history_dir)
  }

  # Stubbene ignorerer appens faste sti og sender alle filoperationer til den
  # friske temp-mappe. Datoen injiceres samtidig, så filnavnet er stabilt.
  save_stub <- function(
    history_df,
    expected_revision,
    history_dir = "./data/indkobssedler",
    date = Sys.Date()
  ) {
    requested_save_dirs <<- c(requested_save_dirs, history_dir)
    requested_expected_revisions <<- c(
      requested_expected_revisions,
      expected_revision
    )
    save_calls <<- save_calls + 1L
    if (fail_next_save) {
      fail_next_save <<- FALSE
      stop("Fremprovokeret historikfejl.", call. = FALSE)
    }

    snapshot <- original_save(
      history_df,
      expected_revision = expected_revision,
      history_dir = test_history_dir,
      date = fixed_date
    )
    returned_snapshot <<- snapshot
    snapshot
  }

  assign(
    "shopping_history_store_read",
    read_stub,
    envir = .GlobalEnv
  )
  assign(
    "shopping_history_store_revision",
    revision_stub,
    envir = .GlobalEnv
  )
  assign(
    "shopping_history_store_save",
    save_stub,
    envir = .GlobalEnv
  )

  shiny::testServer(server, {
    session$flushReact()

    initial_snapshot <- history_state$read$snapshot()
    stopifnot(
      identical(names(initial_snapshot), c("entries", "revision")),
      identical(
        names(initial_snapshot$entries),
        c("filename", "date", "line_number", history_column)
      ),
      nrow(initial_snapshot$entries) == 0L,
      nrow(history_current()) == 0L,
      nrow(popular_items_current()) == 0L,
      nrow(inspiration_api$recipe_statistics()) == 0L,
      identical(requested_read_dirs[[1L]], "./data/indkobssedler")
    )

    history_df <- data.frame(
      value = c(
        "2 stk Integrationsavocado",
        "",
        "Burger (til 2 pers.):"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(history_df) <- history_column

    stopifnot(identical(history_state$commit(history_df), TRUE))
    session$flushReact()

    published_snapshot <- history_state$read$snapshot()
    popular_items <- popular_items_current()
    recipe_statistics <- inspiration_api$recipe_statistics()
    stopifnot(
      save_calls == 1L,
      identical(requested_save_dirs, "./data/indkobssedler"),
      identical(
        requested_expected_revisions,
        initial_snapshot$revision
      ),
      identical(published_snapshot, returned_snapshot),
      identical(history_current(), returned_snapshot$entries),
      identical(
        published_snapshot$revision,
        original_read(test_history_dir)$revision
      ),
      identical(
        unique(published_snapshot$entries$filename),
        "indkobsseddel_20260722.rda"
      ),
      identical(
        unique(published_snapshot$entries$date),
        fixed_date
      ),
      "Integrationsavocado" %in% popular_items[[history_column]],
      any(
        recipe_statistics$retter == "Burger" &
          recipe_statistics$dato == fixed_date
      )
    )

    snapshot_before_failure <- history_state$read$snapshot()
    failed_history <- data.frame(
      value = "1 stk Skal-ikke-publiceres",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(failed_history) <- history_column
    fail_next_save <<- TRUE
    failure <- tryCatch(
      history_state$commit(failed_history),
      error = identity
    )
    session$flushReact()

    stopifnot(
      inherits(failure, "error"),
      grepl("Fremprovokeret historikfejl", conditionMessage(failure)),
      save_calls == 2L,
      identical(
        history_state$read$snapshot(),
        snapshot_before_failure
      ),
      identical(history_current(), snapshot_before_failure$entries),
      !"Skal-ikke-publiceres" %in%
        popular_items_current()[[history_column]],
      identical(
        inspiration_api$recipe_statistics(),
        recipe_statistics
      )
    )

    # En revisionskonflikt genindlæser den nyeste historik i alle forbrugere.
    # Kandidaten gemmes ikke, og næste forsøg bruger den nye revision.
    external_history <- data.frame(
      value = c(
        "1 stk Ekstern-vare",
        "",
        "Tortellini (til 2 pers.):"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(external_history) <- history_column
    external_snapshot <- original_save(
      external_history,
      expected_revision = original_read(test_history_dir)$revision,
      history_dir = test_history_dir,
      date = as.Date("2026-07-23")
    )
    conflicting_history <- data.frame(
      value = "1 stk Må-ikke-overskrive",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(conflicting_history) <- history_column

    conflict <- tryCatch(
      history_state$commit(conflicting_history),
      error = identity
    )
    session$flushReact()

    stopifnot(
      inherits(conflict, "error"),
      grepl("Historikken er nu opdateret", conditionMessage(conflict)),
      save_calls == 3L,
      identical(
        requested_expected_revisions[[3L]],
        snapshot_before_failure$revision
      ),
      identical(
        history_state$read$snapshot(),
        external_snapshot
      ),
      identical(history_current(), external_snapshot$entries),
      "Ekstern-vare" %in%
        popular_items_current()[[history_column]],
      !"Må-ikke-overskrive" %in%
        popular_items_current()[[history_column]],
      any(
        inspiration_api$recipe_statistics()$retter ==
          "Tortellini"
      ),
      length(requested_read_dirs) == 2L
    )

    retry_history <- data.frame(
      value = c(
        "1 stk Efter-genindlæsning",
        "",
        "Burger (til 2 pers.):"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(retry_history) <- history_column
    stopifnot(identical(
      history_state$commit(retry_history),
      TRUE
    ))
    session$flushReact()

    stopifnot(
      save_calls == 4L,
      identical(
        requested_expected_revisions[[4L]],
        external_snapshot$revision
      ),
      identical(
        history_state$read$snapshot(),
        returned_snapshot
      ),
      "Efter-genindlæsning" %in%
        popular_items_current()[[history_column]]
    )

    # Hvis selve genindlæsningen fejler efter en konflikt, bevares det sidste
    # komplette snapshot i alle forbrugere.
    snapshot_before_read_failure <-
      history_state$read$snapshot()
    statistics_before_read_failure <-
      inspiration_api$recipe_statistics()
    newer_disk_history <- data.frame(
      value = "1 stk Endnu-nyere-ekstern-vare",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(newer_disk_history) <- history_column
    invisible(original_save(
      newer_disk_history,
      expected_revision =
        original_read(test_history_dir)$revision,
      history_dir = test_history_dir,
      date = as.Date("2026-07-24")
    ))
    fail_next_read <<- TRUE
    refresh_failure <- tryCatch(
      history_state$commit(conflicting_history),
      error = identity
    )
    session$flushReact()

    stopifnot(
      inherits(refresh_failure, "error"),
      grepl(
        "kunne ikke indlæses",
        conditionMessage(refresh_failure)
      ),
      grepl(
        "Fremprovokeret fejl ved genindlæsning",
        conditionMessage(refresh_failure)
      ),
      save_calls == 5L,
      identical(
        requested_expected_revisions[[5L]],
        snapshot_before_read_failure$revision
      ),
      identical(
        history_state$read$snapshot(),
        snapshot_before_read_failure
      ),
      identical(
        history_current(),
        snapshot_before_read_failure$entries
      ),
      identical(
        inspiration_api$recipe_statistics(),
        statistics_before_read_failure
      )
    )
  })
}

run_shopping_history_integration_tests()
message(paste(
  "Historik-integrationen publicerer et gemt snapshot til forslag og",
  "opskriftsstatistik, bevarer state ved fejl og genindlæser ved konflikt."
))
