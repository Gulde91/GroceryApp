suppressPackageStartupMessages(source("app.R", encoding = "UTF-8"))

run_shopping_history_integration_tests <- function() {
  test_history_dir <- tempfile("shopping-history-integration-")
  dir.create(test_history_dir)
  on.exit(
    unlink(test_history_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  original_read <- shopping_history_store_read
  original_save <- shopping_history_store_save
  on.exit(
    {
      assign(
        "shopping_history_store_read",
        original_read,
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
  requested_save_dirs <- character()
  save_calls <- 0L
  fail_next_save <- FALSE
  returned_snapshot <- NULL
  history_column <- "Indk\u00f8bsliste"

  read_stub <- function(history_dir = "./data/indkobssedler") {
    requested_read_dirs <<- c(requested_read_dirs, history_dir)
    original_read(history_dir = test_history_dir)
  }

  # Stubbene ignorerer appens faste sti og sender alle filoperationer til den
  # friske temp-mappe. Datoen injiceres samtidig, så filnavnet er stabilt.
  save_stub <- function(
    history_df,
    history_dir = "./data/indkobssedler",
    date = Sys.Date()
  ) {
    requested_save_dirs <<- c(requested_save_dirs, history_dir)
    save_calls <<- save_calls + 1L
    if (fail_next_save) {
      fail_next_save <<- FALSE
      stop("Fremprovokeret historikfejl.", call. = FALSE)
    }

    snapshot <- original_save(
      history_df,
      history_dir = test_history_dir,
      date = fixed_date
    )
    snapshot$revision <- paste0("returned-test-revision-", save_calls)
    returned_snapshot <<- snapshot
    snapshot
  }

  assign(
    "shopping_history_store_read",
    read_stub,
    envir = .GlobalEnv
  )
  assign(
    "shopping_history_store_save",
    save_stub,
    envir = .GlobalEnv
  )

  shiny::testServer(server, {
    session$flushReact()

    initial_snapshot <- rv_historyStore()
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

    stopifnot(identical(commit_shopping_history(history_df), TRUE))
    session$flushReact()

    published_snapshot <- rv_historyStore()
    popular_items <- popular_items_current()
    recipe_statistics <- inspiration_api$recipe_statistics()
    stopifnot(
      save_calls == 1L,
      identical(requested_save_dirs, "./data/indkobssedler"),
      identical(published_snapshot, returned_snapshot),
      identical(history_current(), returned_snapshot$entries),
      identical(
        published_snapshot$revision,
        "returned-test-revision-1"
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

    snapshot_before_failure <- rv_historyStore()
    failed_history <- data.frame(
      value = "1 stk Skal-ikke-publiceres",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(failed_history) <- history_column
    fail_next_save <<- TRUE
    failure <- tryCatch(
      commit_shopping_history(failed_history),
      error = identity
    )
    session$flushReact()

    stopifnot(
      inherits(failure, "error"),
      grepl("Fremprovokeret historikfejl", conditionMessage(failure)),
      save_calls == 2L,
      identical(rv_historyStore(), snapshot_before_failure),
      identical(history_current(), snapshot_before_failure$entries),
      !"Skal-ikke-publiceres" %in%
        popular_items_current()[[history_column]],
      identical(
        inspiration_api$recipe_statistics(),
        recipe_statistics
      )
    )
  })
}

run_shopping_history_integration_tests()
message(paste(
  "Historik-integrationen publicerer et gemt snapshot til forslag og",
  "opskriftsstatistik med det samme og bevarer state ved lagringsfejl."
))
