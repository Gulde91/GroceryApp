suppressPackageStartupMessages({
  source(file.path("R", "recipe_schema.R"), encoding = "UTF-8")
  source(file.path("R", "recipe_catalog.R"), encoding = "UTF-8")
  source(
    file.path("R", "recipe_catalog_state.R"),
    encoding = "UTF-8"
  )
})

recipe_catalog_state_fixture <- function(revision = "fixture-revision-1") {
  burger <- data.frame(
    Burger = c("hakket oksekød", "burgerbolle"),
    maengde = c(0.175, 1),
    enhed = c("kg", "stk"),
    kat_1 = c("kød", "brød"),
    kat_2 = c("", ""),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  gemt <- data.frame(
    Gemt = "testvare",
    maengde = 1,
    enhed = "stk",
    kat_1 = "konserves",
    kat_2 = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  list(
    active_retter = data.frame(
      retter = "Burger",
      key = "burger_opskr",
      type = "okse|gris",
      stringsAsFactors = FALSE
    ),
    archived_retter = data.frame(
      retter = "Gemt",
      key = "gemt_opskr",
      type = "vegetar",
      stringsAsFactors = FALSE
    ),
    recipes = list(
      burger_opskr = burger,
      gemt_opskr = gemt
    ),
    links = data.frame(
      ret = "Burger",
      link = "https://example.com/burger",
      stringsAsFactors = FALSE
    ),
    revision = revision
  )
}

recipe_catalog_state_harness <- function(snapshot) {
  harness <- new.env(parent = emptyenv())
  harness$disk_snapshot <- snapshot
  harness$read_calls <- character()
  harness$revision_calls <- character()
  harness$commit_calls <- list()
  harness$notifications <- list()
  harness$log_events <- list()
  harness$next_revision_number <- 1L
  harness$fail_commit_once <- FALSE
  harness$fail_revision_once <- FALSE
  harness$fail_revision_always <- FALSE
  harness$fail_read_once <- FALSE
  harness$invalid_read_once <- FALSE
  harness$fail_logger <- FALSE
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
      invalid <- harness$disk_snapshot
      invalid$revision <- NA_character_
      return(invalid)
    }
    harness$disk_snapshot
  }

  harness$store_revision <- function(data_dir = "./data") {
    harness$revision_calls <- c(harness$revision_calls, data_dir)
    if (
      isTRUE(harness$fail_revision_once) ||
        isTRUE(harness$fail_revision_always)
    ) {
      harness$fail_revision_once <- FALSE
      stop("Fremprovokeret revisionsfejl.", call. = FALSE)
    }
    harness$disk_snapshot$revision
  }

  harness$store_commit <- function(
    data_dir = "./data",
    active_retter = NULL,
    archived_retter = NULL,
    links = NULL,
    recipes = NULL,
    delete_recipe_keys = character(),
    expected_revision = NULL,
    ...
  ) {
    call <- list(
      data_dir = data_dir,
      active_retter = active_retter,
      archived_retter = archived_retter,
      links = links,
      recipes = recipes,
      delete_recipe_keys = delete_recipe_keys,
      expected_revision = expected_revision
    )
    harness$commit_calls[[length(harness$commit_calls) + 1L]] <- call

    if (is.function(harness$state_reader)) {
      harness$visible_during_commit <- harness$state_reader()
    }

    if (isTRUE(harness$fail_commit_once)) {
      harness$fail_commit_once <- FALSE
      stop("Fremprovokeret commitfejl.", call. = FALSE)
    }
    if (!identical(expected_revision, harness$disk_snapshot$revision)) {
      stop(
        paste(
          "Opskriftsdata er ændret i en anden session.",
          "Genindlæs appen og prøv igen."
        ),
        call. = FALSE
      )
    }

    next_disk_snapshot <- harness$disk_snapshot
    if (!is.null(active_retter)) {
      next_disk_snapshot$active_retter <- active_retter
    }
    if (!is.null(archived_retter)) {
      next_disk_snapshot$archived_retter <- archived_retter
    }
    if (!is.null(links)) next_disk_snapshot$links <- links
    if (!is.null(recipes)) {
      for (key in names(recipes)) {
        next_disk_snapshot$recipes[[key]] <- recipes[[key]]
      }
    }
    for (key in delete_recipe_keys) {
      next_disk_snapshot$recipes[[key]] <- NULL
    }

    next_revision <- paste0(
      "memory-revision-",
      harness$next_revision_number
    )
    harness$next_revision_number <- harness$next_revision_number + 1L
    next_disk_snapshot$revision <- next_revision
    harness$disk_snapshot <- next_disk_snapshot
    next_revision
  }

  harness$notify <- function(...) {
    harness$notifications[[length(harness$notifications) + 1L]] <- list(...)
    invisible(NULL)
  }

  harness$log_event <- function(
    level,
    event,
    component,
    fields = list(),
    error = NULL
  ) {
    if (isTRUE(harness$fail_logger)) {
      stop("Fremprovokeret loggerfejl.", call. = FALSE)
    }

    harness$log_events[[length(harness$log_events) + 1L]] <- list(
      level = level,
      event = event,
      component = component,
      fields = fields,
      error = error
    )
    invisible(TRUE)
  }

  harness
}

recipe_catalog_state_test_server <- function(
  id,
  harness,
  data_dir = "memory-recipes",
  poll_interval_ms = 1000L
) {
  shiny::moduleServer(id, function(input, output, session) {
    state_api <- create_recipe_catalog_state(
      session = session,
      data_dir = data_dir,
      poll_interval_ms = poll_interval_ms,
      store_read = harness$store_read,
      store_revision = harness$store_revision,
      store_commit = harness$store_commit,
      notify = harness$notify,
      log_event = harness$log_event
    )
  })
}

local({
  initial <- recipe_catalog_state_fixture()
  harness <- recipe_catalog_state_harness(initial)

  shiny::testServer(
    recipe_catalog_state_test_server,
    args = list(harness = harness),
    {
      stopifnot(
        identical(harness$read_calls, "memory-recipes"),
        identical(names(state_api), c("read", "commit")),
        identical(
          names(state_api$read),
          c(
            "snapshot",
            "recipes",
            "links",
            "active_retter",
            "archived_retter",
            "revision"
          )
        ),
        all(vapply(state_api$read, is.function, logical(1))),
        is.function(state_api$commit),
        identical(state_api$read$snapshot(), initial),
        identical(state_api$read$recipes(), initial$recipes),
        identical(state_api$read$links(), initial$links),
        identical(
          state_api$read$active_retter(),
          initial$active_retter
        ),
        identical(
          state_api$read$archived_retter(),
          initial$archived_retter
        ),
        identical(state_api$read$revision(), initial$revision)
      )

      detached_copy <- state_api$read$snapshot()
      detached_copy$recipes$burger_opskr$maengde[[1]] <- 999
      stopifnot(identical(state_api$read$snapshot(), initial))

      invalidations <- new.env(parent = emptyenv())
      invalidations$recipes <- 0L
      invalidations$links <- 0L
      invalidations$active_retter <- 0L
      invalidations$archived_retter <- 0L
      invalidations$revision <- 0L

      shiny::observe({
        state_api$read$recipes()
        invalidations$recipes <- invalidations$recipes + 1L
      })
      shiny::observe({
        state_api$read$links()
        invalidations$links <- invalidations$links + 1L
      })
      shiny::observe({
        state_api$read$active_retter()
        invalidations$active_retter <- invalidations$active_retter + 1L
      })
      shiny::observe({
        state_api$read$archived_retter()
        invalidations$archived_retter <-
          invalidations$archived_retter + 1L
      })
      shiny::observe({
        state_api$read$revision()
        invalidations$revision <- invalidations$revision + 1L
      })
      session$flushReact()

      counts_before <- as.list(invalidations)
      candidate <- state_api$read$snapshot()
      candidate$recipes$burger_opskr$maengde[[1]] <- 0.25
      harness$state_reader <- state_api$read$snapshot

      stopifnot(isTRUE(state_api$commit(
        candidate,
        log_context = list(
          action = "recipe_ingredient_update",
          recipe_key = "burger_opskr",
          recipe_name = "Burger",
          ingredient_name = "hakket oksekød",
          ingredient_row = 1L,
          success_message = paste(
            'Ingrediensen "hakket oksekød" i opskriften "Burger"',
            "blev opdateret."
          ),
          failure_message = paste(
            'Ingrediensen "hakket oksekød" i opskriften "Burger"',
            "kunne ikke opdateres."
          )
        )
      )))
      session$flushReact()

      commit_call <- harness$commit_calls[[1L]]
      published <- state_api$read$snapshot()
      success_log <- harness$log_events[[1L]]
      stopifnot(
        identical(harness$visible_during_commit, initial),
        identical(commit_call$data_dir, "memory-recipes"),
        identical(commit_call$expected_revision, initial$revision),
        identical(names(commit_call$recipes), "burger_opskr"),
        identical(
          commit_call$recipes$burger_opskr,
          candidate$recipes$burger_opskr
        ),
        is.null(commit_call$active_retter),
        is.null(commit_call$archived_retter),
        is.null(commit_call$links),
        length(commit_call$delete_recipe_keys) == 0L,
        identical(
          published$recipes$burger_opskr,
          candidate$recipes$burger_opskr
        ),
        identical(published$revision, "memory-revision-1"),
        invalidations$recipes == counts_before$recipes + 1L,
        invalidations$revision == counts_before$revision + 1L,
        identical(invalidations$links, counts_before$links),
        identical(
          invalidations$active_retter,
          counts_before$active_retter
        ),
        identical(
          invalidations$archived_retter,
          counts_before$archived_retter
        ),
        length(harness$log_events) == 1L,
        identical(success_log$level, "INFO"),
        identical(success_log$event, "commit_succeeded"),
        identical(success_log$component, "recipe_catalog_state"),
        identical(
          names(success_log$fields),
          c(
            "action",
            "recipe_key",
            "recipe_name",
            "ingredient_name",
            "ingredient_row",
            "message",
            "recipe_count",
            "changed_recipe_count",
            "deleted_recipe_count",
            "duration_ms",
            "stage",
            "outcome"
          )
        ),
        identical(
          success_log$fields$action,
          "recipe_ingredient_update"
        ),
        identical(success_log$fields$recipe_key, "burger_opskr"),
        identical(success_log$fields$recipe_name, "Burger"),
        identical(
          success_log$fields$ingredient_name,
          "hakket oksekød"
        ),
        identical(success_log$fields$ingredient_row, 1L),
        identical(
          success_log$fields$message,
          paste(
            'Ingrediensen "hakket oksekød" i opskriften "Burger"',
            "blev opdateret."
          )
        ),
        identical(success_log$fields$recipe_count, 2L),
        identical(success_log$fields$changed_recipe_count, 1L),
        identical(success_log$fields$deleted_recipe_count, 0L),
        is.numeric(success_log$fields$duration_ms),
        success_log$fields$duration_ms >= 0,
        identical(success_log$fields$stage, "complete"),
        identical(success_log$fields$outcome, "succeeded"),
        is.null(success_log$error)
      )

      counts_before_links <- as.list(invalidations)
      link_candidate <- state_api$read$snapshot()
      link_candidate$links$link[[1]] <-
        "https://example.com/ny-burger"
      stopifnot(isTRUE(state_api$commit(link_candidate)))
      session$flushReact()

      link_call <- harness$commit_calls[[2L]]
      stopifnot(
        is.null(link_call$active_retter),
        is.null(link_call$archived_retter),
        identical(link_call$links, link_candidate$links),
        length(link_call$recipes) == 0L,
        length(link_call$delete_recipe_keys) == 0L,
        invalidations$links == counts_before_links$links + 1L,
        invalidations$revision == counts_before_links$revision + 1L,
        identical(
          invalidations$recipes,
          counts_before_links$recipes
        ),
        identical(
          invalidations$active_retter,
          counts_before_links$active_retter
        ),
        identical(
          invalidations$archived_retter,
          counts_before_links$archived_retter
        ),
        length(harness$log_events) == 2L,
        identical(
          harness$log_events[[2L]]$event,
          "commit_succeeded"
        )
      )
    }
  )
})

local({
  initial <- recipe_catalog_state_fixture()
  harness <- recipe_catalog_state_harness(initial)

  shiny::testServer(
    recipe_catalog_state_test_server,
    args = list(harness = harness),
    {
      harness$state_reader <- state_api$read$snapshot
      candidate <- state_api$read$snapshot()
      candidate$recipes$burger_opskr$maengde[[1]] <- 0.4
      harness$fail_commit_once <- TRUE

      stopifnot(
        identical(
          state_api$commit(
            candidate,
            log_context = list(
              action = "recipe_ingredient_update",
              recipe_key = "burger_opskr",
              recipe_name = "Burger",
              ingredient_name = "hakket oksekød",
              success_message = "Må ikke bruges ved fejl.",
              failure_message = paste(
                'Ingrediensen "hakket oksekød" i opskriften "Burger"',
                "kunne ikke opdateres."
              )
            )
          ),
          FALSE
        ),
        identical(state_api$read$snapshot(), initial),
        identical(harness$disk_snapshot, initial),
        identical(harness$visible_during_commit, initial),
        length(harness$commit_calls) == 1L,
        length(harness$notifications) == 1L,
        grepl(
          "Fremprovokeret commitfejl.",
          paste(unlist(harness$notifications[[1L]]), collapse = " "),
          fixed = TRUE
        ),
        length(harness$log_events) == 1L,
        identical(harness$log_events[[1L]]$level, "ERROR"),
        identical(
          harness$log_events[[1L]]$event,
          "commit_failed"
        ),
        identical(
          harness$log_events[[1L]]$component,
          "recipe_catalog_state"
        ),
        identical(
          harness$log_events[[1L]]$fields$stage,
          "store_commit"
        ),
        identical(
          harness$log_events[[1L]]$fields$outcome,
          "failed"
        ),
        identical(
          harness$log_events[[1L]]$fields$message,
          paste(
            'Ingrediensen "hakket oksekød" i opskriften "Burger"',
            "kunne ikke opdateres."
          )
        ),
        !identical(
          harness$log_events[[1L]]$fields$message,
          "Må ikke bruges ved fejl."
        ),
        inherits(harness$log_events[[1L]]$error, "error")
      )

      conflicting_disk <- initial
      conflicting_disk$revision <- "ekstern-revision"
      harness$disk_snapshot <- conflicting_disk
      calls_before_conflict <- length(harness$commit_calls)
      notifications_before_conflict <- length(harness$notifications)

      stopifnot(
        identical(
          state_api$commit(
            candidate,
            log_context = list(
              action = "recipe_ingredient_update",
              recipe_key = "burger_opskr",
              recipe_name = "Burger",
              ingredient_name = "hakket oksekød",
              success_message = "Må ikke bruges ved fejl.",
              failure_message = paste(
                'Ingrediensen "hakket oksekød" i opskriften "Burger"',
                "kunne ikke opdateres."
              )
            )
          ),
          FALSE
        ),
        length(harness$commit_calls) == calls_before_conflict + 1L,
        length(harness$notifications) ==
          notifications_before_conflict + 1L,
        identical(state_api$read$snapshot(), initial),
        identical(harness$disk_snapshot, conflicting_disk),
        grepl(
          "ændret i en anden session",
          paste(
            unlist(harness$notifications[[
              notifications_before_conflict + 1L
            ]]),
            collapse = " "
          ),
          fixed = TRUE
        ),
        length(harness$log_events) == 2L,
        identical(harness$log_events[[2L]]$level, "WARN"),
        identical(
          harness$log_events[[2L]]$event,
          "commit_conflict"
        ),
        identical(
          harness$log_events[[2L]]$fields$refresh,
          "not_attempted"
        ),
        identical(
          harness$log_events[[2L]]$fields$outcome,
          "rejected"
        ),
        identical(
          harness$log_events[[2L]]$fields$message,
          paste(
            'Ingrediensen "hakket oksekød" i opskriften "Burger"',
            "kunne ikke opdateres."
          )
        ),
        !any(vapply(
          harness$log_events[2L],
          function(entry) identical(entry$event, "commit_failed"),
          logical(1)
        ))
      )
      harness$disk_snapshot <- initial

      calls_before_guard <- length(harness$commit_calls)
      state_before_guard <- state_api$read$snapshot()

      incomplete <- state_before_guard
      incomplete$links <- NULL
      stale <- state_before_guard
      stale$revision <- "forældet-revision"
      implicit_delete <- state_before_guard
      implicit_delete$recipes$gemt_opskr <- NULL
      invalid_explicit_delete <- state_before_guard

      stopifnot(
        identical(state_api$commit(incomplete), FALSE),
        identical(state_api$commit(stale), FALSE),
        identical(state_api$commit(implicit_delete), FALSE),
        identical(
          state_api$commit(
            invalid_explicit_delete,
            delete_recipe_keys = "gemt_opskr"
          ),
          FALSE
        ),
        length(harness$commit_calls) == calls_before_guard,
        identical(state_api$read$snapshot(), state_before_guard)
      )

      valid_delete <- state_before_guard
      valid_delete$archived_retter <- valid_delete$archived_retter[
        valid_delete$archived_retter$key != "gemt_opskr",
        ,
        drop = FALSE
      ]
      valid_delete$recipes$gemt_opskr <- NULL

      stopifnot(isTRUE(state_api$commit(
        valid_delete,
        delete_recipe_keys = "gemt_opskr"
      )))

      delete_call <- harness$commit_calls[[calls_before_guard + 1L]]
      deleted_state <- state_api$read$snapshot()
      stopifnot(
        identical(delete_call$delete_recipe_keys, "gemt_opskr"),
        identical(
          delete_call$archived_retter,
          valid_delete$archived_retter
        ),
        is.null(delete_call$active_retter),
        is.null(delete_call$links),
        length(delete_call$recipes) == 0L,
        !"gemt_opskr" %in% names(deleted_state$recipes),
        !"gemt_opskr" %in% deleted_state$archived_retter$key,
        identical(deleted_state$revision, "memory-revision-1")
      )
    }
  )
})

local({
  initial <- recipe_catalog_state_fixture()
  harness <- recipe_catalog_state_harness(initial)

  shiny::testServer(
    recipe_catalog_state_test_server,
    args = list(harness = harness, poll_interval_ms = 1000L),
    {
      session$flushReact()
      reads_before_unchanged <- length(harness$read_calls)
      revisions_before_unchanged <- length(harness$revision_calls)

      session$elapse(1001L)
      session$flushReact()
      stopifnot(
        length(harness$revision_calls) > revisions_before_unchanged,
        length(harness$read_calls) == reads_before_unchanged,
        identical(state_api$read$snapshot(), initial),
        length(harness$log_events) == 0L
      )

      external <- recipe_catalog_state_fixture(
        revision = "external-revision"
      )
      external$recipes$burger_opskr$maengde[[1]] <- 0.9
      external$active_retter$type[[1]] <- "vegetar"
      external$archived_retter$retter[[1]] <- "Ekstern gemt"
      external$links$link[[1]] <- "https://example.com/ekstern"
      harness$disk_snapshot <- external
      reads_before_changed <- length(harness$read_calls)

      session$elapse(1001L)
      session$flushReact()
      refresh_log <- harness$log_events[[1L]]
      stopifnot(
        length(harness$read_calls) == reads_before_changed + 1L,
        identical(state_api$read$snapshot(), external),
        identical(state_api$read$recipes(), external$recipes),
        identical(state_api$read$links(), external$links),
        identical(
          state_api$read$active_retter(),
          external$active_retter
        ),
        identical(
          state_api$read$archived_retter(),
          external$archived_retter
        ),
        identical(state_api$read$revision(), external$revision),
        length(harness$log_events) == 1L,
        identical(refresh_log$level, "INFO"),
        identical(refresh_log$event, "poll_refreshed"),
        identical(refresh_log$component, "recipe_catalog_state"),
        identical(refresh_log$fields$recipe_count, 2L),
        is.numeric(refresh_log$fields$duration_ms),
        refresh_log$fields$duration_ms >= 0,
        identical(refresh_log$fields$stage, "publish"),
        identical(refresh_log$fields$outcome, "refreshed"),
        is.null(refresh_log$error)
      )
    }
  )
})

local({
  initial <- recipe_catalog_state_fixture()
  harness <- recipe_catalog_state_harness(initial)

  shiny::testServer(
    recipe_catalog_state_test_server,
    args = list(harness = harness, poll_interval_ms = 1000L),
    {
      session$flushReact()
      external <- recipe_catalog_state_fixture(
        revision = "retry-revision"
      )
      external$recipes$burger_opskr$maengde[[1]] <- 1.25
      harness$disk_snapshot <- external

      reads_before_errors <- length(harness$read_calls)
      harness$fail_revision_once <- TRUE
      session$elapse(1001L)
      session$flushReact()
      stopifnot(
        length(harness$read_calls) == reads_before_errors,
        identical(state_api$read$snapshot(), initial),
        length(harness$log_events) == 1L,
        identical(harness$log_events[[1L]]$level, "WARN"),
        identical(harness$log_events[[1L]]$event, "poll_failed"),
        identical(
          harness$log_events[[1L]]$component,
          "recipe_catalog_state"
        ),
        identical(
          harness$log_events[[1L]]$fields$stage,
          "revision"
        ),
        identical(
          harness$log_events[[1L]]$fields$outcome,
          "failed"
        ),
        inherits(harness$log_events[[1L]]$error, "error")
      )

      harness$fail_read_once <- TRUE
      session$elapse(1001L)
      session$flushReact()
      stopifnot(
        length(harness$read_calls) == reads_before_errors + 1L,
        identical(state_api$read$snapshot(), initial),
        length(harness$log_events) == 1L
      )

      session$elapse(1001L)
      session$flushReact()
      stopifnot(
        length(harness$read_calls) == reads_before_errors + 2L,
        identical(state_api$read$snapshot(), external),
        identical(state_api$read$revision(), "retry-revision"),
        length(harness$log_events) == 3L,
        identical(
          harness$log_events[[2L]]$event,
          "poll_recovered"
        ),
        identical(
          harness$log_events[[2L]]$fields$outcome,
          "recovered"
        ),
        is.null(harness$log_events[[2L]]$error),
        identical(
          harness$log_events[[3L]]$event,
          "poll_refreshed"
        )
      )
    }
  )
})

local({
  initial <- recipe_catalog_state_fixture()
  harness <- recipe_catalog_state_harness(initial)
  harness$fail_logger <- TRUE

  shiny::testServer(
    recipe_catalog_state_test_server,
    args = list(harness = harness),
    {
      candidate <- state_api$read$snapshot()
      candidate$links$link[[1L]] <- "https://example.com/logger-test"

      stopifnot(
        isTRUE(state_api$commit(candidate)),
        identical(
          state_api$read$snapshot()$links,
          candidate$links
        ),
        length(harness$log_events) == 0L
      )

      failed_candidate <- state_api$read$snapshot()
      failed_candidate$links$link[[1L]] <-
        "https://example.com/logger-fejl"
      harness$fail_commit_once <- TRUE
      stopifnot(
        identical(state_api$commit(failed_candidate), FALSE),
        identical(
          state_api$read$snapshot()$links,
          candidate$links
        ),
        length(harness$notifications) == 1L,
        length(harness$log_events) == 0L
      )
    }
  )
})

message(paste(
  "Opskriftskatalogets state læser og publicerer komplette snapshots,",
  "gemmer før publicering, sender kun ændrede dele til lageret og",
  "genprøver sikre polling-fejl uden at ændre den kanoniske state."
))
