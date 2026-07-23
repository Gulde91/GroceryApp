suppressPackageStartupMessages(source("app.R", encoding = "UTF-8"))

recipe_ns <- shiny::NS("opskrifter")

set_recipe_inputs <- function(session, ...) {
  values <- list(...)
  names(values) <- recipe_ns(names(values))
  do.call(session$setInputs, values)
}

initialize_recipe_test_inputs <- function(session) {
  set_recipe_inputs(
    session,
    opskrift_valgt_key = "burger_opskr"
  )
}

run_recipe_store_integration_tests <- function() {
  original_commit <- recipe_store_commit
  original_read <- recipe_store_read
  original_revision <- recipe_store_revision
  on.exit(
    {
      assign("recipe_store_commit", original_commit, envir = .GlobalEnv)
      assign("recipe_store_read", original_read, envir = .GlobalEnv)
      assign(
        "recipe_store_revision",
        original_revision,
        envir = .GlobalEnv
      )
    },
    add = TRUE
  )

  commit_calls <- list()
  fail_next_commit <- FALSE

  commit_stub <- function(...) {
    args <- list(...)

    if (fail_next_commit) {
      fail_next_commit <<- FALSE
      stop("Fremprovokeret commitfejl.", call. = FALSE)
    }

    commit_calls[[length(commit_calls) + 1L]] <<- args
    paste0("stub-revision-", length(commit_calls))
  }
  assign("recipe_store_commit", commit_stub, envir = .GlobalEnv)

  shiny::testServer(server, {
    initialize_recipe_test_inputs(session)

    catalog_before <- recipe_state$read$snapshot()
    stopifnot(
      "burger_opskr" %in% catalog_before$active_retter$key,
      "burger_opskr" %in% names(catalog_before$recipes)
    )

    set_recipe_inputs(
      session,
      opskrift_archivePressed = list(key = "burger_opskr", nonce = 1L)
    )
    set_recipe_inputs(session, confirm_delete_ret = 1L)

    catalog_archived <- recipe_state$read$snapshot()
    stopifnot(
      !"burger_opskr" %in% catalog_archived$active_retter$key,
      "burger_opskr" %in% catalog_archived$archived_retter$key,
      "burger_opskr" %in% names(catalog_archived$recipes),
      identical(catalog_archived$revision, "stub-revision-1"),
      length(commit_calls) == 1L,
      identical(
        commit_calls[[1]]$expected_revision,
        catalog_before$revision
      ),
      !is.null(commit_calls[[1]]$active_retter),
      !is.null(commit_calls[[1]]$archived_retter),
      is.null(commit_calls[[1]]$links),
      length(commit_calls[[1]]$recipes) == 0L,
      length(commit_calls[[1]]$delete_recipe_keys) == 0L
    )

    set_recipe_inputs(session, restore_ret = "burger_opskr")
    catalog_restored <- recipe_state$read$snapshot()
    stopifnot(
      "burger_opskr" %in% catalog_restored$active_retter$key,
      !"burger_opskr" %in% catalog_restored$archived_retter$key,
      identical(catalog_restored$revision, "stub-revision-2"),
      length(commit_calls) == 2L
    )

    catalog_with_link <- recipe_state$read$snapshot()
    catalog_with_link$links <- dplyr::bind_rows(
      catalog_with_link$links,
      data.frame(
        ret = "Burger",
        link = "https://example.com/burger",
        stringsAsFactors = FALSE
      )
    )
    stopifnot(recipe_state$commit(catalog_with_link))

    set_recipe_inputs(
      session,
      opskrift_archivePressed = list(key = "burger_opskr", nonce = 2L)
    )
    set_recipe_inputs(session, confirm_delete_ret = 2L)
    set_recipe_inputs(session, delete_archived_ret = "burger_opskr")
    set_recipe_inputs(session, confirm_delete_archived_ret = 1L)

    purge_call <- commit_calls[[length(commit_calls)]]
    catalog_purged <- recipe_state$read$snapshot()
    stopifnot(
      length(commit_calls) == 5L,
      identical(purge_call$delete_recipe_keys, "burger_opskr"),
      !is.null(purge_call$archived_retter),
      !is.null(purge_call$links),
      is.null(purge_call$active_retter),
      length(purge_call$recipes) == 0L,
      identical(
        purge_call$archived_retter,
        catalog_purged$archived_retter
      ),
      identical(purge_call$links, catalog_purged$links),
      !any(purge_call$links$ret == "Burger"),
      !"burger_opskr" %in% catalog_purged$archived_retter$key,
      !"burger_opskr" %in% names(catalog_purged$recipes),
      !any(catalog_purged$links$ret == "Burger"),
      identical(catalog_purged$revision, "stub-revision-5")
    )
  })

  calls_before_failure <- length(commit_calls)
  shiny::testServer(server, {
    initialize_recipe_test_inputs(session)

    catalog_before <- recipe_state$read$snapshot()

    set_recipe_inputs(
      session,
      opskrift_archivePressed = list(key = "burger_opskr", nonce = 3L)
    )
    fail_next_commit <<- TRUE
    set_recipe_inputs(session, confirm_delete_ret = 1L)

    stopifnot(
      identical(recipe_state$read$snapshot(), catalog_before),
      length(commit_calls) == calls_before_failure
    )

    set_recipe_inputs(session, confirm_delete_ret = 2L)

    catalog_after_retry <- recipe_state$read$snapshot()
    stopifnot(
      !"burger_opskr" %in% catalog_after_retry$active_retter$key,
      "burger_opskr" %in% catalog_after_retry$archived_retter$key,
      length(commit_calls) == calls_before_failure + 1L
    )
  })

  calls_before_stale_modal <- length(commit_calls)
  shiny::testServer(server, {
    initialize_recipe_test_inputs(session)

    set_recipe_inputs(
      session,
      opskrift_editPressed = list(key = "burger_opskr", row = 1L)
    )
    opened_edit_revision <- recipe_state$read$revision()

    refreshed_catalog <- recipe_state$read$snapshot()
    refreshed_catalog$recipes[["burger_opskr"]] <-
      refreshed_catalog$recipes[["burger_opskr"]][
        c(2L, 1L, seq.int(3L, nrow(
          refreshed_catalog$recipes[["burger_opskr"]]
        ))),
        ,
        drop = FALSE
      ]
    stopifnot(recipe_state$commit(refreshed_catalog))
    refreshed_catalog <- recipe_state$read$snapshot()

    set_recipe_inputs(
      session,
      opskrift_edit_maengde = 1,
      opskrift_edit_enhed = "kg",
      opskrift_edit_kat1 = "kød",
      opskrift_edit_kat2 = ""
    )
    set_recipe_inputs(session, save_opskrift_row = 1L)

    stopifnot(
      !identical(opened_edit_revision, recipe_state$read$revision()),
      identical(recipe_state$read$snapshot(), refreshed_catalog),
      length(commit_calls) == calls_before_stale_modal + 1L
    )

    set_recipe_inputs(
      session,
      opskrift_deletePressed = list(key = "burger_opskr", row = 1L)
    )
    opened_delete_revision <- recipe_state$read$revision()

    refreshed_again <- recipe_state$read$snapshot()
    refreshed_again$recipes[["burger_opskr"]] <-
      refreshed_again$recipes[["burger_opskr"]][
        rev(seq_len(nrow(refreshed_again$recipes[["burger_opskr"]]))),
        ,
        drop = FALSE
      ]
    stopifnot(recipe_state$commit(refreshed_again))
    refreshed_again <- recipe_state$read$snapshot()
    set_recipe_inputs(session, confirm_delete_opskrift_row = 1L)

    stopifnot(
      !identical(opened_delete_revision, recipe_state$read$revision()),
      identical(recipe_state$read$snapshot(), refreshed_again),
      length(commit_calls) == calls_before_stale_modal + 2L
    )
  })

  calls_before_rejected_candidates <- length(commit_calls)
  shiny::testServer(server, {
    initialize_recipe_test_inputs(session)

    catalog_before <- recipe_state$read$snapshot()
    catalog_without_recipe <- catalog_before
    catalog_without_recipe$recipes[["burger_opskr"]] <- NULL

    stopifnot(
      identical(
        recipe_state$commit(catalog_without_recipe),
        FALSE
      ),
      identical(recipe_state$read$snapshot(), catalog_before),
      length(commit_calls) == calls_before_rejected_candidates
    )

    stale_catalog <- catalog_before
    stale_catalog$revision <- "stale-revision"

    stopifnot(
      identical(recipe_state$commit(stale_catalog), FALSE),
      identical(recipe_state$read$snapshot(), catalog_before),
      length(commit_calls) == calls_before_rejected_candidates
    )
  })

  calls_before_recipe_update <- length(commit_calls)
  shiny::testServer(server, {
    initialize_recipe_test_inputs(session)

    next_catalog <- recipe_state$read$snapshot()
    next_catalog$recipes[["burger_opskr"]]$maengde[[1]] <-
      next_catalog$recipes[["burger_opskr"]]$maengde[[1]] + 0.01

    stopifnot(recipe_state$commit(next_catalog))

    update_call <- commit_calls[[length(commit_calls)]]
    published_catalog <- recipe_state$read$snapshot()
    stopifnot(
      length(commit_calls) == calls_before_recipe_update + 1L,
      identical(names(update_call$recipes), "burger_opskr"),
      is.null(update_call$active_retter),
      is.null(update_call$archived_retter),
      is.null(update_call$links),
      length(update_call$delete_recipe_keys) == 0L,
      identical(
        published_catalog$recipes[["burger_opskr"]],
        next_catalog$recipes[["burger_opskr"]]
      ),
      identical(
        published_catalog$revision,
        paste0("stub-revision-", calls_before_recipe_update + 1L)
      )
    )
  })

  calls_before_ingredient_update <- length(commit_calls)
  shiny::testServer(server, {
    initialize_recipe_test_inputs(session)

    key <- "burger_opskr"
    row <- 1L
    catalog_before <- recipe_state$read$snapshot()
    recipe_before <- catalog_before$recipes[[key]]
    updated_amount <- recipe_before$maengde[[row]] + 0.5

    set_recipe_inputs(
      session,
      opskrift_editPressed = list(key = key, row = row)
    )
    set_recipe_inputs(
      session,
      opskrift_edit_maengde = updated_amount,
      opskrift_edit_enhed = "kg",
      opskrift_edit_kat1 = "konserves",
      opskrift_edit_kat2 = ""
    )
    set_recipe_inputs(session, save_opskrift_row = 1L)

    catalog_after <- recipe_state$read$snapshot()
    recipe_after <- catalog_after$recipes[[key]]
    update_call <- commit_calls[[calls_before_ingredient_update + 1L]]
    stopifnot(
      length(commit_calls) == calls_before_ingredient_update + 1L,
      identical(recipe_after$maengde[[row]], updated_amount),
      identical(recipe_after$enhed[[row]], "kg"),
      identical(recipe_after$kat_1[[row]], "konserves"),
      identical(recipe_after$kat_2[[row]], ""),
      identical(catalog_after$active_retter, catalog_before$active_retter),
      identical(
        catalog_after$archived_retter,
        catalog_before$archived_retter
      ),
      identical(catalog_after$links, catalog_before$links),
      identical(names(update_call$recipes), key),
      identical(update_call$recipes[[key]], recipe_after),
      is.null(update_call$active_retter),
      is.null(update_call$archived_retter),
      is.null(update_call$links),
      length(update_call$delete_recipe_keys) == 0L,
      identical(update_call$expected_revision, catalog_before$revision)
    )
  })

  calls_before_ingredient_add <- length(commit_calls)
  shiny::testServer(server, {
    initialize_recipe_test_inputs(session)

    key <- "burger_opskr"
    ingredient_name <- "integrationstestvare"
    catalog_before <- recipe_state$read$snapshot()
    recipe_before <- catalog_before$recipes[[key]]

    set_recipe_inputs(
      session,
      opskrift_addPressed = list(key = key, nonce = 1L)
    )
    set_recipe_inputs(
      session,
      opskrift_add_navn = ingredient_name,
      opskrift_add_maengde = 2.5,
      opskrift_add_enhed = "stk",
      opskrift_add_kat1 = "konserves",
      opskrift_add_kat2 = ""
    )
    set_recipe_inputs(session, save_opskrift_new_row = 1L)

    catalog_after <- recipe_state$read$snapshot()
    recipe_after <- catalog_after$recipes[[key]]
    added_row <- nrow(recipe_after)
    add_call <- commit_calls[[calls_before_ingredient_add + 1L]]
    stopifnot(
      length(commit_calls) == calls_before_ingredient_add + 1L,
      nrow(recipe_after) == nrow(recipe_before) + 1L,
      identical(as.character(recipe_after[[1]][[added_row]]), ingredient_name),
      identical(recipe_after$maengde[[added_row]], 2.5),
      identical(recipe_after$enhed[[added_row]], "stk"),
      identical(recipe_after$kat_1[[added_row]], "konserves"),
      identical(recipe_after$kat_2[[added_row]], ""),
      identical(catalog_after$active_retter, catalog_before$active_retter),
      identical(
        catalog_after$archived_retter,
        catalog_before$archived_retter
      ),
      identical(catalog_after$links, catalog_before$links),
      identical(names(add_call$recipes), key),
      identical(add_call$recipes[[key]], recipe_after),
      is.null(add_call$active_retter),
      is.null(add_call$archived_retter),
      is.null(add_call$links),
      length(add_call$delete_recipe_keys) == 0L,
      identical(add_call$expected_revision, catalog_before$revision)
    )
  })

  calls_before_ingredient_delete <- length(commit_calls)
  shiny::testServer(server, {
    initialize_recipe_test_inputs(session)

    key <- "burger_opskr"
    row <- 2L
    catalog_before <- recipe_state$read$snapshot()
    recipe_before <- catalog_before$recipes[[key]]
    expected_recipe <- recipe_before[-row, , drop = FALSE]
    rownames(expected_recipe) <- NULL

    set_recipe_inputs(
      session,
      opskrift_deletePressed = list(key = key, row = row)
    )
    set_recipe_inputs(session, confirm_delete_opskrift_row = 1L)

    catalog_after <- recipe_state$read$snapshot()
    recipe_after <- catalog_after$recipes[[key]]
    delete_call <- commit_calls[[calls_before_ingredient_delete + 1L]]
    stopifnot(
      length(commit_calls) == calls_before_ingredient_delete + 1L,
      identical(recipe_after, expected_recipe),
      identical(catalog_after$active_retter, catalog_before$active_retter),
      identical(
        catalog_after$archived_retter,
        catalog_before$archived_retter
      ),
      identical(catalog_after$links, catalog_before$links),
      identical(names(delete_call$recipes), key),
      identical(delete_call$recipes[[key]], recipe_after),
      is.null(delete_call$active_retter),
      is.null(delete_call$archived_retter),
      is.null(delete_call$links),
      length(delete_call$delete_recipe_keys) == 0L,
      identical(delete_call$expected_revision, catalog_before$revision)
    )
  })

  calls_before_create <- length(commit_calls)
  shiny::testServer(server, {
    initialize_recipe_test_inputs(session)

    set_recipe_inputs(
      session,
      ny_ret_navn = "Kanonisk testret",
      ny_ret_type = "vegetar",
      ny_ret_link = "www.example.com"
    )
    set_recipe_inputs(session, save_ny_ret = 1L)

    created_catalog <- recipe_state$read$snapshot()
    create_call <- commit_calls[[length(commit_calls)]]
    stopifnot(
      length(commit_calls) == calls_before_create + 1L,
      "kanonisk_testret_opskr" %in% created_catalog$active_retter$key,
      "kanonisk_testret_opskr" %in% names(created_catalog$recipes),
      any(
        created_catalog$links$ret == "Kanonisk testret" &
          created_catalog$links$link == "https://www.example.com"
      ),
      identical(names(create_call$recipes), "kanonisk_testret_opskr"),
      !is.null(create_call$active_retter),
      !is.null(create_call$links),
      is.null(create_call$archived_retter),
      length(create_call$delete_recipe_keys) == 0L
    )
  })

  calls_before_ghost_purge <- length(commit_calls)
  shiny::testServer(server, {
    initialize_recipe_test_inputs(session)

    catalog_with_ghost <- recipe_state$read$snapshot()
    catalog_with_ghost$archived_retter <- dplyr::bind_rows(
      catalog_with_ghost$archived_retter,
      data.frame(
        retter = "Manglende opskrift",
        key = "manglende_opskr",
        type = "vegetar",
        stringsAsFactors = FALSE
      )
    )
    stopifnot(recipe_state$commit(catalog_with_ghost))

    set_recipe_inputs(session, delete_archived_ret = "manglende_opskr")
    set_recipe_inputs(session, confirm_delete_archived_ret = 1L)

    purged_catalog <- recipe_state$read$snapshot()
    purge_call <- commit_calls[[length(commit_calls)]]
    stopifnot(
      length(commit_calls) == calls_before_ghost_purge + 2L,
      identical(purge_call$delete_recipe_keys, "manglende_opskr"),
      !is.null(purge_call$archived_retter),
      is.null(purge_call$active_retter),
      is.null(purge_call$links),
      length(purge_call$recipes) == 0L,
      identical(
        purge_call$archived_retter,
        purged_catalog$archived_retter
      ),
      !"manglende_opskr" %in% purged_catalog$archived_retter$key,
      !"manglende_opskr" %in% names(purged_catalog$recipes)
    )
  })

  base_poll_snapshot <- original_read("./data")
  polled_snapshot <- base_poll_snapshot
  polled_revision <- base_poll_snapshot$revision
  poll_read_calls <- 0L

  assign(
    "recipe_store_revision",
    function(...) polled_revision,
    envir = .GlobalEnv
  )
  assign(
    "recipe_store_read",
    function(...) {
      poll_read_calls <<- poll_read_calls + 1L
      polled_snapshot
    },
    envir = .GlobalEnv
  )

  shiny::testServer(server, {
    initialize_recipe_test_inputs(session)

    external_snapshot <- recipe_state$read$snapshot()
    external_snapshot$recipes[["burger_opskr"]]$maengde[[1]] <- 0.321
    external_snapshot$links <- dplyr::bind_rows(
      external_snapshot$links,
      data.frame(
        ret = "Polling test",
        link = "https://example.com/polling",
        stringsAsFactors = FALSE
      )
    )
    external_snapshot$revision <- "external-poll-revision"

    polled_snapshot <<- external_snapshot
    polled_revision <<- external_snapshot$revision
    reads_before_refresh <- poll_read_calls

    session$elapse(2001)
    session$flushReact()

    stopifnot(
      poll_read_calls == reads_before_refresh + 1L,
      identical(recipe_state$read$snapshot(), external_snapshot),
      identical(recipe_state$read$recipes(), external_snapshot$recipes),
      identical(recipe_state$read$links(), external_snapshot$links),
      identical(
        recipe_state$read$revision(),
        external_snapshot$revision
      )
    )
  })
}

run_recipe_store_integration_tests()
message("Recipe-store integrationen publicerer kun ét komplet katalog efter en vellykket commit.")
