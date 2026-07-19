suppressPackageStartupMessages(source("app.R", encoding = "UTF-8"))

initialize_recipe_test_inputs <- function(session) {
  session$setInputs(
    ret = "",
    pers = 2,
    salat = "",
    tilbehor = "",
    basis_varer = "agurk",
    basis_varer_manuel = "agurk",
    menu_type = "Alle",
    date_from = Sys.Date() - 30,
    date_to = Sys.Date(),
    top_n = 5,
    opskrift_valgt_key = "burger_opskr"
  )
}

run_recipe_store_integration_tests <- function() {
  original_commit <- recipe_store_commit
  on.exit(
    assign("recipe_store_commit", original_commit, envir = .GlobalEnv),
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

    active_before <- rv_retter_custom()
    recipes_before <- rv_opskrifter_custom()
    stopifnot(
      "burger_opskr" %in% active_before$key,
      "burger_opskr" %in% names(recipes_before)
    )

    session$setInputs(
      opskrift_archivePressed = list(key = "burger_opskr", nonce = 1L)
    )
    session$setInputs(confirm_delete_ret = 1L)

    stopifnot(
      !"burger_opskr" %in% rv_retter_custom()$key,
      "burger_opskr" %in% rv_retter_arkiv()$key,
      "burger_opskr" %in% names(rv_opskrifter_custom()),
      length(commit_calls) == 1L,
      !is.null(commit_calls[[1]]$active_retter),
      !is.null(commit_calls[[1]]$archived_retter),
      is.null(commit_calls[[1]]$delete_recipe_keys)
    )

    session$setInputs(restore_ret = "burger_opskr")
    stopifnot(
      "burger_opskr" %in% rv_retter_custom()$key,
      !"burger_opskr" %in% rv_retter_arkiv()$key,
      length(commit_calls) == 2L
    )

    rv_links_custom(dplyr::bind_rows(
      rv_links_custom(),
      data.frame(
        ret = "Burger",
        link = "https://example.com/burger",
        stringsAsFactors = FALSE
      )
    ))

    session$setInputs(
      opskrift_archivePressed = list(key = "burger_opskr", nonce = 2L)
    )
    session$setInputs(confirm_delete_ret = 2L)
    session$setInputs(delete_archived_ret = "burger_opskr")
    session$setInputs(confirm_delete_archived_ret = 1L)

    purge_call <- commit_calls[[length(commit_calls)]]
    stopifnot(
      length(commit_calls) == 4L,
      identical(purge_call$delete_recipe_keys, "burger_opskr"),
      !any(purge_call$links$ret == "Burger"),
      !"burger_opskr" %in% rv_retter_arkiv()$key,
      !"burger_opskr" %in% names(rv_opskrifter_custom()),
      !any(rv_links_custom()$ret == "Burger")
    )
  })

  calls_before_failure <- length(commit_calls)
  shiny::testServer(server, {
    initialize_recipe_test_inputs(session)

    active_before <- rv_retter_custom()
    archive_before <- rv_retter_arkiv()
    revision_before <- rv_recipeStoreRevision()

    session$setInputs(
      opskrift_archivePressed = list(key = "burger_opskr", nonce = 3L)
    )
    fail_next_commit <<- TRUE
    session$setInputs(confirm_delete_ret = 1L)

    stopifnot(
      identical(rv_retter_custom(), active_before),
      identical(rv_retter_arkiv(), archive_before),
      identical(rv_recipeStoreRevision(), revision_before),
      identical(rv_recipeArchiveState$key, "burger_opskr"),
      length(commit_calls) == calls_before_failure
    )
  })
}

run_recipe_store_integration_tests()
message("Recipe-store integrationen publicerer kun reaktiv state efter en vellykket commit.")
