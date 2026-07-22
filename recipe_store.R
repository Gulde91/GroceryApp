# Holdbar lagring af opskriftskataloget -------------------------------------
#
# Funktionerne i denne fil er bevidst uafhængige af Shiny. En commit skriver
# først til stage-filer, flytter eksisterende filer til reversible backups og
# publicerer derefter de nye filer. Ved en almindelig R- eller filsystemfejl
# gendannes det oprindelige snapshot.

.recipe_store_metadata_paths <- function(data_dir) {
  c(
    retter = file.path(data_dir, "retter.txt"),
    retter_arkiv = file.path(data_dir, "retter_arkiv.txt"),
    links = file.path(data_dir, "links.txt")
  )
}

.recipe_store_recipe_dir <- function(data_dir) {
  file.path(data_dir, "opskrifter")
}

.recipe_store_recipe_path <- function(data_dir, key) {
  file.path(.recipe_store_recipe_dir(data_dir), paste0(key, ".txt"))
}

.recipe_store_validate_key <- function(key, label = "Opskriftsnøgle") {
  if (
    length(key) != 1L ||
      is.na(key) ||
      !nzchar(key) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", key)
  ) {
    stop(
      sprintf("%s er ugyldig: %s", label, paste(key, collapse = ", ")),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.recipe_store_validate_delimited_values <- function(df, label) {
  bad_header <- grepl("[;\r\n]", names(df))
  if (any(bad_header)) {
    stop(
      sprintf("%s har et kolonnenavn med semikolon eller linjeskift.", label),
      call. = FALSE
    )
  }

  for (column_name in names(df)) {
    values <- as.character(df[[column_name]])
    bad_value <- !is.na(values) & grepl("[;\r\n]", values)

    if (any(bad_value)) {
      stop(
        sprintf(
          "%s indeholder semikolon eller linjeskift i kolonnen '%s'.",
          label,
          column_name
        ),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

.recipe_store_validate_table <- function(df, expected_names, label) {
  if (!is.data.frame(df)) {
    stop(sprintf("%s skal være en data frame.", label), call. = FALSE)
  }

  if (!identical(names(df), expected_names)) {
    stop(
      sprintf(
        "%s skal have kolonnerne: %s.",
        label,
        paste(expected_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  .recipe_store_validate_delimited_values(df, label)
  invisible(TRUE)
}

.recipe_store_validate_recipe <- function(df, key) {
  label <- sprintf("Opskriften '%s'", key)

  if (!is.data.frame(df)) {
    stop(sprintf("%s skal være en data frame.", label), call. = FALSE)
  }

  if (
    ncol(df) != 5L ||
      !identical(names(df)[2:5], c("maengde", "enhed", "kat_1", "kat_2"))
  ) {
    stop(
      sprintf(
        "%s skal have retten som første kolonne og derefter maengde, enhed, kat_1 og kat_2.",
        label
      ),
      call. = FALSE
    )
  }

  if (!is.numeric(df$maengde)) {
    stop(sprintf("%s skal have en numerisk maengde-kolonne.", label), call. = FALSE)
  }

  .recipe_store_validate_delimited_values(df, label)
  invisible(TRUE)
}

.recipe_store_validate_catalog_tables <- function(
  active_retter,
  archived_retter,
  links
) {
  if (!is.null(active_retter)) {
    .recipe_store_validate_table(
      active_retter,
      c("retter", "key", "type"),
      "Aktive retter"
    )

    if (anyDuplicated(tolower(active_retter$key))) {
      stop("Aktive retter indeholder dublerede nøgler.", call. = FALSE)
    }
  }

  if (!is.null(archived_retter)) {
    .recipe_store_validate_table(
      archived_retter,
      c("retter", "key", "type"),
      "Arkiverede retter"
    )

    if (anyDuplicated(tolower(archived_retter$key))) {
      stop("Arkiverede retter indeholder dublerede nøgler.", call. = FALSE)
    }
  }

  if (!is.null(active_retter) && !is.null(archived_retter)) {
    overlap <- intersect(
      tolower(active_retter$key),
      tolower(archived_retter$key)
    )
    if (length(overlap) > 0L) {
      stop(
        sprintf(
          "Følgende nøgler er både aktive og arkiverede: %s",
          paste(overlap, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  if (!is.null(links)) {
    .recipe_store_validate_table(links, c("ret", "link"), "Opskriftslinks")
  }

  invisible(TRUE)
}

.recipe_store_write_table <- function(df, path) {
  write.table(
    df,
    file = path,
    sep = ";",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )

  header <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  expected_header <- paste(names(df), collapse = ";")

  if (length(header) != 1L || !identical(header, expected_header)) {
    stop(
      sprintf("Stage-filen '%s' kunne ikke valideres.", basename(path)),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.recipe_store_move <- function(from, to, description) {
  if (!file.rename(from, to)) {
    stop(sprintf("Kunne ikke %s.", description), call. = FALSE)
  }

  invisible(TRUE)
}

.recipe_store_checkpoint <- function(fail_at, step, crash_at = NULL) {
  if (!is.null(crash_at) && step %in% crash_at) {
    condition <- structure(
      list(
        message = sprintf(
          "Simuleret processtop ved recipe-store-trinnet '%s'.",
          step
        ),
        call = NULL
      ),
      class = c("recipe_store_simulated_crash", "error", "condition")
    )
    stop(condition)
  }

  if (!is.null(fail_at) && step %in% fail_at) {
    stop(sprintf("Testfejl ved recipe-store-trinnet '%s'.", step), call. = FALSE)
  }

  invisible(TRUE)
}

.recipe_store_release_lock <- function(lock_path) {
  if (dir.exists(lock_path)) {
    unlink(lock_path, recursive = TRUE, force = TRUE)
  }

  invisible(NULL)
}

.recipe_store_transaction_paths <- function(data_dir) {
  c(
    journal = file.path(data_dir, ".recipe-store-transaction.rds"),
    committed = file.path(data_dir, ".recipe-store-transaction.committed")
  )
}

.recipe_store_file_hash <- function(path) {
  if (!file.exists(path)) return("<missing>")
  unname(tools::md5sum(path))
}

.recipe_store_remove_files <- function(paths) {
  paths <- unique(paths[file.exists(paths)])
  if (length(paths) == 0L) return(character())

  removed <- file.remove(paths)
  basename(paths[!removed])
}

.recipe_store_write_journal <- function(data_dir, journal) {
  transaction_paths <- .recipe_store_transaction_paths(data_dir)
  journal_path <- unname(transaction_paths[["journal"]])
  staged_journal <- tempfile(
    pattern = ".recipe-store-journal-stage-",
    tmpdir = data_dir
  )

  saveRDS(journal, staged_journal, version = 2)
  if (!file.rename(staged_journal, journal_path)) {
    if (file.exists(staged_journal)) file.remove(staged_journal)
    stop("Transaktionsjournalen kunne ikke oprettes.", call. = FALSE)
  }

  invisible(journal_path)
}

.recipe_store_mark_committed <- function(data_dir) {
  transaction_paths <- .recipe_store_transaction_paths(data_dir)
  marker_path <- unname(transaction_paths[["committed"]])
  staged_marker <- tempfile(
    pattern = ".recipe-store-commit-stage-",
    tmpdir = data_dir
  )

  writeLines("committed", staged_marker, useBytes = TRUE)
  if (!file.rename(staged_marker, marker_path)) {
    if (file.exists(staged_marker)) file.remove(staged_marker)
    stop("Commit-markøren kunne ikke oprettes.", call. = FALSE)
  }

  invisible(marker_path)
}

.recipe_store_rollback_journal <- function(journal) {
  errors <- character()

  if (length(journal$target_paths) > 0L) {
    for (i in rev(seq_along(journal$target_paths))) {
      target <- journal$target_paths[[i]]
      backup <- journal$backup_paths[[i]]

      if (file.exists(backup)) {
        if (file.exists(target) && !file.remove(target)) {
          errors <- c(
            errors,
            sprintf("kunne ikke fjerne ny fil '%s'", basename(target))
          )
          next
        }

        if (!file.rename(backup, target)) {
          errors <- c(
            errors,
            sprintf("kunne ikke gendanne '%s'", basename(target))
          )
        }
      } else if (isFALSE(journal$had_original[[i]]) && file.exists(target)) {
        if (!file.remove(target)) {
          errors <- c(
            errors,
            sprintf("kunne ikke fjerne ny fil '%s'", basename(target))
          )
        }
      } else if (isTRUE(journal$had_original[[i]])) {
        if (
          !file.exists(target) ||
            !identical(
              .recipe_store_file_hash(target),
              journal$original_hashes[[i]]
            )
        ) {
          errors <- c(
            errors,
            sprintf("den oprindelige fil '%s' kan ikke identificeres", basename(target))
          )
        }
      }
    }
  }

  if (length(journal$delete_targets) > 0L) {
    for (i in rev(seq_along(journal$delete_targets))) {
      target <- journal$delete_targets[[i]]
      quarantine <- journal$quarantine_paths[[i]]

      if (file.exists(quarantine)) {
        if (file.exists(target)) {
          errors <- c(
            errors,
            sprintf("opskriften '%s' findes allerede", basename(target))
          )
        } else if (!file.rename(quarantine, target)) {
          errors <- c(
            errors,
            sprintf("kunne ikke gendanne '%s'", basename(target))
          )
        }
      } else if (isTRUE(journal$delete_existed[[i]])) {
        if (
          !file.exists(target) ||
            !identical(
              .recipe_store_file_hash(target),
              journal$delete_hashes[[i]]
            )
        ) {
          errors <- c(
            errors,
            sprintf("den slettede fil '%s' kan ikke identificeres", basename(target))
          )
        }
      }
    }
  }

  if (length(errors) == 0L) {
    failed_cleanup <- .recipe_store_remove_files(journal$stage_paths)
    if (length(failed_cleanup) > 0L) {
      errors <- c(
        errors,
        sprintf(
          "kunne ikke fjerne stage-filer: %s",
          paste(failed_cleanup, collapse = ", ")
        )
      )
    }
  }

  errors
}

.recipe_store_recover_locked <- function(data_dir) {
  transaction_paths <- .recipe_store_transaction_paths(data_dir)
  journal_path <- unname(transaction_paths[["journal"]])
  marker_path <- unname(transaction_paths[["committed"]])

  if (!file.exists(journal_path)) {
    if (file.exists(marker_path)) file.remove(marker_path)
    return(TRUE)
  }

  journal <- tryCatch(
    readRDS(journal_path),
    error = function(error) {
      stop(
        paste("Transaktionsjournalen kan ikke læses:", conditionMessage(error)),
        call. = FALSE
      )
    }
  )

  if (file.exists(marker_path)) {
    leftovers <- c(
      journal$stage_paths,
      journal$backup_paths,
      journal$quarantine_paths
    )
    failed_cleanup <- .recipe_store_remove_files(leftovers)
    if (length(failed_cleanup) > 0L) return(FALSE)

    if (!file.remove(journal_path)) return(FALSE)
    if (file.exists(marker_path) && !file.remove(marker_path)) return(FALSE)
    return(TRUE)
  }

  rollback_errors <- .recipe_store_rollback_journal(journal)
  if (length(rollback_errors) > 0L) {
    stop(
      paste(
        "En afbrudt opskriftstransaktion kunne ikke gendannes:",
        paste(rollback_errors, collapse = "; ")
      ),
      call. = FALSE
    )
  }

  if (!file.remove(journal_path)) {
    stop("Den gendannede transaktionsjournal kunne ikke fjernes.", call. = FALSE)
  }
  TRUE
}

.recipe_store_acquire_lock <- function(
  data_dir,
  wait_seconds = 1,
  stale_after_seconds = 30
) {
  lock_path <- file.path(data_dir, ".recipe-store-lock")
  deadline <- Sys.time() + wait_seconds

  repeat {
    if (dir.create(lock_path, showWarnings = FALSE)) {
      return(lock_path)
    }

    lock_info <- file.info(lock_path)
    lock_age <- as.numeric(
      difftime(Sys.time(), lock_info$mtime, units = "secs")
    )
    if (
      !is.na(lock_age) &&
        lock_age >= stale_after_seconds
    ) {
      unlink(lock_path, recursive = TRUE, force = TRUE)
      next
    }

    if (Sys.time() >= deadline) {
      stop(
        "Opskriftslageret er i brug af en anden handling. Prøv igen om et øjeblik.",
        call. = FALSE
      )
    }

    Sys.sleep(0.02)
  }
}

recipe_store_revision <- function(data_dir = "./data") {
  data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
  recipe_dir <- .recipe_store_recipe_dir(data_dir)
  metadata_paths <- .recipe_store_metadata_paths(data_dir)

  recipe_paths <- if (dir.exists(recipe_dir)) {
    list.files(
      recipe_dir,
      pattern = "^[^.].*\\.txt$",
      full.names = TRUE
    )
  } else {
    character()
  }

  paths <- c(metadata_paths, recipe_paths)
  labels <- c(
    paste0("metadata/", basename(metadata_paths)),
    paste0("opskrifter/", basename(recipe_paths))
  )

  hashes <- vapply(
    paths,
    function(path) {
      if (!file.exists(path)) return("<missing>")
      unname(tools::md5sum(path))
    },
    character(1)
  )

  paste(paste(labels, hashes, sep = "="), collapse = "\n")
}

.recipe_store_empty_retter <- function() {
  data.frame(
    retter = character(),
    key = character(),
    type = character(),
    stringsAsFactors = FALSE
  )
}

.recipe_store_empty_links <- function() {
  data.frame(
    ret = character(),
    link = character(),
    stringsAsFactors = FALSE
  )
}

.recipe_store_read_retter <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0L) {
    return(.recipe_store_empty_retter())
  }

  result <- readr::read_delim(
    path,
    col_types = c("c", "c", "c"),
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE,
    show_col_types = FALSE
  )

  dplyr::arrange(result, retter)
}

.recipe_store_read_links <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0L) {
    return(.recipe_store_empty_links())
  }

  readr::read_delim(
    path,
    col_types = c("c", "c"),
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE,
    show_col_types = FALSE
  )
}

.recipe_store_read_recipes <- function(data_dir) {
  recipe_dir <- .recipe_store_recipe_dir(data_dir)
  if (!dir.exists(recipe_dir)) return(list())

  files <- list.files(
    recipe_dir,
    pattern = "^[^.].*\\.txt$",
    full.names = TRUE
  )

  recipes <- lapply(
    files,
    function(path) {
      recipe <- readr::read_delim(
        path,
        col_types = c("c", "d", "c", "c", "c"),
        delim = ";",
        escape_double = FALSE,
        trim_ws = TRUE,
        na = "",
        show_col_types = FALSE
      )

      recipe$enhed[is.na(recipe$enhed)] <- ""
      recipe$kat_2[is.na(recipe$kat_2)] <- ""
      recipe
    }
  )

  names(recipes) <- tools::file_path_sans_ext(basename(files))
  recipes
}

recipe_store_read <- function(data_dir = "./data") {
  data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
  lock_path <- .recipe_store_acquire_lock(data_dir)
  on.exit(.recipe_store_release_lock(lock_path), add = TRUE)
  .recipe_store_recover_locked(data_dir)

  metadata_paths <- .recipe_store_metadata_paths(data_dir)
  snapshot <- list(
    active_retter = .recipe_store_read_retter(
      unname(metadata_paths[["retter"]])
    ),
    archived_retter = .recipe_store_read_retter(
      unname(metadata_paths[["retter_arkiv"]])
    ),
    recipes = .recipe_store_read_recipes(data_dir),
    links = .recipe_store_read_links(
      unname(metadata_paths[["links"]])
    )
  )
  snapshot$revision <- recipe_store_revision(data_dir)
  snapshot
}

recipe_store_commit <- function(
  data_dir = "./data",
  active_retter = NULL,
  archived_retter = NULL,
  links = NULL,
  recipes = NULL,
  delete_recipe_keys = character(),
  expected_revision = NULL,
  .fail_at = NULL,
  .crash_at = NULL
) {
  data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
  recipe_dir <- .recipe_store_recipe_dir(data_dir)

  if (!dir.exists(recipe_dir) && !dir.create(recipe_dir, recursive = FALSE)) {
    stop("Mappen til opskriftsfiler kunne ikke oprettes.", call. = FALSE)
  }

  .recipe_store_validate_catalog_tables(
    active_retter,
    archived_retter,
    links
  )

  if (is.null(recipes)) recipes <- list()
  if (!is.list(recipes)) {
    stop("recipes skal være en navngivet liste.", call. = FALSE)
  }

  if (length(recipes) > 0L) {
    if (
      is.null(names(recipes)) ||
        any(!nzchar(names(recipes))) ||
        anyDuplicated(tolower(names(recipes)))
    ) {
      stop("recipes skal have unikke, ikke-tomme nøgler.", call. = FALSE)
    }

    for (key in names(recipes)) {
      .recipe_store_validate_key(key)
      .recipe_store_validate_recipe(recipes[[key]], key)
    }
  }

  delete_recipe_keys <- as.character(delete_recipe_keys)
  if (length(delete_recipe_keys) > 1L) {
    stop("Kun én opskriftsfil kan slettes pr. commit.", call. = FALSE)
  }

  if (length(delete_recipe_keys) == 1L) {
    .recipe_store_validate_key(delete_recipe_keys[[1]], "Nøglen der skal slettes")
  }

  overlap <- intersect(
    tolower(names(recipes)),
    tolower(delete_recipe_keys)
  )
  if (length(overlap) > 0L) {
    stop("En opskrift kan ikke både skrives og slettes i samme commit.", call. = FALSE)
  }

  metadata_paths <- .recipe_store_metadata_paths(data_dir)
  write_specs <- list()

  if (!is.null(active_retter)) {
    write_specs[[length(write_specs) + 1L]] <- list(
      target = unname(metadata_paths[["retter"]]),
      data = active_retter
    )
  }

  if (!is.null(archived_retter)) {
    write_specs[[length(write_specs) + 1L]] <- list(
      target = unname(metadata_paths[["retter_arkiv"]]),
      data = archived_retter
    )
  }

  if (!is.null(links)) {
    write_specs[[length(write_specs) + 1L]] <- list(
      target = unname(metadata_paths[["links"]]),
      data = links
    )
  }

  for (key in names(recipes)) {
    write_specs[[length(write_specs) + 1L]] <- list(
      target = .recipe_store_recipe_path(data_dir, key),
      data = recipes[[key]]
    )
  }

  lock_path <- .recipe_store_acquire_lock(data_dir)
  release_lock <- TRUE
  on.exit(
    if (release_lock) .recipe_store_release_lock(lock_path),
    add = TRUE
  )

  recovered_cleanly <- .recipe_store_recover_locked(data_dir)
  if (!recovered_cleanly) {
    stop(
      "En tidligere opskriftstransaktion mangler stadig filoprydning.",
      call. = FALSE
    )
  }

  current_revision <- recipe_store_revision(data_dir)
  if (
    !is.null(expected_revision) &&
      !identical(current_revision, expected_revision)
  ) {
    stop(
      "Opskriftsdata er ændret i en anden session. Genindlæs appen og prøv igen.",
      call. = FALSE
    )
  }

  live_metadata <- .recipe_store_metadata_paths(data_dir)
  candidate_active <- if (is.null(active_retter)) {
    .recipe_store_read_retter(unname(live_metadata[["retter"]]))
  } else {
    active_retter
  }
  candidate_archive <- if (is.null(archived_retter)) {
    .recipe_store_read_retter(unname(live_metadata[["retter_arkiv"]]))
  } else {
    archived_retter
  }
  candidate_links <- if (is.null(links)) {
    .recipe_store_read_links(unname(live_metadata[["links"]]))
  } else {
    links
  }
  .recipe_store_validate_catalog_tables(
    candidate_active,
    candidate_archive,
    candidate_links
  )

  if (
    length(delete_recipe_keys) == 1L &&
      tolower(delete_recipe_keys[[1]]) %in% tolower(candidate_active$key)
  ) {
    stop(
      "En aktiv ret kan ikke slettes permanent fra arkivet.",
      call. = FALSE
    )
  }

  existing_recipe_keys <- tools::file_path_sans_ext(
    basename(list.files(
      recipe_dir,
      pattern = "^[^.].*\\.txt$",
      full.names = FALSE
    ))
  )
  for (key in names(recipes)) {
    case_collision <- existing_recipe_keys[
      tolower(existing_recipe_keys) == tolower(key) &
        existing_recipe_keys != key
    ]
    if (length(case_collision) > 0L) {
      stop(
        sprintf(
          "Opskriftsnøglen '%s' kolliderer med den eksisterende nøgle '%s'.",
          key,
          case_collision[[1]]
        ),
        call. = FALSE
      )
    }
  }

  if (length(write_specs) == 0L && length(delete_recipe_keys) == 0L) {
    return(current_revision)
  }

  target_paths <- vapply(write_specs, `[[`, character(1), "target")
  stage_paths <- vapply(
    target_paths,
    function(path) {
      tempfile(
        pattern = paste0(".", basename(path), ".stage-"),
        tmpdir = dirname(path)
      )
    },
    character(1)
  )
  backup_paths <- vapply(
    target_paths,
    function(path) {
      tempfile(
        pattern = paste0(".", basename(path), ".backup-"),
        tmpdir = dirname(path)
      )
    },
    character(1)
  )

  delete_targets <- vapply(
    delete_recipe_keys,
    function(key) .recipe_store_recipe_path(data_dir, key),
    character(1)
  )
  quarantine_paths <- vapply(
    delete_targets,
    function(path) {
      tempfile(
        pattern = paste0(".", basename(path), ".quarantine-"),
        tmpdir = dirname(path)
      )
    },
    character(1)
  )
  commit_error <- NULL
  next_revision <- NULL
  journal_prepared <- FALSE

  tryCatch(
    {
      for (i in seq_along(write_specs)) {
        .recipe_store_write_table(write_specs[[i]]$data, stage_paths[[i]])
      }

      journal <- list(
        target_paths = target_paths,
        stage_paths = stage_paths,
        backup_paths = backup_paths,
        had_original = file.exists(target_paths),
        original_hashes = vapply(
          target_paths,
          .recipe_store_file_hash,
          character(1)
        ),
        delete_targets = delete_targets,
        quarantine_paths = quarantine_paths,
        delete_existed = file.exists(delete_targets),
        delete_hashes = vapply(
          delete_targets,
          .recipe_store_file_hash,
          character(1)
        )
      )
      .recipe_store_write_journal(data_dir, journal)
      journal_prepared <- TRUE
      .recipe_store_checkpoint(.fail_at, "after_stage", .crash_at)

      for (i in seq_along(write_specs)) {
        if (file.exists(target_paths[[i]])) {
          .recipe_store_move(
            target_paths[[i]],
            backup_paths[[i]],
            sprintf("flytte '%s' til backup", basename(target_paths[[i]]))
          )
        }
        .recipe_store_checkpoint(
          .fail_at,
          paste0("after_backup_", i),
          .crash_at
        )
      }

      for (i in seq_along(delete_targets)) {
        if (file.exists(delete_targets[[i]])) {
          .recipe_store_move(
            delete_targets[[i]],
            quarantine_paths[[i]],
            sprintf("flytte '%s' til karantæne", basename(delete_targets[[i]]))
          )
        }
        .recipe_store_checkpoint(
          .fail_at,
          paste0("after_quarantine_", i),
          .crash_at
        )
      }

      for (i in seq_along(write_specs)) {
        .recipe_store_move(
          stage_paths[[i]],
          target_paths[[i]],
          sprintf("publicere '%s'", basename(target_paths[[i]]))
        )
        .recipe_store_checkpoint(
          .fail_at,
          paste0("after_promote_", i),
          .crash_at
        )
      }

      next_revision <- recipe_store_revision(data_dir)
      .recipe_store_mark_committed(data_dir)
      .recipe_store_checkpoint(NULL, "after_commit_marker", .crash_at)
      .recipe_store_recover_locked(data_dir)
    },
    error = function(error) {
      commit_error <<- error
    }
  )

  if (!is.null(commit_error)) {
    if (inherits(commit_error, "recipe_store_simulated_crash")) {
      release_lock <- FALSE
      stop(conditionMessage(commit_error), call. = FALSE)
    }

    if (journal_prepared) {
      recovery_error <- tryCatch(
        {
          .recipe_store_recover_locked(data_dir)
          NULL
        },
        error = identity
      )
    } else {
      failed_cleanup <- .recipe_store_remove_files(
        c(stage_paths, backup_paths, quarantine_paths)
      )
      recovery_error <- if (length(failed_cleanup) > 0L) {
        simpleError(sprintf(
          "Stage-filer kunne ikke fjernes: %s",
          paste(failed_cleanup, collapse = ", ")
        ))
      } else {
        NULL
      }
    }

    if (!is.null(recovery_error)) {
      stop(
        paste(
          conditionMessage(commit_error),
          "Rollback kunne ikke gennemføres fuldt:",
          conditionMessage(recovery_error)
        ),
        call. = FALSE
      )
    }

    stop(conditionMessage(commit_error), call. = FALSE)
  }

  next_revision
}
