suppressPackageStartupMessages({
  source(file.path("R", "store_lock.R"), encoding = "UTF-8")
  source(file.path("R", "basis_varer_store.R"), encoding = "UTF-8")
})

basis_expect_error <- function(code, pattern = NULL) {
  error <- tryCatch(
    {
      force(code)
      NULL
    },
    error = identity
  )

  stopifnot(inherits(error, "error"))
  if (!is.null(pattern)) {
    stopifnot(
      grepl(pattern, conditionMessage(error), fixed = TRUE)
    )
  }
  invisible(error)
}

basis_read_raw <- function(path) {
  readBin(
    path,
    what = "raw",
    n = file.info(path)$size
  )
}

basis_store_artifacts <- function(data_dir) {
  list.files(
    data_dir,
    pattern = "^\\.basis-varer-store",
    all.files = TRUE,
    full.names = FALSE
  )
}

basis_expect_lock_available <- function(data_dir) {
  lock_handle <- .basis_varer_store_acquire_lock(
    data_dir,
    wait_seconds = 0.1
  )
  stopifnot(
    isTRUE(dbIsValid(lock_handle$connection)),
    isTRUE(.basis_varer_store_release_lock(lock_handle))
  )
  invisible(TRUE)
}

basis_fixture <- function() {
  data.frame(
    Indkobsliste = c("Mælk", "Banan"),
    maengde = c(1, 1),
    enhed = c("liter", "stk"),
    kat_1 = c("mejeri", "frugt og grønt"),
    kat_2 = c("mælk", ""),
    stringsAsFactors = FALSE
  )
}

basis_row <- function(
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

run_basis_varer_store_tests <- function() {
  root <- tempfile("groceryapp-basis-store-")
  dir.create(root)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  temp_root <- normalizePath(
    tempdir(),
    winslash = "/",
    mustWork = TRUE
  )
  stopifnot(
    startsWith(
      tolower(root),
      paste0(tolower(temp_root), "/")
    )
  )
  on.exit(
    unlink(root, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  target <- file.path(root, "basis_varer.txt")
  write.csv(
    basis_fixture(),
    target,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  initial_snapshot <- basis_varer_store_read(root)
  stopifnot(
    identical(
      names(initial_snapshot),
      c("varer", "revision")
    ),
    identical(
      initial_snapshot$varer$Indkobsliste,
      c("Banan", "Mælk")
    ),
    identical(
      names(initial_snapshot$varer),
      c(
        "Indkobsliste",
        "maengde",
        "enhed",
        "kat_1",
        "kat_2"
      )
    ),
    is.character(initial_snapshot$revision),
    length(basis_store_artifacts(root)) == 0L
  )

  success_candidate <- rbind(
    initial_snapshot$varer,
    basis_row(
      "Æbler",
      enhed = "kg",
      kat_1 = "frugt og grønt"
    )
  )
  success_snapshot <- basis_varer_store_commit(
    success_candidate,
    initial_snapshot$revision,
    root
  )
  stored_after_success <- basis_varer_store_read(root)
  stopifnot(
    !identical(
      success_snapshot$revision,
      initial_snapshot$revision
    ),
    identical(success_snapshot, stored_after_success),
    "Æbler" %in% success_snapshot$varer$Indkobsliste,
    success_snapshot$varer$kat_2[
      success_snapshot$varer$Indkobsliste == "Æbler"
    ] == "",
    length(basis_store_artifacts(root)) == 0L
  )

  rollback_candidate <- rbind(
    success_snapshot$varer,
    basis_row("Citron")
  )
  for (step in c(
    "after_stage",
    "after_backup",
    "after_promote"
  )) {
    bytes_before <- basis_read_raw(target)
    revision_before <- success_snapshot$revision

    basis_expect_error(
      basis_varer_store_commit(
        rollback_candidate,
        revision_before,
        root,
        .fail_at = step
      ),
      "Testfejl"
    )

    stopifnot(
      basis_expect_lock_available(root),
      length(basis_store_artifacts(root)) == 0L,
      identical(basis_read_raw(target), bytes_before),
      identical(
        basis_varer_store_revision(root),
        revision_before
      )
    )
  }

  # Et almindeligt problem efter commit-markøren er stadig en succes:
  # recovery kan se, at den nye, validerede fil er den committed version.
  marker_candidate <- rbind(
    success_snapshot$varer,
    basis_row("Dild")
  )
  marker_snapshot <- basis_varer_store_commit(
    marker_candidate,
    success_snapshot$revision,
    root,
    .fail_at = "after_commit_marker"
  )
  stopifnot(
    "Dild" %in% marker_snapshot$varer$Indkobsliste,
    identical(
      marker_snapshot,
      basis_varer_store_read(root)
    ),
    length(basis_store_artifacts(root)) == 0L
  )

  # Oprydning efter commit må ikke ændre en varigt gemt ændring til en fejl.
  # Markøren skal blive liggende, indtil både stage og backup er fjernet.
  original_remove_files <- .basis_varer_store_remove_files
  on.exit(
    assign(
      ".basis_varer_store_remove_files",
      original_remove_files,
      envir = .GlobalEnv
    ),
    add = TRUE
  )
  make_cleanup_failure <- function(failing_basename) {
    has_failed <- FALSE
    function(paths) {
      if (
        !has_failed &&
          any(basename(paths) == failing_basename)
      ) {
        has_failed <<- TRUE
        stop("Simuleret oprydningsfejl.", call. = FALSE)
      }
      original_remove_files(paths)
    }
  }

  backup_cleanup_candidate <- rbind(
    marker_snapshot$varer,
    basis_row("Basilikum")
  )
  assign(
    ".basis_varer_store_remove_files",
    make_cleanup_failure(".basis-varer-store.backup"),
    envir = .GlobalEnv
  )
  backup_cleanup_snapshot <- basis_varer_store_commit(
    backup_cleanup_candidate,
    marker_snapshot$revision,
    root
  )
  stopifnot(
    "Basilikum" %in%
      backup_cleanup_snapshot$varer$Indkobsliste,
    file.exists(
      file.path(root, ".basis-varer-store.backup")
    ),
    file.exists(
      file.path(root, ".basis-varer-store.committed")
    )
  )
  assign(
    ".basis_varer_store_remove_files",
    original_remove_files,
    envir = .GlobalEnv
  )
  marker_snapshot <- basis_varer_store_read(root)
  stopifnot(
    identical(marker_snapshot, backup_cleanup_snapshot),
    length(basis_store_artifacts(root)) == 0L
  )

  marker_cleanup_candidate <- rbind(
    marker_snapshot$varer,
    basis_row("Citrongræs")
  )
  assign(
    ".basis_varer_store_remove_files",
    make_cleanup_failure(".basis-varer-store.committed"),
    envir = .GlobalEnv
  )
  marker_cleanup_snapshot <- basis_varer_store_commit(
    marker_cleanup_candidate,
    marker_snapshot$revision,
    root
  )
  stopifnot(
    "Citrongræs" %in%
      marker_cleanup_snapshot$varer$Indkobsliste,
    !file.exists(
      file.path(root, ".basis-varer-store.backup")
    ),
    file.exists(
      file.path(root, ".basis-varer-store.committed")
    )
  )
  assign(
    ".basis_varer_store_remove_files",
    original_remove_files,
    envir = .GlobalEnv
  )
  marker_snapshot <- basis_varer_store_read(root)
  stopifnot(
    identical(marker_snapshot, marker_cleanup_snapshot),
    length(basis_store_artifacts(root)) == 0L
  )

  # En gammel markør med uafsluttet oprydning må aldrig få den næste,
  # mislykkede ændring til at se gemt ud.
  persistent_cleanup_failure <- function(paths) {
    if (
      any(
        basename(paths) ==
          ".basis-varer-store.backup"
      )
    ) {
      stop("Vedvarende oprydningsfejl.", call. = FALSE)
    }
    original_remove_files(paths)
  }
  pending_cleanup_candidate <- rbind(
    marker_snapshot$varer,
    basis_row("Dåsetomat")
  )
  assign(
    ".basis_varer_store_remove_files",
    persistent_cleanup_failure,
    envir = .GlobalEnv
  )
  pending_cleanup_snapshot <- basis_varer_store_commit(
    pending_cleanup_candidate,
    marker_snapshot$revision,
    root
  )
  bytes_with_pending_cleanup <- basis_read_raw(target)
  falsely_successful_candidate <- rbind(
    pending_cleanup_snapshot$varer,
    basis_row("Eddike")
  )
  pending_error <- basis_expect_error(
    basis_varer_store_commit(
      falsely_successful_candidate,
      pending_cleanup_snapshot$revision,
      root,
      .fail_at = "after_stage"
    ),
    "oprydningen er endnu ikke afsluttet"
  )
  stopifnot(
    inherits(pending_error, "error"),
    "Dåsetomat" %in%
      pending_cleanup_snapshot$varer$Indkobsliste,
    !"Eddike" %in%
      .basis_varer_store_read_file(target)$Indkobsliste,
    identical(
      basis_read_raw(target),
      bytes_with_pending_cleanup
    ),
    file.exists(
      file.path(root, ".basis-varer-store.backup")
    ),
    file.exists(
      file.path(root, ".basis-varer-store.committed")
    )
  )
  assign(
    ".basis_varer_store_remove_files",
    original_remove_files,
    envir = .GlobalEnv
  )
  marker_snapshot <- basis_varer_store_read(root)
  stopifnot(
    identical(marker_snapshot, pending_cleanup_snapshot),
    length(basis_store_artifacts(root)) == 0L
  )

  # Et processtop før markøren efterlader transaktionsfilerne. OS-låsen er
  # straks fri, og næste læsning ruller automatisk tilbage.
  crash_rollback_candidate <- rbind(
    marker_snapshot$varer,
    basis_row("Estragon")
  )
  bytes_before_crash <- basis_read_raw(target)
  crash_error <- basis_expect_error(
    basis_varer_store_commit(
      crash_rollback_candidate,
      marker_snapshot$revision,
      root,
      .crash_at = "after_backup"
    ),
    "Simuleret processtop"
  )
  stopifnot(
    inherits(
      crash_error,
      "basis_varer_store_simulated_crash"
    ),
    basis_expect_lock_available(root),
    length(basis_store_artifacts(root)) > 0L
  )
  recovered_old <- basis_varer_store_read(root)
  stopifnot(
    identical(recovered_old, marker_snapshot),
    identical(basis_read_raw(target), bytes_before_crash),
    length(basis_store_artifacts(root)) == 0L
  )

  # Et processtop efter markøren beholder derimod den nye fil.
  crash_commit_candidate <- rbind(
    recovered_old$varer,
    basis_row("Fennikel")
  )
  basis_expect_error(
    basis_varer_store_commit(
      crash_commit_candidate,
      recovered_old$revision,
      root,
      .crash_at = "after_commit_marker"
    ),
    "Simuleret processtop"
  )
  stopifnot(basis_expect_lock_available(root))
  recovered_new <- basis_varer_store_read(root)
  stopifnot(
    "Fennikel" %in% recovered_new$varer$Indkobsliste,
    !"Estragon" %in% recovered_new$varer$Indkobsliste,
    length(basis_store_artifacts(root)) == 0L
  )

  # SQLite-låsen serialiserer samtidige ejere og frigives straks.
  lock_path <- file.path(root, "basis-varer-lock.sqlite")
  first_owner <- .basis_varer_store_acquire_lock(root)
  basis_expect_error(
    .basis_varer_store_acquire_lock(
      root,
      wait_seconds = 0.05
    ),
    "i brug"
  )
  stopifnot(
    file.exists(lock_path),
    isTRUE(.basis_varer_store_release_lock(first_owner)),
    !isTRUE(.basis_varer_store_release_lock(first_owner))
  )
  second_owner <- .basis_varer_store_acquire_lock(
    root,
    wait_seconds = 0.1
  )
  stopifnot(
    identical(second_owner$path, lock_path),
    isTRUE(.basis_varer_store_release_lock(second_owner))
  )

  # To gamle snapshots må ikke overskrive hinanden. Efter konflikten kan den
  # anden bruger genlæse og prøve igen med begge ændringer bevaret.
  shared_start <- basis_varer_store_read(root)
  with_tea <- rbind(shared_start$varer, basis_row("Te"))
  tea_snapshot <- basis_varer_store_commit(
    with_tea,
    shared_start$revision,
    root
  )

  stale_coffee <- rbind(
    shared_start$varer,
    basis_row("Kaffe")
  )
  conflict <- basis_expect_error(
    basis_varer_store_commit(
      stale_coffee,
      shared_start$revision,
      root
    ),
    "ændret i en anden session"
  )
  stopifnot(
    inherits(conflict, "basis_varer_store_conflict"),
    identical(basis_varer_store_read(root), tea_snapshot)
  )

  with_both <- rbind(tea_snapshot$varer, basis_row("Kaffe"))
  both_snapshot <- basis_varer_store_commit(
    with_both,
    tea_snapshot$revision,
    root
  )
  stopifnot(
    all(
      c("Te", "Kaffe") %in%
        both_snapshot$varer$Indkobsliste
    )
  )

  # Valideringsfejl opstår før en store-transaktion og må ikke ændre en
  # eneste byte i den eksisterende fil.
  valid_before_rejections <- both_snapshot$varer
  invalid_candidates <- list(
    list(Indkobsliste = "ikke en data frame"),
    valid_before_rejections[, -5, drop = FALSE],
    transform(
      valid_before_rejections,
      Indkobsliste = replace(Indkobsliste, 1, " ")
    ),
    rbind(
      valid_before_rejections,
      transform(
        valid_before_rejections[1, , drop = FALSE],
        Indkobsliste = paste0(
          " ",
          toupper(valid_before_rejections$Indkobsliste[[1]]),
          " "
        )
      )
    ),
    transform(
      valid_before_rejections,
      maengde = replace(maengde, 1, 0)
    ),
    transform(
      valid_before_rejections,
      enhed = replace(enhed, 1, "stk\nny linje")
    )
  )
  bytes_before_rejections <- basis_read_raw(target)
  for (invalid in invalid_candidates) {
    basis_expect_error(
      basis_varer_store_commit(
        invalid,
        both_snapshot$revision,
        root
      )
    )
    stopifnot(
      identical(
        basis_read_raw(target),
        bytes_before_rejections
      ),
      length(basis_store_artifacts(root)) == 0L
    )
  }

  # En beskadiget fil giver fejl og frigiver låsen. Den bliver aldrig
  # fortolket som en tom, gyldig vareliste.
  writeLines(
    "ikke,en,gyldig,basisvarefil",
    target,
    useBytes = TRUE
  )
  basis_expect_error(basis_varer_store_read(root))
  stopifnot(
    basis_expect_lock_available(root),
    length(basis_store_artifacts(root)) == 0L
  )
}

run_basis_varer_store_tests()

store_lines <- readLines(
  file.path("R", "basis_varer_store.R"),
  encoding = "UTF-8"
)
store_function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  store_lines
)
store_has_roxygen <- vapply(
  store_function_lines,
  function(line_number) {
    line_number > 1L &&
      grepl("^#'", store_lines[[line_number - 1L]])
  },
  logical(1)
)
store_libraries <- sub(
  "^library\\(([^)]+)\\).*$",
  "\\1",
  trimws(grep(
    "^library\\(",
    store_lines,
    value = TRUE
  ))
)
stopifnot(
  length(store_function_lines) > 0L,
  all(store_has_roxygen),
  !any(grepl("::", store_lines, fixed = TRUE)),
  all(c("utils", "tools") %in% store_libraries)
)

message(
  paste(
    "Basisvare-store bestod tests for sikker publicering, rollback,",
    "recovery, revisioner og validering."
  )
)
