suppressPackageStartupMessages(
  source(file.path("R", "store_lock.R"), encoding = "UTF-8")
)

store_lock_test_expect_error <- function(code, pattern = NULL) {
  error <- tryCatch(
    {
      force(code)
      NULL
    },
    error = identity
  )

  stopifnot(inherits(error, "error"))
  if (!is.null(pattern)) {
    stopifnot(grepl(
      pattern,
      conditionMessage(error),
      fixed = TRUE
    ))
  }
  invisible(error)
}

store_lock_test_acquire <- function(
  lock_path,
  wait_seconds = 0.1
) {
  store_lock_acquire(
    lock_path = lock_path,
    store_label = "testlageret",
    lock_lost_class = "test_store_lock_lost",
    wait_seconds = wait_seconds
  )
}

store_lock_test_release <- function(lock_handle) {
  store_lock_release(
    lock_handle,
    store_label = "testlageret"
  )
}

store_lock_test_wait_for_file <- function(
  path,
  timeout_seconds = 10
) {
  deadline <- Sys.time() + timeout_seconds
  while (!file.exists(path) && Sys.time() < deadline) {
    Sys.sleep(0.02)
  }
  file.exists(path)
}

run_store_lock_tests <- function() {
test_root <- tempfile("groceryapp-store-lock-")
dir.create(test_root)
on.exit(
  unlink(test_root, recursive = TRUE, force = TRUE),
  add = TRUE
)

# To forbindelser kan ikke eje samme SQLite-lås samtidig. Låsen kan tages
# straks igen, når den første forbindelse er lukket.
lock_path <- file.path(test_root, "shared-lock.sqlite")
first_owner <- store_lock_test_acquire(lock_path)
busy_error <- store_lock_test_expect_error(
  store_lock_test_acquire(
    lock_path,
    wait_seconds = 0.05
  ),
  "i brug"
)
stopifnot(
  inherits(first_owner$connection, "DBIConnection"),
  isTRUE(dbIsValid(first_owner$connection)),
  inherits(busy_error, "error"),
  isTRUE(store_lock_test_release(first_owner)),
  !isTRUE(store_lock_test_release(first_owner))
)

second_owner <- store_lock_test_acquire(lock_path)
stopifnot(isTRUE(store_lock_test_release(second_owner)))

# Hvis den afsluttende probe fejler efter BEGIN, lukker acquire selv
# forbindelsen. Næste handling må derfor kunne tage låsen med det samme.
original_db_get_query <- dbGetQuery
assign(
  "dbGetQuery",
  function(...) {
    stop("Fremprovokeret SQLite-probefejl.", call. = FALSE)
  },
  envir = .GlobalEnv
)
probe_error <- store_lock_test_expect_error(
  store_lock_test_acquire(lock_path),
  "mistet"
)
assign(
  "dbGetQuery",
  original_db_get_query,
  envir = .GlobalEnv
)
after_probe_owner <- store_lock_test_acquire(lock_path)
stopifnot(
  inherits(probe_error, "test_store_lock_lost"),
  inherits(probe_error, "store_lock_lost"),
  isTRUE(store_lock_test_release(after_probe_owner))
)

# Et ugyldigt håndtag giver lagerets genkendelige lock-lost-klasse.
lost_owner <- store_lock_test_acquire(lock_path)
dbDisconnect(lost_owner$connection)
lost_error <- store_lock_test_expect_error(
  store_lock_touch(
    lost_owner,
    store_label = "testlageret",
    lock_lost_class = "test_store_lock_lost"
  ),
  "mistet"
)
stopifnot(
  inherits(lost_error, "test_store_lock_lost"),
  inherits(lost_error, "store_lock_lost"),
  !isTRUE(store_lock_test_release(lost_owner))
)

# En ugyldig SQLite-fil og ugyldige argumenter afvises med tydelige fejl.
invalid_lock_path <- file.path(test_root, "invalid-lock.sqlite")
writeLines(
  "ikke en SQLite-database",
  invalid_lock_path,
  useBytes = TRUE
)
store_lock_test_expect_error(
  store_lock_test_acquire(invalid_lock_path),
  "kunne ikke oprettes"
)
store_lock_test_expect_error(
  store_lock_test_acquire(lock_path, wait_seconds = -1),
  "Ventetiden"
)
store_lock_test_expect_error(
  store_lock_acquire(
    lock_path = lock_path,
    store_label = "",
    lock_lost_class = "test_store_lock_lost"
  ),
  "Konteksten"
)

# En separat R-proces holder låsen længe nok til, at forældreprocessen bliver
# afvist. Barnet afslutter derefter uden release; operativsystemet skal stadig
# frigive låsen, så forælderen kan tage den uden stale-lock-overtagelse.
process_lock_path <- file.path(
  test_root,
  "process-lock.sqlite"
)
ready_path <- file.path(test_root, "child-ready")
release_path <- file.path(test_root, "parent-release")
child_log <- file.path(test_root, "child.log")
child_script <- file.path(test_root, "lock-child.R")
store_lock_script <- normalizePath(
  file.path("R", "store_lock.R"),
  winslash = "/",
  mustWork = TRUE
)
writeLines(
  c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "suppressPackageStartupMessages(source(args[[1]], encoding = 'UTF-8'))",
    "handle <- store_lock_acquire(",
    "  lock_path = args[[2]],",
    "  store_label = 'testlageret',",
    "  lock_lost_class = 'test_store_lock_lost',",
    "  wait_seconds = 2",
    ")",
    "writeLines('ready', args[[3]], useBytes = TRUE)",
    "deadline <- Sys.time() + 30",
    "while (!file.exists(args[[4]]) && Sys.time() < deadline) {",
    "  Sys.sleep(0.05)",
    "}",
    "quit(save = 'no', status = 0, runLast = FALSE)"
  ),
  child_script,
  useBytes = TRUE
)
rscript <- file.path(
  R.home("bin"),
  if (.Platform$OS.type == "windows") {
    "Rscript.exe"
  } else {
    "Rscript"
  }
)
system2(
  rscript,
  args = shQuote(c(
    child_script,
    store_lock_script,
    process_lock_path,
    ready_path,
    release_path
  )),
  wait = FALSE,
  stdout = child_log,
  stderr = child_log
)
if (!store_lock_test_wait_for_file(ready_path)) {
  child_output <- if (file.exists(child_log)) {
    paste(readLines(child_log, warn = FALSE), collapse = "\n")
  } else {
    "<ingen child-log>"
  }
  stop(
    paste(
      "Testprocessen tog ikke låsen:",
      child_output
    ),
    call. = FALSE
  )
}

busy_check_completed <- tryCatch(
  {
    store_lock_test_expect_error(
      store_lock_test_acquire(
        process_lock_path,
        wait_seconds = 0.05
      ),
      "i brug"
    )
    TRUE
  },
  finally = writeLines(
    "release",
    release_path,
    useBytes = TRUE
  )
)
stopifnot(isTRUE(busy_check_completed))

reacquired_after_process_stop <- NULL
reacquire_deadline <- Sys.time() + 10
while (
  is.null(reacquired_after_process_stop) &&
    Sys.time() < reacquire_deadline
) {
  reacquired_after_process_stop <- tryCatch(
    store_lock_test_acquire(
      process_lock_path,
      wait_seconds = 0.05
    ),
    error = function(error) NULL
  )
  if (is.null(reacquired_after_process_stop)) {
    Sys.sleep(0.05)
  }
}
stopifnot(
  !is.null(reacquired_after_process_stop),
  isTRUE(
    store_lock_test_release(
      reacquired_after_process_stop
    )
  )
)
}

run_store_lock_tests()

lock_consumer_lines <- lapply(
  c(
    file.path("R", "recipe_store.R"),
    file.path("R", "basis_varer_store.R")
  ),
  readLines,
  warn = FALSE,
  encoding = "UTF-8"
)
stopifnot(all(vapply(
  lock_consumer_lines,
  function(lines) {
    all(vapply(
      c(
        "store_lock_acquire",
        "store_lock_touch",
        "store_lock_release"
      ),
      function(function_name) {
        any(grepl(function_name, lines, fixed = TRUE))
      },
      logical(1)
    )) &&
      !any(grepl("stale_after_seconds", lines, fixed = TRUE)) &&
      !any(grepl("Sys.setFileTime", lines, fixed = TRUE))
  },
  logical(1)
)))

message(paste(
  "Den fælles SQLite-lås serialiserer samtidige forbindelser, frigives ved",
  "processtop og giver lagerspecifikke fejl uden stale-lock-overtagelse."
))
