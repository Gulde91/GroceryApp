suppressPackageStartupMessages({
  source(
    file.path("R", "store_lock.R"),
    encoding = "UTF-8"
  )
  source(
    file.path("R", "shopping_history_store.R"),
    encoding = "UTF-8"
  )
})

history_test_frame <- function(label) {
  data.frame(
    Indkøbsliste = c(
      paste("1 stk", label),
      "",
      paste0(label, " (2 pers.)")
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

history_test_bytes <- function(history_dir) {
  files <- .shopping_history_files(history_dir)
  result <- lapply(
    file.path(history_dir, files),
    function(path) {
      readBin(
        path,
        what = "raw",
        n = file.info(path)$size
      )
    }
  )
  names(result) <- files
  result
}

history_test_artifacts <- function(history_dir) {
  list.files(
    history_dir,
    pattern = "^\\.shopping-history-store",
    all.files = TRUE,
    full.names = FALSE
  )
}

history_test_expect_error <- function(expression) {
  result <- tryCatch(
    force(expression),
    error = identity
  )
  stopifnot(inherits(result, "error"))
  result
}

history_test_new_dir <- function(prefix) {
  path <- tempfile(prefix)
  dir.create(path)
  path
}

test_dirs <- character()

# En forventet revision er obligatorisk, og ugyldige værdier må ikke skrive.
revision_dir <- history_test_new_dir(
  "groceryapp-history-expected-revision-"
)
test_dirs <- c(test_dirs, revision_dir)
revision_candidate <- history_test_frame("Revision")

invisible(history_test_expect_error(
  shopping_history_store_save(
    revision_candidate,
    history_dir = revision_dir,
    date = "2026-07-01"
  )
))
for (invalid_revision in list(
  NULL,
  NA_character_,
  c("a", "b")
)) {
  history_test_expect_error(
    shopping_history_store_save(
      revision_candidate,
      expected_revision = invalid_revision,
      history_dir = revision_dir,
      date = "2026-07-01"
    )
  )
}
stopifnot(
  identical(.shopping_history_files(revision_dir), character()),
  identical(history_test_artifacts(revision_dir), character())
)

# To sessioner kan læse samme revision, men kun den første må gemme på den.
conflict_dir <- history_test_new_dir(
  "groceryapp-history-conflict-"
)
test_dirs <- c(test_dirs, conflict_dir)
session_a <- shopping_history_store_read(conflict_dir)
session_b <- shopping_history_store_read(conflict_dir)
saved_a <- shopping_history_store_save(
  history_test_frame("Session A"),
  expected_revision = session_a$revision,
  history_dir = conflict_dir,
  date = "2026-07-02"
)
bytes_after_a <- history_test_bytes(conflict_dir)
conflict <- tryCatch(
  shopping_history_store_save(
    history_test_frame("Session B"),
    expected_revision = session_b$revision,
    history_dir = conflict_dir,
    date = "2026-07-02"
  ),
  shopping_history_store_conflict = identity
)
stopifnot(
  inherits(conflict, "shopping_history_store_conflict"),
  identical(history_test_bytes(conflict_dir), bytes_after_a),
  identical(
    shopping_history_store_read(conflict_dir),
    saved_a
  ),
  identical(history_test_artifacts(conflict_dir), character())
)

# Revisionen beskytter hele historikmappen, ikke kun dagens target-fil.
other_date_conflict <- tryCatch(
  shopping_history_store_save(
    history_test_frame("Session B anden dato"),
    expected_revision = session_b$revision,
    history_dir = conflict_dir,
    date = "2026-07-03"
  ),
  shopping_history_store_conflict = identity
)
stopifnot(
  inherits(
    other_date_conflict,
    "shopping_history_store_conflict"
  ),
  identical(history_test_bytes(conflict_dir), bytes_after_a),
  !file.exists(file.path(
    conflict_dir,
    "indkobsseddel_20260703.rda"
  ))
)

fresh_after_conflict <- shopping_history_store_read(conflict_dir)
saved_b <- shopping_history_store_save(
  history_test_frame("Session B"),
  expected_revision = fresh_after_conflict$revision,
  history_dir = conflict_dir,
  date = "2026-07-02"
)
stopifnot(
  identical(
    saved_b$entries$Indkøbsliste,
    history_test_frame("Session B")$Indkøbsliste
  )
)
saved_b_bytes <- history_test_bytes(conflict_dir)
saved_b_again <- shopping_history_store_save(
  history_test_frame("Session B"),
  expected_revision = saved_b$revision,
  history_dir = conflict_dir,
  date = "2026-07-02"
)
stopifnot(
  identical(saved_b_again, saved_b),
  identical(history_test_bytes(conflict_dir), saved_b_bytes),
  identical(history_test_artifacts(conflict_dir), character())
)

# Almindelige fejl før commit-markøren gendanner en eksisterende dags fil
# byte for byte og efterlader ingen transaktionsfiler.
for (step in c(
  "after_stage",
  "after_backup",
  "after_promote"
)) {
  rollback_dir <- history_test_new_dir(
    paste0("groceryapp-history-rollback-", step, "-")
  )
  test_dirs <- c(test_dirs, rollback_dir)
  baseline <- shopping_history_store_save(
    history_test_frame("Oprindelig"),
    expected_revision = "empty",
    history_dir = rollback_dir,
    date = "2026-07-03"
  )
  baseline_bytes <- history_test_bytes(rollback_dir)

  rollback_error <- history_test_expect_error(
    shopping_history_store_save(
      history_test_frame("Ny kandidat"),
      expected_revision = baseline$revision,
      history_dir = rollback_dir,
      date = "2026-07-03",
      .fail_at = step
    )
  )

  stopifnot(
    grepl(step, conditionMessage(rollback_error), fixed = TRUE),
    identical(history_test_bytes(rollback_dir), baseline_bytes),
    identical(
      shopping_history_store_read(rollback_dir)$revision,
      baseline$revision
    ),
    identical(history_test_artifacts(rollback_dir), character())
  )
}

# Det samme gælder en helt ny dato: ved fejl må den nye target-fil forsvinde,
# mens andre historikfiler forbliver urørte.
for (step in c(
  "after_stage",
  "after_backup",
  "after_promote"
)) {
  new_date_dir <- history_test_new_dir(
    paste0("groceryapp-history-new-date-", step, "-")
  )
  test_dirs <- c(test_dirs, new_date_dir)
  baseline <- shopping_history_store_save(
    history_test_frame("Anden dato"),
    expected_revision = "empty",
    history_dir = new_date_dir,
    date = "2026-07-04"
  )
  baseline_bytes <- history_test_bytes(new_date_dir)

  rollback_error <- history_test_expect_error(
    shopping_history_store_save(
      history_test_frame("Ny dato fejler"),
      expected_revision = baseline$revision,
      history_dir = new_date_dir,
      date = "2026-07-05",
      .fail_at = step
    )
  )

  stopifnot(
    grepl(step, conditionMessage(rollback_error), fixed = TRUE),
    identical(history_test_bytes(new_date_dir), baseline_bytes),
    !file.exists(file.path(
      new_date_dir,
      "indkobsseddel_20260705.rda"
    )),
    identical(history_test_artifacts(new_date_dir), character())
  )
}

# En fejl efter commit-markøren er ikke længere en mislykket gemning. Den nye
# fil er varigt publiceret, og oprydningen afsluttes før snapshotet returneres.
committed_dir <- history_test_new_dir(
  "groceryapp-history-committed-"
)
test_dirs <- c(test_dirs, committed_dir)
old_committed <- shopping_history_store_save(
  history_test_frame("Før commit"),
  expected_revision = "empty",
  history_dir = committed_dir,
  date = "2026-07-06"
)
new_committed <- shopping_history_store_save(
  history_test_frame("Efter commit"),
  expected_revision = old_committed$revision,
  history_dir = committed_dir,
  date = "2026-07-06",
  .fail_at = "after_commit_marker"
)
stopifnot(
  identical(
    new_committed$entries$Indkøbsliste,
    history_test_frame("Efter commit")$Indkøbsliste
  ),
  identical(history_test_artifacts(committed_dir), character())
)

# Recovery håndterer også et processtop lige efter journal/stage og lige efter
# en eksisterende target er flyttet til backup.
crash_stage_dir <- history_test_new_dir(
  "groceryapp-history-crash-stage-"
)
test_dirs <- c(test_dirs, crash_stage_dir)
crash_stage_base <- shopping_history_store_save(
  history_test_frame("Stage-base"),
  expected_revision = "empty",
  history_dir = crash_stage_dir,
  date = "2026-07-11"
)
crash_stage_bytes <- history_test_bytes(crash_stage_dir)
crash_stage <- history_test_expect_error(
  shopping_history_store_save(
    history_test_frame("Stage-ufærdig"),
    expected_revision = crash_stage_base$revision,
    history_dir = crash_stage_dir,
    date = "2026-07-12",
    .crash_at = "after_stage"
  )
)
stopifnot(
  inherits(crash_stage, "shopping_history_store_simulated_crash"),
  file.exists(file.path(
    crash_stage_dir,
    ".shopping-history-store-transaction.rds"
  )),
  file.exists(file.path(
    crash_stage_dir,
    ".shopping-history-store.stage"
  )),
  !file.exists(file.path(
    crash_stage_dir,
    "indkobsseddel_20260712.rda"
  ))
)
invisible(shopping_history_store_read(crash_stage_dir))
stopifnot(
  identical(history_test_bytes(crash_stage_dir), crash_stage_bytes),
  identical(history_test_artifacts(crash_stage_dir), character())
)

crash_backup_dir <- history_test_new_dir(
  "groceryapp-history-crash-backup-"
)
test_dirs <- c(test_dirs, crash_backup_dir)
crash_backup_base <- shopping_history_store_save(
  history_test_frame("Backup-base"),
  expected_revision = "empty",
  history_dir = crash_backup_dir,
  date = "2026-07-13"
)
crash_backup_bytes <- history_test_bytes(crash_backup_dir)
crash_backup <- history_test_expect_error(
  shopping_history_store_save(
    history_test_frame("Backup-ufærdig"),
    expected_revision = crash_backup_base$revision,
    history_dir = crash_backup_dir,
    date = "2026-07-13",
    .crash_at = "after_backup"
  )
)
stopifnot(
  inherits(crash_backup, "shopping_history_store_simulated_crash"),
  file.exists(file.path(
    crash_backup_dir,
    ".shopping-history-store.backup"
  )),
  !file.exists(file.path(
    crash_backup_dir,
    "indkobsseddel_20260713.rda"
  ))
)
invisible(shopping_history_store_read(crash_backup_dir))
stopifnot(
  identical(history_test_bytes(crash_backup_dir), crash_backup_bytes),
  identical(history_test_artifacts(crash_backup_dir), character())
)

# Et simuleret processtop før commit efterlader journalen. Når OS-låsen er
# frigivet af processtoppet, ruller næste læsning en eksisterende fil tilbage.
crash_existing_dir <- history_test_new_dir(
  "groceryapp-history-crash-existing-"
)
test_dirs <- c(test_dirs, crash_existing_dir)
crash_existing_base <- shopping_history_store_save(
  history_test_frame("Stabil"),
  expected_revision = "empty",
  history_dir = crash_existing_dir,
  date = "2026-07-07"
)
crash_existing_bytes <- history_test_bytes(crash_existing_dir)
crash_existing <- history_test_expect_error(
  shopping_history_store_save(
    history_test_frame("Ufærdig"),
    expected_revision = crash_existing_base$revision,
    history_dir = crash_existing_dir,
    date = "2026-07-07",
    .crash_at = "after_promote"
  )
)
stopifnot(
  inherits(
    crash_existing,
    "shopping_history_store_simulated_crash"
  ),
  grepl(
    "after_promote",
    conditionMessage(crash_existing),
    fixed = TRUE
  ),
  file.exists(file.path(
    crash_existing_dir,
    ".shopping-history-store-transaction.rds"
  )),
  file.exists(file.path(
    crash_existing_dir,
    ".shopping-history-store.backup"
  )),
  !file.exists(file.path(
    crash_existing_dir,
    ".shopping-history-store-transaction.committed"
  ))
)
recovered_existing <- shopping_history_store_read(
  crash_existing_dir
)
stopifnot(
  identical(
    history_test_bytes(crash_existing_dir),
    crash_existing_bytes
  ),
  identical(
    recovered_existing$revision,
    crash_existing_base$revision
  ),
  identical(
    history_test_artifacts(crash_existing_dir),
    character()
  )
)

# Ved en ny dato fjerner recovery den halvpublicerede target-fil.
crash_new_dir <- history_test_new_dir(
  "groceryapp-history-crash-new-"
)
test_dirs <- c(test_dirs, crash_new_dir)
crash_new_base <- shopping_history_store_save(
  history_test_frame("Beholdes"),
  expected_revision = "empty",
  history_dir = crash_new_dir,
  date = "2026-07-08"
)
crash_new_bytes <- history_test_bytes(crash_new_dir)
crash_new <- history_test_expect_error(
  shopping_history_store_save(
    history_test_frame("Fjernes"),
    expected_revision = crash_new_base$revision,
    history_dir = crash_new_dir,
    date = "2026-07-09",
    .crash_at = "after_promote"
  )
)
stopifnot(
  inherits(crash_new, "shopping_history_store_simulated_crash"),
  grepl(
    "after_promote",
    conditionMessage(crash_new),
    fixed = TRUE
  ),
  file.exists(file.path(
    crash_new_dir,
    ".shopping-history-store-transaction.rds"
  )),
  !file.exists(file.path(
    crash_new_dir,
    ".shopping-history-store.backup"
  )),
  file.exists(file.path(
    crash_new_dir,
    "indkobsseddel_20260709.rda"
  ))
)
invisible(shopping_history_store_read(crash_new_dir))
stopifnot(
  identical(history_test_bytes(crash_new_dir), crash_new_bytes),
  !file.exists(file.path(
    crash_new_dir,
    "indkobsseddel_20260709.rda"
  )),
  identical(history_test_artifacts(crash_new_dir), character())
)

# Et processtop efter commit-markøren skal derimod beholde den nye fil.
crash_committed_dir <- history_test_new_dir(
  "groceryapp-history-crash-committed-"
)
test_dirs <- c(test_dirs, crash_committed_dir)
crash_committed_base <- shopping_history_store_save(
  history_test_frame("Gammel"),
  expected_revision = "empty",
  history_dir = crash_committed_dir,
  date = "2026-07-10"
)
crash_committed <- history_test_expect_error(
  shopping_history_store_save(
    history_test_frame("Varigt gemt"),
    expected_revision = crash_committed_base$revision,
    history_dir = crash_committed_dir,
    date = "2026-07-10",
    .crash_at = "after_commit_marker"
  )
)
stopifnot(
  inherits(
    crash_committed,
    "shopping_history_store_simulated_crash"
  ),
  grepl(
    "after_commit_marker",
    conditionMessage(crash_committed),
    fixed = TRUE
  ),
  file.exists(file.path(
    crash_committed_dir,
    ".shopping-history-store-transaction.rds"
  )),
  file.exists(file.path(
    crash_committed_dir,
    ".shopping-history-store-transaction.committed"
  )),
  file.exists(file.path(
    crash_committed_dir,
    ".shopping-history-store.backup"
  ))
)
recovered_committed <- shopping_history_store_read(
  crash_committed_dir
)
stopifnot(
  identical(
    recovered_committed$entries$Indkøbsliste,
    history_test_frame("Varigt gemt")$Indkøbsliste
  ),
  identical(
    history_test_artifacts(crash_committed_dir),
    character()
  )
)

# En committed fil på en helt ny dato skal også beholdes uden en backup.
crash_committed_new_dir <- history_test_new_dir(
  "groceryapp-history-crash-committed-new-"
)
test_dirs <- c(test_dirs, crash_committed_new_dir)
crash_committed_new <- history_test_expect_error(
  shopping_history_store_save(
    history_test_frame("Ny varigt gemt dato"),
    expected_revision = "empty",
    history_dir = crash_committed_new_dir,
    date = "2026-07-14",
    .crash_at = "after_commit_marker"
  )
)
stopifnot(
  inherits(
    crash_committed_new,
    "shopping_history_store_simulated_crash"
  ),
  !file.exists(file.path(
    crash_committed_new_dir,
    ".shopping-history-store.backup"
  )),
  file.exists(file.path(
    crash_committed_new_dir,
    ".shopping-history-store-transaction.committed"
  ))
)
recovered_committed_new <- shopping_history_store_read(
  crash_committed_new_dir
)
stopifnot(
  identical(
    recovered_committed_new$entries$Indkøbsliste,
    history_test_frame("Ny varigt gemt dato")$Indkøbsliste
  ),
  identical(
    history_test_artifacts(crash_committed_new_dir),
    character()
  )
)

# SQLite-låsen serialiserer to forbindelser. Når den første frigives, kan den
# næste handling straks tage låsen uden stale-lock-overtagelse.
lock_dir <- history_test_new_dir("groceryapp-history-lock-")
test_dirs <- c(test_dirs, lock_dir)
first_owner <- .shopping_history_store_acquire_lock(lock_dir)
invisible(history_test_expect_error(
  .shopping_history_store_acquire_lock(
    lock_dir,
    wait_seconds = 0.05
  )
))
stopifnot(
  isTRUE(dbIsValid(first_owner$connection)),
  isTRUE(.shopping_history_store_release_lock(first_owner)),
  !isTRUE(.shopping_history_store_release_lock(first_owner))
)
second_owner <- .shopping_history_store_acquire_lock(
  lock_dir,
  wait_seconds = 0.2
)
stopifnot(
  isTRUE(dbIsValid(second_owner$connection)),
  isTRUE(.shopping_history_store_release_lock(second_owner))
)

# Hvis den afsluttende probe fejler efter BEGIN, skal acquire selv lukke
# forbindelsen. En ny acquire må derfor lykkes uden at vente på garbage
# collection.
original_db_get_query <- dbGetQuery
assign(
  "dbGetQuery",
  function(...) {
    stop("Fremprovokeret SQLite-probefejl.", call. = FALSE)
  },
  envir = .GlobalEnv
)
probe_error <- history_test_expect_error(
  .shopping_history_store_acquire_lock(
    lock_dir,
    wait_seconds = 0.05
  )
)
assign(
  "dbGetQuery",
  original_db_get_query,
  envir = .GlobalEnv
)
stopifnot(grepl(
  "mistet",
  conditionMessage(probe_error),
  fixed = TRUE
))
stopifnot(
  inherits(
    probe_error,
    "shopping_history_store_lock_lost"
  ),
  inherits(probe_error, "store_lock_lost")
)
after_probe_owner <- .shopping_history_store_acquire_lock(
  lock_dir,
  wait_seconds = 0.05
)
stopifnot(isTRUE(
  .shopping_history_store_release_lock(after_probe_owner)
))

# En ugyldig SQLite-låsefil skal give en hurtig, tydelig fejl.
invalid_lock_dir <- history_test_new_dir(
  "groceryapp-history-invalid-lock-"
)
test_dirs <- c(test_dirs, invalid_lock_dir)
invalid_lock_path <- file.path(
  invalid_lock_dir,
  "shopping-history-lock.sqlite"
)
writeLines("ikke en SQLite-database", invalid_lock_path, useBytes = TRUE)
invalid_lock_error <- history_test_expect_error(
  .shopping_history_store_acquire_lock(
    invalid_lock_dir,
    wait_seconds = 0.05
  )
)
stopifnot(grepl(
  "kunne ikke oprettes",
  conditionMessage(invalid_lock_error),
  fixed = TRUE
))

for (path in test_dirs) {
  unlink(path, recursive = TRUE, force = TRUE)
}

message(paste(
  "Shopping-history-store bestod tests for revisionskonflikter,",
  "atomisk publicering, rollback, processtop og låseejerskab."
))
