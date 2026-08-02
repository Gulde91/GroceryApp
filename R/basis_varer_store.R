# Sikker lagring af basisvarer ----------------------------------------------
#
# Denne fil læser og gemmer brugerens basisvarer på disken. Den håndterer
# validering, revisioner, låse, midlertidige filer og gendannelse efter en
# afbrudt gemning uden at kende til Shiny eller appens brugerflade.

library(utils)
library(tools)

#' Læs et konsistent snapshot af basisvarerne
#'
#' Funktionen tager kortvarigt store-låsen, rydder op efter en eventuelt
#' afbrudt tidligere gemning og læser derefter både den kanoniske varetabel
#' og filens aktuelle revision. En manglende eller ugyldig fil giver en
#' tydelig fejl; den erstattes aldrig automatisk med en tom tabel.
#'
#' @param data_dir Mappen, som indeholder `basis_varer.txt`.
#'
#' @return En liste med elementerne `varer` og `revision`.
#' @keywords internal
basis_varer_store_read <- function(data_dir = "./data") {
  paths <- .basis_varer_store_paths(data_dir)
  lock_handle <- .basis_varer_store_acquire_lock(
    paths$data_dir
  )
  on.exit(
    .basis_varer_store_release_lock(lock_handle),
    add = TRUE
  )

  .basis_varer_store_recover(paths, lock_handle)
  .basis_varer_store_snapshot_unlocked(paths)
}

#' Find revisionen for de gemte basisvarer
#'
#' Revisionen er en MD5-værdi af den faktiske CSV-fil. To snapshots med
#' samme revision har derfor identiske bytes på disken. Funktionen bruger
#' samme lås og recovery som en almindelig læsning, så den ikke observerer
#' filen midt i en store-transaktion.
#'
#' @param data_dir Mappen, som indeholder `basis_varer.txt`.
#'
#' @return Filens revision som én tekstværdi.
#' @keywords internal
basis_varer_store_revision <- function(data_dir = "./data") {
  paths <- .basis_varer_store_paths(data_dir)
  lock_handle <- .basis_varer_store_acquire_lock(
    paths$data_dir
  )
  on.exit(
    .basis_varer_store_release_lock(lock_handle),
    add = TRUE
  )

  .basis_varer_store_recover(paths, lock_handle)
  .basis_varer_store_revision_unlocked(paths$target)
}

#' Gem et nyt snapshot af basisvarerne
#'
#' Kandidaten valideres og skrives først til en stage-fil i samme mappe som
#' den eksisterende CSV. Den gamle fil flyttes derefter til en backup, før
#' stage-filen publiceres. Fejl før commit-markøren gendanner den oprindelige
#' fil. En forventet revision forhindrer, at en gammel browser-session
#' overskriver en nyere ændring.
#'
#' @param varer Den komplette nye tabel med basisvarer.
#' @param expected_revision Den revision, som kandidaten er bygget fra.
#' @param data_dir Mappen, som indeholder `basis_varer.txt`.
#' @param .fail_at Test-hook til en almindelig fejl ved et bestemt trin.
#' @param .crash_at Test-hook, som efterligner et processtop uden oprydning.
#'
#' @return Det faktisk gemte snapshot med `varer` og `revision`.
#' @keywords internal
basis_varer_store_commit <- function(
  varer,
  expected_revision,
  data_dir = "./data",
  .fail_at = NULL,
  .crash_at = NULL
) {
  if (
    missing(expected_revision) ||
      is.null(expected_revision) ||
      length(expected_revision) != 1L ||
      is.na(expected_revision)
  ) {
    stop(
      "En forventet basisvarerevision er påkrævet.",
      call. = FALSE
    )
  }

  candidate <- .basis_varer_store_normalize(varer)
  paths <- .basis_varer_store_paths(data_dir)
  lock_handle <- .basis_varer_store_acquire_lock(
    paths$data_dir
  )
  release_lock <- TRUE
  on.exit(
    if (release_lock) {
      .basis_varer_store_release_lock(lock_handle)
    },
    add = TRUE
  )

  initial_recovery <- .basis_varer_store_recover(
    paths,
    lock_handle
  )
  if (identical(
    initial_recovery,
    "committed_pending_cleanup"
  )) {
    stop(
      paste(
        "En tidligere basisvareændring er gemt,",
        "men oprydningen er endnu ikke afsluttet.",
        "Prøv handlingen igen om et øjeblik."
      ),
      call. = FALSE
    )
  }
  current_revision <- .basis_varer_store_revision_unlocked(
    paths$target
  )
  if (!identical(current_revision, expected_revision)) {
    .basis_varer_store_stop_conflict()
  }

  current_snapshot <- .basis_varer_store_snapshot_unlocked(paths)
  if (identical(candidate, current_snapshot$varer)) {
    return(current_snapshot)
  }

  committed_snapshot <- NULL
  marker_created <- FALSE
  commit_error <- tryCatch(
    {
      .basis_varer_store_touch_lock(lock_handle)
      .basis_varer_store_write_stage(candidate, paths$stage)
      .basis_varer_store_touch_lock(lock_handle)
      .basis_varer_store_checkpoint(
        .fail_at,
        "after_stage",
        .crash_at
      )

      .basis_varer_store_touch_lock(lock_handle)
      .basis_varer_store_move(
        paths$target,
        paths$backup,
        "flytte den eksisterende basisvarefil til backup"
      )
      .basis_varer_store_checkpoint(
        .fail_at,
        "after_backup",
        .crash_at
      )

      .basis_varer_store_touch_lock(lock_handle)
      .basis_varer_store_move(
        paths$stage,
        paths$target,
        "publicere den nye basisvarefil"
      )
      published <- .basis_varer_store_read_file(paths$target)
      if (!identical(published, candidate)) {
        stop(
          "Den publicerede basisvarefil svarer ikke til kandidaten.",
          call. = FALSE
        )
      }
      .basis_varer_store_checkpoint(
        .fail_at,
        "after_promote",
        .crash_at
      )

      .basis_varer_store_touch_lock(lock_handle)
      if (!file.create(paths$marker)) {
        stop(
          "Commit-markøren for basisvarerne kunne ikke oprettes.",
          call. = FALSE
        )
      }
      marker_created <- TRUE
      .basis_varer_store_checkpoint(
        .fail_at,
        "after_commit_marker",
        .crash_at
      )

      recovery_outcome <- .basis_varer_store_recover(
        paths,
        lock_handle
      )
      if (!recovery_outcome %in% c(
        "committed",
        "committed_pending_cleanup"
      )) {
        stop(
          "Basisvaretransaktionen blev ikke afsluttet som forventet.",
          call. = FALSE
        )
      }

      committed_snapshot <-
        .basis_varer_store_snapshot_unlocked(paths)
      NULL
    },
    error = identity
  )

  if (is.null(commit_error)) {
    return(committed_snapshot)
  }

  if (
    inherits(
      commit_error,
      "basis_varer_store_simulated_crash"
    )
  ) {
    release_lock <- FALSE
    .basis_varer_store_release_lock(lock_handle)
    stop(commit_error)
  }

  if (
    inherits(
      commit_error,
      "basis_varer_store_lock_lost"
    )
  ) {
    stop(commit_error)
  }

  recovery_result <- tryCatch(
    .basis_varer_store_recover(paths, lock_handle),
    error = identity
  )
  if (inherits(
    recovery_result,
    "basis_varer_store_lock_lost"
  )) {
    stop(recovery_result)
  }
  if (inherits(recovery_result, "error")) {
    stop(
      paste(
        conditionMessage(commit_error),
        "Rollback kunne ikke gennemføres fuldt:",
        conditionMessage(recovery_result)
      ),
      call. = FALSE
    )
  }

  if (
    marker_created &&
      recovery_result %in% c(
        "committed",
        "committed_pending_cleanup"
      )
  ) {
    return(.basis_varer_store_snapshot_unlocked(paths))
  }

  stop(commit_error)
}

#' Byg alle interne stier til basisvarelageret
#'
#' Alle midlertidige filer ligger ved siden af målfilen. Dermed foregår
#' omdøbningerne på samme filsystem, hvilket er nødvendigt for en sikker
#' publicering på Windows.
#'
#' @param data_dir Mappen, som indeholder basisvarefilen.
#'
#' @return En navngivet liste med normaliserede stier.
#' @keywords internal
.basis_varer_store_paths <- function(data_dir) {
  if (
    length(data_dir) != 1L ||
      is.na(data_dir) ||
      !nzchar(data_dir) ||
      !dir.exists(data_dir)
  ) {
    stop("Mappen til basisvarer findes ikke.", call. = FALSE)
  }

  data_dir <- normalizePath(
    data_dir,
    winslash = "/",
    mustWork = TRUE
  )

  list(
    data_dir = data_dir,
    target = file.path(data_dir, "basis_varer.txt"),
    stage = file.path(
      data_dir,
      ".basis-varer-store.stage"
    ),
    backup = file.path(
      data_dir,
      ".basis-varer-store.backup"
    ),
    marker = file.path(
      data_dir,
      ".basis-varer-store.committed"
    ),
    lock = file.path(
      data_dir,
      "basis-varer-lock.sqlite"
    )
  )
}

#' Validér og kanonisér en basisvaretabel
#'
#' Funktionen sikrer det faste schema, renser tekst, afviser dubletter og
#' ugyldige mængder samt sorterer rækkerne stabilt efter det normaliserede
#' varenavn. Det samme resultat bruges ved både læsning og skrivning.
#'
#' @param varer En kandidat til basisvaretabellen.
#'
#' @return Den validerede og kanoniske data frame.
#' @keywords internal
.basis_varer_store_normalize <- function(varer) {
  expected_columns <- c(
    "Indkobsliste",
    "maengde",
    "enhed",
    "kat_1",
    "kat_2"
  )
  if (
    !is.data.frame(varer) ||
      !identical(names(varer), expected_columns)
  ) {
    stop(
      paste0(
        "Basisvaretabellen skal have præcis kolonnerne: ",
        paste(expected_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  result <- varer[, expected_columns, drop = FALSE]
  result$Indkobsliste <- trimws(
    as.character(result$Indkobsliste)
  )
  invalid_names <- is.na(result$Indkobsliste) |
    !nzchar(result$Indkobsliste)
  if (any(invalid_names)) {
    stop("Alle basisvarer skal have et navn.", call. = FALSE)
  }

  normalized_names <- tolower(result$Indkobsliste)
  if (anyDuplicated(normalized_names)) {
    stop(
      "Den samme basisvare må ikke optræde flere gange.",
      call. = FALSE
    )
  }

  result$maengde <- suppressWarnings(
    as.numeric(as.character(result$maengde))
  )
  invalid_amounts <- is.na(result$maengde) |
    !is.finite(result$maengde) |
    result$maengde <= 0
  if (any(invalid_amounts)) {
    stop(
      "Alle basisvarer skal have en endelig mængde større end 0.",
      call. = FALSE
    )
  }

  text_columns <- c(
    "Indkobsliste",
    "enhed",
    "kat_1",
    "kat_2"
  )
  for (column_name in text_columns) {
    values <- as.character(result[[column_name]])
    values[is.na(values)] <- ""
    values <- trimws(values)
    if (any(grepl("[\r\n]", values))) {
      stop(
        sprintf(
          "Kolonnen '%s' må ikke indeholde linjeskift.",
          column_name
        ),
        call. = FALSE
      )
    }
    result[[column_name]] <- values
  }

  normalized_names <- tolower(result$Indkobsliste)
  result <- result[
    order(normalized_names, result$Indkobsliste),
    expected_columns,
    drop = FALSE
  ]
  rownames(result) <- NULL
  result
}

#' Læs og validér én basisvarefil
#'
#' Funktionen bevarer kolonnenavne og tekstværdier, hvorefter det fælles
#' normaliseringstrin kontrollerer hele tabellen.
#'
#' @param path Stien til CSV-filen.
#'
#' @return Den kanoniske basisvaretabel.
#' @keywords internal
.basis_varer_store_read_file <- function(path) {
  if (!file.exists(path) || isTRUE(file.info(path)$isdir)) {
    stop(
      "Basisvarefilen mangler.",
      call. = FALSE
    )
  }
  if (is.na(file.info(path)$size) || file.info(path)$size == 0L) {
    stop(
      "Basisvarefilen er tom.",
      call. = FALSE
    )
  }

  result <- read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
  .basis_varer_store_normalize(result)
}

#' Skriv og kontrollér store-transaktionens stage-fil
#'
#' Stage-filen genlæses efter skrivning. Dermed opdages blandt andet et
#' forkert schema eller en ufuldstændig skrivning, før den eksisterende fil
#' flyttes til backup.
#'
#' @param varer Den allerede kanoniske basisvaretabel.
#' @param path Stien til stage-filen.
#'
#' @return `NULL` usynligt.
#' @keywords internal
.basis_varer_store_write_stage <- function(varer, path) {
  write.csv(
    varer,
    file = path,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )

  staged <- .basis_varer_store_read_file(path)
  if (!identical(staged, varer)) {
    stop(
      "Stage-filen kunne ikke valideres efter skrivning.",
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Beregn filrevision uden at tage store-låsen
#'
#' Hjælperen må kun kaldes, mens den relevante lås allerede holdes.
#'
#' @param path Stien til den levende basisvarefil.
#'
#' @return Filens MD5-værdi.
#' @keywords internal
.basis_varer_store_revision_unlocked <- function(path) {
  if (!file.exists(path) || isTRUE(file.info(path)$isdir)) {
    stop("Basisvarefilen mangler.", call. = FALSE)
  }

  revision <- unname(md5sum(path))
  if (length(revision) != 1L || is.na(revision)) {
    stop(
      "Basisvarefilens revision kunne ikke beregnes.",
      call. = FALSE
    )
  }
  revision
}

#' Læs data og revision, mens store-låsen holdes
#'
#' @param paths Store-lagets interne stier.
#'
#' @return Et snapshot med `varer` og `revision`.
#' @keywords internal
.basis_varer_store_snapshot_unlocked <- function(paths) {
  list(
    varer = .basis_varer_store_read_file(paths$target),
    revision = .basis_varer_store_revision_unlocked(
      paths$target
    )
  )
}

#' Flyt en store-fil og kontrollér resultatet
#'
#' @param from Den eksisterende fil.
#' @param to Den nye sti.
#' @param description En letlæselig beskrivelse til en eventuel fejl.
#'
#' @return `NULL` usynligt.
#' @keywords internal
.basis_varer_store_move <- function(
  from,
  to,
  description
) {
  if (!file.rename(from, to)) {
    stop(
      sprintf("Kunne ikke %s.", description),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Fjern store-filer og kontrollér oprydningen
#'
#' @param paths De filer, der skal fjernes, hvis de findes.
#'
#' @return `NULL` usynligt.
#' @keywords internal
.basis_varer_store_remove_files <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing) == 0L) return(invisible(NULL))

  removed <- file.remove(existing)
  if (any(!removed)) {
    stop(
      sprintf(
        "Store-filer kunne ikke fjernes: %s",
        paste(basename(existing[!removed]), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Gendan eller afslut en afbrudt basisvaretransaktion
#'
#' Uden commit-markør gendannes en eventuel backup. Med markør beholdes den
#' validerede nye fil, og de gamle artefakter fjernes. Funktionen kaldes
#' under lås før både læsning og skrivning.
#'
#' @param paths Store-lagets interne stier.
#'
#' @param lock_handle Den lås, som beskytter recovery-forløbet.
#'
#' @return `"clean"`, `"rolled_back"`, `"committed"` eller
#'   `"committed_pending_cleanup"` usynligt.
#' @keywords internal
.basis_varer_store_recover <- function(
  paths,
  lock_handle = NULL
) {
  if (!is.null(lock_handle)) {
    .basis_varer_store_touch_lock(lock_handle)
  }

  marker_exists <- file.exists(paths$marker)
  backup_exists <- file.exists(paths$backup)

  if (marker_exists) {
    target_error <- tryCatch(
      {
        .basis_varer_store_read_file(paths$target)
        NULL
      },
      error = identity
    )
    if (is.null(target_error)) {
      return(
        invisible(
          .basis_varer_store_cleanup_committed(
            paths,
            lock_handle
          )
        )
      )
    }

    if (!backup_exists) {
      stop(
        paste(
          "Den markerede basisvarefil er ugyldig,",
          "og der findes ingen backup."
        ),
        call. = FALSE
      )
    }

    if (!is.null(lock_handle)) {
      .basis_varer_store_touch_lock(lock_handle)
    }
    .basis_varer_store_remove_files(paths$target)
    .basis_varer_store_move(
      paths$backup,
      paths$target,
      "gendanne basisvarefilens backup"
    )
    cleanup_error <- tryCatch(
      {
        if (!is.null(lock_handle)) {
          .basis_varer_store_touch_lock(lock_handle)
        }
        .basis_varer_store_remove_files(paths$stage)
        if (!is.null(lock_handle)) {
          .basis_varer_store_touch_lock(lock_handle)
        }
        .basis_varer_store_remove_files(paths$marker)
        NULL
      },
      error = identity
    )
    if (inherits(
      cleanup_error,
      "basis_varer_store_lock_lost"
    )) {
      stop(cleanup_error)
    }
    if (inherits(cleanup_error, "error")) {
      stop(
        paste(
          "Den tidligere basisvarefil er gendannet,",
          "men oprydningen er ikke afsluttet:",
          conditionMessage(cleanup_error)
        ),
        call. = FALSE
      )
    }
    stop(
      paste(
        "Den nye basisvarefil var ugyldig.",
        "Den tidligere fil er gendannet."
      ),
      call. = FALSE
    )
  }

  if (backup_exists) {
    if (!is.null(lock_handle)) {
      .basis_varer_store_touch_lock(lock_handle)
    }
    .basis_varer_store_remove_files(paths$target)
    .basis_varer_store_move(
      paths$backup,
      paths$target,
      "rulle basisvaretransaktionen tilbage"
    )
    .basis_varer_store_remove_files(paths$stage)
    .basis_varer_store_read_file(paths$target)
    return(invisible("rolled_back"))
  }

  if (!is.null(lock_handle)) {
    .basis_varer_store_touch_lock(lock_handle)
  }
  .basis_varer_store_remove_files(paths$stage)
  if (!file.exists(paths$target)) {
    stop(
      "Basisvarefilen mangler, og der findes ingen backup.",
      call. = FALSE
    )
  }

  invisible("clean")
}

#' Ryd op efter en varigt publiceret basisvaretransaktion
#'
#' Stage og backup fjernes før commit-markøren. Markøren fjernes altid sidst,
#' fordi den fortæller næste proces, at målfilen allerede er den nye,
#' committed version. En oprydningsfejl ændrer derfor ikke en vellykket
#' gemning til en fejl; næste læsning kan sikkert fortsætte oprydningen.
#'
#' @param paths Store-lagets interne stier.
#' @param lock_handle Den lås, som beskytter oprydningen.
#'
#' @return `"committed"` eller `"committed_pending_cleanup"`.
#' @keywords internal
.basis_varer_store_cleanup_committed <- function(
  paths,
  lock_handle = NULL
) {
  for (path in c(paths$stage, paths$backup, paths$marker)) {
    if (!is.null(lock_handle)) {
      .basis_varer_store_touch_lock(lock_handle)
    }

    cleanup_error <- tryCatch(
      {
        .basis_varer_store_remove_files(path)
        NULL
      },
      error = identity
    )
    if (inherits(
      cleanup_error,
      "basis_varer_store_lock_lost"
    )) {
      stop(cleanup_error)
    }
    if (inherits(cleanup_error, "error")) {
      return("committed_pending_cleanup")
    }
  }

  "committed"
}

#' Kontrollér basisvarelåsen mellem kritiske filtrin
#'
#' En lille SQLite-forespørgsel bekræfter, at forbindelsen og den eksklusive
#' OS-lås fortsat er aktive.
#'
#' @param lock_handle Den åbne SQLite-forbindelse og låsefilens sti.
#'
#' @return `TRUE` usynligt.
#' @keywords internal
.basis_varer_store_touch_lock <- function(lock_handle) {
  store_lock_touch(
    lock_handle,
    store_label = "basisvarelageret",
    lock_lost_class = "basis_varer_store_lock_lost"
  )
}

#' Tag den eksklusive basisvarelås
#'
#' En eksklusiv SQLite-transaktion bruger operativsystemets fillås. Låsen
#' frigives automatisk ved processtop og kan derfor ikke blive efterladt som
#' en gammel låsemappe.
#'
#' @param data_dir Mappen med basisvarelageret.
#' @param wait_seconds Hvor længe funktionen højst venter på en aktiv lås.
#'
#' @return Et låsehåndtag med forbindelse og sti.
#' @keywords internal
.basis_varer_store_acquire_lock <- function(
  data_dir,
  wait_seconds = 1
) {
  store_lock_acquire(
    lock_path = file.path(
      data_dir,
      "basis-varer-lock.sqlite"
    ),
    store_label = "basisvarelageret",
    lock_lost_class = "basis_varer_store_lock_lost",
    wait_seconds = wait_seconds
  )
}

#' Frigiv basisvarelåsen
#'
#' @param lock_handle Den åbne SQLite-forbindelse og låsefilens sti.
#'
#' @return `TRUE`, hvis forbindelsen blev lukket, ellers `FALSE`, usynligt.
#' @keywords internal
.basis_varer_store_release_lock <- function(lock_handle) {
  store_lock_release(
    lock_handle,
    store_label = "basisvarelageret"
  )
}

#' Stop med en genkendelig revisionskonflikt
#'
#' Den særskilte condition-klasse gør det muligt for Shiny-appen at
#' genindlæse det nyeste snapshot og lade brugeren prøve handlingen igen.
#'
#' @return Funktionen returnerer ikke.
#' @keywords internal
.basis_varer_store_stop_conflict <- function() {
  condition <- structure(
    list(
      message = paste(
        "Basisvarerne er ændret i en anden session."
      ),
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

#' Fremprovokér en kontrolleret store-fejl i tests
#'
#' En almindelig testfejl går gennem rollback. Et simuleret processtop
#' efterlader derimod transaktionsfiler, så næste læsning kan teste recovery.
#' I en rigtig afsluttet proces frigiver operativsystemet SQLite-låsen.
#'
#' @param fail_at Navnene på trin med almindelig testfejl.
#' @param step Det aktuelle store-trin.
#' @param crash_at Navnene på trin med simuleret processtop.
#'
#' @return `NULL` usynligt, hvis trinnet ikke skal fejle.
#' @keywords internal
.basis_varer_store_checkpoint <- function(
  fail_at,
  step,
  crash_at
) {
  if (!is.null(crash_at) && step %in% crash_at) {
    condition <- structure(
      list(
        message = sprintf(
          "Simuleret processtop ved basisvaretrinnet '%s'.",
          step
        ),
        call = NULL
      ),
      class = c(
        "basis_varer_store_simulated_crash",
        "error",
        "condition"
      )
    )
    stop(condition)
  }

  if (!is.null(fail_at) && step %in% fail_at) {
    stop(
      sprintf(
        "Testfejl ved basisvaretrinnet '%s'.",
        step
      ),
      call. = FALSE
    )
  }

  invisible(NULL)
}
