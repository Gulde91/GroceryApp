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
      ".basis-varer-store-lock"
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

#' Opret et unikt ejerskabstoken til basisvarelåsen
#'
#' Tokenet gør det muligt at skelne en gammel låseejer fra en ny proces, som
#' har overtaget den samme faste låsemappe.
#'
#' @return Et token som én tekstværdi.
#' @keywords internal
.basis_varer_store_new_lock_token <- function() {
  paste(
    Sys.getpid(),
    format(Sys.time(), "%Y%m%d%H%M%OS6"),
    basename(tempfile("basis-owner-")),
    sep = "-"
  )
}

#' Læs ejeren af en basisvarelås
#'
#' En manglende eller ufuldstændig ejerfil behandles som ukendt ejerskab. Det
#' gør også gamle låse fra tidligere appversioner mulige at overtage, når de
#' er blevet forældede.
#'
#' @param lock_path Stien til låsemappen.
#'
#' @return Ejerens token eller `NULL`, hvis det ikke kan læses.
#' @keywords internal
.basis_varer_store_lock_owner <- function(lock_path) {
  owner_path <- file.path(lock_path, "owner")
  if (!file.exists(owner_path)) return(NULL)

  owner <- tryCatch(
    readLines(
      owner_path,
      n = 1L,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    error = function(error) character()
  )
  if (length(owner) != 1L || !nzchar(owner[[1L]])) {
    return(NULL)
  }

  owner[[1L]]
}

#' Kontrollér at den aktuelle proces stadig ejer basisvarelåsen
#'
#' Hvis en langsom eller pauset proces har fået sin lås overtaget, må den ikke
#' længere flytte eller slette store-filer.
#'
#' @param lock_handle Låsens sti og ejerskabstoken.
#'
#' @return `TRUE` usynligt.
#' @keywords internal
.basis_varer_store_assert_lock_owner <- function(lock_handle) {
  valid_handle <- is.list(lock_handle) &&
    identical(
      sort(names(lock_handle)),
      c("path", "token")
    ) &&
    length(lock_handle$path) == 1L &&
    length(lock_handle$token) == 1L

  if (
    !valid_handle ||
      !dir.exists(lock_handle$path) ||
      !identical(
        .basis_varer_store_lock_owner(lock_handle$path),
        lock_handle$token
      )
  ) {
    .basis_varer_store_stop_lock_lost()
  }

  invisible(TRUE)
}

#' Forny basisvarelåsen og kontrollér ejerskabet
#'
#' Mappens tidsstempel holdes friskt ved de kritiske store-trin, så en aktiv
#' proces ikke fejlagtigt ligner en efterladt lås.
#'
#' @param lock_handle Låsens sti og ejerskabstoken.
#'
#' @return `TRUE` usynligt.
#' @keywords internal
.basis_varer_store_touch_lock <- function(lock_handle) {
  .basis_varer_store_assert_lock_owner(lock_handle)
  touched <- Sys.setFileTime(lock_handle$path, Sys.time())
  if (length(touched) != 1L || !isTRUE(touched)) {
    .basis_varer_store_stop_lock_lost()
  }
  .basis_varer_store_assert_lock_owner(lock_handle)
  invisible(TRUE)
}

#' Tag den eksklusive basisvarelås
#'
#' En mappe bruges som lås, fordi oprettelsen er atomisk på de understøttede
#' filsystemer. En efterladt lås kan overtages efter 30 sekunder.
#'
#' @param data_dir Mappen med basisvarelageret.
#' @param wait_seconds Hvor længe funktionen højst venter på en aktiv lås.
#' @param stale_after_seconds Hvornår en efterladt lås må overtages.
#'
#' @return Et låsehåndtag med sti og et unikt ejerskabstoken.
#' @keywords internal
.basis_varer_store_acquire_lock <- function(
  data_dir,
  wait_seconds = 1,
  stale_after_seconds = 30
) {
  lock_path <- file.path(
    data_dir,
    ".basis-varer-store-lock"
  )
  deadline <- Sys.time() + wait_seconds
  token <- .basis_varer_store_new_lock_token()

  repeat {
    if (dir.create(lock_path, showWarnings = FALSE)) {
      lock_handle <- list(
        path = lock_path,
        token = token
      )
      owner_error <- tryCatch(
        {
          writeLines(
            token,
            file.path(lock_path, "owner"),
            useBytes = TRUE
          )
          NULL
        },
        error = identity
      )
      if (
        inherits(owner_error, "error") ||
          !identical(
            .basis_varer_store_lock_owner(lock_path),
            token
          )
      ) {
        unlink(lock_path, recursive = TRUE, force = TRUE)
        stop(
          "Basisvarelåsens ejerskab kunne ikke registreres.",
          call. = FALSE
        )
      }
      .basis_varer_store_touch_lock(lock_handle)
      return(lock_handle)
    }

    lock_info <- file.info(lock_path)
    lock_age <- as.numeric(
      difftime(
        Sys.time(),
        lock_info$mtime,
        units = "secs"
      )
    )
    if (
      !is.na(lock_age) &&
        lock_age >= stale_after_seconds
    ) {
      observed_owner <- .basis_varer_store_lock_owner(
        lock_path
      )
      confirmed_info <- file.info(lock_path)
      confirmed_owner <- .basis_varer_store_lock_owner(
        lock_path
      )
      unchanged_lock <- dir.exists(lock_path) &&
        identical(observed_owner, confirmed_owner) &&
        identical(
          as.numeric(lock_info$mtime),
          as.numeric(confirmed_info$mtime)
        )
      if (unchanged_lock) {
        unlink(lock_path, recursive = TRUE, force = TRUE)
      }
      next
    }

    if (Sys.time() >= deadline) {
      stop(
        paste(
          "Basisvarelageret er i brug af en anden handling.",
          "Prøv igen om et øjeblik."
        ),
        call. = FALSE
      )
    }

    Sys.sleep(0.02)
  }
}

#' Frigiv basisvarelåsen
#'
#' Kun den ejer, der oprettede det aktuelle token, må fjerne låsemappen. En
#' ældre proces kan derfor ikke komme til at frigive en nyere ejers lås.
#'
#' @param lock_handle Låsens sti og ejerskabstoken.
#'
#' @return `TRUE`, hvis låsen blev fjernet, ellers `FALSE`, usynligt.
#' @keywords internal
.basis_varer_store_release_lock <- function(lock_handle) {
  valid_handle <- is.list(lock_handle) &&
    identical(
      sort(names(lock_handle)),
      c("path", "token")
    )
  if (!valid_handle || !dir.exists(lock_handle$path)) {
    return(invisible(FALSE))
  }
  if (!identical(
    .basis_varer_store_lock_owner(lock_handle$path),
    lock_handle$token
  )) {
    return(invisible(FALSE))
  }

  unlink(lock_handle$path, recursive = TRUE, force = TRUE)
  if (dir.exists(lock_handle$path)) {
    warning(
      "Basisvarelåsen kunne ikke frigives; den kan overtages automatisk senere.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }

  invisible(TRUE)
}

#' Stop når ejerskabet af basisvarelåsen er mistet
#'
#' Den særskilte condition-klasse sikrer, at en gammel proces afbryder uden
#' at forsøge rollback i filer, som en nyere låseejer kan være i gang med.
#'
#' @return Funktionen returnerer ikke.
#' @keywords internal
.basis_varer_store_stop_lock_lost <- function() {
  condition <- structure(
    list(
      message = paste(
        "Basisvarelåsens ejerskab blev mistet.",
        "Prøv handlingen igen."
      ),
      call = NULL
    ),
    class = c(
      "basis_varer_store_lock_lost",
      "error",
      "condition"
    )
  )
  stop(condition)
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
#' efterlader derimod lås og transaktionsfiler, så næste læsning kan teste
#' recovery-forløbet.
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
