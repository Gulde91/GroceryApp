# Holdbar lagring og analyse af indkøbshistorikken -------------------------
#
# Denne fil læser og gemmer historiske indkøbssedler som komplette snapshots.
# Den håndterer revisioner, låsning og gendannelse efter afbrudte gemninger og
# indeholder indtil videre også analyser af populære varer og opskriftsbrug.

library(tools)
library(DBI, exclude = "show")
library(RSQLite, exclude = "show")

#' Opret en tom, kanonisk indkøbshistorik
#'
#' Den faste kolonnerækkefølge og de faste typer gør, at resten af appen kan
#' bruge samme kontrakt, uanset om historikmappen mangler, er tom eller kun
#' indeholder filer, der ikke kan læses.
#'
#' @return En tom data frame med filnavn, dato, linjenummer og tekst.
#' @keywords internal
.shopping_history_empty_entries <- function() {
  data.frame(
    filename = character(),
    date = as.Date(character()),
    line_number = integer(),
    Indkøbsliste = character(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Find de filer, som tilhører indkøbshistorikken
#'
#' Kun det eksisterende navneformat accepteres. Andre filer i mappen bliver
#' dermed ikke opfattet som indkøbssedler ved en fejl.
#'
#' @param history_dir Mappen med gemte indkøbssedler.
#'
#' @return En alfabetisk sorteret tegnvektor med filnavne uden mappesti.
#' @keywords internal
.shopping_history_files <- function(history_dir) {
  if (
    length(history_dir) != 1L ||
      is.na(history_dir) ||
      !dir.exists(history_dir)
  ) {
    return(character())
  }

  sort(list.files(
    history_dir,
    pattern = "^indkobsseddel_[0-9]{8}\\.rda[0-9]*$",
    full.names = FALSE
  ))
}

#' Byg de faste stier til historiklagerets transaktionsfiler
#'
#' Stage, backup, journal og commit-markør ligger i samme mappe som de gemte
#' indkøbssedler. Derfor kan publiceringen ske som en omdøbning på det samme
#' filsystem, hvilket også er vigtigt på Windows.
#'
#' @param history_dir Mappen med gemte indkøbssedler.
#'
#' @return En navngivet liste med normaliserede stier.
#' @keywords internal
.shopping_history_store_paths <- function(history_dir) {
  if (
    length(history_dir) != 1L ||
      is.na(history_dir) ||
      !nzchar(history_dir) ||
      !dir.exists(history_dir)
  ) {
    stop(
      "Mappen til historiske indkøbssedler findes ikke.",
      call. = FALSE
    )
  }

  history_dir <- normalizePath(
    history_dir,
    winslash = "/",
    mustWork = TRUE
  )

  list(
    history_dir = history_dir,
    stage = file.path(
      history_dir,
      ".shopping-history-store.stage"
    ),
    backup = file.path(
      history_dir,
      ".shopping-history-store.backup"
    ),
    journal = file.path(
      history_dir,
      ".shopping-history-store-transaction.rds"
    ),
    journal_stage = file.path(
      history_dir,
      ".shopping-history-store-journal.stage"
    ),
    marker = file.path(
      history_dir,
      ".shopping-history-store-transaction.committed"
    ),
    marker_stage = file.path(
      history_dir,
      ".shopping-history-store-marker.stage"
    ),
    lock = file.path(
      history_dir,
      ".shopping-history-lock.sqlite"
    )
  )
}

#' Læs datoen fra et historikfilnavn
#'
#' Filnavnet valideres både mod navnemønsteret og som en rigtig kalenderdato.
#' Eksempelvis bliver en fil med datoen 30. februar derfor afvist.
#'
#' @param filename Filnavnet uden mappesti.
#'
#' @return Datoen som én `Date`-værdi.
#' @keywords internal
.shopping_history_date_from_filename <- function(filename) {
  if (
    length(filename) != 1L ||
      is.na(filename) ||
      !grepl(
        "^indkobsseddel_[0-9]{8}\\.rda[0-9]*$",
        basename(filename)
      )
  ) {
    stop("Historikfilens navn er ugyldigt.", call. = FALSE)
  }

  date_text <- sub(
    "^indkobsseddel_([0-9]{8})\\.rda[0-9]*$",
    "\\1",
    basename(filename)
  )
  parsed <- suppressWarnings(as.Date(date_text, format = "%Y%m%d"))
  if (
    length(parsed) != 1L ||
      is.na(parsed) ||
      !identical(format(parsed, "%Y%m%d"), date_text)
  ) {
    stop("Historikfilens navn indeholder ikke en gyldig dato.", call. = FALSE)
  }

  parsed
}

#' Valider den gemte data frame ved indlæsning
#'
#' Ældre filer må gerne indeholde ekstra kolonner, men de skal indeholde netop
#' én kolonne med navnet `Indkøbsliste`. Teksten normaliseres til character i
#' det kanoniske snapshot.
#'
#' @param history Objektet `df` fra en historikfil.
#'
#' @return Kolonnen `Indkøbsliste` som en tegnvektor.
#' @keywords internal
.shopping_history_lines_from_frame <- function(history) {
  if (
    !is.data.frame(history) ||
      sum(names(history) == "Indkøbsliste") != 1L
  ) {
    stop(
      "Historikfilens 'df' skal indeholde kolonnen 'Indkøbsliste'.",
      call. = FALSE
    )
  }

  lines <- history[["Indkøbsliste"]]
  if (!is.atomic(lines) || is.object(lines) && !is.factor(lines)) {
    stop(
      "Historikfilens kolonne 'Indkøbsliste' har en ugyldig type.",
      call. = FALSE
    )
  }

  as.character(lines)
}

#' Indlæs én historikfil i et isoleret miljø
#'
#' Det isolerede miljø forhindrer, at objektet `df` eller andre objekter fra en
#' gammel fil overskriver værdier i appens globale miljø.
#'
#' @param filename Filnavnet uden mappesti.
#' @param history_dir Mappen med historikfilen.
#'
#' @return Kanoniske rækker fra filen.
#' @keywords internal
.shopping_history_file_entries <- function(filename, history_dir) {
  date <- .shopping_history_date_from_filename(filename)
  loaded <- new.env(parent = emptyenv())
  load(file.path(history_dir, filename), envir = loaded)
  if (!exists("df", envir = loaded, inherits = FALSE)) {
    stop("Historikfilen indeholder ikke objektet 'df'.", call. = FALSE)
  }

  lines <- .shopping_history_lines_from_frame(
    get("df", envir = loaded, inherits = FALSE)
  )
  if (length(lines) == 0L) {
    return(.shopping_history_empty_entries())
  }

  data.frame(
    filename = rep(filename, length(lines)),
    date = rep(date, length(lines)),
    line_number = seq_along(lines),
    Indkøbsliste = lines,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Omsæt en fejl i én historikfil til et tomt resultat
#'
#' Callbacken bruges ved sikker indlæsning. En beskadiget fil skal ikke
#' forhindre de øvrige indkøbssedler i at blive læst.
#'
#' @param error Den fangede fejl fra indlæsningen.
#'
#' @return `NULL`, så filen kan udelades fra det samlede snapshot.
#' @keywords internal
.shopping_history_ignore_file_error <- function(error) {
  NULL
}

#' Indlæs én historikfil uden at stoppe hele læsningen
#'
#' Alle fejl og advarsler fra netop denne fil undertrykkes. Det omfatter blandt
#' andet korrupte RData-filer, forkert objektnavn og ugyldige datoer.
#'
#' @param filename Filnavnet uden mappesti.
#' @param history_dir Mappen med historikfilen.
#'
#' @return Kanoniske rækker eller `NULL`, hvis filen ikke kan bruges.
#' @keywords internal
.shopping_history_file_entries_safe <- function(filename, history_dir) {
  suppressWarnings(
    tryCatch(
      .shopping_history_file_entries(filename, history_dir),
      error = .shopping_history_ignore_file_error
    )
  )
}

#' Beregn en sikker hash for én fil
#'
#' En manglende fil får den faste værdi `"<missing>"`. Andre filer skal kunne
#' hashes entydigt; ellers stoppes handlingen, så lageret ikke fortsætter på
#' et usikkert grundlag.
#'
#' @param path Stien til filen.
#'
#' @return En MD5-værdi eller `"<missing>"`.
#' @keywords internal
.shopping_history_store_file_hash <- function(path) {
  if (!file.exists(path)) {
    return("<missing>")
  }
  if (isTRUE(file.info(path)$isdir)) {
    stop(
      sprintf("Historikstien '%s' er ikke en fil.", basename(path)),
      call. = FALSE
    )
  }

  hash <- suppressWarnings(unname(md5sum(path)))
  if (
    length(hash) != 1L ||
      is.na(hash) ||
      !nzchar(hash)
  ) {
    stop(
      sprintf(
        "Historikfilen '%s' kunne ikke kontrolleres.",
        basename(path)
      ),
      call. = FALSE
    )
  }

  hash
}

#' Beregn historikrevisionen uden at tage fillåsen
#'
#' Hjælperen må kun bruges, mens den fælles historiklås allerede holdes, eller
#' når mappen med sikkerhed ikke kan blive ændret samtidig.
#'
#' @param history_dir Mappen med gemte indkøbssedler.
#' @param files De allerede sorterede historikfilnavne.
#'
#' @return Revisionen som én character-værdi.
#' @keywords internal
.shopping_history_store_revision_unlocked <- function(
  history_dir,
  files = .shopping_history_files(history_dir)
) {
  if (length(files) == 0L) {
    return("empty")
  }

  hashes <- vapply(
    file.path(history_dir, files),
    .shopping_history_store_file_hash,
    character(1)
  )

  paste(
    paste(files, unname(hashes), sep = "="),
    collapse = "|"
  )
}

#' Læs historik og revision som ét låst snapshot
#'
#' De samme sorterede filnavne bruges til både data og revision. Dermed kan
#' snapshotets revision ikke komme fra en anden filliste end de publicerede
#' historikrækker.
#'
#' @param history_dir Mappen med gemte indkøbssedler.
#'
#' @return En liste med præcis elementerne `entries` og `revision`.
#' @keywords internal
.shopping_history_store_snapshot_unlocked <- function(history_dir) {
  files <- .shopping_history_files(history_dir)
  rows <- vector("list", length(files))

  if (length(files) > 0L) {
    for (index in seq_along(files)) {
      rows[[index]] <- .shopping_history_file_entries_safe(
        files[[index]],
        history_dir
      )
    }
  }

  rows <- Filter(.shopping_history_has_rows, rows)
  entries <- if (length(rows) == 0L) {
    .shopping_history_empty_entries()
  } else {
    result <- do.call(rbind, rows)
    row.names(result) <- NULL
    result
  }

  list(
    entries = entries,
    revision = .shopping_history_store_revision_unlocked(
      history_dir,
      files
    )
  )
}

#' Flyt en transaktionsfil og kontrollér resultatet
#'
#' @param from Den eksisterende fil.
#' @param to Filens nye sti.
#' @param description En letlæselig beskrivelse til en eventuel fejl.
#'
#' @return `NULL` usynligt.
#' @keywords internal
.shopping_history_store_move <- function(
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

#' Fjern transaktionsfiler og kontrollér oprydningen
#'
#' @param paths De filer, som skal fjernes, hvis de findes.
#'
#' @return `NULL` usynligt.
#' @keywords internal
.shopping_history_store_remove_files <- function(paths) {
  existing <- unique(paths[file.exists(paths)])
  if (length(existing) == 0L) {
    return(invisible(NULL))
  }

  removed <- file.remove(existing)
  if (any(!removed)) {
    stop(
      sprintf(
        "Historiklagerets midlertidige filer kunne ikke fjernes: %s",
        paste(basename(existing[!removed]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Læs en ny-format historikfil med den strenge skrivekontrakt
#'
#' En fil skrevet af det nye lager skal kun indeholde objektet `df`, og dette
#' objekt skal have præcis den samme form som en kandidat til gemning.
#'
#' @param path Stien til historikfilen.
#'
#' @return Den validerede data frame `df`.
#' @keywords internal
.shopping_history_store_read_saved_frame <- function(path) {
  if (!file.exists(path) || isTRUE(file.info(path)$isdir)) {
    stop(
      sprintf("Historikfilen '%s' mangler.", basename(path)),
      call. = FALSE
    )
  }

  loaded <- new.env(parent = emptyenv())
  loaded_names <- load(path, envir = loaded)
  if (!identical(loaded_names, "df")) {
    stop(
      paste(
        "En ny historikfil skal kun indeholde objektet 'df'."
      ),
      call. = FALSE
    )
  }

  .shopping_history_validate_save_frame(
    get("df", envir = loaded, inherits = FALSE)
  )
}

#' Skriv og genlæs historikkens stage-fil
#'
#' Den eksisterende historik røres ikke, før stage-filen både kan indlæses og
#' er identisk med kandidaten. Objektets navn forbliver `df`, så det gamle
#' `.rda`-format bevares.
#'
#' @param history_df Den allerede validerede kandidat.
#' @param path Stien til stage-filen.
#'
#' @return `NULL` usynligt.
#' @keywords internal
.shopping_history_store_write_stage <- function(
  history_df,
  path
) {
  df <- history_df
  save(df, file = path)

  staged <- .shopping_history_store_read_saved_frame(path)
  if (!identical(staged, history_df)) {
    stop(
      "Historikkens stage-fil svarer ikke til kandidaten.",
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Kontrollér indholdet af en transaktionsjournal
#'
#' Journalen må kun pege på dagens normale historikfil i den allerede
#' validerede historikmappe. Det forhindrer en beskadiget journal i at få
#' recovery til at flytte eller slette en uvedkommende fil.
#'
#' @param journal Det indlæste journalobjekt.
#' @param paths Historiklagerets faste stier.
#'
#' @return Journalen uændret efter validering.
#' @keywords internal
.shopping_history_store_validate_journal <- function(
  journal,
  paths
) {
  expected_names <- c(
    "version",
    "target_filename",
    "had_original",
    "original_hash",
    "candidate_hash"
  )
  valid <- is.list(journal) &&
    identical(names(journal), expected_names) &&
    identical(journal$version, 1L) &&
    is.character(journal$target_filename) &&
    length(journal$target_filename) == 1L &&
    !is.na(journal$target_filename) &&
    grepl(
      "^indkobsseddel_[0-9]{8}\\.rda$",
      journal$target_filename
    ) &&
    identical(
      basename(journal$target_filename),
      journal$target_filename
    ) &&
    is.logical(journal$had_original) &&
    length(journal$had_original) == 1L &&
    !is.na(journal$had_original) &&
    is.character(journal$original_hash) &&
    length(journal$original_hash) == 1L &&
    !is.na(journal$original_hash) &&
    is.character(journal$candidate_hash) &&
    length(journal$candidate_hash) == 1L &&
    !is.na(journal$candidate_hash) &&
    grepl("^[[:xdigit:]]{32}$", journal$candidate_hash)

  if (!isTRUE(valid)) {
    stop(
      "Historiklagerets transaktionsjournal er ugyldig.",
      call. = FALSE
    )
  }

  date_error <- tryCatch(
    {
      .shopping_history_date_from_filename(
        journal$target_filename
      )
      NULL
    },
    error = identity
  )
  original_hash_valid <- if (isTRUE(journal$had_original)) {
    grepl("^[[:xdigit:]]{32}$", journal$original_hash)
  } else {
    identical(journal$original_hash, "<missing>")
  }
  target_path <- normalizePath(
    file.path(paths$history_dir, journal$target_filename),
    winslash = "/",
    mustWork = FALSE
  )
  expected_target <- file.path(
    paths$history_dir,
    journal$target_filename
  )

  if (
    inherits(date_error, "error") ||
      !isTRUE(original_hash_valid) ||
      !identical(target_path, expected_target)
  ) {
    stop(
      "Historiklagerets transaktionsjournal er ugyldig.",
      call. = FALSE
    )
  }

  journal
}

#' Skriv transaktionsjournalen via en kontrolleret stage-fil
#'
#' Journalen oprettes før den levende historikfil flyttes. Hvis processen
#' stopper derefter, ved næste læsning derfor præcis hvilken fil der skal
#' gendannes eller fjernes.
#'
#' @param journal Journalens validerede indhold.
#' @param paths Historiklagerets faste stier.
#'
#' @return `NULL` usynligt.
#' @keywords internal
.shopping_history_store_write_journal <- function(
  journal,
  paths
) {
  journal <- .shopping_history_store_validate_journal(
    journal,
    paths
  )
  saveRDS(journal, paths$journal_stage, version = 2)

  staged <- tryCatch(
    readRDS(paths$journal_stage),
    error = identity
  )
  if (
    inherits(staged, "error") ||
      !identical(staged, journal)
  ) {
    stop(
      "Historiklagerets transaktionsjournal kunne ikke valideres.",
      call. = FALSE
    )
  }

  .shopping_history_store_move(
    paths$journal_stage,
    paths$journal,
    "publicere historiklagerets transaktionsjournal"
  )
  published <- tryCatch(
    readRDS(paths$journal),
    error = identity
  )
  if (
    inherits(published, "error") ||
      !identical(published, journal)
  ) {
    stop(
      "Historiklagerets publicerede transaktionsjournal er ugyldig.",
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Læs og validér den aktive transaktionsjournal
#'
#' @param paths Historiklagerets faste stier.
#'
#' @return Den validerede journal.
#' @keywords internal
.shopping_history_store_read_journal <- function(paths) {
  journal <- tryCatch(
    readRDS(paths$journal),
    error = function(error) {
      stop(
        paste(
          "Historiklagerets transaktionsjournal kan ikke læses:",
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )

  .shopping_history_store_validate_journal(journal, paths)
}

#' Opret commit-markøren via en kontrolleret stage-fil
#'
#' Markøren skrives først efter den nye historikfil er publiceret og
#' genkontrolleret. Dens tilstedeværelse betyder derfor, at recovery skal
#' beholde den nye fil og kun afslutte oprydningen.
#'
#' @param paths Historiklagerets faste stier.
#'
#' @return `NULL` usynligt.
#' @keywords internal
.shopping_history_store_mark_committed <- function(paths) {
  writeLines("committed", paths$marker_stage, useBytes = TRUE)
  marker_text <- readLines(
    paths$marker_stage,
    n = 1L,
    warn = FALSE,
    encoding = "UTF-8"
  )
  if (!identical(marker_text, "committed")) {
    stop(
      "Historiklagerets commit-markør kunne ikke valideres.",
      call. = FALSE
    )
  }

  .shopping_history_store_move(
    paths$marker_stage,
    paths$marker,
    "oprette historiklagerets commit-markør"
  )

  invisible(NULL)
}

#' Stop når SQLite-låsen til historikken er mistet
#'
#' Den særskilte fejlklasse sikrer, at en handling stopper uden rollback, hvis
#' forbindelsen til den OS-understøttede lås ikke længere er gyldig.
#'
#' @return Funktionen returnerer ikke.
#' @keywords internal
.shopping_history_store_stop_lock_lost <- function() {
  condition <- structure(
    list(
      message = paste(
        "Låsen til indkøbshistorikken blev mistet.",
        "Prøv handlingen igen."
      ),
      call = NULL
    ),
    class = c(
      "shopping_history_store_lock_lost",
      "error",
      "condition"
    )
  )
  stop(condition)
}

#' Kontrollér at SQLite-forbindelsen stadig holder historiklåsen
#'
#' SQLite bruger operativsystemets fillås. Låsen frigives automatisk, hvis
#' processen stopper, og kan derfor ikke blive efterladt som en gammel
#' låsemappe, der senere skal stjæles.
#'
#' @param lock_handle Den åbne SQLite-forbindelse og låsefilens sti.
#'
#' @return `TRUE` usynligt.
#' @keywords internal
.shopping_history_store_assert_lock_owner <- function(
  lock_handle
) {
  valid_handle <- is.list(lock_handle) &&
    identical(
      sort(names(lock_handle)),
      c("connection", "path")
    ) &&
    length(lock_handle$path) == 1L &&
    !is.na(lock_handle$path) &&
    inherits(lock_handle$connection, "DBIConnection") &&
    isTRUE(dbIsValid(lock_handle$connection))

  if (!isTRUE(valid_handle)) {
    .shopping_history_store_stop_lock_lost()
  }

  invisible(TRUE)
}

#' Kontrollér den aktive SQLite-lås mellem kritiske filtrin
#'
#' En lille forespørgsel bekræfter, at forbindelsen fortsat er brugbar.
#' Selve den eksklusive transaktion forbliver åben indtil release.
#'
#' @param lock_handle Den åbne SQLite-forbindelse og låsefilens sti.
#'
#' @return `TRUE` usynligt.
#' @keywords internal
.shopping_history_store_touch_lock <- function(lock_handle) {
  .shopping_history_store_assert_lock_owner(lock_handle)
  probe <- tryCatch(
    dbGetQuery(
      lock_handle$connection,
      "SELECT 1 AS lock_is_alive"
    ),
    error = identity
  )
  if (
    inherits(probe, "error") ||
      !identical(probe$lock_is_alive, 1L)
  ) {
    .shopping_history_store_stop_lock_lost()
  }

  invisible(TRUE)
}

#' Tag den eksklusive OS-understøttede historiklås
#'
#' En eksklusiv SQLite-transaktion fungerer som fillås på både Windows og
#' Unix. Operativsystemet frigiver låsen ved et processtop, mens SQLite
#' serialiserer samtidige sessioner uden en sårbar stale-lock-overtagelse.
#'
#' @param history_dir Mappen med historiklageret.
#' @param wait_seconds Hvor længe der højst ventes på en aktiv lås.
#'
#' @return Et låsehåndtag med forbindelse og sti.
#' @keywords internal
.shopping_history_store_acquire_lock <- function(
  history_dir,
  wait_seconds = 1
) {
  valid_wait <- is.numeric(wait_seconds) &&
    length(wait_seconds) == 1L &&
    !is.na(wait_seconds) &&
    is.finite(wait_seconds) &&
    wait_seconds >= 0 &&
    wait_seconds <= 3600
  if (!isTRUE(valid_wait)) {
    stop(
      "Ventetiden for historiklåsen er ugyldig.",
      call. = FALSE
    )
  }

  lock_path <- file.path(
    history_dir,
    ".shopping-history-lock.sqlite"
  )
  connection <- tryCatch(
    dbConnect(
      SQLite(),
      lock_path,
      synchronous = NULL
    ),
    error = identity
  )
  if (inherits(connection, "error")) {
    stop(
      paste(
        "Historiklåsens database kunne ikke åbnes:",
        conditionMessage(connection)
      ),
      call. = FALSE
    )
  }

  wait_milliseconds <- as.integer(
    ceiling(wait_seconds * 1000)
  )
  lock_error <- tryCatch(
    {
      dbExecute(
        connection,
        paste0(
          "PRAGMA busy_timeout = ",
          wait_milliseconds
        )
      )
      dbExecute(
        connection,
        "BEGIN EXCLUSIVE TRANSACTION"
      )
      NULL
    },
    error = identity
  )
  if (inherits(lock_error, "error")) {
    try(
      dbDisconnect(connection),
      silent = TRUE
    )
    lock_message <- conditionMessage(lock_error)
    if (grepl(
      "locked|busy",
      lock_message,
      ignore.case = TRUE
    )) {
      stop(
        paste(
          "Indkøbshistorikken er i brug af en anden handling.",
          "Prøv igen om et øjeblik."
        ),
        call. = FALSE
      )
    }
    stop(
      paste(
        "Historiklåsen kunne ikke oprettes:",
        lock_message
      ),
      call. = FALSE
    )
  }

  lock_handle <- list(
    connection = connection,
    path = lock_path
  )
  lock_ready <- FALSE
  on.exit(
    if (!lock_ready) {
      .shopping_history_store_release_lock(lock_handle)
    },
    add = TRUE
  )
  .shopping_history_store_touch_lock(lock_handle)
  lock_ready <- TRUE
  lock_handle
}

#' Frigiv den eksklusive SQLite-lås
#'
#' Transaktionen rulles tilbage, fordi låsedatabasen ikke indeholder
#' forretningsdata. Forbindelsen lukkes derefter, så operativsystemets fillås
#' frigives med det samme.
#'
#' @param lock_handle Den åbne SQLite-forbindelse og låsefilens sti.
#'
#' @return `TRUE`, hvis forbindelsen blev lukket, ellers `FALSE`, usynligt.
#' @keywords internal
.shopping_history_store_release_lock <- function(lock_handle) {
  valid_handle <- is.list(lock_handle) &&
    identical(
      sort(names(lock_handle)),
      c("connection", "path")
    ) &&
    inherits(lock_handle$connection, "DBIConnection") &&
    isTRUE(dbIsValid(lock_handle$connection))
  if (!isTRUE(valid_handle)) {
    return(invisible(FALSE))
  }

  try(
    dbExecute(
      lock_handle$connection,
      "ROLLBACK"
    ),
    silent = TRUE
  )
  disconnect_error <- tryCatch(
    {
      dbDisconnect(lock_handle$connection)
      NULL
    },
    error = identity
  )
  if (inherits(disconnect_error, "error")) {
    warning(
      paste(
        "Historiklåsens forbindelse kunne ikke lukkes:",
        conditionMessage(disconnect_error)
      ),
      call. = FALSE
    )
    return(invisible(FALSE))
  }

  invisible(TRUE)
}

#' Stop med en genkendelig revisionskonflikt
#'
#' Fejlklassen gør det muligt for appen at genindlæse det nyeste snapshot,
#' uden at andre typer lagringsfejl bliver skjult.
#'
#' @return Funktionen returnerer ikke.
#' @keywords internal
.shopping_history_store_stop_conflict <- function() {
  condition <- structure(
    list(
      message = paste(
        "Indkøbshistorikken er ændret i en anden session."
      ),
      call = NULL
    ),
    class = c(
      "shopping_history_store_conflict",
      "error",
      "condition"
    )
  )
  stop(condition)
}

#' Fremprovokér en kontrolleret historikfejl i tests
#'
#' En almindelig testfejl går gennem rollback. Et simuleret processtop
#' efterlader derimod lås og transaktionsfiler, så næste læsning kan afprøve
#' recovery-forløbet.
#'
#' @param fail_at Navnene på trin med en almindelig testfejl.
#' @param step Det aktuelle transaktionstrin.
#' @param crash_at Navnene på trin med et simuleret processtop.
#'
#' @return `NULL` usynligt, hvis trinnet ikke skal fejle.
#' @keywords internal
.shopping_history_store_checkpoint <- function(
  fail_at,
  step,
  crash_at
) {
  if (!is.null(crash_at) && step %in% crash_at) {
    condition <- structure(
      list(
        message = sprintf(
          "Simuleret processtop ved historiktrinnet '%s'.",
          step
        ),
        call = NULL
      ),
      class = c(
        "shopping_history_store_simulated_crash",
        "error",
        "condition"
      )
    )
    stop(condition)
  }

  if (!is.null(fail_at) && step %in% fail_at) {
    stop(
      sprintf(
        "Testfejl ved historiktrinnet '%s'.",
        step
      ),
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Ryd op efter en varigt publiceret historiktransaktion
#'
#' Stage og backup fjernes før journalen, og commit-markøren fjernes altid
#' sidst. Hvis oprydningen stopper undervejs, fortæller journal og markør den
#' næste læsning, at den nye historikfil allerede skal beholdes.
#'
#' @param paths Historiklagerets faste stier.
#' @param lock_handle Den lås, som beskytter oprydningen.
#'
#' @return `"committed"` eller `"committed_pending_cleanup"`.
#' @keywords internal
.shopping_history_store_cleanup_committed <- function(
  paths,
  lock_handle
) {
  cleanup_paths <- c(
    paths$stage,
    paths$backup,
    paths$journal_stage,
    paths$marker_stage,
    paths$journal,
    paths$marker
  )

  for (path in cleanup_paths) {
    .shopping_history_store_touch_lock(lock_handle)
    cleanup_error <- tryCatch(
      {
        .shopping_history_store_remove_files(path)
        NULL
      },
      error = identity
    )
    if (inherits(
      cleanup_error,
      "shopping_history_store_lock_lost"
    )) {
      stop(cleanup_error)
    }
    if (inherits(cleanup_error, "error")) {
      return("committed_pending_cleanup")
    }
  }

  "committed"
}

#' Rul en uafsluttet historiktransaktion tilbage
#'
#' Hvis dagens fil fandtes før transaktionen, gendannes dens oprindelige bytes
#' fra backup. Hvis datoen var ny, fjernes en eventuelt publiceret kandidat.
#' Ukendte filbytes afvises som udgangspunkt, så recovery ikke overskriver en
#' mulig ekstern ændring.
#'
#' @param journal Den validerede transaktionsjournal.
#' @param paths Historiklagerets faste stier.
#' @param lock_handle Den lås, som beskytter rollback.
#' @param allow_unknown_target Om et ugyldigt markeret target må erstattes.
#'
#' @return `"rolled_back"` usynligt.
#' @keywords internal
.shopping_history_store_rollback_transaction <- function(
  journal,
  paths,
  lock_handle,
  allow_unknown_target = FALSE
) {
  target <- file.path(
    paths$history_dir,
    journal$target_filename
  )
  backup_exists <- file.exists(paths$backup)
  target_exists <- file.exists(target)

  if (isTRUE(journal$had_original)) {
    if (backup_exists) {
      backup_hash <- .shopping_history_store_file_hash(
        paths$backup
      )
      if (!identical(backup_hash, journal$original_hash)) {
        stop(
          paste(
            "Historikkens backup svarer ikke til den oprindelige fil.",
            "Rollback er stoppet uden at overskrive flere data."
          ),
          call. = FALSE
        )
      }

      if (target_exists) {
        target_hash <- .shopping_history_store_file_hash(target)
        known_target <- target_hash %in% c(
          journal$original_hash,
          journal$candidate_hash
        )
        if (
          !isTRUE(allow_unknown_target) &&
            !isTRUE(known_target)
        ) {
          stop(
            paste(
              "Dagens historikfil har ukendt indhold.",
              "Rollback er stoppet uden at overskrive filen."
            ),
            call. = FALSE
          )
        }
        .shopping_history_store_touch_lock(lock_handle)
        .shopping_history_store_remove_files(target)
      }

      .shopping_history_store_touch_lock(lock_handle)
      .shopping_history_store_move(
        paths$backup,
        target,
        "gendanne den tidligere historikfil"
      )
      if (!identical(
        .shopping_history_store_file_hash(target),
        journal$original_hash
      )) {
        stop(
          "Den gendannede historikfil kunne ikke verificeres.",
          call. = FALSE
        )
      }
    } else {
      if (
        !target_exists ||
          !identical(
            .shopping_history_store_file_hash(target),
            journal$original_hash
          )
      ) {
        stop(
          paste(
            "Den oprindelige historikfil kan ikke identificeres,",
            "og der findes ingen brugbar backup."
          ),
          call. = FALSE
        )
      }
    }
  } else {
    if (backup_exists) {
      stop(
        paste(
          "Historiklageret fandt en uventet backup.",
          "Recovery er stoppet uden at overskrive data."
        ),
        call. = FALSE
      )
    }

    if (target_exists) {
      target_hash <- .shopping_history_store_file_hash(target)
      if (
        !isTRUE(allow_unknown_target) &&
          !identical(target_hash, journal$candidate_hash)
      ) {
        stop(
          paste(
            "Den nye historikfil har ukendt indhold.",
            "Rollback er stoppet uden at overskrive filen."
          ),
          call. = FALSE
        )
      }
      .shopping_history_store_touch_lock(lock_handle)
      .shopping_history_store_remove_files(target)
    }
  }

  for (path in c(
    paths$stage,
    paths$journal_stage,
    paths$marker_stage,
    paths$journal,
    paths$marker
  )) {
    .shopping_history_store_touch_lock(lock_handle)
    .shopping_history_store_remove_files(path)
  }

  invisible("rolled_back")
}

#' Gendan eller afslut en afbrudt historiktransaktion
#'
#' Uden commit-markør rulles ændringen tilbage. Med commit-markør beholdes
#' den verificerede nye fil, og kun transaktionsfilerne ryddes. Funktionen
#' kaldes under lås før både læsning, revisionsberegning og gemning.
#'
#' @param paths Historiklagerets faste stier.
#' @param lock_handle Den lås, som beskytter recovery.
#'
#' @return `"clean"`, `"rolled_back"`, `"committed"` eller
#'   `"committed_pending_cleanup"`.
#' @keywords internal
.shopping_history_store_recover <- function(
  paths,
  lock_handle
) {
  .shopping_history_store_touch_lock(lock_handle)
  journal_exists <- file.exists(paths$journal)
  marker_exists <- file.exists(paths$marker)
  backup_exists <- file.exists(paths$backup)

  if (!journal_exists) {
    if (backup_exists) {
      stop(
        paste(
          "Historiklageret fandt en backup uden en læsbar journal.",
          "Ingen data er blevet overskrevet."
        ),
        call. = FALSE
      )
    }

    for (path in c(
      paths$stage,
      paths$journal_stage,
      paths$marker_stage
    )) {
      .shopping_history_store_touch_lock(lock_handle)
      .shopping_history_store_remove_files(path)
    }

    if (marker_exists) {
      marker_error <- tryCatch(
        {
          .shopping_history_store_touch_lock(lock_handle)
          .shopping_history_store_remove_files(paths$marker)
          NULL
        },
        error = identity
      )
      if (inherits(
        marker_error,
        "shopping_history_store_lock_lost"
      )) {
        stop(marker_error)
      }
      if (inherits(marker_error, "error")) {
        return("committed_pending_cleanup")
      }
      return("committed")
    }

    return("clean")
  }

  journal <- .shopping_history_store_read_journal(paths)
  target <- file.path(
    paths$history_dir,
    journal$target_filename
  )

  if (marker_exists) {
    target_error <- tryCatch(
      {
        .shopping_history_store_read_saved_frame(target)
        if (!identical(
          .shopping_history_store_file_hash(target),
          journal$candidate_hash
        )) {
          stop(
            "Den markerede historikfil har ændrede bytes.",
            call. = FALSE
          )
        }
        NULL
      },
      error = identity
    )

    if (is.null(target_error)) {
      return(
        .shopping_history_store_cleanup_committed(
          paths,
          lock_handle
        )
      )
    }

    rollback_error <- tryCatch(
      {
        .shopping_history_store_rollback_transaction(
          journal,
          paths,
          lock_handle,
          allow_unknown_target = TRUE
        )
        NULL
      },
      error = identity
    )
    if (inherits(
      rollback_error,
      "shopping_history_store_lock_lost"
    )) {
      stop(rollback_error)
    }
    if (inherits(rollback_error, "error")) {
      stop(
        paste(
          "Den markerede historikfil er ugyldig, og den tidligere",
          "version kunne ikke gendannes:",
          conditionMessage(rollback_error)
        ),
        call. = FALSE
      )
    }

    stop(
      paste(
        "Den nye historikfil var ugyldig.",
        "Den tidligere version er gendannet."
      ),
      call. = FALSE
    )
  }

  .shopping_history_store_rollback_transaction(
    journal,
    paths,
    lock_handle
  )
}

#' Beregn revisionen for indkøbshistorikkens filer
#'
#' Revisionen indeholder filnavn og MD5-værdi for hver fil, som følger det
#' kanoniske navnemønster. Den ændrer sig derfor, når en relevant fil bliver
#' tilføjet, fjernet, omdøbt eller får ændret sit indhold. Uvedkommende filer
#' påvirker ikke revisionen. En manglende mappe giver revisionen `"empty"`.
#' For en eksisterende mappe tager funktionen fillåsen og kan færdiggøre
#' recovery efter en tidligere afbrudt gemning, før revisionen beregnes.
#'
#' @param history_dir Mappen med gemte indkøbssedler.
#'
#' @return Revisionen som præcis én character-værdi.
#' @keywords internal
shopping_history_store_revision <- function(
  history_dir = "./data/indkobssedler"
) {
  if (
    length(history_dir) != 1L ||
      is.na(history_dir) ||
      !nzchar(history_dir) ||
      !dir.exists(history_dir)
  ) {
    return("empty")
  }

  paths <- .shopping_history_store_paths(history_dir)
  lock_handle <- .shopping_history_store_acquire_lock(
    paths$history_dir
  )
  on.exit(
    .shopping_history_store_release_lock(lock_handle),
    add = TRUE
  )

  .shopping_history_store_recover(paths, lock_handle)
  .shopping_history_store_revision_unlocked(
    paths$history_dir
  )
}

#' Læs et kanonisk snapshot af indkøbshistorikken
#'
#' Funktionen læser kun historikfiler med det eksisterende navneformat og
#' samler deres linjer i én data frame. Rækkefølgen inde i hver fil bevares.
#' En enkelt ugyldig eller beskadiget fil bliver sprunget over uden at blokere
#' resten af historikken. En manglende mappe giver et tomt snapshot. Ved en
#' eksisterende mappe kan læsningen også rulle en uafsluttet transaktion
#' tilbage eller afslutte oprydningen efter en committed transaktion.
#'
#' @param history_dir Mappen med gemte indkøbssedler.
#'
#' @return En liste med præcis elementerne `entries` og `revision`.
#' @keywords internal
shopping_history_store_read <- function(
  history_dir = "./data/indkobssedler"
) {
  if (
    length(history_dir) != 1L ||
      is.na(history_dir) ||
      !nzchar(history_dir) ||
      !dir.exists(history_dir)
  ) {
    return(list(
      entries = .shopping_history_empty_entries(),
      revision = "empty"
    ))
  }

  paths <- .shopping_history_store_paths(history_dir)
  lock_handle <- .shopping_history_store_acquire_lock(
    paths$history_dir
  )
  on.exit(
    .shopping_history_store_release_lock(lock_handle),
    add = TRUE
  )

  .shopping_history_store_recover(paths, lock_handle)
  .shopping_history_store_snapshot_unlocked(
    paths$history_dir
  )
}

#' Afgør om et filudtræk indeholder historikrækker
#'
#' Funktionen er en navngivet callback til `Filter`, så alle selvdefinerede
#' funktioner i scriptet ligger på topniveau og kan testes isoleret.
#'
#' @param value Et muligt data frame-udtræk fra én fil.
#'
#' @return `TRUE`, når udtrækket indeholder mindst én række.
#' @keywords internal
.shopping_history_has_rows <- function(value) {
  is.data.frame(value) && nrow(value) > 0L
}

#' Normaliser en dato til brug i et historikfilnavn
#'
#' Kun én gyldig kalenderdato accepteres, så en gemning aldrig opretter en fil
#' med et tvetydigt eller ugyldigt navn.
#'
#' @param date En `Date` eller en værdi, der kan konverteres til `Date`.
#'
#' @return Datoen som én `Date`-værdi.
#' @keywords internal
.shopping_history_normalize_date <- function(date) {
  parsed <- suppressWarnings(
    tryCatch(as.Date(date), error = .shopping_history_ignore_file_error)
  )
  if (length(parsed) != 1L || is.na(parsed)) {
    stop("Datoen for indkøbssedlen er ugyldig.", call. = FALSE)
  }

  parsed
}

#' Valider en indkøbsseddel før gemning
#'
#' Nye filer har en streng kontrakt: en almindelig data frame med præcis én
#' character-kolonne ved navn `Indkøbsliste`. Det forhindrer, at nye filer
#' tilfører endnu et historikformat.
#'
#' @param history_df Den data frame, der ønskes gemt.
#'
#' @return Den validerede data frame uændret.
#' @keywords internal
.shopping_history_validate_save_frame <- function(history_df) {
  if (
    !is.data.frame(history_df) ||
      !identical(names(history_df), "Indkøbsliste") ||
      !is.character(history_df[["Indkøbsliste"]])
  ) {
    stop(
      paste(
        "Indkøbssedlens historik skal være en data frame med præcis",
        "character-kolonnen 'Indkøbsliste'."
      ),
      call. = FALSE
    )
  }

  history_df
}

#' Valider den revision, som en gemning er bygget fra
#'
#' Revisionen er obligatorisk. Dermed kan ingen kalder utilsigtet slå
#' beskyttelsen mod overskrivning fra ved at glemme argumentet.
#'
#' @param expected_revision Revisionen fra appens aktuelle historiksnapshot.
#'
#' @return Revisionen uændret.
#' @keywords internal
.shopping_history_validate_expected_revision <- function(
  expected_revision
) {
  if (
    is.null(expected_revision) ||
      !is.character(expected_revision) ||
      length(expected_revision) != 1L ||
      is.na(expected_revision) ||
      !nzchar(expected_revision)
  ) {
    stop(
      "En forventet revision af indkøbshistorikken er påkrævet.",
      call. = FALSE
    )
  }

  expected_revision
}

#' Gem en indkøbsseddel og returner den friske historik
#'
#' Kandidaten skrives og kontrolleres først i en stage-fil. Den eksisterende
#' fil flyttes derefter til backup, før kandidaten publiceres og markeres som
#' committed. Fejl før markøren ruller tilbage; fejl efter markøren beholder
#' den nye fil. Den forventede revision forhindrer en gammel session i at
#' overskrive en nyere historik. Ved revisionsforskel kastes condition-klassen
#' `shopping_history_store_conflict`, som appen bruger til at genindlæse det
#' nyeste snapshot. En manglende destinationsmappe er derimod en almindelig
#' lagringsfejl.
#'
#' @param history_df En data frame med præcis character-kolonnen
#'   `Indkøbsliste`.
#' @param expected_revision Revisionen fra det snapshot, kandidaten bygger på.
#' @param history_dir En eksisterende mappe til historikfilerne.
#' @param date Datoen, der skal indgå i filnavnet.
#' @param .fail_at Test-hook til en almindelig fejl ved `after_stage`,
#'   `after_backup`, `after_promote` eller `after_commit_marker`.
#' @param .crash_at Test-hook til et simuleret processtop ved de samme trin.
#'
#' @return Et frisk snapshot med elementerne `entries` og `revision`.
#' @keywords internal
shopping_history_store_save <- function(
  history_df,
  expected_revision,
  history_dir = "./data/indkobssedler",
  date = Sys.Date(),
  .fail_at = NULL,
  .crash_at = NULL
) {
  if (missing(expected_revision)) {
    stop(
      "En forventet revision af indkøbshistorikken er påkrævet.",
      call. = FALSE
    )
  }
  expected_revision <-
    .shopping_history_validate_expected_revision(
      expected_revision
    )
  history_df <- .shopping_history_validate_save_frame(history_df)
  paths <- .shopping_history_store_paths(history_dir)
  date <- .shopping_history_normalize_date(date)
  filename <- paste0(
    "indkobsseddel_",
    format(date, "%Y%m%d"),
    ".rda"
  )
  target <- file.path(paths$history_dir, filename)

  lock_handle <- .shopping_history_store_acquire_lock(
    paths$history_dir
  )
  release_lock <- TRUE
  on.exit(
    if (release_lock) {
      .shopping_history_store_release_lock(lock_handle)
    },
    add = TRUE
  )

  initial_recovery <- .shopping_history_store_recover(
    paths,
    lock_handle
  )
  if (identical(
    initial_recovery,
    "committed_pending_cleanup"
  )) {
    stop(
      paste(
        "En tidligere historikændring er gemt,",
        "men oprydningen er endnu ikke afsluttet.",
        "Prøv handlingen igen om et øjeblik."
      ),
      call. = FALSE
    )
  }

  current_revision <-
    .shopping_history_store_revision_unlocked(
      paths$history_dir
    )
  if (!identical(current_revision, expected_revision)) {
    .shopping_history_store_stop_conflict()
  }

  if (file.exists(target)) {
    current_target <- tryCatch(
      .shopping_history_store_read_saved_frame(target),
      error = .shopping_history_ignore_file_error
    )
    if (
      is.data.frame(current_target) &&
        identical(current_target, history_df)
    ) {
      return(
        .shopping_history_store_snapshot_unlocked(
          paths$history_dir
        )
      )
    }
  }

  committed_snapshot <- NULL
  marker_created <- FALSE
  commit_error <- tryCatch(
    {
      .shopping_history_store_touch_lock(lock_handle)
      .shopping_history_store_write_stage(
        history_df,
        paths$stage
      )
      candidate_hash <- .shopping_history_store_file_hash(
        paths$stage
      )

      journal <- list(
        version = 1L,
        target_filename = filename,
        had_original = file.exists(target),
        original_hash = .shopping_history_store_file_hash(
          target
        ),
        candidate_hash = candidate_hash
      )
      .shopping_history_store_touch_lock(lock_handle)
      .shopping_history_store_write_journal(
        journal,
        paths
      )
      .shopping_history_store_checkpoint(
        .fail_at,
        "after_stage",
        .crash_at
      )

      if (isTRUE(journal$had_original)) {
        .shopping_history_store_touch_lock(lock_handle)
        .shopping_history_store_move(
          target,
          paths$backup,
          "flytte dagens eksisterende historikfil til backup"
        )
      }
      .shopping_history_store_checkpoint(
        .fail_at,
        "after_backup",
        .crash_at
      )

      .shopping_history_store_touch_lock(lock_handle)
      .shopping_history_store_move(
        paths$stage,
        target,
        "publicere den nye historikfil"
      )
      published <- .shopping_history_store_read_saved_frame(
        target
      )
      if (
        !identical(published, history_df) ||
          !identical(
            .shopping_history_store_file_hash(target),
            journal$candidate_hash
          )
      ) {
        stop(
          "Den publicerede historikfil svarer ikke til kandidaten.",
          call. = FALSE
        )
      }
      .shopping_history_store_checkpoint(
        .fail_at,
        "after_promote",
        .crash_at
      )

      .shopping_history_store_touch_lock(lock_handle)
      .shopping_history_store_mark_committed(paths)
      marker_created <- TRUE
      .shopping_history_store_checkpoint(
        .fail_at,
        "after_commit_marker",
        .crash_at
      )

      recovery_outcome <- .shopping_history_store_recover(
        paths,
        lock_handle
      )
      if (!recovery_outcome %in% c(
        "committed",
        "committed_pending_cleanup"
      )) {
        stop(
          "Historiktransaktionen blev ikke afsluttet som forventet.",
          call. = FALSE
        )
      }

      committed_snapshot <-
        .shopping_history_store_snapshot_unlocked(
          paths$history_dir
        )
      NULL
    },
    error = identity
  )

  if (is.null(commit_error)) {
    return(committed_snapshot)
  }

  if (inherits(
    commit_error,
    "shopping_history_store_simulated_crash"
  )) {
    release_lock <- FALSE
    .shopping_history_store_release_lock(lock_handle)
    stop(commit_error)
  }

  if (inherits(
    commit_error,
    "shopping_history_store_lock_lost"
  )) {
    stop(commit_error)
  }

  recovery_result <- tryCatch(
    .shopping_history_store_recover(
      paths,
      lock_handle
    ),
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
    return(
      .shopping_history_store_snapshot_unlocked(
        paths$history_dir
      )
    )
  }

  stop(commit_error)
}

#' Valider et kanonisk historikudtræk
#'
#' Analysefunktionerne modtager samme faste kontrakt som lagerets snapshots.
#' En tydelig fejl her gør programmeringsfejl lettere at finde end et delvist
#' eller misvisende statistikresultat.
#'
#' @param entries En data frame fra `shopping_history_store_read()`.
#'
#' @return `entries` uændret efter validering.
#' @keywords internal
.shopping_history_validate_entries <- function(entries) {
  valid <- is.data.frame(entries) &&
    identical(
      names(entries),
      c("filename", "date", "line_number", "Indkøbsliste")
    ) &&
    is.character(entries$filename) &&
    inherits(entries$date, "Date") &&
    is.integer(entries$line_number) &&
    is.character(entries$Indkøbsliste)

  if (!isTRUE(valid)) {
    stop("Indkøbshistorikken har ikke det kanoniske schema.", call. = FALSE)
  }

  entries
}

#' Udvælg varelinjerne før den første separator i hver fil
#'
#' En tom linje adskiller varerne øverst på indkøbssedlen fra opskrifterne
#' nederst. Kun linjer med et lavere linjenummer end den første tomme linje
#' i samme fil bliver derfor taget med i varestatistikken.
#'
#' @param entries Et kanonisk historikudtræk.
#'
#' @return En tegnvektor med de rå varelinjer.
#' @keywords internal
.shopping_history_item_lines <- function(entries) {
  entries <- .shopping_history_validate_entries(entries)
  if (nrow(entries) == 0L) {
    return(character())
  }

  keep <- rep(FALSE, nrow(entries))
  filenames <- unique(entries$filename)
  for (filename in filenames) {
    file_rows <- which(entries$filename == filename)
    blank_rows <- file_rows[
      !is.na(entries$Indkøbsliste[file_rows]) &
        trimws(entries$Indkøbsliste[file_rows]) == ""
    ]
    first_blank <- if (length(blank_rows) == 0L) {
      Inf
    } else {
      min(entries$line_number[blank_rows])
    }
    keep[file_rows] <- entries$line_number[file_rows] < first_blank
  }

  entries$Indkøbsliste[keep]
}

#' Escape specialtegn i tekst til et regulært udtryk
#'
#' Enheder som `pakke(r)` skal behandles som almindelig tekst og ikke som et
#' regulært udtryk. Funktionen beskytter alle tegn med særlig regex-betydning.
#'
#' @param values Tegnvektor med tekst, der skal bruges bogstaveligt.
#'
#' @return Tegnvektoren med escapede regex-specialtegn.
#' @keywords internal
.shopping_history_escape_regex <- function(values) {
  gsub(
    "([][{}()+*^$|\\\\?.])",
    "\\\\\\1",
    values
  )
}

#' Rens historiske varelinjer på samme måde som den gamle statistik
#'
#' Mængder, kendte enheder og markørerne `(tilsmagning)` og `(tilbehør)`
#' fjernes. Enhederne sorteres længst først, så eksempelvis `kg` ikke bliver
#' delvist ramt af den kortere enhed `g`.
#'
#' @param lines Tegnvektor med rå varelinjer.
#' @param units Tegnvektor med appens kendte enheder.
#'
#' @return Rensede varenavne som en tegnvektor.
#' @keywords internal
.shopping_history_clean_items <- function(lines, units) {
  lines <- as.character(lines)
  lines <- sub("\\((tilsmagning|tilbehør)\\)", "", lines)
  lines <- sub("\\d+\\.*\\d*", "", lines, perl = TRUE)

  units <- unique(trimws(as.character(units)))
  units <- units[!is.na(units) & nzchar(units)]
  if (length(units) > 0L) {
    units <- units[order(nchar(units), decreasing = TRUE)]
    unit_pattern <- paste(
      .shopping_history_escape_regex(units),
      collapse = "|"
    )
    lines <- sub(unit_pattern, "", lines, perl = TRUE)
  }

  trimws(lines)
}

#' Find de mest brugte varer i det kanoniske historikudtræk
#'
#' Kun vareafsnittet før første tomme linje i hver indkøbsseddel indgår.
#' Linjerne renses for mængder, enheder og tilbehørsmarkører, optælles og
#' sorteres med de hyppigste varer først. Antalskolonnen er bevidst skjult for
#' at bevare den eksisterende kontrakt til forslagstabellen.
#'
#' @param entries Et kanonisk historikudtræk.
#' @param units Tegnvektor med appens kendte enheder.
#'
#' @return En data frame med character-kolonnen `Indkøbsliste`.
#' @keywords internal
shopping_history_popular_items <- function(entries, units) {
  lines <- .shopping_history_item_lines(entries)
  if (length(lines) == 0L) {
    return(data.frame(
      Indkøbsliste = character(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  items <- .shopping_history_clean_items(lines, units)
  items <- items[!is.na(items) & nzchar(items)]
  if (length(items) == 0L) {
    return(data.frame(
      Indkøbsliste = character(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  counts <- table(items)
  item_names <- names(counts)
  item_names <- item_names[
    order(-as.integer(counts), item_names)
  ]

  data.frame(
    Indkøbsliste = item_names,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Find det længste naturlige opskriftspræfiks i en linje
#'
#' `startsWith()` gør matchningen sikker for navne med parenteser, plus-tegn
#' og andre regex-specialtegn. Når eksempelvis både `Pizza` og `Pizza surdej`
#' matcher, vælges det længste navn. Efter navnet kræves slutningen af linjen
#' eller en naturlig grænse som mellemrum, kolon eller parentes.
#'
#' @param line En linje fra en gemt indkøbsseddel.
#' @param recipe_names Tegnvektor med kendte opskriftsnavne.
#'
#' @return Det matchede navn eller `NA_character_`.
#' @keywords internal
.shopping_history_recipe_for_line <- function(line, recipe_names) {
  if (length(line) != 1L || is.na(line)) {
    return(NA_character_)
  }

  line <- trimws(as.character(line))
  candidates <- recipe_names[startsWith(line, recipe_names)]
  if (length(candidates) == 0L) {
    return(NA_character_)
  }

  suffixes <- substring(line, nchar(candidates) + 1L)
  natural_boundary <- !nzchar(suffixes) |
    grepl("^[[:space:]:\\(]", suffixes)
  candidates <- candidates[natural_boundary]
  if (length(candidates) == 0L) {
    return(NA_character_)
  }

  candidates[[which.max(nchar(candidates))]]
}

#' Udtræk opskriftsbrug fra den fælles indkøbshistorik
#'
#' Hver historiklinje sammenlignes med de kendte opskriftsnavne. Resultatet
#' bevarer den eksisterende statistik-kontrakt med kolonnerne `retter` og
#' `dato` og sorteres efter ret og dato.
#'
#' @param entries Et kanonisk historikudtræk.
#' @param recipe_names Tegnvektor med kendte opskriftsnavne.
#'
#' @return En data frame med kolonnerne `retter` og `dato`.
#' @keywords internal
shopping_history_recipe_usage <- function(entries, recipe_names) {
  entries <- .shopping_history_validate_entries(entries)
  recipe_names <- unique(trimws(as.character(recipe_names)))
  recipe_names <- recipe_names[
    !is.na(recipe_names) & nzchar(recipe_names)
  ]
  if (nrow(entries) == 0L || length(recipe_names) == 0L) {
    return(data.frame(
      retter = character(),
      dato = as.Date(character()),
      stringsAsFactors = FALSE
    ))
  }

  matches <- vapply(
    entries$Indkøbsliste,
    .shopping_history_recipe_for_line,
    character(1),
    recipe_names = recipe_names
  )
  matched_rows <- which(!is.na(matches))
  if (length(matched_rows) == 0L) {
    return(data.frame(
      retter = character(),
      dato = as.Date(character()),
      stringsAsFactors = FALSE
    ))
  }

  result <- data.frame(
    retter = unname(matches[matched_rows]),
    dato = entries$date[matched_rows],
    stringsAsFactors = FALSE
  )
  result <- result[order(result$retter, result$dato), , drop = FALSE]
  row.names(result) <- NULL
  result
}
