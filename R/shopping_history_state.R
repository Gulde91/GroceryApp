# Reaktiv state og lagringskoordinering for indkøbshistorikken --------------
#
# Denne fil holder sessionens aktuelle indkøbshistorik og dens revision samlet
# i ét autoritativt snapshot. Den forbinder appen med historiklageret og giver
# resten af appen kontrolleret adgang til læsning, gemning, genindlæsning og
# konflikthåndtering.

library(shiny)

#' Kontrollér et komplet snapshot til indkøbshistorikkens state
#'
#' Et snapshot består af alle læste historiklinjer og den revision, som
#' historikfilerne havde på disken ved læsningen. Revisionen gør det muligt at
#' opdage, om en anden session har gemt nyere data.
#'
#' @param snapshot Et komplet snapshot fra historiklageret.
#'
#' @return Usynligt `TRUE`, hvis snapshottet kan bruges som kanonisk state.
#' @keywords internal
shopping_history_state_validate_snapshot <- function(snapshot) {
  expected_columns <- c(
    "filename",
    "date",
    "line_number",
    "Indkøbsliste"
  )

  valid_entries <- is.list(snapshot) &&
    identical(names(snapshot), c("entries", "revision")) &&
    is.data.frame(snapshot$entries) &&
    identical(names(snapshot$entries), expected_columns)

  valid_entry_types <- valid_entries &&
    is.character(snapshot$entries$filename) &&
    inherits(snapshot$entries$date, "Date") &&
    is.integer(snapshot$entries$line_number) &&
    is.character(snapshot$entries$Indkøbsliste)

  valid_revision <- is.list(snapshot) &&
    is.character(snapshot$revision) &&
    length(snapshot$revision) == 1L &&
    !is.na(snapshot$revision) &&
    nzchar(snapshot$revision)

  if (!valid_entries || !valid_entry_types || !valid_revision) {
    stop(
      paste(
        "Historiksnapshottet skal indeholde en gyldig historiktabel",
        "og præcis én gyldig revision."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Find de dele af historiksnapshottet, som faktisk er ændret
#'
#' Små feltsignaler bruger resultatet til kun at genberegne de dele af appen,
#' der afhænger af et ændret felt. En ændret revision alene behøver derfor
#' ikke genberegne statistik, hvis selve historiklinjerne er uændrede.
#'
#' @param current_snapshot Det nuværende kanoniske snapshot.
#' @param next_snapshot Det snapshot, som skal publiceres.
#'
#' @return En tekstvektor med navnene på de ændrede felter.
#' @keywords internal
shopping_history_state_changed_fields <- function(
  current_snapshot,
  next_snapshot
) {
  fields <- c("entries", "revision")

  fields[vapply(
    fields,
    function(field) {
      !identical(
        current_snapshot[[field]],
        next_snapshot[[field]]
      )
    },
    logical(1)
  )]
}

#' Opret den kanoniske, reaktive state for indkøbshistorikken
#'
#' Constructoren læser ét komplet snapshot fra historiklageret og beholder det
#' som sessionens eneste autoritative udgave. `commit` gemmer altid først på
#' disken og publicerer kun det snapshot, som lageret returnerer efter en
#' vellykket gemning. Ved en almindelig fejl forbliver den hidtidige state
#' derfor uændret.
#'
#' Hvis en anden session har gemt først, genindlæses dens snapshot med det
#' samme. Den oprindelige gemning afvises fortsat, så brugeren kan prøve igen
#' på grundlag af den opdaterede historik uden at overskrive nyere data.
#'
#' En let polling-observer sammenligner med jævne mellemrum revisionen på
#' disken. Hele historikken genindlæses kun, når revisionen er ændret.
#' Midlertidige læsefejl ignoreres og prøves igen ved næste interval.
#'
#' Lagerfunktionerne kan udskiftes i tests, så state-forløbet kan afprøves
#' uden at ændre globale funktioner eller skrive til projektets datafiler.
#'
#' @param session Den aktuelle Shiny-session.
#' @param history_dir Mappen med historiske indkøbssedler.
#' @param poll_interval_ms Antal millisekunder mellem revisionstjek.
#' @param store_read Funktion, der læser et komplet historiksnapshot.
#' @param store_revision Funktion, der læser revisionen på disken.
#' @param store_save Funktion, der gemmer en indkøbsseddel atomisk.
#'
#' @return En liste med `read` og `commit`. `read` indeholder en isoleret
#'   snapshot-getter og reaktive getters for historiklinjer og revision.
#'   `commit` gemmer en indkøbsseddel og returnerer usynligt `TRUE`.
create_shopping_history_state <- function(
  session,
  history_dir = "./data/indkobssedler",
  poll_interval_ms = 2000L,
  store_read = shopping_history_store_read,
  store_revision = shopping_history_store_revision,
  store_save = shopping_history_store_save
) {
  dependencies <- list(
    store_read = store_read,
    store_revision = store_revision,
    store_save = store_save
  )
  if (!all(vapply(dependencies, is.function, logical(1)))) {
    stop(
      "State-lagets lagerafhængigheder skal være funktioner.",
      call. = FALSE
    )
  }
  if (
    length(poll_interval_ms) != 1L ||
      !is.numeric(poll_interval_ms) ||
      is.na(poll_interval_ms) ||
      !is.finite(poll_interval_ms) ||
      poll_interval_ms <= 0
  ) {
    stop("Polling-intervallet skal være et positivt tal.", call. = FALSE)
  }

  initial_snapshot <- store_read(history_dir)
  shopping_history_state_validate_snapshot(initial_snapshot)

  canonical_snapshot <- reactiveVal(initial_snapshot)
  field_signals <- reactiveValues(
    entries = 0L,
    revision = 0L
  )

  publish <- function(next_snapshot) {
    shopping_history_state_validate_snapshot(next_snapshot)
    current_snapshot <- isolate(canonical_snapshot())
    changed_fields <- shopping_history_state_changed_fields(
      current_snapshot,
      next_snapshot
    )

    canonical_snapshot(next_snapshot)
    for (field in changed_fields) {
      field_signals[[field]] <- isolate(field_signals[[field]]) + 1L
    }

    invisible(next_snapshot)
  }

  read_field <- function(field) {
    force(field)
    reactive({
      field_signals[[field]]
      isolate(canonical_snapshot()[[field]])
    })
  }

  read <- list(
    snapshot = function() isolate(canonical_snapshot()),
    entries = read_field("entries"),
    revision = read_field("revision")
  )

  commit <- function(history_df) {
    current_snapshot <- isolate(canonical_snapshot())

    tryCatch(
      {
        persisted_snapshot <- store_save(
          history_df,
          expected_revision = current_snapshot$revision,
          history_dir = history_dir
        )
        shopping_history_state_validate_snapshot(
          persisted_snapshot
        )
        publish(persisted_snapshot)
        invisible(TRUE)
      },
      shopping_history_store_conflict = function(error) {
        refreshed_snapshot <- tryCatch(
          {
            latest_snapshot <- store_read(history_dir)
            shopping_history_state_validate_snapshot(
              latest_snapshot
            )
            publish(latest_snapshot)
            latest_snapshot
          },
          error = identity
        )

        if (inherits(refreshed_snapshot, "error")) {
          stop(
            paste(
              "Indkøbshistorikken er ændret i en anden session,",
              "og den nyeste historik kunne ikke indlæses:",
              conditionMessage(refreshed_snapshot)
            ),
            call. = FALSE
          )
        }

        stop(
          paste(
            "Indkøbshistorikken blev ændret i en anden session.",
            "Historikken er nu opdateret; prøv at gemme igen."
          ),
          call. = FALSE
        )
      }
    )
  }

  observe({
    invalidateLater(poll_interval_ms, session)

    disk_revision <- tryCatch(
      store_revision(history_dir),
      error = function(error) NULL
    )
    known_revision <- isolate(canonical_snapshot()$revision)
    if (
      is.null(disk_revision) ||
        identical(disk_revision, known_revision)
    ) {
      return(invisible(NULL))
    }

    refreshed_snapshot <- tryCatch(
      store_read(history_dir),
      error = function(error) NULL
    )
    if (is.null(refreshed_snapshot)) {
      return(invisible(NULL))
    }

    tryCatch(
      publish(refreshed_snapshot),
      error = function(error) NULL
    )
    invisible(NULL)
  })

  list(
    read = read,
    commit = commit
  )
}
