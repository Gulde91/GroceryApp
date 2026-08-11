# Reaktiv state og lagringskoordinering for basisvarer ----------------------
#
# Denne fil binder basisvarelageret i basis_varer_store.R sammen med resten
# af appen. Resten af appen får kun læseadgang og en kontrolleret
# commit-funktion. Den reaktive state og publiceringen holdes private i
# create_basis_varer_state().

library(shiny)

#' Kontrollér et komplet snapshot til basisvarernes reaktive state
#'
#' Et snapshot består af den gemte varetabel og den revision, som tabellen
#' havde på disken, da den blev læst. Revisionen bruges til at opdage, om en
#' anden browsersession har gemt nyere data.
#'
#' @param snapshot Et komplet basisvaresnapshot.
#'
#' @return Usynligt `TRUE`, hvis snapshottet kan bruges som kanonisk state.
#' @keywords internal
basis_varer_state_validate_snapshot <- function(snapshot) {
  expected_columns <- c(
    "Indkobsliste",
    "maengde",
    "enhed",
    "kat_1",
    "kat_2"
  )

  if (
    !is.list(snapshot) ||
      !identical(names(snapshot), c("varer", "revision")) ||
      !is.data.frame(snapshot$varer) ||
      !identical(names(snapshot$varer), expected_columns) ||
      !is.character(snapshot$revision) ||
      length(snapshot$revision) != 1L ||
      is.na(snapshot$revision) ||
      !nzchar(snapshot$revision)
  ) {
    stop(
      paste(
        "Basisvaresnapshottet skal indeholde en varetabel",
        "og præcis én gyldig revision."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Find de dele af basisvaresnapshottet, som faktisk er ændret
#'
#' State-lagets små feltsignaler bruger resultatet til kun at genberegne de
#' dele af appen, som afhænger af et ændret felt. Hvis kun filrevisionen er
#' ændret, behøver det samlede varekatalog eksempelvis ikke at blive bygget
#' på ny.
#'
#' @param current_snapshot Det nuværende kanoniske snapshot.
#' @param next_snapshot Det snapshot, som skal publiceres.
#'
#' @return En tekstvektor med navnene på de ændrede felter.
#' @keywords internal
basis_varer_state_changed_fields <- function(
  current_snapshot,
  next_snapshot
) {
  fields <- c("varer", "revision")

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

#' Opret den kanoniske, reaktive state for basisvarerne
#'
#' Constructoren læser ét komplet snapshot fra basisvarelageret og beholder
#' det som sessionens eneste autoritative udgave. Den returnerede
#' commit-funktion gemmer altid ændringen på disken, før det nye snapshot
#' publiceres til resten af appen. Hvis gemningen fejler, bliver den hidtidige
#' state derfor stående.
#'
#' Hvis en anden session har gemt først, genindlæses dens snapshot med det
#' samme. Brugeren kan derefter prøve handlingen igen på grundlag af den
#' opdaterede liste uden at overskrive den anden sessions ændring.
#'
#' En let polling-observer sammenligner med jævne mellemrum revisionen på
#' disken. Hele tabellen genindlæses kun, når revisionen er ændret.
#' Midlertidige læsefejl er tavse og prøves igen ved næste interval.
#'
#' De injicerbare lagerfunktioner og notifikationsfunktionen gør forløbet
#' direkte testbart uden at ændre globale funktioner.
#'
#' @param session Den aktuelle Shiny-session.
#' @param data_dir Mappen med basisvarelagerets fil.
#' @param poll_interval_ms Antal millisekunder mellem revisionstjek.
#' @param store_read Funktion, der læser et komplet basisvaresnapshot.
#' @param store_revision Funktion, der læser revisionen på disken.
#' @param store_commit Funktion, der udfører den atomiske lagring.
#' @param notify Funktion, der viser en fejlbesked til brugeren.
#'
#' @return En liste med præcis `read` og `commit`. `read` indeholder en
#'   isoleret snapshot-getter og reaktive getters for varer og revision.
#'   `commit` gemmer en ny komplet varetabel og returnerer `TRUE` eller
#'   `FALSE`.
create_basis_varer_state <- function(
  session,
  data_dir = "./data",
  poll_interval_ms = 2000L,
  store_read = basis_varer_store_read,
  store_revision = basis_varer_store_revision,
  store_commit = basis_varer_store_commit,
  notify = showNotification
) {
  dependencies <- list(
    store_read = store_read,
    store_revision = store_revision,
    store_commit = store_commit,
    notify = notify
  )
  valid_dependencies <- vapply(dependencies, is.function, logical(1))
  if (!all(valid_dependencies)) {
    stop(
      "State-lagets lager- og notifikationsafhængigheder skal være funktioner.",
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

  initial_snapshot <- store_read(data_dir)
  basis_varer_state_validate_snapshot(initial_snapshot)

  canonical_snapshot <- reactiveVal(initial_snapshot)
  field_signals <- reactiveValues(
    varer = 0L,
    revision = 0L
  )

  publish <- function(next_snapshot) {
    basis_varer_state_validate_snapshot(next_snapshot)
    current_snapshot <- isolate(canonical_snapshot())
    changed_fields <- basis_varer_state_changed_fields(
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
    varer = read_field("varer"),
    revision = read_field("revision")
  )

  commit <- function(
    next_varer,
    error_message = "Ændringen af basisvarerne kunne ikke gemmes."
  ) {
    tryCatch(
      {
        current_snapshot <- isolate(canonical_snapshot())
        persisted_snapshot <- store_commit(
          varer = next_varer,
          expected_revision = current_snapshot$revision,
          data_dir = data_dir
        )
        basis_varer_state_validate_snapshot(persisted_snapshot)
        publish(persisted_snapshot)
        invisible(TRUE)
      },
      error = function(error) {
        notification_detail <- conditionMessage(error)

        if (inherits(error, "basis_varer_store_conflict")) {
          refreshed_snapshot <- tryCatch(
            {
              latest_snapshot <- store_read(data_dir)
              basis_varer_state_validate_snapshot(latest_snapshot)
              publish(latest_snapshot)
              latest_snapshot
            },
            error = identity
          )

          if (!inherits(refreshed_snapshot, "error")) {
            notification_detail <- paste(
              "Basisvarerne var ændret i en anden session.",
              "Listen er nu opdateret; prøv handlingen igen."
            )
          } else {
            notification_detail <- paste(
              conditionMessage(error),
              "Den nyeste liste kunne ikke genindlæses:",
              conditionMessage(refreshed_snapshot)
            )
          }
        }

        notify(
          paste(error_message, notification_detail),
          type = "error",
          duration = NULL
        )
        invisible(FALSE)
      }
    )
  }

  observe({
    invalidateLater(poll_interval_ms, session)

    disk_revision <- tryCatch(
      store_revision(data_dir),
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
      store_read(data_dir),
      error = function(error) NULL
    )
    if (is.null(refreshed_snapshot)) return(invisible(NULL))

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
