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
#' @param log_event Valgfri callback til struktureret driftslogging. Callbacken
#'   modtager `level`, `event`, `component`, `fields` og `error`. Logfejl
#'   ignoreres altid og kan derfor ikke ændre state eller brugerflow.
#'
#' @return En liste med præcis `read` og `commit`. `read` indeholder en
#'   isoleret snapshot-getter og reaktive getters for varer og revision.
#'   `commit` gemmer en ny komplet varetabel og returnerer `TRUE` eller
#'   `FALSE`. Dets valgfrie `log_context` kan indeholde handling, varenavn og
#'   særskilte succes- og fejlbeskeder.
create_basis_varer_state <- function(
  session,
  data_dir = "./data",
  poll_interval_ms = 2000L,
  store_read = basis_varer_store_read,
  store_revision = basis_varer_store_revision,
  store_commit = basis_varer_store_commit,
  notify = showNotification,
  log_event = function(
    level,
    event,
    component,
    fields = list(),
    error = NULL
  ) invisible(FALSE)
) {
  dependencies <- list(
    store_read = store_read,
    store_revision = store_revision,
    store_commit = store_commit,
    notify = notify,
    log_event = log_event
  )
  valid_dependencies <- vapply(dependencies, is.function, logical(1))
  if (!all(valid_dependencies)) {
    stop(
      paste(
        "State-lagets lager-, notifikations- og",
        "logafhængigheder skal være funktioner."
      ),
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

  log_safely <- function(
    level,
    event,
    fields = list(),
    error = NULL
  ) {
    tryCatch(
      {
        log_event(
          level = level,
          event = event,
          component = "basis_varer_state",
          fields = fields,
          error = error
        )
        invisible(TRUE)
      },
      error = function(log_error) invisible(FALSE)
    )
  }

  elapsed_ms <- function(started_at) {
    as.integer(round(max(
      0,
      as.numeric(proc.time()[["elapsed"]] - started_at) * 1000
    )))
  }

  normalize_commit_log_context <- function(value) {
    allowed_fields <- c(
      "action",
      "item_name",
      "previous_item_name",
      "success_message",
      "failure_message"
    )
    if (!is.list(value) || is.null(names(value))) return(list())

    field_names <- names(value)
    keep <- !is.na(field_names) &
      nzchar(field_names) &
      field_names %in% allowed_fields &
      !duplicated(field_names)
    value[keep]
  }

  commit_log_fields <- function(
    context,
    succeeded,
    fallback_message
  ) {
    tryCatch(
      {
        context <- normalize_commit_log_context(context)
        message_field <- if (isTRUE(succeeded)) {
          "success_message"
        } else {
          "failure_message"
        }
        message <- context[[message_field]]
        context$success_message <- NULL
        context$failure_message <- NULL

        if (
          is.null(message) ||
            length(message) == 0L ||
            is.na(message[[1L]]) ||
            !nzchar(trimws(as.character(message[[1L]])))
        ) {
          message <- fallback_message
        }
        context$message <- as.character(message[[1L]])
        context
      },
      error = function(error) {
        list(message = as.character(fallback_message[[1L]]))
      }
    )
  }

  poll_failure_active <- FALSE
  record_poll_failure <- function(error, stage, started_at) {
    if (!isTRUE(poll_failure_active)) {
      log_safely(
        level = "WARN",
        event = "poll_failed",
        fields = list(
          duration_ms = elapsed_ms(started_at),
          stage = stage,
          outcome = "failed"
        ),
        error = error
      )
    }
    poll_failure_active <<- TRUE
    invisible(NULL)
  }

  record_poll_recovery <- function(stage, started_at) {
    if (!isTRUE(poll_failure_active)) return(invisible(NULL))

    poll_failure_active <<- FALSE
    log_safely(
      level = "INFO",
      event = "poll_recovered",
      fields = list(
        duration_ms = elapsed_ms(started_at),
        stage = stage,
        outcome = "recovered"
      )
    )
    invisible(NULL)
  }

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
    error_message = "Ændringen af basisvarerne kunne ikke gemmes.",
    log_context = list()
  ) {
    started_at <- proc.time()[["elapsed"]]
    candidate_row_count <- if (is.data.frame(next_varer)) {
      as.integer(nrow(next_varer))
    } else {
      NA_integer_
    }
    commit_stage <- "store_commit"

    tryCatch(
      {
        current_snapshot <- isolate(canonical_snapshot())
        persisted_snapshot <- store_commit(
          varer = next_varer,
          expected_revision = current_snapshot$revision,
          data_dir = data_dir
        )
        commit_stage <- "validate"
        basis_varer_state_validate_snapshot(persisted_snapshot)
        commit_stage <- "publish"
        publish(persisted_snapshot)
        commit_stage <- "complete"
        log_safely(
          level = "INFO",
          event = "commit_succeeded",
          fields = c(
            commit_log_fields(
              log_context,
              succeeded = TRUE,
              fallback_message = "Bruttolisten blev gemt."
            ),
            list(
              row_count = as.integer(nrow(persisted_snapshot$varer)),
              duration_ms = elapsed_ms(started_at),
              stage = commit_stage,
              outcome = "succeeded"
            )
          )
        )
        invisible(TRUE)
      },
      error = function(error) {
        notification_detail <- conditionMessage(error)
        conflict <- inherits(error, "basis_varer_store_conflict")
        refresh_outcome <- NULL
        refresh_error <- NULL

        if (conflict) {
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
            refresh_outcome <- "succeeded"
            notification_detail <- paste(
              "Basisvarerne var ændret i en anden session.",
              "Listen er nu opdateret; prøv handlingen igen."
            )
          } else {
            refresh_outcome <- "failed"
            refresh_error <- refreshed_snapshot
            notification_detail <- paste(
              conditionMessage(error),
              "Den nyeste liste kunne ikke genindlæses:",
              conditionMessage(refreshed_snapshot)
            )
          }
        }

        if (conflict) {
          conflict_fields <- c(
            commit_log_fields(
              log_context,
              succeeded = FALSE,
              fallback_message = error_message
            ),
            list(
              row_count = candidate_row_count,
              duration_ms = elapsed_ms(started_at),
              stage = commit_stage,
              refresh = refresh_outcome
            )
          )
          if (inherits(refresh_error, "error")) {
            conflict_fields$refresh_error_class <-
              class(refresh_error)[[1L]]
            conflict_fields$refresh_error_message <-
              conditionMessage(refresh_error)
          }
          conflict_fields$outcome <- "rejected"
          log_safely(
            level = "WARN",
            event = "commit_conflict",
            fields = conflict_fields,
            error = error
          )
        } else {
          log_safely(
            level = "ERROR",
            event = "commit_failed",
            fields = c(
              commit_log_fields(
                log_context,
                succeeded = FALSE,
                fallback_message = error_message
              ),
              list(
                row_count = candidate_row_count,
                duration_ms = elapsed_ms(started_at),
                stage = commit_stage,
                outcome = "failed"
              )
            ),
            error = error
          )
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
    started_at <- proc.time()[["elapsed"]]
    invalidateLater(poll_interval_ms, session)

    disk_revision <- tryCatch(
      store_revision(data_dir),
      error = identity
    )
    if (inherits(disk_revision, "error")) {
      record_poll_failure(
        disk_revision,
        stage = "revision",
        started_at = started_at
      )
      return(invisible(NULL))
    }

    known_revision <- isolate(canonical_snapshot()$revision)
    if (is.null(disk_revision)) {
      revision_error <- simpleError(
        "Basisvarelageret returnerede ingen revision."
      )
      record_poll_failure(
        revision_error,
        stage = "revision",
        started_at = started_at
      )
      return(invisible(NULL))
    }
    if (identical(disk_revision, known_revision)) {
      record_poll_recovery("revision", started_at)
      return(invisible(NULL))
    }

    refreshed_snapshot <- tryCatch(
      store_read(data_dir),
      error = identity
    )
    if (inherits(refreshed_snapshot, "error")) {
      record_poll_failure(
        refreshed_snapshot,
        stage = "refresh",
        started_at = started_at
      )
      return(invisible(NULL))
    }
    if (is.null(refreshed_snapshot)) {
      refresh_error <- simpleError(
        "Basisvarelageret returnerede intet snapshot."
      )
      record_poll_failure(
        refresh_error,
        stage = "refresh",
        started_at = started_at
      )
      return(invisible(NULL))
    }

    publish_error <- tryCatch(
      {
        publish(refreshed_snapshot)
        NULL
      },
      error = identity
    )
    if (inherits(publish_error, "error")) {
      record_poll_failure(
        publish_error,
        stage = "publish",
        started_at = started_at
      )
      return(invisible(NULL))
    }

    record_poll_recovery("publish", started_at)
    log_safely(
      level = "INFO",
      event = "poll_refreshed",
      fields = list(
        row_count = as.integer(nrow(refreshed_snapshot$varer)),
        duration_ms = elapsed_ms(started_at),
        stage = "publish",
        outcome = "refreshed"
      )
    )
    invisible(NULL)
  })

  list(
    read = read,
    commit = commit
  )
}
