# Reaktiv state og lagringskoordinering for opskrifter -----------------------
#
# Denne fil binder det rene opskriftskatalog i recipe_catalog.R sammen med
# fillageret i recipe_store.R. Resten af appen får kun læseadgang og en
# kontrolleret commit-funktion. Den reaktive state og publiceringen holdes
# private i create_recipe_catalog_state().

library(shiny)

#' Kontroller et komplet snapshot til den reaktive opskriftstilstand
#'
#' Funktionen genbruger katalogets almindelige strukturkontrol og sikrer
#' derudover, at snapshottet har præcis én kendt revision. Revisionen bruges
#' til at opdage, hvis en anden browsersession har gemt nyere data.
#'
#' @param snapshot Et komplet opskriftskatalog med en `revision`.
#'
#' @return Usynligt `TRUE`, hvis snapshottet kan bruges som kanonisk state.
#' @keywords internal
recipe_catalog_state_validate_snapshot <- function(snapshot) {
  recipe_catalog_validate(snapshot)

  if (
    !"revision" %in% names(snapshot) ||
      length(snapshot$revision) != 1L ||
      is.na(snapshot$revision)
  ) {
    stop(
      "Opskriftskataloget skal have præcis én gyldig revision.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Find de dele af opskriftskataloget, som faktisk er ændret
#'
#' De små feltsignaler i state-laget bruger resultatet til kun at genberegne
#' de visninger, der afhænger af en ændret del. En ingrediensændring behøver
#' eksempelvis ikke at genberegne statistik over aktive retter.
#'
#' @param current_catalog Det nuværende kanoniske snapshot.
#' @param next_catalog Det snapshot, som skal publiceres.
#'
#' @return En tekstvektor med navnene på de ændrede katalogdele.
#' @keywords internal
recipe_catalog_state_changed_fields <- function(
  current_catalog,
  next_catalog
) {
  fields <- c(
    "recipes",
    "links",
    "active_retter",
    "archived_retter",
    "revision"
  )

  fields[vapply(
    fields,
    function(field) {
      !identical(current_catalog[[field]], next_catalog[[field]])
    },
    logical(1)
  )]
}

#' Lav den mindst mulige skriveplan for opskriftslageret
#'
#' Funktionen sammenligner et ændringsforslag med den nuværende kanoniske
#' state. Kun ændrede tabeller og opskrifter kommer med i skriveplanen. Den
#' kontrollerer også revisionen og kræver, at fjernelse af en opskriftsfil er
#' angivet udtrykkeligt i `delete_recipe_keys`.
#'
#' Funktionen skriver ikke filer og ændrer ikke sine argumenter.
#'
#' @param current_catalog Det nuværende kanoniske snapshot.
#' @param next_catalog Det foreslåede nye snapshot.
#' @param delete_recipe_keys Opskriftsnøgler, hvis filer udtrykkeligt må
#'   slettes.
#'
#' @return En navngivet liste, som kan gives til `recipe_store_commit()`.
#' @keywords internal
recipe_catalog_state_commit_plan <- function(
  current_catalog,
  next_catalog,
  delete_recipe_keys = character()
) {
  recipe_catalog_state_validate_snapshot(current_catalog)
  recipe_catalog_state_validate_snapshot(next_catalog)

  if (!identical(next_catalog$revision, current_catalog$revision)) {
    stop(
      "Opskriftskataloget er ændret, siden handlingen begyndte.",
      call. = FALSE
    )
  }

  current_keys <- names(current_catalog$recipes)
  next_keys <- names(next_catalog$recipes)
  if (is.null(current_keys)) current_keys <- character()
  if (is.null(next_keys)) next_keys <- character()

  changed_recipe_keys <- next_keys[vapply(
    next_keys,
    function(key) {
      !key %in% current_keys ||
        !identical(
          next_catalog$recipes[[key]],
          current_catalog$recipes[[key]]
        )
    },
    logical(1)
  )]

  removed_recipe_keys <- setdiff(current_keys, next_keys)
  delete_recipe_keys <- as.character(delete_recipe_keys)
  if (
    !all(removed_recipe_keys %in% delete_recipe_keys) ||
      any(delete_recipe_keys %in% next_keys)
  ) {
    stop(
      paste(
        "Sletning af en opskriftsfil kræver en udtrykkelig",
        "opskriftsnøgle."
      ),
      call. = FALSE
    )
  }

  list(
    active_retter = if (
      identical(next_catalog$active_retter, current_catalog$active_retter)
    ) NULL else next_catalog$active_retter,
    archived_retter = if (
      identical(next_catalog$archived_retter, current_catalog$archived_retter)
    ) NULL else next_catalog$archived_retter,
    links = if (
      identical(next_catalog$links, current_catalog$links)
    ) NULL else next_catalog$links,
    recipes = next_catalog$recipes[changed_recipe_keys],
    delete_recipe_keys = delete_recipe_keys,
    expected_revision = current_catalog$revision
  )
}

#' Opret den kanoniske, reaktive state for opskriftskataloget
#'
#' Constructoren læser ét komplet snapshot fra opskriftslageret og beholder
#' det som sessionens eneste autoritative udgave. Den returnerede
#' commit-funktion gemmer altid ændringen på disken, før det nye snapshot
#' publiceres til resten af appen. Hvis gemningen fejler, bliver den hidtidige
#' state derfor stående.
#'
#' En let polling-observer sammenligner med jævne mellemrum revisionen på
#' disken. Hele kataloget genindlæses kun, når revisionen er ændret. Midlertidige
#' læsefejl logges én gang og prøves igen ved næste interval.
#'
#' De injicerbare lagerfunktioner og notifikationsfunktionen gør forløbet
#' direkte testbart uden at ændre globale funktioner.
#'
#' @param session Den aktuelle Shiny-session.
#' @param data_dir Mappen med opskriftslagerets filer.
#' @param poll_interval_ms Antal millisekunder mellem revisionstjek.
#' @param store_read Funktion, der læser et komplet katalog-snapshot.
#' @param store_revision Funktion, der læser revisionen på disken.
#' @param store_commit Funktion, der udfører den atomiske lagring.
#' @param notify Funktion, der viser en fejlbesked til brugeren.
#' @param log_event Valgfri callback til struktureret driftslogging. Callbacken
#'   modtager `level`, `event`, `component`, `fields` og `error`. Logfejl
#'   ignoreres altid og kan derfor ikke ændre state eller brugerflow.
#'
#' @return En liste med præcis `read` og `commit`. `read` indeholder en isoleret
#'   snapshot-getter og reaktive getters for katalogets fem felter. `commit`
#'   gemmer et ændringsforslag og returnerer `TRUE` eller `FALSE`. Dets
#'   valgfrie `log_context` kan indeholde handling, opskrifts-/ingrediensnavn
#'   og særskilte succes- og fejlbeskeder.
create_recipe_catalog_state <- function(
  session,
  data_dir = "./data",
  poll_interval_ms = 2000L,
  store_read = recipe_store_read,
  store_revision = recipe_store_revision,
  store_commit = recipe_store_commit,
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

  initial_catalog <- store_read(data_dir)
  recipe_catalog_state_validate_snapshot(initial_catalog)

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
          component = "recipe_catalog_state",
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
      "recipe_key",
      "recipe_name",
      "ingredient_name",
      "ingredient_row",
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
    if (isTRUE(poll_failure_active)) return(invisible(NULL))

    poll_failure_active <<- TRUE
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

  canonical_catalog <- reactiveVal(initial_catalog)
  field_signals <- reactiveValues(
    recipes = 0L,
    links = 0L,
    active_retter = 0L,
    archived_retter = 0L,
    revision = 0L
  )

  publish <- function(next_catalog) {
    recipe_catalog_state_validate_snapshot(next_catalog)
    current_catalog <- isolate(canonical_catalog())
    changed_fields <- recipe_catalog_state_changed_fields(
      current_catalog,
      next_catalog
    )

    canonical_catalog(next_catalog)
    for (field in changed_fields) {
      field_signals[[field]] <- isolate(field_signals[[field]]) + 1L
    }

    invisible(next_catalog)
  }

  read_field <- function(field) {
    force(field)
    reactive({
      field_signals[[field]]
      isolate(canonical_catalog()[[field]])
    })
  }

  read <- list(
    snapshot = function() isolate(canonical_catalog()),
    recipes = read_field("recipes"),
    links = read_field("links"),
    active_retter = read_field("active_retter"),
    archived_retter = read_field("archived_retter"),
    revision = read_field("revision")
  )

  commit <- function(
    next_catalog,
    delete_recipe_keys = character(),
    error_message = "Ændringen kunne ikke gemmes.",
    log_context = list()
  ) {
    started_at <- proc.time()[["elapsed"]]
    recipe_count <- if (
      is.list(next_catalog) && is.list(next_catalog$recipes)
    ) {
      as.integer(length(next_catalog$recipes))
    } else {
      NA_integer_
    }
    changed_recipe_count <- NA_integer_
    deleted_recipe_count <- if (is.atomic(delete_recipe_keys)) {
      as.integer(length(delete_recipe_keys))
    } else {
      NA_integer_
    }
    commit_stage <- "plan"

    tryCatch(
      {
        current_catalog <- isolate(canonical_catalog())
        plan <- recipe_catalog_state_commit_plan(
          current_catalog,
          next_catalog,
          delete_recipe_keys
        )
        changed_recipe_count <- as.integer(length(plan$recipes))
        deleted_recipe_count <- as.integer(length(plan$delete_recipe_keys))

        commit_stage <- "store_commit"
        next_revision <- store_commit(
          data_dir = data_dir,
          active_retter = plan$active_retter,
          archived_retter = plan$archived_retter,
          links = plan$links,
          recipes = plan$recipes,
          delete_recipe_keys = plan$delete_recipe_keys,
          expected_revision = plan$expected_revision
        )
        commit_stage <- "validate_revision"
        if (
          length(next_revision) != 1L ||
            is.na(next_revision)
        ) {
          stop(
            "Opskriftslageret returnerede ikke en gyldig revision.",
            call. = FALSE
          )
        }

        persisted_catalog <- next_catalog
        persisted_catalog$revision <- next_revision
        commit_stage <- "publish"
        publish(persisted_catalog)
        commit_stage <- "complete"
        log_safely(
          level = "INFO",
          event = "commit_succeeded",
          fields = c(
            commit_log_fields(
              log_context,
              succeeded = TRUE,
              fallback_message = "Opskriftskataloget blev gemt."
            ),
            list(
              recipe_count = recipe_count,
              changed_recipe_count = changed_recipe_count,
              deleted_recipe_count = deleted_recipe_count,
              duration_ms = elapsed_ms(started_at),
              stage = commit_stage,
              outcome = "succeeded"
            )
          )
        )
        invisible(TRUE)
      },
      error = function(error) {
        conflict <- inherits(error, "recipe_store_conflict") ||
          grepl(
            "ændret i en anden session",
            conditionMessage(error),
            fixed = TRUE
          ) ||
          grepl(
            "ændret, siden handlingen begyndte",
            conditionMessage(error),
            fixed = TRUE
          )
        fields <- c(
          commit_log_fields(
            log_context,
            succeeded = FALSE,
            fallback_message = error_message
          ),
          list(
            recipe_count = recipe_count,
            changed_recipe_count = changed_recipe_count,
            deleted_recipe_count = deleted_recipe_count,
            duration_ms = elapsed_ms(started_at),
            stage = commit_stage
          )
        )
        if (conflict) fields$refresh <- "not_attempted"
        fields$outcome <- if (conflict) "rejected" else "failed"
        log_safely(
          level = if (conflict) "WARN" else "ERROR",
          event = if (conflict) {
            "commit_conflict"
          } else {
            "commit_failed"
          },
          fields = fields,
          error = error
        )
        notify(
          paste(error_message, conditionMessage(error)),
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

    known_revision <- isolate(canonical_catalog()$revision)
    if (is.null(disk_revision)) {
      record_poll_failure(
        simpleError("Opskriftslageret returnerede ingen revision."),
        stage = "revision",
        started_at = started_at
      )
      return(invisible(NULL))
    }
    if (identical(disk_revision, known_revision)) {
      record_poll_recovery("revision", started_at)
      return(invisible(NULL))
    }

    refreshed_catalog <- tryCatch(
      store_read(data_dir),
      error = identity
    )
    if (inherits(refreshed_catalog, "error")) {
      record_poll_failure(
        refreshed_catalog,
        stage = "refresh",
        started_at = started_at
      )
      return(invisible(NULL))
    }
    if (is.null(refreshed_catalog)) {
      record_poll_failure(
        simpleError("Opskriftslageret returnerede intet snapshot."),
        stage = "refresh",
        started_at = started_at
      )
      return(invisible(NULL))
    }

    publish_error <- tryCatch(
      {
        publish(refreshed_catalog)
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
        recipe_count = as.integer(length(refreshed_catalog$recipes)),
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
