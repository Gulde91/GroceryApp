library(shiny)

# Reaktiv state og lagringskoordinering for opskrifter -----------------------
#
# Denne fil binder det rene opskriftskatalog i recipe_catalog.R sammen med
# fillageret i recipe_store.R. Resten af appen får kun læseadgang og en
# kontrolleret commit-funktion. Den reaktive state og publiceringen holdes
# private i create_recipe_catalog_state().

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
#' læsefejl er tavse og prøves igen ved næste interval.
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
#'
#' @return En liste med præcis `read` og `commit`. `read` indeholder en isoleret
#'   snapshot-getter og reaktive getters for katalogets fem felter. `commit`
#'   gemmer et ændringsforslag og returnerer `TRUE` eller `FALSE`.
create_recipe_catalog_state <- function(
  session,
  data_dir = "./data",
  poll_interval_ms = 2000L,
  store_read = recipe_store_read,
  store_revision = recipe_store_revision,
  store_commit = recipe_store_commit,
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

  initial_catalog <- store_read(data_dir)
  recipe_catalog_state_validate_snapshot(initial_catalog)

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
    error_message = "Ændringen kunne ikke gemmes."
  ) {
    tryCatch(
      {
        current_catalog <- isolate(canonical_catalog())
        plan <- recipe_catalog_state_commit_plan(
          current_catalog,
          next_catalog,
          delete_recipe_keys
        )

        next_revision <- store_commit(
          data_dir = data_dir,
          active_retter = plan$active_retter,
          archived_retter = plan$archived_retter,
          links = plan$links,
          recipes = plan$recipes,
          delete_recipe_keys = plan$delete_recipe_keys,
          expected_revision = plan$expected_revision
        )
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
        publish(persisted_catalog)
        invisible(TRUE)
      },
      error = function(error) {
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
    invalidateLater(poll_interval_ms, session)

    disk_revision <- tryCatch(
      store_revision(data_dir),
      error = function(error) NULL
    )
    known_revision <- isolate(canonical_catalog()$revision)
    if (
      is.null(disk_revision) ||
        identical(disk_revision, known_revision)
    ) {
      return(invisible(NULL))
    }

    refreshed_catalog <- tryCatch(
      store_read(data_dir),
      error = function(error) NULL
    )
    if (is.null(refreshed_catalog)) return(invisible(NULL))

    tryCatch(
      publish(refreshed_catalog),
      error = function(error) NULL
    )
    invisible(NULL)
  })

  list(
    read = read,
    commit = commit
  )
}
