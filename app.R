# ShinyMobile-skal for GroceryApp
library(shiny)
library(shinyMobile)
library(readr)
library(dplyr)
library(fontawesome)
library(shinyjs)

source("./recipe_store.R")
source("./basis_varer_store.R")
source("./data.R")
source("./funktioner.R")
source("./varer_module.R")
source("./cart_state.R")
source("./indkobsseddel_module.R")
source("./recipe_module.R")
source("./inspiration_module.R")


ui <- f7Page(
  
  # opsætning ----
  tags$head(
    includeCSS("www/styles.css"),
    fa_html_dependency(),
    htmltools::singleton(tags$script(src = "selectize-mobile.js")),
    htmltools::singleton(tags$script(src = "button-press.js")),
    htmltools::singleton(tags$script(src = "copy-helper.js")),
    htmltools::singleton(tags$script(src = "DT-copy-feedback.js")),
    htmltools::singleton(tags$script(src = "pwa-viewport-fix.js"))
  ),
  
  useShinyjs(),

  title = "IndkøbsApp",
  options = list(
    theme = "auto",
    dark = TRUE,
    preloader = TRUE
    ),
  
  f7TabLayout(
    navbar = f7Navbar(title = "IndkøbsApp"),
    f7Tabs(
      id = "main_tabs",
      animated = FALSE,
      swipeable = FALSE,
      # Indkøbsliste ----
      f7Tab(
        tabName = "Indkøbsseddel",
        icon = f7Icon("cart"),
        active = TRUE,
        mod_indkobsseddel_ui("indkobsseddel")
      ),
      # Varer (bruttoliste) ----
      f7Tab(
        tabName = "Varer",
        icon = f7Icon("square_list"),
        active = FALSE,
        mod_varer_ui("varer")
      ),
      # Opskrifter ----
      f7Tab(
        tabName = "Opskrifter",
        icon = f7Icon("book"),
        active = FALSE,
        mod_opskrifter_ui("opskrifter")
      ),
      # Inspiration----
      f7Tab(
        tabName = "Inspiration",
        icon = f7Icon("sparkles"),
        mod_inspiration_ui("inspiration")
      )
    ),
    
    # Modals ----
    mod_indkobsseddel_dialogs_ui(
      "indkobsseddel",
      salat_choices = salater$retter,
      tilbehor_choices = tilbehor$Indkobsliste
    ),
    mod_varer_dialogs_ui("varer"),
    mod_opskrifter_dialogs_ui("opskrifter"),
    mod_inspiration_filters_ui("inspiration")
    
  )

)

server <- function(input, output, session) {

  # Sætter reaktive værdier ----
  initial_recipe_store <- recipe_store_read("./data")
  initial_basis_varer_store <- basis_varer_store_read("./data")
  rv_recipeCatalog <- reactiveVal(initial_recipe_store)
  rv_basisVarerStore <- reactiveVal(initial_basis_varer_store)
  rv_recipeCatalogSignals <- reactiveValues(
    recipes = 0L,
    links = 0L,
    active_retter = 0L,
    archived_retter = 0L,
    revision = 0L
  )
  rv_varer_custom <- reactive({
    rv_basisVarerStore()$varer
  })

  # Hele opskriftskataloget har én reaktiv datakilde. Signalerne indeholder
  # ingen kopier af data; de sørger blot for, at fx en ingrediensændring ikke
  # genberegner statistik, som kun afhænger af aktive retter.
  recipe_catalog_current <- reactive({
    rv_recipeCatalog()
  })

  recipes_current <- reactive({
    rv_recipeCatalogSignals$recipes
    isolate(rv_recipeCatalog()$recipes)
  })

  recipe_links_current <- reactive({
    rv_recipeCatalogSignals$links
    isolate(rv_recipeCatalog()$links)
  })

  active_recipes_current <- reactive({
    rv_recipeCatalogSignals$active_retter
    isolate(rv_recipeCatalog()$active_retter)
  })

  archived_recipes_current <- reactive({
    rv_recipeCatalogSignals$archived_retter
    isolate(rv_recipeCatalog()$archived_retter)
  })

  recipe_revision_current <- reactive({
    rv_recipeCatalogSignals$revision
    isolate(rv_recipeCatalog()$revision)
  })

  publish_recipe_catalog <- function(next_catalog) {
    current_catalog <- isolate(rv_recipeCatalog())

    changed_fields <- c(
      "recipes",
      "links",
      "active_retter",
      "archived_retter",
      "revision"
    )
    changed_fields <- changed_fields[vapply(
      changed_fields,
      function(field) {
        !identical(current_catalog[[field]], next_catalog[[field]])
      },
      logical(1)
    )]

    rv_recipeCatalog(next_catalog)
    for (field in changed_fields) {
      rv_recipeCatalogSignals[[field]] <-
        isolate(rv_recipeCatalogSignals[[field]]) + 1L
    }

    invisible(next_catalog)
  }

  publish_basis_varer_store <- function(next_snapshot) {
    if (
      !is.list(next_snapshot) ||
        !identical(
          names(next_snapshot),
          c("varer", "revision")
        ) ||
        !is.data.frame(next_snapshot$varer) ||
        length(next_snapshot$revision) != 1L ||
        is.na(next_snapshot$revision)
    ) {
      stop(
        "Det nye basisvaresnapshot er ugyldigt.",
        call. = FALSE
      )
    }

    rv_basisVarerStore(next_snapshot)
    invisible(next_snapshot)
  }

  # Andre browser-sessioner deler de samme filer. Hent et nyt snapshot, hvis
  # revisionen på disken ændres, så en åben fane ikke fortsætter med gamle
  # opskrifter efter fx arkivering eller permanent sletning et andet sted.
  observe({
    invalidateLater(2000, session)

    disk_revision <- tryCatch(
      recipe_store_revision("./data"),
      error = function(error) NULL
    )
    known_revision <- isolate(recipe_revision_current())

    if (
      is.null(disk_revision) ||
        identical(disk_revision, known_revision)
    ) {
      return(invisible(NULL))
    }

    snapshot <- tryCatch(
      recipe_store_read("./data"),
      error = function(error) NULL
    )
    if (is.null(snapshot)) return(invisible(NULL))

    publish_recipe_catalog(snapshot)
  })

  # Basisvarer bruger samme lette refresh-mønster som opskrifterne. I den
  # normale én-bruger-situation er dette blot et MD5-tjek af én lille fil.
  observe({
    invalidateLater(2000, session)

    disk_revision <- tryCatch(
      basis_varer_store_revision("./data"),
      error = identity
    )
    if (inherits(disk_revision, "error")) {
      return(invisible(NULL))
    }

    known_revision <- isolate(
      rv_basisVarerStore()$revision
    )
    if (identical(disk_revision, known_revision)) {
      return(invisible(NULL))
    }

    snapshot <- tryCatch(
      basis_varer_store_read("./data"),
      error = identity
    )
    if (inherits(snapshot, "error")) {
      return(invisible(NULL))
    }

    publish_basis_varer_store(snapshot)
  })
  
  # laves som reactive (og ikke reactiveVal) fordi der ikke kan indgå
  # reactive elementer i en reactiveVal
  rv_varer <- reactive({
    opskrift_df_custom <- c(recipes_current(), salater_opskrifter) |>
      lapply(function(x) {names(x)[1] <- "Indkobsliste"; x}) |>
      bind_rows() |>
      mutate(maengde = 1)

    # Brugerens basisvare vinder over en opskriftsingrediens med samme navn.
    # Det samlede katalog har derefter præcis én række pr. normaliseret navn.
    bind_rows(rv_varer_custom(), opskrift_df_custom) |>
      mutate(
        Indkobsliste = trimws(as.character(Indkobsliste)),
        vare_nogle = tolower(Indkobsliste),
        maengde = 1
      ) |>
      filter(!is.na(Indkobsliste), nzchar(Indkobsliste)) |>
      distinct(vare_nogle, .keep_all = TRUE) |>
      select(-vare_nogle) |>
      arrange(Indkobsliste) |>
      select(Indkobsliste, maengde, enhed, kat_1, kat_2)
  }) 

  # Basisvarer gemmes ét sted og publiceres først til resten af appen, når
  # filskrivningen er lykkedes.
  commit_basis_varer_change <- function(
    next_varer,
    error_message = "Ændringen af basisvarerne kunne ikke gemmes."
  ) {
    tryCatch(
      {
        next_snapshot <- basis_varer_store_commit(
          next_varer,
          expected_revision = isolate(
            rv_basisVarerStore()$revision
          ),
          data_dir = "./data"
        )
        publish_basis_varer_store(next_snapshot)
        TRUE
      },
      error = function(error) {
        notification_detail <- conditionMessage(error)
        if (
          inherits(
            error,
            "basis_varer_store_conflict"
          )
        ) {
          latest_snapshot <- tryCatch(
            basis_varer_store_read("./data"),
            error = identity
          )
          if (!inherits(latest_snapshot, "error")) {
            publish_basis_varer_store(latest_snapshot)
            notification_detail <- paste(
              "Basisvarerne var ændret i en anden session.",
              "Listen er nu opdateret; prøv handlingen igen."
            )
          } else {
            notification_detail <- paste(
              conditionMessage(error),
              "Den nyeste liste kunne ikke genindlæses:",
              conditionMessage(latest_snapshot)
            )
          }
        }

        showNotification(
          paste(error_message, notification_detail),
          type = "error",
          duration = NULL
        )
        FALSE
      }
    )
  }

  callModule(
    mod_varer_server,
    "varer",
    varer_custom_current = rv_varer_custom,
    varer_all_current = rv_varer,
    commit_varer = commit_basis_varer_change
  )
  
  # Opskriftsmodulet gemmer gennem root, så katalogets kanoniske state
  # og persistens fortsat har ét ansvarligt sted.
  commit_recipe_store_change <- function(
    next_catalog,
    delete_recipe_keys = character(),
    error_message = "Ændringen kunne ikke gemmes."
  ) {
    tryCatch(
      {
        required_fields <- c(
          "active_retter",
          "archived_retter",
          "recipes",
          "links",
          "revision"
        )
        if (
          !is.list(next_catalog) ||
            !all(required_fields %in% names(next_catalog))
        ) {
          stop("Det nye opskriftskatalog er ufuldstændigt.", call. = FALSE)
        }

        current_catalog <- isolate(recipe_catalog_current())
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
            "Sletning af en opskriftsfil kræver en udtrykkelig opskriftsnøgle.",
            call. = FALSE
          )
        }

        changed_or_null <- function(field) {
          if (identical(next_catalog[[field]], current_catalog[[field]])) {
            NULL
          } else {
            next_catalog[[field]]
          }
        }

        next_revision <- recipe_store_commit(
          data_dir = "./data",
          active_retter = changed_or_null("active_retter"),
          archived_retter = changed_or_null("archived_retter"),
          links = changed_or_null("links"),
          recipes = next_catalog$recipes[changed_recipe_keys],
          delete_recipe_keys = delete_recipe_keys,
          expected_revision = current_catalog$revision
        )

        next_catalog$revision <- next_revision
        publish_recipe_catalog(next_catalog)
        TRUE
      },
      error = function(error) {
        showNotification(
          paste(error_message, conditionMessage(error)),
          type = "error",
          duration = NULL
        )
        FALSE
      }
    )
  }
  recipe_catalog_read <- list(
    snapshot = function() isolate(recipe_catalog_current()),
    recipes = recipes_current,
    links = recipe_links_current,
    active_retter = active_recipes_current,
    archived_retter = archived_recipes_current,
    revision = recipe_revision_current,
    salater = reactive(salater),
    salater_opskrifter = reactive(salater_opskrifter),
    tilbehor = reactive(tilbehor)
  )

  popular_items_current <- reactive({
    mest_brugte_varer(rv_varer()$enhed)
  })

  callModule(
    mod_inspiration_server,
    "inspiration",
    active_recipes_current = active_recipes_current
  )

  indkobsseddel_api <- callModule(
    mod_indkobsseddel_server,
    "indkobsseddel",
    recipe_read = recipe_catalog_read,
    varer_current = rv_varer,
    save_cart = indkobsseddel_save_history,
    popular_items = popular_items_current
  )

  callModule(
    mod_opskrifter_server,
    "opskrifter",
    catalog_read = recipe_catalog_read,
    commit_catalog = commit_recipe_store_change,
    varer_current = rv_varer
  )

}

shinyApp(ui = ui, server = server)
