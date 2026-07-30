# ShinyMobile-skal for GroceryApp
library(shiny)
library(shinyMobile)
library(readr)
library(dplyr)
library(fontawesome)
library(shinyjs)

source("./recipe_schema.R")
source("./recipe_store.R")
source("./recipe_catalog.R")
source("./recipe_catalog_state.R")
source("./basis_varer_store.R")
source("./basis_varer_state.R")
source("./shopping_history_store.R")
source("./data.R")
source("./funktioner.R")
source("./varer_module.R")
source("./cart_state.R")
source("./indkobsseddel_catalog.R")
source("./indkobsseddel_view.R")
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
  history_dir <- "./data/indkobssedler"
  initial_history_store <- shopping_history_store_read(history_dir)
  rv_historyStore <- reactiveVal(initial_history_store)
  recipe_state <- create_recipe_catalog_state(
    session = session,
    data_dir = "./data"
  )
  basis_state <- create_basis_varer_state(
    session = session,
    data_dir = "./data"
  )

  history_current <- reactive({
    rv_historyStore()$entries
  })
  
  # laves som reactive (og ikke reactiveVal) fordi der ikke kan indgå
  # reactive elementer i en reactiveVal
  rv_varer <- reactive({
    opskrift_df_custom <- c(
      recipe_state$read$recipes(),
      salater_opskrifter
    ) |>
      lapply(function(x) {names(x)[1] <- "Indkobsliste"; x}) |>
      bind_rows() |>
      mutate(maengde = 1)

    # Brugerens basisvare vinder over en opskriftsingrediens med samme navn.
    # Det samlede katalog har derefter præcis én række pr. normaliseret navn.
    bind_rows(basis_state$read$varer(), opskrift_df_custom) |>
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

  callModule(
    mod_varer_server,
    "varer",
    varer_custom_current = basis_state$read$varer,
    varer_all_current = rv_varer,
    commit_varer = basis_state$commit
  )

  # Historiklageret returnerer først et nyt komplet snapshot, når filen er
  # gemt. Begge historikforbrugere ser derfor samme opdatering på én gang.
  commit_shopping_history <- function(history_df) {
    current_snapshot <- isolate(rv_historyStore())
    save_result <- tryCatch(
      shopping_history_store_save(
        history_df,
        expected_revision = current_snapshot$revision,
        history_dir = history_dir
      ),
      shopping_history_store_conflict = identity
    )

    if (inherits(
      save_result,
      "shopping_history_store_conflict"
    )) {
      refreshed <- tryCatch(
        shopping_history_store_read(history_dir),
        error = identity
      )
      if (inherits(refreshed, "error")) {
        stop(
          paste(
            "Indkøbshistorikken er ændret i en anden session,",
            "og den nyeste historik kunne ikke indlæses:",
            conditionMessage(refreshed)
          ),
          call. = FALSE
        )
      }

      rv_historyStore(refreshed)
      stop(
        paste(
          "Indkøbshistorikken blev ændret i en anden session.",
          "Historikken er nu opdateret; prøv at gemme igen."
        ),
        call. = FALSE
      )
    }

    rv_historyStore(save_result)
    TRUE
  }
  
  recipe_catalog_read <- c(
    recipe_state$read,
    list(
      salater = reactive(salater),
      salater_opskrifter = reactive(salater_opskrifter),
      tilbehor = reactive(tilbehor)
    )
  )

  popular_items_current <- reactive({
    shopping_history_popular_items(
      history_current(),
      rv_varer()$enhed
    )
  })

  inspiration_api <- callModule(
    mod_inspiration_server,
    "inspiration",
    active_recipes_current = recipe_state$read$active_retter,
    history_current = history_current
  )

  indkobsseddel_api <- callModule(
    mod_indkobsseddel_server,
    "indkobsseddel",
    recipe_read = recipe_catalog_read,
    varer_current = rv_varer,
    save_cart = commit_shopping_history,
    popular_items = popular_items_current
  )

  callModule(
    mod_opskrifter_server,
    "opskrifter",
    catalog_read = recipe_catalog_read,
    commit_catalog = recipe_state$commit,
    varer_current = rv_varer
  )

}

shinyApp(ui = ui, server = server)
