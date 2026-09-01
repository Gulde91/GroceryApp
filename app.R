# ShinyMobile-skal for GroceryApp
library(shiny)
library(shinyMobile)
library(readr)
library(dplyr)
library(fontawesome)
library(shinyjs)

# Ved normal appstart indlæser Shiny automatisk alle scripts i R-mappen.
# Denne korte fallback gør det fortsat muligt at source app.R direkte i tests.
if (!exists("mod_varer_server", mode = "function")) {
  invisible(lapply(
    sort(list.files("R", pattern = "\\.R$", full.names = TRUE)),
    source,
    local = FALSE,
    encoding = "UTF-8"
  ))
}


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

# Registrér procesniveauets livscyklus i journald. App-stop-hooket oprettes i
# onStart-konteksten, hvor Shiny registrerer det for hele applikationen.
app_on_start <- function() {
  process_context <- app_log_context()
  app_log_event(
    level = "INFO",
    event = "application_started",
    component = "application",
    fields = list(
      message = "GroceryApp blev startet.",
      process_id = Sys.getpid()
    ),
    context = process_context
  )

  onStop(function() {
    app_log_event(
      level = "INFO",
      event = "application_stopped",
      component = "application",
      fields = list(
        message = "GroceryApp blev stoppet.",
        process_id = Sys.getpid()
      ),
      context = process_context
    )
  })
}

server <- function(input, output, session) {

  session_context <- app_log_new_session_context()
  session_log <- app_log_bind(session_context)
  session_started_at <- proc.time()[["elapsed"]]
  session_log(
    "INFO",
    "session_started",
    "application",
    fields = list(message = "En brugersession blev startet.")
  )
  session$onSessionEnded(function() {
    duration_ms <- as.integer(round(
      max(0, proc.time()[["elapsed"]] - session_started_at) * 1000
    ))
    session_log(
      "INFO",
      "session_ended",
      "application",
      fields = list(
        message = "En brugersession blev afsluttet.",
        duration_ms = duration_ms
      )
    )
  })
  if (exists("onUnhandledError", mode = "function")) {
    onUnhandledError(
      function(error) {
        session_log(
          "ERROR",
          "session_unhandled_error",
          "application",
          fields = list(
            message = "En uventet fejl opstod i brugersessionen.",
            fatal = inherits(error, "shiny.error.fatal")
          ),
          error = error
        )
      },
      session = session
    )
  }

  # Opretter appens kanoniske state-lag ----
  history_state <- create_shopping_history_state(
    session = session,
    history_dir = "./data/indkobssedler",
    log_event = session_log
  )
  recipe_state <- create_recipe_catalog_state(
    session = session,
    data_dir = "./data",
    log_event = session_log
  )
  basis_state <- create_basis_varer_state(
    session = session,
    data_dir = "./data",
    log_event = session_log
  )

  history_current <- history_state$read$entries
  
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
    save_cart = history_state$commit,
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

shinyApp(ui = ui, server = server, onStart = app_on_start)
