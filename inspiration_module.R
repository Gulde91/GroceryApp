library(shiny)
library(shinyMobile)
library(dplyr)
library(ggplot2)
library(forcats)
library(wordcloud2)

#' Byg brugerfladen til fanen Inspiration
#'
#' Funktionen samler statistikplottet, knappen til filtre og ordskyen. Alle
#' input og output får modulets namespace, så fanen ikke deler id'er med
#' andre dele af appen.
#'
#' @param id Modul-id'et, som også bruges ved kaldet til servermodulet.
#'
#' @return En Shiny-tagliste, der kan indsættes direkte i fanen Inspiration.
#' @keywords internal
mod_inspiration_ui <- function(id) {
  ns <- NS(id)

  tagList(
    f7BlockTitle(title = "Inspiration"),
    f7Block(
      inset = TRUE,
      strong = TRUE,
      tags$a(
        class = "sheet-open",
        `data-sheet` = paste0("#", ns("plot_filters_sheet")),
        f7Button(
          inputId = ns("open_filters"),
          label = "Filtre",
          icon = f7Icon("slider_horizontal_3"),
          fill = TRUE,
          color = "blue"
        )
      ),
      br(),
      plotOutput(ns("opskrifter_statistik_plot"))
    ),
    f7Block(
      inset = TRUE,
      strong = TRUE,
      selectInput(
        inputId = ns("menu_type"),
        label = "Vælg type",
        choices = c("Alle", "Vegetar", "Kylling", "Gris", "Okse", "Fisk"),
        width = "100%",
        selectize = FALSE
      ),
      wordcloud2Output(ns("wordcloud_retter"), height = "250px")
    )
  )
}

#' Byg det namespacede filter-sheet til inspirationsfanen
#'
#' Filter-sheetet ligger separat fra fanens almindelige indhold, fordi
#' Framework7 viser det som et lag oven på siden. Knappen i
#' `mod_inspiration_ui()` peger på sheetets namespacede HTML-id.
#'
#' @param id Det samme modul-id, som bruges til inspirationsfanens UI og
#'   server.
#'
#' @return Et Framework7-sheet med valg af periode og antal opskrifter.
#' @keywords internal
mod_inspiration_filters_ui <- function(id) {
  ns <- NS(id)

  f7Sheet(
    id = ns("plot_filters_sheet"),
    label = "Filtre for statistik",
    orientation = "bottom",
    swipeToClose = TRUE,
    backdrop = TRUE,
    f7Block(
      strong = TRUE,
      f7Slider(
        ns("top_n"),
        "Antal top-opskrifter",
        min = 1,
        max = 20,
        value = 10
      ),
      f7DatePicker(ns("date_from"), "Fra dato"),
      f7DatePicker(ns("date_to"), "Til dato"),
      tags$a(
        class = "sheet-close",
        f7Button(
          ns("close_filters"),
          "Luk",
          fill = TRUE,
          color = "gray"
        )
      )
    )
  )
}

#' Kør serverlogikken til fanen Inspiration
#'
#' Modulet læser både aktive opskrifter og indkøbshistorik gennem read-only
#' getters. Det ejer dermed ingen lokal kopi af data og behøver ikke kende
#' placeringen eller formatet på historikfilerne.
#'
#' @param input Modulets namespacede Shiny-input.
#' @param output Modulets namespacede Shiny-output.
#' @param session Modulets Shiny-session.
#' @param active_recipes_current Read-only getter, der returnerer de aktuelle
#'   aktive retter som en data frame med kolonnerne `retter` og `type`.
#' @param history_current Read-only getter, der returnerer den kanoniske
#'   indkøbshistorik. Hver række beskriver én linje fra en gemt
#'   indkøbsseddel.
#' @param today Dagens dato. Kan injiceres i tests for at gøre
#'   datoinitialiseringen deterministisk.
#'
#' @return En liste med read-only reaktive getters til filtrerede retter og
#'   statistikdata. Listen er primært nyttig i isolerede tests.
#' @keywords internal
mod_inspiration_server <- function(
  input,
  output,
  session,
  active_recipes_current,
  history_current,
  today = Sys.Date
) {
  if (!is.function(active_recipes_current)) {
    stop("active_recipes_current skal være en read-only getter.", call. = FALSE)
  }
  if (!is.function(history_current)) {
    stop("history_current skal være en read-only getter.", call. = FALSE)
  }
  if (!is.function(today)) {
    stop("today skal være en funktion.", call. = FALSE)
  }

  observe({
    current_date <- as.Date(today())
    updateF7DatePicker(
      "date_from",
      inspiration_minimum_date(current_date),
      dateFormat = "dd-mm-yyyy",
      session = session
    )
    updateF7DatePicker(
      "date_to",
      current_date,
      dateFormat = "dd-mm-yyyy",
      session = session
    )
  })

  active_recipes <- reactive({
    inspiration_active_recipes(active_recipes_current())
  })

  filtered_recipes <- reactive({
    inspiration_filter_recipes(active_recipes(), input$menu_type)
  })

  recipe_statistics <- reactive({
    shopping_history_recipe_usage(
      history_current(),
      active_recipes()$retter
    )
  })

  output$wordcloud_retter <- renderWordcloud2({
    recipes <- filtered_recipes()
    req(nrow(recipes) > 0L)
    inspiration_wordcloud(recipes)
  })

  output$opskrifter_statistik_plot <- renderPlot({
    req(input$date_from, input$date_to, input$top_n)
    plot_brugte_opskrifter(
      recipe_statistics(),
      dato_start = input$date_from,
      dato_slut = input$date_to,
      top_n = input$top_n
    )
  })

  list(
    active_recipes = active_recipes,
    filtered_recipes = filtered_recipes,
    recipe_statistics = recipe_statistics
  )
}

#' Beregn inspirationsfanens tidligste standarddato
#'
#' Appen viser højst det seneste år, men aldrig data før 1. december 2025.
#' Funktionen er separat, så reglen kan testes uden en Shiny-session.
#'
#' @param current_date Dagens dato som `Date` eller en konvertérbar værdi.
#'
#' @return En `Date` med filterets tidligste standarddato.
#' @keywords internal
inspiration_minimum_date <- function(current_date = Sys.Date()) {
  current_date <- as.Date(current_date)
  max(
    as.Date("2025-12-01"),
    inspiration_previous_year_date(current_date)
  )
}

#' Find samme kalenderdato året før
#'
#' Beregningen bruger kun base R og håndterer skuddag eksplicit. Dermed
#' undgår modulet en ekstra datoafhængighed og en navnekonflikt med
#' shinyjs-funktionen `show()` i de øvrige moduler.
#'
#' @param current_date En dato, der kan konverteres med `as.Date()`.
#'
#' @return Datoen ét kalenderår tidligere. Den 29. februar bliver til den
#'   28. februar i et år uden skuddag.
#' @keywords internal
inspiration_previous_year_date <- function(current_date) {
  current_date <- as.Date(current_date)
  previous_year <- as.integer(format(current_date, "%Y")) - 1L
  candidate <- suppressWarnings(as.Date(
    sprintf(
      "%04d-%s",
      previous_year,
      format(current_date, "%m-%d")
    ),
    format = "%Y-%m-%d"
  ))

  if (is.na(candidate)) {
    candidate <- as.Date(sprintf("%04d-02-28", previous_year))
  }

  candidate
}

#' Kontrollér formen på de aktive opskrifter
#'
#' En tydelig validering giver en brugbar fejl, hvis root-serverens getter en
#' dag får en anden kontrakt end modulet forventer.
#'
#' @param recipes Data frame med aktive opskrifter.
#'
#' @return `recipes` uændret, når de krævede kolonner findes.
#' @keywords internal
inspiration_active_recipes <- function(recipes) {
  required_columns <- c("retter", "type")
  if (
    !is.data.frame(recipes) ||
      !all(required_columns %in% names(recipes))
  ) {
    stop(
      "Aktive opskrifter skal have kolonnerne 'retter' og 'type'.",
      call. = FALSE
    )
  }

  recipes
}

#' Filtrér aktive opskrifter til ordskyen
#'
#' Pladsholderen `Vælg ret` fjernes altid. Et konkret menuvalg filtrerer på
#' opskriftens type på samme måde som den oprindelige inspirationsfane.
#'
#' @param recipes Valideret data frame med aktive opskrifter.
#' @param menu_type Valgt type, eksempelvis `Alle` eller `Vegetar`.
#'
#' @return En data frame med de opskrifter, der skal vises i ordskyen.
#' @keywords internal
inspiration_filter_recipes <- function(recipes, menu_type = "Alle") {
  recipes <- inspiration_active_recipes(recipes)
  if (is.null(menu_type) || length(menu_type) != 1L || is.na(menu_type)) {
    menu_type <- "Alle"
  }

  recipe_names <- trimws(as.character(recipes$retter))
  recipe_types <- trimws(as.character(recipes$type))
  keep <- !is.na(recipe_names) &
    nzchar(recipe_names) &
    recipe_names != "Vælg ret"
  filtered <- recipes[keep, , drop = FALSE]
  filtered$retter <- recipe_names[keep]
  filtered$type <- recipe_types[keep]
  if (!identical(menu_type, "Alle")) {
    keep <- grepl(tolower(menu_type), tolower(filtered$type))
    keep[is.na(keep)] <- FALSE
    filtered <- filtered[keep, , drop = FALSE]
  }

  filtered
}

#' Opret en ordsky af de filtrerede opskrifter
#'
#' Alle retter får en lille tilfældig visuel vægt og en farve fra appens
#' eksisterende palette. Tilfældigheden ændrer kun udseendet, ikke hvilke
#' opskrifter der er med.
#'
#' @param recipes Filtrerede aktive opskrifter.
#'
#' @return Et `wordcloud2`-widget til Shiny-outputtet.
#' @keywords internal
inspiration_wordcloud <- function(recipes) {
  cloud_data <- data.frame(
    retter = as.character(recipes$retter),
    count = sample(
      c(0.4, 0.45, 0.5),
      nrow(recipes),
      replace = TRUE,
      prob = c(0.6, 0.3, 0.1)
    ),
    stringsAsFactors = FALSE
  )
  colors <- c("#fde68a", "#bef264", "#6ee7b7", "#93c5fd", "#e5e7eb")

  wordcloud2(
    cloud_data,
    size = 0.1,
    color = sample(colors, size = nrow(cloud_data), replace = TRUE),
    backgroundColor = "#1c1c1e",
    shape = "circle",
    rotateRatio = 0
  )
}

#' Tegn et søjlediagram over de mest brugte opskrifter
#'
#' Historikken filtreres til den valgte periode. Derefter vises de `top_n`
#' hyppigste retter med samme mørke udtryk som resten af appen. Et tomt eller
#' resultatløst interval giver i stedet et forklarende, tomt plot.
#'
#' @param df Data frame med kolonnerne `retter` og `dato`.
#' @param dato_start Periodens første dato.
#' @param dato_slut Periodens sidste dato.
#' @param top_n Det maksimale antal opskrifter, der skal vises.
#'
#' @return Et `ggplot`-objekt med opskriftsstatistikken.
#' @keywords internal
plot_brugte_opskrifter <- function(
  df,
  dato_start,
  dato_slut,
  top_n = 5
) {
  required_columns <- c("retter", "dato")
  if (!is.data.frame(df) || !all(required_columns %in% names(df))) {
    stop(
      "Opskriftshistorikken skal have kolonnerne 'retter' og 'dato'.",
      call. = FALSE
    )
  }
  if (nrow(df) == 0L) {
    return(
      ggplot() +
        geom_blank() +
        labs(title = "Ingen data i valgt interval")
    )
  }

  start_date <- as.Date(dato_start)
  end_date <- as.Date(dato_slut)
  df_plot <- df |>
    filter(dato >= start_date, dato <= end_date) |>
    mutate(retter = trimws(as.character(retter))) |>
    filter(!is.na(retter), nzchar(retter))

  if (nrow(df_plot) == 0L) {
    return(
      ggplot() +
        geom_blank() +
        labs(title = "Ingen data i dette datointerval")
    )
  }

  top_n <- inspiration_top_n(top_n)
  top_recipes <- df_plot |>
    count(retter) |>
    arrange(desc(n), retter) |>
    slice_head(n = top_n) |>
    pull(retter)
  df_plot <- filter(df_plot, retter %in% top_recipes)

  ggplot(df_plot, aes(x = fct_infreq(retter))) +
    geom_bar(fill = "#8EB5FF") +
    labs(x = "Retter", y = "Antal", title = "Mest brugte opskrifter") +
    theme_minimal(base_size = 16) +
    theme(
      plot.background = element_rect(fill = "#1c1c1e", color = NA),
      panel.background = element_rect(fill = "#1c1c1e", color = NA),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      plot.title = element_text(color = "white", face = "bold", size = 18),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}

#' Kontrollér antallet af opskrifter i statistikplottet
#'
#' @param value Den valgte `top_n`-værdi.
#'
#' @return Et positivt heltal.
#' @keywords internal
inspiration_top_n <- function(value) {
  number <- suppressWarnings(as.numeric(value))
  if (
    length(number) != 1L ||
      is.na(number) ||
      !is.finite(number) ||
      number < 1 ||
      number != floor(number)
  ) {
    stop("top_n skal være ét positivt heltal.", call. = FALSE)
  }

  as.integer(number)
}
