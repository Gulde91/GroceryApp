mod_inspiration_tab_ui <- function(id) {
  ns <- NS(id)

  f7Tab(
      tabName = "Inspiration",
      icon = f7Icon("sparkles"),
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
        sInput(
          ns("menu_type"),
          "Vælg type",
          c("Alle", "Vegetar", "Kylling", "Gris", "Okse", "Fisk")
        ),
        wordcloud2Output(ns("wordcloud_retter"), height = "250px")
      )
    )
}

mod_inspiration_sheet_ui <- function(id) {
  ns <- NS(id)

  f7Sheet(
    id = ns("plot_filters_sheet"),
    label = "Filtre for statistik",
    orientation = "bottom",
    swipeToClose = TRUE,
    backdrop = TRUE,
    f7Block(
      strong = TRUE,
      f7Slider(ns("top_n"), "Antal top-opskrifter", 1, 20, 10),
      f7DatePicker(ns("date_from"), "Fra dato"),
      f7DatePicker(ns("date_to"), "Til dato"),
      tags$a(
        class = "sheet-close",
        f7Button(ns("close_filters"), "Luk", fill = TRUE, color = "gray")
      )
    )
  )
}

mod_inspiration_server <- function(id, retter) {
  moduleServer(id, function(input, output, session) {
    output$wordcloud_retter <- renderWordcloud2({
      retter_tmp <- retter

      if (input$menu_type != "Alle") {
        retter_tmp <- filter(retter_tmp, grepl(tolower(input$menu_type), type))
      }

      farver <- c("#fde68a", "#bef264", "#6ee7b7", "#93c5fd", "#e5e7eb")

      retter_tmp %>%
        filter(retter != "Vælg ret") %>%
        select(retter) %>%
        mutate(count = sample(c(0.4, 0.45, 0.5), nrow(.),
          replace = TRUE, prob = c(0.6, 0.3, 0.1)
        )) %>%
        wordcloud2(
          size = 0.1,
          color = sample(farver, size = nrow(.), replace = TRUE),
          backgroundColor = "#1c1c1e",
          shape = "circle",
          rotateRatio = 0
        )
    })

    output$opskrifter_statistik_plot <- renderPlot({
      opskrifter_statistik <- brugte_opskrifter(retter$retter)

      plot_brugte_opskrifter(
        opskrifter_statistik,
        dato_start = input$date_from,
        dato_slut = input$date_to,
        top_n = input$top_n
      )
    })
  })
}
