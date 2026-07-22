suppressPackageStartupMessages({
  source("shopping_history_store.R", encoding = "UTF-8")
  source("inspiration_module.R", encoding = "UTF-8")
})

inspiration_test_server <- function(
  id,
  active_recipes_current,
  history_current,
  today
) {
  moduleServer(id, function(input, output, session) {
    module_api <- mod_inspiration_server(
      input = input,
      output = output,
      session = session,
      active_recipes_current = active_recipes_current,
      history_current = history_current,
      today = today
    )
  })
}

inspiration_expect_error <- function(expression, pattern) {
  error <- tryCatch(
    {
      force(expression)
      NULL
    },
    error = identity
  )
  stopifnot(
    inherits(error, "error"),
    grepl(pattern, conditionMessage(error), fixed = TRUE)
  )
}

run_inspiration_module_tests <- function() {
  # Fanen og sheetet skal dele namespace, mens knappen peger på det
  # namespacede sheet-id.
  module_html <- paste(
    as.character(
      tagList(
        mod_inspiration_ui("inspiration"),
        mod_inspiration_filters_ui("inspiration")
      )
    ),
    collapse = ""
  )
  expected_ids <- c(
    "inspiration-open_filters",
    "inspiration-opskrifter_statistik_plot",
    "inspiration-menu_type",
    "inspiration-wordcloud_retter",
    "inspiration-plot_filters_sheet",
    "inspiration-top_n",
    "inspiration-date_from",
    "inspiration-date_to",
    "inspiration-close_filters"
  )
  stopifnot(all(vapply(
    expected_ids,
    function(id) grepl(paste0('id="', id, '"'), module_html, fixed = TRUE),
    logical(1)
  )))
  stopifnot(
    grepl(
      'data-sheet="#inspiration-plot_filters_sheet"',
      module_html,
      fixed = TRUE
    ),
    !grepl('id="plot_filters_sheet"', module_html, fixed = TRUE),
    !grepl('id="menu_type"', module_html, fixed = TRUE)
  )

  # Standardperioden bevarer den oprindelige regel: seneste år, men aldrig
  # før december 2025.
  stopifnot(
    identical(
      inspiration_minimum_date(as.Date("2026-07-21")),
      as.Date("2025-12-01")
    ),
    identical(
      inspiration_minimum_date(as.Date("2027-07-21")),
      as.Date("2026-07-21")
    ),
    identical(
      inspiration_previous_year_date(as.Date("2024-02-29")),
      as.Date("2023-02-28")
    )
  )

  active_fixture <- data.frame(
    retter = c(
      "Vælg ret",
      "Burger",
      "Tortellini",
      "Fiskefrikadeller",
      " ",
      NA_character_
    ),
    type = c("", "okse", "Vegetar", "fisk", "", NA_character_),
    key = c(
      "",
      "burger",
      "tortellini",
      "fiskefrikadeller",
      "blank",
      "mangler"
    ),
    stringsAsFactors = FALSE
  )
  all_recipes <- inspiration_filter_recipes(active_fixture, "Alle")
  vegetarian_recipes <- inspiration_filter_recipes(active_fixture, "Vegetar")
  stopifnot(
    identical(
      all_recipes$retter,
      c("Burger", "Tortellini", "Fiskefrikadeller")
    ),
    identical(vegetarian_recipes$retter, "Tortellini")
  )
  inspiration_expect_error(
    inspiration_active_recipes(data.frame(retter = "Burger")),
    "kolonnerne 'retter' og 'type'"
  )

  # Modulet modtager allerede validerede historikrækker fra det fælles store.
  # Filindlæsning og fortolkning af opskriftsoverskrifter testes derfor dér.
  history_entries <- data.frame(
    filename = c(
      "indkobsseddel_20260601.rda",
      "indkobsseddel_20260601.rda",
      "indkobsseddel_20260608.rda1"
    ),
    date = as.Date(c("2026-06-01", "2026-06-01", "2026-06-08")),
    line_number = c(1L, 2L, 1L),
    Indkøbsliste = c(
      "Burger (til 2 pers.):",
      "500 gram oksekød",
      "Burger (til 3 pers.):"
    ),
    stringsAsFactors = FALSE
  )
  history <- shopping_history_recipe_usage(
    history_entries,
    active_fixture$retter
  )

  # Statistikplottet viser de hyppigste retter og giver en tydelig besked for
  # tomme perioder.
  plot <- plot_brugte_opskrifter(
    history,
    dato_start = as.Date("2026-06-01"),
    dato_slut = as.Date("2026-06-30"),
    top_n = 1
  )
  built_plot <- ggplot_build(plot)
  stopifnot(
    identical(plot$labels$title, "Mest brugte opskrifter"),
    nrow(built_plot$data[[1L]]) == 1L,
    identical(built_plot$data[[1L]]$count, 2)
  )
  empty_plot <- plot_brugte_opskrifter(
    history,
    dato_start = as.Date("2025-01-01"),
    dato_slut = as.Date("2025-01-31"),
    top_n = 10
  )
  stopifnot(identical(
    empty_plot$labels$title,
    "Ingen data i dette datointerval"
  ))
  inspiration_expect_error(
    plot_brugte_opskrifter(
      history,
      dato_start = as.Date("2026-01-01"),
      dato_slut = as.Date("2026-12-31"),
      top_n = 0
    ),
    "top_n skal være ét positivt heltal."
  )

  # Serveren modtager både katalog og historik gennem getters. Når historikken
  # ændres, skal statistikken genberegnes straks uden genstart af modulet.
  active_state <- reactiveVal(active_fixture)
  history_state <- reactiveVal(history_entries)
  testServer(
    inspiration_test_server,
    args = list(
      active_recipes_current = active_state,
      history_current = history_state,
      today = function() as.Date("2026-07-21")
    ),
    {
      session$setInputs(
        menu_type = "Vegetar",
        date_from = as.Date("2026-01-01"),
        date_to = as.Date("2026-12-31"),
        top_n = 10
      )
      stopifnot(
        identical(
          module_api$filtered_recipes()$retter,
          "Tortellini"
        ),
        identical(
          module_api$recipe_statistics(),
          shopping_history_recipe_usage(
            history_entries,
            active_fixture$retter
          )
        )
      )

      next_history <- rbind(
        history_entries,
        data.frame(
          filename = "indkobsseddel_20260615.rda",
          date = as.Date("2026-06-15"),
          line_number = 1L,
          Indkøbsliste = "Tortellini (til 2 pers.):",
          stringsAsFactors = FALSE
        )
      )
      history_state(next_history)
      session$flushReact()
      stopifnot(identical(
        module_api$recipe_statistics(),
        shopping_history_recipe_usage(
          next_history,
          active_fixture$retter
        )
      ))

      next_fixture <- rbind(
        active_fixture,
        data.frame(
          retter = "Bønnesalat",
          type = "vegetar",
          key = "boennesalat",
          stringsAsFactors = FALSE
        )
      )
      active_state(next_fixture)
      session$flushReact()
      stopifnot(
        identical(
          module_api$filtered_recipes()$retter,
          c("Tortellini", "Bønnesalat")
        ),
        identical(
          module_api$recipe_statistics(),
          shopping_history_recipe_usage(
            next_history,
            next_fixture$retter
          )
        ),
        identical(
          names(module_api),
          c("active_recipes", "filtered_recipes", "recipe_statistics")
        )
      )
    }
  )

  # Modulfilen skal bestå af dokumenterede top-level funktioner og må ikke
  # bruge pakkenotation med dobbelte koloner.
  module_lines <- readLines("inspiration_module.R", encoding = "UTF-8")
  function_lines <- grep(
    "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
    module_lines
  )
  stopifnot(
    length(function_lines) > 0L,
    all(function_lines > 1L),
    all(grepl("^#'", module_lines[function_lines - 1L])),
    !any(grepl("::", module_lines, fixed = TRUE))
  )

  cat("Inspiration module tests passed.\n")
}

run_inspiration_module_tests()
