suppressPackageStartupMessages({
  source("inspiration_module.R", encoding = "UTF-8")
})

inspiration_test_server <- function(
  id,
  active_recipes_current,
  history_reader,
  history_dir,
  today
) {
  moduleServer(id, function(input, output, session) {
    module_api <- mod_inspiration_server(
      input = input,
      output = output,
      session = session,
      active_recipes_current = active_recipes_current,
      history_reader = history_reader,
      history_dir = history_dir,
      today = today
    )
  })
}

inspiration_test_history_reader <- function(alle_retter, history_dir) {
  data.frame(
    retter = alle_retter,
    dato = rep(as.Date("2026-06-01"), length(alle_retter)),
    stringsAsFactors = FALSE
  )
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

  # Historiklæsningen bruger udelukkende en frisk temp-mappe. Fixtures dækker
  # både persontekst, kolon, m.-tekst, flere filer og specialtegn i retten.
  history_dir <- tempfile("inspiration-history-")
  dir.create(history_dir)
  on.exit(unlink(history_dir, recursive = TRUE, force = TRUE), add = TRUE)

  df <- data.frame(
    Indkøbsliste = c(
      "Burger (til 2 pers.):",
      "500 gram oksekød",
      "Pasta (special) (til 4 pers.): m. pesto",
      "Pizza surdej (til 2 pers.):",
      "C++ gryde (til 2 pers.):",
      "Burgerboller (til 2 pers.):",
      "Ukendt ret (til 2 pers.):"
    ),
    stringsAsFactors = FALSE
  )
  save(df, file = file.path(history_dir, "indkobsseddel_20260601.rda"))

  df <- data.frame(
    Indkøbsliste = c(
      "Burger (til 3 pers.):",
      "3 stk boller"
    ),
    stringsAsFactors = FALSE
  )
  save(df, file = file.path(history_dir, "indkobsseddel_20260608.rda1"))

  # En uvedkommende fil skal ignoreres.
  df <- data.frame(Indkøbsliste = "Tortellini (til 2 pers.):")
  save(df, file = file.path(history_dir, "anden_fil.rda"))

  # Matchende, men defekte filer skal kun springes over. De må ikke blokere
  # statistik fra de gyldige historikfiler.
  not_df <- "mangler historikobjektet"
  save(
    not_df,
    file = file.path(history_dir, "indkobsseddel_20260615.rda")
  )
  df <- data.frame(Indkøbsliste = "Burger (til 2 pers.):")
  save(df, file = file.path(history_dir, "indkobsseddel_20261399.rda"))

  extracted <- find_retter(
    "indkobsseddel_20260601.rda",
    c(
      "Burger",
      "Pasta (special)",
      "Pizza",
      "Pizza surdej",
      "C++ gryde",
      "Tortellini"
    ),
    history_dir
  )
  stopifnot(identical(
    extracted,
    c("Burger", "Pasta (special)", "Pizza surdej", "C++ gryde")
  ))

  history <- brugte_opskrifter(
    c(
      "Burger",
      "Pasta (special)",
      "Pizza",
      "Pizza surdej",
      "C++ gryde",
      "Tortellini"
    ),
    history_dir
  )
  expected_history <- data.frame(
    retter = c(
      "Burger",
      "Burger",
      "C++ gryde",
      "Pasta (special)",
      "Pizza surdej"
    ),
    dato = as.Date(c(
      "2026-06-01",
      "2026-06-08",
      "2026-06-01",
      "2026-06-01",
      "2026-06-01"
    )),
    stringsAsFactors = FALSE
  )
  stopifnot(identical(history, expected_history))
  stopifnot(
    identical(
      brugte_opskrifter(character(), history_dir),
      inspiration_empty_history()
    ),
    identical(
      brugte_opskrifter("Burger", file.path(history_dir, "findes-ikke")),
      inspiration_empty_history()
    )
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

  # Serveren modtager kataloget gennem en getter. Når getterens state ændres,
  # afspejler de read-only getters straks de nye data uden en lokal kopi.
  active_state <- reactiveVal(active_fixture)
  testServer(
    inspiration_test_server,
    args = list(
      active_recipes_current = active_state,
      history_reader = inspiration_test_history_reader,
      history_dir = history_dir,
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
          module_api$recipe_statistics()$retter,
          active_fixture$retter
        )
      )

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
          module_api$recipe_statistics()$retter,
          next_fixture$retter
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
