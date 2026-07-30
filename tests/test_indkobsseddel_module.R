suppressPackageStartupMessages({
  source("cart_state.R", encoding = "UTF-8")
  source("funktioner.R", encoding = "UTF-8")
  source("indkobsseddel_catalog.R", encoding = "UTF-8")
  source("indkobsseddel_view.R", encoding = "UTF-8")
  source("indkobsseddel_module.R", encoding = "UTF-8")
})

indkobsseddel_test_varer <- function() {
  data.frame(
    Indkobsliste = c(
      "Banan",
      "Mælk",
      "Dublet",
      " DUBLET "
    ),
    maengde = c(1, 0.5, 1, 1),
    enhed = c("stk", "liter", "stk", "pakke"),
    kat_1 = c(
      "frugt og grønt",
      "mejeri",
      "konserves",
      "konserves"
    ),
    kat_2 = c("", "mælk", "", ""),
    stringsAsFactors = FALSE
  )
}

indkobsseddel_test_recipe_read <- function() {
  list(
    recipes = function() list(),
    active_retter = function() {
      data.frame(
        retter = character(),
        key = character(),
        type = character(),
        stringsAsFactors = FALSE
      )
    },
    links = function() {
      data.frame(
        ret = character(),
        link = character(),
        stringsAsFactors = FALSE
      )
    },
    salater = function() {
      data.frame(
        retter = "",
        key = "",
        type = "",
        stringsAsFactors = FALSE
      )
    },
    salater_opskrifter = function() list(),
    tilbehor = function() indkobsseddel_empty_rows()
  )
}

indkobsseddel_test_server <- function(
  id,
  recipe_read,
  varer_current,
  save_cart,
  popular_items
) {
  moduleServer(id, function(input, output, session) {
    module_api <- mod_indkobsseddel_server(
      input = input,
      output = output,
      session = session,
      recipe_read = recipe_read,
      varer_current = varer_current,
      save_cart = save_cart,
      popular_items = popular_items
    )
  })
}

indkobsseddel_assert_unchanged <- function(actual, expected) {
  stopifnot(identical(actual, expected))
}

run_indkobsseddel_module_tests <- function() {
  # UI og dialoger må kunne bruges flere gange på samme side uden
  # sammenfaldende HTML-id'er.
  module_ui_html <- paste(
    as.character(
      tagList(
        mod_indkobsseddel_ui("kurv"),
        mod_indkobsseddel_dialogs_ui(
          "kurv",
          salat_choices = c("", "Bønnesalat"),
          tilbehor_choices = c("", "kartofler")
        )
      )
    ),
    collapse = ""
  )

  expected_namespaced_ids <- c(
    "kurv-open_recipe",
    "kurv-open_catalog",
    "kurv-open_manual",
    "kurv-cart_table",
    "kurv-save_history",
    "kurv-history_suggestions",
    "kurv-catalog_dialog",
    "kurv-catalog_item",
    "kurv-manual_dialog",
    "kurv-manual_name",
    "kurv-recipe_dialog",
    "kurv-recipe_name",
    "kurv-edit_dialog",
    "kurv-edit_value",
    "kurv-confirm_edit"
  )
  stopifnot(all(vapply(
    expected_namespaced_ids,
    function(id) {
      grepl(
        paste0('id="', id, '"'),
        module_ui_html,
        fixed = TRUE
      )
    },
    logical(1)
  )))
  stopifnot(
    !grepl('id="open_catalog"', module_ui_html, fixed = TRUE),
    !grepl('id="manual_dialog"', module_ui_html, fixed = TRUE),
    !grepl('id="edit_dialog"', module_ui_html, fixed = TRUE),
    grepl("ga-edit-overlay", module_ui_html, fixed = TRUE),
    grepl("ga-edit-dialog", module_ui_html, fixed = TRUE),
    grepl("ga-edit-input", module_ui_html, fixed = TRUE),
    grepl("ga-save-cart", module_ui_html, fixed = TRUE),
    grepl(">konserves</option>", module_ui_html, fixed = TRUE)
  )

  # Selv et manglende eller ufuldstændigt katalog giver sikre startvalg.
  fallback_categories <- indkobsseddel_manual_category_choices(NULL)
  stopifnot(
    identical(fallback_categories$category_1, "konserves"),
    identical(fallback_categories$category_2, ""),
    identical(
      indkobsseddel_preserved_choice(
        "mejeri",
        c("konserves", "mejeri"),
        preferred = "konserves"
      ),
      "mejeri"
    ),
    identical(
      indkobsseddel_preserved_choice(
        "udgået",
        c("konserves", "mejeri"),
        preferred = "konserves"
      ),
      "konserves"
    )
  )

  # Opskriftsdialogen skal altid starte uden salat og tilbehør. Samtidig
  # bygges valgene af det katalog, som gælder på åbningstidspunktet.
  first_dialog_values <- indkobsseddel_recipe_dialog_values(
    recipes = list(burger = data.frame()),
    active_retter = data.frame(
      retter = "Burger",
      key = "burger",
      stringsAsFactors = FALSE
    ),
    salater = data.frame(
      retter = c("", "Broccolisalat"),
      stringsAsFactors = FALSE
    ),
    tilbehor = data.frame(
      Indkobsliste = c("", "Kartofler"),
      stringsAsFactors = FALSE
    )
  )
  updated_dialog_values <- indkobsseddel_recipe_dialog_values(
    recipes = list(
      burger = data.frame(),
      tortellini = data.frame()
    ),
    active_retter = data.frame(
      retter = c("Burger", "Tortellini"),
      key = c("burger", "tortellini"),
      stringsAsFactors = FALSE
    ),
    salater = data.frame(
      retter = c("", "Broccolisalat", "Bønnesalat"),
      stringsAsFactors = FALSE
    ),
    tilbehor = data.frame(
      Indkobsliste = c("", "Kartofler", "Ris"),
      stringsAsFactors = FALSE
    )
  )
  stopifnot(
    identical(first_dialog_values$selected_recipe, ""),
    identical(first_dialog_values$persons, 2),
    identical(first_dialog_values$selected_salad, ""),
    identical(first_dialog_values$selected_accessory, ""),
    identical(
      updated_dialog_values$recipe_choices,
      c("", "Burger", "Tortellini")
    ),
    "Bønnesalat" %in% updated_dialog_values$salad_choices,
    "Ris" %in% updated_dialog_values$accessory_choices
  )

  # Kontrollér også selve open-eventet: et tidligere salatvalg skal nulstilles,
  # og et katalog, som er ændret siden sidste åbning, skal læses på ny.
  dialog_messages <- list()
  dialog_session <- shiny::MockShinySession$new()
  dialog_session$sendInputMessage <- function(inputId, message) {
    dialog_messages[[inputId]] <<- message
    invisible(NULL)
  }
  dialog_catalog <- new.env(parent = emptyenv())
  dialog_catalog$recipes <- list(burger = data.frame())
  dialog_catalog$active <- data.frame(
    retter = "Burger",
    key = "burger",
    stringsAsFactors = FALSE
  )
  dialog_catalog$salater <- data.frame(
    retter = c("", "Broccolisalat"),
    stringsAsFactors = FALSE
  )
  dialog_catalog$tilbehor <- data.frame(
    Indkobsliste = c("", "Kartofler"),
    stringsAsFactors = FALSE
  )
  dialog_recipe_read <- list(
    recipes = function() dialog_catalog$recipes,
    active_retter = function() dialog_catalog$active,
    links = function() data.frame(),
    salater = function() dialog_catalog$salater,
    salater_opskrifter = function() list(),
    tilbehor = function() dialog_catalog$tilbehor
  )
  first_open_messages <- NULL
  second_open_messages <- NULL
  category_update_messages <- NULL
  dialog_varer <- shiny::reactiveVal(indkobsseddel_test_varer())

  shiny::testServer(
    indkobsseddel_test_server,
    args = list(
      recipe_read = dialog_recipe_read,
      varer_current = dialog_varer,
      save_cart = function(history_df) TRUE,
      popular_items = function() character()
    ),
    session = dialog_session,
    {
      session$setInputs(salad_name = "Broccolisalat")
      session$setInputs(open_recipe = 1L)
      first_open_messages <<- dialog_messages

      dialog_catalog$recipes <- list(
        burger = data.frame(),
        tortellini = data.frame()
      )
      dialog_catalog$active <- data.frame(
        retter = c("Burger", "Tortellini"),
        key = c("burger", "tortellini"),
        stringsAsFactors = FALSE
      )
      dialog_catalog$salater <- data.frame(
        retter = c("", "Broccolisalat", "Bønnesalat"),
        stringsAsFactors = FALSE
      )
      session$setInputs(open_recipe = 2L)
      second_open_messages <<- dialog_messages

      # Kategorierne skal følge det reaktive varekatalog uden genstart. De
      # nuværende, fortsat gyldige valg skal samtidig bevares.
      session$setInputs(
        manual_category_1 = "mejeri",
        manual_category_2 = "mælk"
      )
      dialog_varer(rbind(
        indkobsseddel_test_varer(),
        data.frame(
          Indkobsliste = "Risnudler",
          maengde = 1,
          enhed = "pakke",
          kat_1 = "asiatisk",
          kat_2 = "nudler",
          stringsAsFactors = FALSE
        )
      ))
      session$flushReact()
      category_update_messages <<- dialog_messages
    }
  )

  first_salad_id <- grep(
    "salad_name$",
    names(first_open_messages),
    value = TRUE
  )
  second_recipe_id <- grep(
    "recipe_name$",
    names(second_open_messages),
    value = TRUE
  )
  second_salad_id <- grep(
    "salad_name$",
    names(second_open_messages),
    value = TRUE
  )
  category_1_id <- grep(
    "manual_category_1$",
    names(category_update_messages),
    value = TRUE
  )
  category_2_id <- grep(
    "manual_category_2$",
    names(category_update_messages),
    value = TRUE
  )
  stopifnot(
    length(first_salad_id) == 1L,
    identical(first_open_messages[[first_salad_id]]$value, ""),
    length(second_recipe_id) == 1L,
    grepl(
      "Tortellini",
      second_open_messages[[second_recipe_id]]$options,
      fixed = TRUE
    ),
    length(second_salad_id) == 1L,
    grepl(
      "Bønnesalat",
      second_open_messages[[second_salad_id]]$options,
      fixed = TRUE
    ),
    identical(second_open_messages[[second_salad_id]]$value, ""),
    length(category_1_id) == 1L,
    grepl(
      "asiatisk",
      category_update_messages[[category_1_id]]$options,
      fixed = TRUE
    ),
    identical(
      category_update_messages[[category_1_id]]$value,
      "mejeri"
    ),
    length(category_2_id) == 1L,
    grepl(
      "nudler",
      category_update_messages[[category_2_id]]$options,
      fixed = TRUE
    ),
    identical(
      category_update_messages[[category_2_id]]$value,
      "mælk"
    )
  )

  # Selve tabel-widgeten skal sende klik til modulets namespacede inputs.
  button_state <- cart_add_rows(
    new_cart_state(),
    data.frame(
      Indkobsliste = "Mælk",
      maengde = 1,
      enhed = "liter",
      kat_1 = "mejeri",
      kat_2 = "mælk",
      stringsAsFactors = FALSE
    )
  )
  button_widget <- indkobsseddel_cart_widget(
    cart_copy_payload(button_state),
    NS("kurv")
  )
  edit_button_html <- button_widget$x$data$edit[[1]]
  delete_button_html <- button_widget$x$data$delete[[1]]

  stopifnot(
    grepl(
      'id="kurv-edit_cart_1"',
      edit_button_html,
      fixed = TRUE
    ),
    grepl(
      paste0(
        "Shiny.setInputValue(&quot;kurv-edit_pressed&quot;, ",
        "&quot;cart_1&quot;"
      ),
      edit_button_html,
      fixed = TRUE
    ),
    grepl(
      'id="kurv-delete_cart_1"',
      delete_button_html,
      fixed = TRUE
    ),
    grepl(
      paste0(
        "Shiny.setInputValue(&quot;kurv-delete_pressed&quot;, ",
        "&quot;cart_1&quot;"
      ),
      delete_button_html,
      fixed = TRUE
    )
  )

  test_root <- tempfile(
    "indkobsseddel-module-",
    tmpdir = tempdir()
  )
  stopifnot(dir.create(test_root))
  on.exit(
    unlink(test_root, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  save_calls <- list()
  saved_paths <- character()
  save_cart_stub <- function(history_df) {
    save_calls[[length(save_calls) + 1L]] <<- history_df
    path <- file.path(
      test_root,
      paste0("historik-", length(save_calls), ".rds")
    )
    saveRDS(history_df, path)
    saved_paths <<- c(saved_paths, path)
    TRUE
  }

  popular_calls <- 0L
  popular_fixture <- c(
    " RUGBRØD ",
    "citronsaft",
    "Kaffe",
    "Te",
    "Kaffe",
    "Salt",
    "Peber",
    "Ris",
    "Pasta",
    "Æbler",
    "Pærer",
    "Mel",
    "Sukker",
    "Smør",
    "Gær"
  )
  popular_items_stub <- function() {
    popular_calls <<- popular_calls + 1L
    popular_fixture
  }

  shiny::testServer(
    indkobsseddel_test_server,
    args = list(
      recipe_read = indkobsseddel_test_recipe_read(),
      varer_current = indkobsseddel_test_varer,
      save_cart = save_cart_stub,
      popular_items = popular_items_stub
    ),
    {
      session$setInputs(
        recipe_name = "",
        recipe_persons = 2,
        salad_name = "",
        accessory_name = "",
        catalog_item = "",
        catalog_amount = 1,
        catalog_unit = "stk",
        manual_name = "",
        manual_amount = 1,
        manual_unit = "stk",
        manual_category_1 = "konserves",
        manual_category_2 = "",
        edit_value = ""
      )

      # API'et udstiller kun tre getters. En ændret lokal kopi må ikke kunne
      # ændre modulets interne reactiveVal.
      stopifnot(
        identical(
          names(module_api),
          c("cart_current", "visible_rows", "copy_payload")
        ),
        all(vapply(module_api, is.function, logical(1)))
      )
      initial_snapshot <- module_api$cart_current()
      detached_snapshot <- initial_snapshot
      detached_snapshot$next_line_id <- 999L
      stopifnot(
        identical(module_api$cart_current(), initial_snapshot),
        module_api$cart_current()$next_line_id == 1L
      )

      # Katalog-flowet matcher navn uden forskel på store/små bogstaver,
      # ganger standardmængden og bruger den valgte enhed.
      session$setInputs(
        catalog_item = "  mæLK ",
        catalog_amount = 3,
        catalog_unit = "liter"
      )
      session$setInputs(add_catalog_item = 1L)

      rows_after_catalog <- module_api$visible_rows()
      milk_row <- rows_after_catalog[
        rows_after_catalog$Indkobsliste == "Mælk",
        ,
        drop = FALSE
      ]
      stopifnot(
        nrow(milk_row) == 1L,
        identical(milk_row$maengde[[1]], 1.5),
        identical(milk_row$enhed[[1]], "liter")
      )

      # Ugyldige katalogvalg er no-ops: tomt/ukendt/tvetydigt navn,
      # ikke-positiv mængde og manglende enhed.
      catalog_before_invalid <- module_api$cart_current()

      session$setInputs(
        catalog_item = " ",
        catalog_amount = 1,
        catalog_unit = "stk"
      )
      session$setInputs(add_catalog_item = 2L)
      indkobsseddel_assert_unchanged(
        module_api$cart_current(),
        catalog_before_invalid
      )

      session$setInputs(
        catalog_item = "Findes ikke",
        catalog_amount = 1,
        catalog_unit = "stk"
      )
      session$setInputs(add_catalog_item = 3L)
      indkobsseddel_assert_unchanged(
        module_api$cart_current(),
        catalog_before_invalid
      )

      session$setInputs(
        catalog_item = "dublet",
        catalog_amount = 1,
        catalog_unit = "stk"
      )
      session$setInputs(add_catalog_item = 4L)
      indkobsseddel_assert_unchanged(
        module_api$cart_current(),
        catalog_before_invalid
      )

      session$setInputs(
        catalog_item = "Mælk",
        catalog_amount = 0,
        catalog_unit = "liter"
      )
      session$setInputs(add_catalog_item = 5L)
      indkobsseddel_assert_unchanged(
        module_api$cart_current(),
        catalog_before_invalid
      )

      session$setInputs(
        catalog_item = "Mælk",
        catalog_amount = 1,
        catalog_unit = " "
      )
      session$setInputs(add_catalog_item = 6L)
      indkobsseddel_assert_unchanged(
        module_api$cart_current(),
        catalog_before_invalid
      )

      # Manuel indtastning afviser de samme grundlæggende fejl.
      manual_before_invalid <- module_api$cart_current()

      session$setInputs(
        manual_name = " ",
        manual_amount = 1,
        manual_unit = "stk"
      )
      session$setInputs(add_manual_item = 1L)
      indkobsseddel_assert_unchanged(
        module_api$cart_current(),
        manual_before_invalid
      )

      session$setInputs(
        manual_name = "Rugbrød",
        manual_amount = 0,
        manual_unit = "stk"
      )
      session$setInputs(add_manual_item = 2L)
      indkobsseddel_assert_unchanged(
        module_api$cart_current(),
        manual_before_invalid
      )

      session$setInputs(
        manual_name = "Rugbrød",
        manual_amount = 2,
        manual_unit = " "
      )
      session$setInputs(add_manual_item = 3L)
      indkobsseddel_assert_unchanged(
        module_api$cart_current(),
        manual_before_invalid
      )

      session$setInputs(
        manual_name = "  Rugbrød  ",
        manual_amount = 2,
        manual_unit = "stk",
        manual_category_1 = "konserves",
        manual_category_2 = "brød"
      )
      session$setInputs(add_manual_item = 4L)

      rows_after_manual <- module_api$visible_rows()
      bread_row <- rows_after_manual[
        rows_after_manual$Indkobsliste == "Rugbrød",
        ,
        drop = FALSE
      ]
      stopifnot(
        nrow(bread_row) == 1L,
        identical(bread_row$maengde[[1]], 2),
        identical(bread_row$enhed[[1]], "stk"),
        identical(bread_row$kat_2[[1]], "brød")
      )

      # Historiknavne og cart-navne skal normaliseres ens. Ellers ville
      # "citronsaft" blive foreslået, selv om varianten med "(tilbehør)"
      # allerede står på indkøbssedlen.
      session$setInputs(
        manual_name = "citronsaft (tilbehør)",
        manual_amount = 1,
        manual_unit = "liter",
        manual_category_1 = "konserves",
        manual_category_2 = ""
      )
      session$setInputs(add_manual_item = 5L)

      # Redigering og sletning bruger stabile line_id'er, ikke tabellens
      # aktuelle rækkeplacering.
      rows_before_edit <- module_api$visible_rows()
      milk_id <- rows_before_edit$line_id[
        rows_before_edit$Indkobsliste == "Mælk"
      ][[1]]
      bread_id <- rows_before_edit$line_id[
        rows_before_edit$Indkobsliste == "Rugbrød"
      ][[1]]

      session$setInputs(edit_pressed = milk_id)
      session$setInputs(
        edit_value = "Husk økologisk mælk",
        confirm_edit = 1L
      )

      rows_after_milk_edit <- module_api$visible_rows()
      stopifnot(
        identical(
          rows_after_milk_edit$display[
            rows_after_milk_edit$line_id == milk_id
          ],
          "Husk økologisk mælk"
        ),
        identical(
          rows_after_milk_edit$display[
            rows_after_milk_edit$line_id == bread_id
          ],
          "2 stk Rugbrød"
        )
      )

      # En tom redigering ændrer ikke state og bevarer den valgte linje,
      # så brugeren kan rette teksten og prøve igen.
      session$setInputs(edit_pressed = bread_id)
      state_before_blank_edit <- module_api$cart_current()
      session$setInputs(
        edit_value = " ",
        confirm_edit = 2L
      )
      indkobsseddel_assert_unchanged(
        module_api$cart_current(),
        state_before_blank_edit
      )

      session$setInputs(
        edit_value = "To rugbrød",
        confirm_edit = 3L
      )
      stopifnot(identical(
        module_api$visible_rows()$display[
          module_api$visible_rows()$line_id == bread_id
        ],
        "To rugbrød"
      ))

      session$setInputs(delete_pressed = milk_id)
      stopifnot(
        !"Mælk" %in% module_api$visible_rows()$Indkobsliste,
        "Rugbrød" %in% module_api$visible_rows()$Indkobsliste
      )

      state_before_stale_delete <- module_api$cart_current()
      session$setInputs(delete_pressed = "cart_999999")
      indkobsseddel_assert_unchanged(
        module_api$cart_current(),
        state_before_stale_delete
      )

      # Historikforslag sammenlignes med det oprindelige varenavn. Derfor
      # foreslås Rugbrød ikke igen, selv om brugerens visningstekst er ændret.
      expected_suggestions <- indkobsseddel_history_suggestions(
        popular_fixture,
        module_api$visible_rows(),
        units = indkobsseddel_test_varer()$enhed,
        limit = 10L
      )
      stopifnot(
        nrow(expected_suggestions) == 10L,
        !"RUGBRØD" %in% toupper(expected_suggestions$Forslag),
        !"CITRONSAFT" %in% toupper(expected_suggestions$Forslag),
        identical(expected_suggestions$Forslag[[1]], "Kaffe")
      )

      suggestions_html <- output$history_suggestions
      stopifnot(
        popular_calls > 0L,
        grepl("Kaffe", suggestions_html, fixed = TRUE),
        !grepl("RUGBRØD", toupper(suggestions_html), fixed = TRUE),
        !grepl("CITRONSAFT", toupper(suggestions_html), fixed = TRUE)
      )

      # Gemning går udelukkende gennem callbacken. Stubben skriver kun under
      # tempdir og modtager den samme afledte tekst som copy-payloaden.
      session$setInputs(save_history = 1L)
      stopifnot(
        length(save_calls) == 1L,
        length(saved_paths) == 1L,
        file.exists(saved_paths[[1]]),
        identical(names(save_calls[[1]]), "Indkøbsliste"),
        identical(
          save_calls[[1]]$Indkøbsliste,
          module_api$copy_payload()$visible
        ),
        identical(
          readRDS(saved_paths[[1]]),
          save_calls[[1]]
        ),
        startsWith(
          normalizePath(saved_paths[[1]], winslash = "/"),
          normalizePath(tempdir(), winslash = "/")
        )
      )

      # Også efter mutationer er API'et fortsat read-only.
      final_internal <- module_api$cart_current()
      detached_final <- final_internal
      detached_final$rows$Indkobsliste[[1]] <- "Manipuleret"
      stopifnot(identical(
        module_api$cart_current(),
        final_internal
      ))
    }
  )
}

run_indkobsseddel_module_tests()

# Modulet må kun kommunikere med historiklageret gennem de injicerede
# callbacks. Direkte filadgang hører hjemme i det centrale historiklager.
indkobsseddel_source <- unlist(
  lapply(
    c(
      "indkobsseddel_catalog.R",
      "indkobsseddel_view.R",
      "indkobsseddel_module.R"
    ),
    readLines,
    warn = FALSE,
    encoding = "UTF-8"
  ),
  use.names = FALSE
)
direct_history_file_calls <- c(
  "\\bsave\\s*\\(",
  "\\bload\\s*\\(",
  "\\blist\\.files\\s*\\("
)
stopifnot(!any(vapply(
  direct_history_file_calls,
  function(pattern) {
    any(grepl(pattern, indkobsseddel_source, perl = TRUE))
  },
  logical(1)
)))

message(
  paste(
    "Indkøbsseddel-modulet bestod isolerede tests for namespace,",
    "inputvalidering, stabile linje-id'er, historik og read-only state."
  )
)
