suppressPackageStartupMessages({
  library(shiny)
  library(utils)
  source(file.path("R", "funktioner.R"), encoding = "UTF-8")
  source(file.path("R", "recipe_view.R"), encoding = "UTF-8")
})

recipe_view_html <- function(ui) {
  paste(as.character(ui), collapse = "")
}

ns <- NS("opskrifter_test")

module_ui_html <- recipe_view_html(mod_opskrifter_ui("opskrifter_test"))
dialogs_ui_html <- recipe_view_html(
  mod_opskrifter_dialogs_ui("opskrifter_test")
)
stopifnot(
  grepl(
    'id="opskrifter_test-open_ny_ret"',
    module_ui_html,
    fixed = TRUE
  ),
  grepl(
    'id="opskrifter_test-opskrifter_ui"',
    module_ui_html,
    fixed = TRUE
  ),
  grepl(
    'id="opskrifter_test-popup_opskrift_rediger"',
    dialogs_ui_html,
    fixed = TRUE
  ),
  grepl(
    'id="opskrifter_test-confirm_delete_archived_ret"',
    dialogs_ui_html,
    fixed = TRUE
  )
)

recipe <- data.frame(
  Testret = c("Tomat <frisk>", "Bacon & ost"),
  maengde = c(2, 50),
  enhed = c("stk", "gram"),
  kat_1 = c("grønt", "kød"),
  kat_2 = c("", ""),
  stringsAsFactors = FALSE
)

ingredient_rows <- recipe_ingredient_table_rows(
  recipe,
  key = "testret_opskr",
  ns = ns
)
stopifnot(
  identical(
    names(ingredient_rows),
    c("Ingrediens", "Rediger", "Slet")
  ),
  nrow(ingredient_rows) == 2L,
  identical(
    ingredient_rows$Ingrediens,
    c(
      "2 stk Tomat &lt;frisk&gt;",
      "50 gram Bacon &amp; ost"
    )
  ),
  grepl(
    'id="opskrifter_test-opskrift_row_btn_testret_opskr_1"',
    ingredient_rows$Rediger[[1L]],
    fixed = TRUE
  ),
  grepl(
    "opskrifter_test-opskrift_editPressed",
    ingredient_rows$Rediger[[1L]],
    fixed = TRUE
  ),
  grepl(
    "key: &#39;testret_opskr&#39;, row: 1",
    ingredient_rows$Rediger[[1L]],
    fixed = TRUE
  ),
  grepl(
    'id="opskrifter_test-opskrift_row_del_testret_opskr_2"',
    ingredient_rows$Slet[[2L]],
    fixed = TRUE
  ),
  grepl(
    "opskrifter_test-opskrift_deletePressed",
    ingredient_rows$Slet[[2L]],
    fixed = TRUE
  ),
  grepl(
    "key: &#39;testret_opskr&#39;, row: 2",
    ingredient_rows$Slet[[2L]],
    fixed = TRUE
  )
)

empty_ingredient_rows <- recipe_ingredient_table_rows(
  recipe[FALSE, , drop = FALSE],
  key = "testret_opskr",
  ns = ns
)
ingredient_widget <- recipe_ingredient_table_widget(ingredient_rows)
empty_ingredient_widget <- recipe_ingredient_table_widget(
  empty_ingredient_rows
)
stopifnot(
  nrow(empty_ingredient_rows) == 0L,
  identical(names(empty_ingredient_rows), names(ingredient_rows)),
  inherits(ingredient_widget, "datatables"),
  nrow(ingredient_widget$x$data) == 2L,
  identical(names(ingredient_widget$x$data), names(ingredient_rows)),
  identical(ingredient_widget$x$options$dom, "t"),
  identical(ingredient_widget$x$options$paging, FALSE),
  identical(ingredient_widget$x$options$ordering, FALSE),
  identical(ingredient_widget$x$options$searching, FALSE),
  inherits(empty_ingredient_widget, "datatables"),
  nrow(empty_ingredient_widget$x$data) == 0L
)

active_retter <- data.frame(
  retter = c("Zebraret", "Burger", "Uden opskrift"),
  key = c("zebra_opskr", "burger_opskr", "mangler_opskr"),
  type = c("vegetar", "okse", "fisk"),
  stringsAsFactors = FALSE
)
empty_active_retter <- active_retter[FALSE, , drop = FALSE]
empty_archive <- active_retter[FALSE, , drop = FALSE]
archive <- data.frame(
  retter = "Gammel ret",
  key = "gammel_ret_opskr",
  type = "vegetar",
  stringsAsFactors = FALSE
)

empty_overview_html <- recipe_view_html(recipe_overview_ui(
  active_retter = empty_active_retter,
  recipe_keys = character(),
  archive = empty_archive,
  selected_key = NULL,
  ns = ns
))
active_overview_html <- recipe_view_html(recipe_overview_ui(
  active_retter = active_retter,
  recipe_keys = c("zebra_opskr", "burger_opskr"),
  archive = empty_archive,
  selected_key = "zebra_opskr",
  ns = ns
))
archive_overview_html <- recipe_view_html(recipe_overview_ui(
  active_retter = empty_active_retter,
  recipe_keys = character(),
  archive = archive,
  selected_key = NULL,
  ns = ns
))
stopifnot(
  grepl(
    "Der er ingen aktive opskrifter.",
    empty_overview_html,
    fixed = TRUE
  ),
  !grepl(
    "opskrifter_test-opskrift_valgt_key",
    empty_overview_html,
    fixed = TRUE
  ),
  grepl(
    'id="opskrifter_test-opskrift_valgt_key"',
    active_overview_html,
    fixed = TRUE
  ),
  grepl("Burger", active_overview_html, fixed = TRUE),
  grepl("Zebraret", active_overview_html, fixed = TRUE),
  !grepl("Uden opskrift", active_overview_html, fixed = TRUE),
  grepl(
    'value="zebra_opskr" selected',
    active_overview_html,
    fixed = TRUE
  ),
  grepl("Arkiv", archive_overview_html, fixed = TRUE),
  grepl("Gammel ret", archive_overview_html, fixed = TRUE),
  grepl(
    'id="opskrifter_test-restore_ret_btn_gammel_ret_opskr"',
    archive_overview_html,
    fixed = TRUE
  ),
  grepl(
    "opskrifter_test-restore_ret",
    archive_overview_html,
    fixed = TRUE
  ),
  grepl(
    'id="opskrifter_test-delete_archived_ret_btn_gammel_ret_opskr"',
    archive_overview_html,
    fixed = TRUE
  ),
  grepl(
    "opskrifter_test-delete_archived_ret",
    archive_overview_html,
    fixed = TRUE
  )
)

selected_model <- list(
  key = "burger_opskr",
  ret_navn = "Burger",
  link_url = "https://example.com/burger"
)
selected_ui_html <- recipe_view_html(recipe_selected_ui(selected_model, ns))
selected_without_link_html <- recipe_view_html(recipe_selected_ui(
  modifyList(selected_model, list(link_url = "")),
  ns
))
stopifnot(
  grepl("<h3>Burger</h3>", selected_ui_html, fixed = TRUE),
  grepl(
    'id="opskrifter_test-opskrift_burger_opskr"',
    selected_ui_html,
    fixed = TRUE
  ),
  grepl(
    'id="opskrifter_test-opskrift_add_btn_burger_opskr"',
    selected_ui_html,
    fixed = TRUE
  ),
  grepl(
    "opskrifter_test-opskrift_addPressed",
    selected_ui_html,
    fixed = TRUE
  ),
  grepl(
    'id="opskrifter_test-opskrift_archive_btn_burger_opskr"',
    selected_ui_html,
    fixed = TRUE
  ),
  grepl(
    "opskrifter_test-opskrift_archivePressed",
    selected_ui_html,
    fixed = TRUE
  ),
  grepl(
    'id="opskrifter_test-opskrift_tbl_valgt"',
    selected_ui_html,
    fixed = TRUE
  ),
  grepl(
    'href="https://example.com/burger"',
    selected_ui_html,
    fixed = TRUE
  ),
  grepl('target="_blank"', selected_ui_html, fixed = TRUE),
  grepl(
    'rel="noopener noreferrer"',
    selected_ui_html,
    fixed = TRUE
  ),
  !grepl("opskrift-link-url", selected_without_link_html, fixed = TRUE),
  !grepl("href=", selected_without_link_html, fixed = TRUE),
  grepl(
    'id="opskrifter_test-opskrift_tbl_valgt"',
    selected_without_link_html,
    fixed = TRUE
  )
)

message(paste(
  "Opskriftsfanens view-buildere laver namespacede knapper og tabeller,",
  "viser tomme, aktive og arkiverede oversigter og håndterer links sikkert."
))
