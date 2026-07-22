suppressPackageStartupMessages(source("app.R", encoding = "UTF-8"))

fixture_catalog <- recipe_store_read("./data")
catalog_state <- shiny::reactiveVal(fixture_catalog)

catalog_read <- list(
  snapshot = function() shiny::isolate(catalog_state()),
  recipes = shiny::reactive(catalog_state()$recipes),
  links = shiny::reactive(catalog_state()$links),
  active_retter = shiny::reactive(catalog_state()$active_retter),
  archived_retter = shiny::reactive(catalog_state()$archived_retter),
  revision = shiny::reactive(catalog_state()$revision)
)

commit_catalog <- function(
  next_catalog,
  delete_recipe_keys = character(),
  error_message = ""
) {
  catalog_state(next_catalog)
  TRUE
}

varer_current <- function() {
  data.frame(
    Indkobsliste = c("agurk", "bacon"),
    maengde = 1,
    enhed = c("stk", "gram"),
    kat_1 = c("grønt", "kød"),
    kat_2 = c("", ""),
    stringsAsFactors = FALSE
  )
}

test_opskrifter_module <- function(
  id,
  catalog_read,
  commit_catalog,
  varer_current,
  kategori_1 = character(),
  kategori_2 = character()
) {
  shiny::moduleServer(id, function(input, output, session) {
    mod_opskrifter_server(
      input = input,
      output = output,
      session = session,
      catalog_read = catalog_read,
      commit_catalog = commit_catalog,
      varer_current = varer_current,
      kategori_1 = kategori_1,
      kategori_2 = kategori_2
    )
  })
}

module_ui_html <- paste(
  as.character(
    shiny::tagList(
      mod_opskrifter_ui("opskrifter"),
      mod_opskrifter_dialogs_ui("opskrifter")
    )
  ),
  collapse = ""
)

stopifnot(
  grepl('id="opskrifter-open_ny_ret"', module_ui_html, fixed = TRUE),
  grepl('id="opskrifter-opskrifter_ui"', module_ui_html, fixed = TRUE),
  grepl(
    'id="opskrifter-popup_opskrift_rediger"',
    module_ui_html,
    fixed = TRUE
  ),
  grepl(
    'id="opskrifter-confirm_delete_archived_ret"',
    module_ui_html,
    fixed = TRUE
  )
)

shiny::testServer(
  test_opskrifter_module,
  args = list(
    catalog_read = catalog_read,
    commit_catalog = commit_catalog,
    varer_current = varer_current,
    kategori_1 = c("grønt", "kød"),
    kategori_2 = c("")
  ),
  {
    session$setInputs(opskrift_valgt_key = "burger_opskr")

    burger_table <- output$opskrift_tbl_valgt
    stopifnot(is.character(burger_table), length(burger_table) == 1)
    stopifnot(grepl("<th>Ingrediens", burger_table, fixed = TRUE))

    selected_ui <- paste(as.character(output$valgt_opskrift_ui), collapse = "")
    stopifnot(
      grepl(
        session$ns("opskrift_addPressed"),
        selected_ui,
        fixed = TRUE
      ),
      grepl(
        session$ns("opskrift_archivePressed"),
        selected_ui,
        fixed = TRUE
      )
    )

    session$setInputs(
      opskrift_editPressed = list(key = "burger_opskr", row = 1L)
    )
    stopifnot(identical(
      output$opskrift_edit_context,
      "0.175 kg hakket oksekød"
    ))

    session$setInputs(cancel_opskrift_row = 1)

    session$setInputs(
      opskrift_editPressed = list(key = "burger_opskr", row = 2L)
    )
    stopifnot(identical(
      output$opskrift_edit_context,
      "50 gram bacon i skiver"
    ))

    session$setInputs(opskrift_addPressed = list(key = "burger_opskr"))
    stopifnot(identical(
      output$opskrift_add_context,
      "Tilføj ny ingrediens til 'Burger'"
    ))

    session$setInputs(opskrift_addPressed = list(key = "carbonara_opskr"))
    stopifnot(identical(
      output$opskrift_add_context,
      "Tilføj ny ingrediens til 'Carbonara'"
    ))

    session$setInputs(
      opskrift_deletePressed = list(key = "burger_opskr", row = 1L)
    )
    stopifnot(identical(
      output$opskrift_delete_context,
      "0.175 kg hakket oksekød"
    ))

    session$setInputs(
      opskrift_deletePressed = list(key = "burger_opskr", row = 2L)
    )
    stopifnot(identical(
      output$opskrift_delete_context,
      "50 gram bacon i skiver"
    ))

    session$setInputs(opskrift_archivePressed = list(key = "burger_opskr"))
    stopifnot(identical(
      output$ret_delete_context,
      'Er du sikker paa, at du vil arkivere "Burger"?'
    ))

    session$setInputs(opskrift_archivePressed = list(key = "carbonara_opskr"))
    stopifnot(identical(
      output$ret_delete_context,
      'Er du sikker paa, at du vil arkivere "Carbonara"?'
    ))

    catalog_with_archive <- shiny::isolate(catalog_state())
    catalog_with_archive$archived_retter <- data.frame(
      retter = "Arkivtest",
      key = "arkivtest_opskr",
      type = "vegetar",
      stringsAsFactors = FALSE
    )
    catalog_state(catalog_with_archive)
    session$flushReact()

    recipes_ui_with_archive <- paste(
      as.character(output$opskrifter_ui),
      collapse = ""
    )
    stopifnot(
      grepl(
        session$ns("restore_ret"),
        recipes_ui_with_archive,
        fixed = TRUE
      ),
      grepl(
        session$ns("delete_archived_ret"),
        recipes_ui_with_archive,
        fixed = TRUE
      )
    )

    session$setInputs(delete_archived_ret = "arkivtest_opskr")
    stopifnot(identical(
      output$ret_permanent_delete_context,
      'Er du sikker paa, at du vil slette "Arkivtest" permanent?'
    ))

    replacement_catalog <- shiny::isolate(catalog_state())
    replacement_catalog$recipes[["burger_opskr"]][[1]][[1]] <-
      "opdateret oksekød"
    replacement_catalog$recipes[["burger_opskr"]]$maengde[[1]] <- 0.2
    replacement_catalog$links <- dplyr::bind_rows(
      dplyr::filter(replacement_catalog$links, ret != "Burger"),
      data.frame(
        ret = "Burger",
        link = "https://example.com/opdateret-burger",
        stringsAsFactors = FALSE
      )
    )
    replacement_catalog$revision <- "replacement-revision"
    catalog_state(replacement_catalog)
    session$flushReact()

    updated_burger_ui <- paste(
      as.character(output$valgt_opskrift_ui),
      collapse = ""
    )
    session$setInputs(
      opskrift_editPressed = list(
        key = "burger_opskr",
        row = 1L,
        nonce = 1L
      )
    )
    stopifnot(
      identical(
        output$opskrift_edit_context,
        "0.2 kg opdateret oksekød"
      ),
      grepl(
        "https://example.com/opdateret-burger",
        updated_burger_ui,
        fixed = TRUE
      )
    )

    session$setInputs(opskrift_valgt_key = "tortellini_opskr")
    tortellini_table <- output$opskrift_tbl_valgt
    tortellini_ui <- paste(
      as.character(output$valgt_opskrift_ui),
      collapse = ""
    )
    session$setInputs(
      opskrift_editPressed = list(
        key = "tortellini_opskr",
        row = 1L,
        nonce = 2L
      )
    )
    stopifnot(is.character(tortellini_table), length(tortellini_table) == 1)
    stopifnot(
      grepl("<th>Ingrediens", tortellini_table, fixed = TRUE),
      identical(
        output$opskrift_edit_context,
        "0.5 pakke tortellini"
      ),
      grepl("Tortellini", tortellini_ui, fixed = TRUE),
      !grepl("Burger", tortellini_ui, fixed = TRUE)
    )

    catalog_without_burger <- shiny::isolate(catalog_state())
    catalog_without_burger$active_retter <- dplyr::filter(
      catalog_without_burger$active_retter,
      key != "burger_opskr"
    )
    catalog_without_burger$revision <- "inactive-burger-revision"
    catalog_state(catalog_without_burger)
    session$flushReact()

    recipes_ui <- output$opskrifter_ui
    recipes_ui_html <- if (
      is.list(recipes_ui) &&
        !is.null(recipes_ui$html)
    ) {
      recipes_ui$html
    } else {
      paste(as.character(recipes_ui), collapse = "")
    }
    stopifnot(
      !"burger_opskr" %in% catalog_without_burger$active_retter$key,
      !grepl("Burger", recipes_ui_html, fixed = TRUE),
      grepl("Tortellini", recipes_ui_html, fixed = TRUE)
    )
  }
)

# Kategoriargumenterne er kun valgfrie standarder. En kategori, der bliver
# tilføjet til varekataloget efter modulstart, skal derfor stadig dukke op,
# når dialogen til en ny ingrediens åbnes.
local({
  had_global_update <- exists(
    "updateSelectInput",
    envir = .GlobalEnv,
    inherits = FALSE
  )
  original_update <- get(
    "updateSelectInput",
    envir = .GlobalEnv,
    inherits = TRUE
  )
  on.exit(
    if (had_global_update) {
      assign("updateSelectInput", original_update, envir = .GlobalEnv)
    } else {
      rm("updateSelectInput", envir = .GlobalEnv)
    },
    add = TRUE
  )

  select_updates <- list()
  assign(
    "updateSelectInput",
    function(
      session,
      inputId,
      label = NULL,
      choices = NULL,
      selected = NULL
    ) {
      select_updates[[inputId]] <<- list(
        choices = choices,
        selected = selected
      )
      invisible(NULL)
    },
    envir = .GlobalEnv
  )

  runtime_varer <- shiny::reactiveVal(varer_current())
  catalog_state(fixture_catalog)

  shiny::testServer(
    test_opskrifter_module,
    args = list(
      catalog_read = catalog_read,
      commit_catalog = commit_catalog,
      varer_current = runtime_varer
    ),
    {
      runtime_rows <- runtime_varer()
      runtime_rows <- rbind(
        runtime_rows,
        data.frame(
          Indkobsliste = "Runtime-vare",
          maengde = 1,
          enhed = "pose",
          kat_1 = "runtime-kategori",
          kat_2 = "runtime-underkategori",
          stringsAsFactors = FALSE
        )
      )
      runtime_varer(runtime_rows)
      session$setInputs(
        opskrift_addPressed = list(key = "burger_opskr")
      )

      stopifnot(
        "runtime-kategori" %in%
          select_updates$opskrift_add_kat1$choices,
        "runtime-underkategori" %in%
          select_updates$opskrift_add_kat2$choices
      )
    }
  )
})

message("Opskriftsmodulets outputs følger den aktuelle state uden genregistrering.")
