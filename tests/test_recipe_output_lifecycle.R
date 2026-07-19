suppressPackageStartupMessages(source("app.R", encoding = "UTF-8"))

shiny::testServer(server, {
  session$setInputs(
    ret = "",
    pers = 2,
    salat = "",
    tilbehor = "",
    basis_varer = "agurk",
    basis_varer_manuel = "agurk",
    menu_type = "Alle",
    date_from = Sys.Date() - 30,
    date_to = Sys.Date(),
    top_n = 5,
    opskrift_valgt_key = "burger_opskr"
  )

  burger_model <- selected_recipe_model()
  stopifnot(
    burger_model$key == "burger_opskr",
    burger_model$ret_navn == "Burger",
    nrow(burger_model$df) == 11,
    burger_model$df[[1]][[1]] == "hakket oksekød"
  )

  burger_table_model <- selected_recipe_table_model()
  stopifnot(
    burger_table_model$key == "burger_opskr",
    nrow(burger_table_model$rows) == 11,
    burger_table_model$rows$Ingrediens[[1]] == "0.175 kg hakket oksekød",
    grepl("burger_opskr", burger_table_model$rows$Rediger[[1]], fixed = TRUE),
    grepl("burger_opskr", burger_table_model$rows$Slet[[1]], fixed = TRUE)
  )

  burger_table <- output$opskrift_tbl_valgt
  stopifnot(is.character(burger_table), length(burger_table) == 1)
  stopifnot(grepl("<th>Ingrediens", burger_table, fixed = TRUE))

  session$setInputs(
    opskrift_editPressed = list(key = "burger_opskr", row = 1L)
  )
  stopifnot(identical(
    output$opskrift_edit_context,
    "0.175 kg hakket oksekød"
  ))

  session$setInputs(cancel_opskrift_row = 1)
  stopifnot(
    is.null(rv_recipeEditState$key),
    is.null(rv_recipeEditState$row),
    is.null(rv_recipeEditState$revision)
  )

  session$setInputs(
    opskrift_editPressed = list(key = "burger_opskr", row = 2L)
  )
  stopifnot(identical(
    output$opskrift_edit_context,
    "50 gram bacon i skiver"
  ))

  session$setInputs(
    opskrift_addPressed = list(key = "burger_opskr")
  )
  stopifnot(identical(
    output$opskrift_add_context,
    "Tilføj ny ingrediens til 'Burger'"
  ))

  session$setInputs(
    opskrift_addPressed = list(key = "carbonara_opskr")
  )
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

  session$setInputs(
    opskrift_archivePressed = list(key = "burger_opskr")
  )
  stopifnot(identical(
    output$ret_delete_context,
    'Er du sikker paa, at du vil arkivere "Burger"?'
  ))

  session$setInputs(
    opskrift_archivePressed = list(key = "carbonara_opskr")
  )
  stopifnot(identical(
    output$ret_delete_context,
    'Er du sikker paa, at du vil arkivere "Carbonara"?'
  ))

  catalog_with_archive <- recipe_catalog_current()
  catalog_with_archive$archived_retter <- data.frame(
    retter = "Arkivtest",
    key = "arkivtest_opskr",
    type = "vegetar",
    stringsAsFactors = FALSE
  )
  publish_recipe_catalog(catalog_with_archive)
  session$setInputs(delete_archived_ret = "arkivtest_opskr")
  stopifnot(identical(
    output$ret_permanent_delete_context,
    'Er du sikker paa, at du vil slette "Arkivtest" permanent?'
  ))

  replacement_catalog <- recipe_catalog_current()
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
  publish_recipe_catalog(replacement_catalog)

  updated_burger_model <- selected_recipe_model()
  updated_burger_table <- selected_recipe_table_model()
  stopifnot(
    updated_burger_model$df[[1]][[1]] == "opdateret oksekød",
    updated_burger_model$link_url ==
      "https://example.com/opdateret-burger",
    updated_burger_table$rows$Ingrediens[[1]] ==
      "0.2 kg opdateret oksekød"
  )

  session$setInputs(opskrift_valgt_key = "tortellini_opskr")
  tortellini_model <- selected_recipe_model()
  stopifnot(
    tortellini_model$key == "tortellini_opskr",
    tortellini_model$ret_navn == "Tortellini",
    nrow(tortellini_model$df) == 2,
    tortellini_model$df[[1]][[1]] == "tortellini"
  )

  tortellini_table_model <- selected_recipe_table_model()
  stopifnot(
    tortellini_table_model$key == "tortellini_opskr",
    nrow(tortellini_table_model$rows) == 2,
    grepl("tortellini", tortellini_table_model$rows$Ingrediens[[1]], fixed = TRUE),
    grepl("tortellini_opskr", tortellini_table_model$rows$Rediger[[1]], fixed = TRUE),
    !grepl("burger_opskr", tortellini_table_model$rows$Rediger[[1]], fixed = TRUE)
  )

  tortellini_table <- output$opskrift_tbl_valgt
  stopifnot(is.character(tortellini_table), length(tortellini_table) == 1)
  stopifnot(grepl("<th>Ingrediens", tortellini_table, fixed = TRUE))

  catalog_without_burger <- recipe_catalog_current()
  catalog_without_burger$active_retter <- dplyr::filter(
    catalog_without_burger$active_retter,
    key != "burger_opskr"
  )
  catalog_without_burger$revision <- "inactive-burger-revision"
  publish_recipe_catalog(catalog_without_burger)
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
    !"burger_opskr" %in% active_recipes_current()$key,
    !grepl("Burger", recipes_ui_html, fixed = TRUE),
    grepl("Tortellini", recipes_ui_html, fixed = TRUE)
  )
})

message("Opskriftsoutputs følger den aktuelle state uden genregistrering.")
