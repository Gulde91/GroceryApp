suppressPackageStartupMessages(source("app.R", encoding = "UTF-8"))

varer_fixture <- function() {
  data.frame(
    Indkobsliste = c("Banan", "Mælk"),
    maengde = c(1, 1),
    enhed = c("stk", "liter"),
    kat_1 = c("frugt og grønt", "mejeri"),
    kat_2 = c("", "mælk"),
    stringsAsFactors = FALSE
  )
}

test_varer_module <- function(
  id,
  varer_custom_current,
  varer_all_current,
  commit_varer,
  kategori_1 = character(),
  kategori_2 = character()
) {
  shiny::moduleServer(id, function(input, output, session) {
    mod_varer_server(
      input = input,
      output = output,
      session = session,
      varer_custom_current = varer_custom_current,
      varer_all_current = varer_all_current,
      commit_varer = commit_varer,
      kategori_1 = kategori_1,
      kategori_2 = kategori_2
    )
  })
}

module_ui_html <- paste(
  as.character(
    shiny::tagList(
      mod_varer_ui("varer"),
      mod_varer_dialogs_ui("varer")
    )
  ),
  collapse = ""
)

stopifnot(
  grepl('id="varer-open_ny_vare"', module_ui_html, fixed = TRUE),
  grepl('id="varer-varer_tbl"', module_ui_html, fixed = TRUE),
  grepl('id="varer-popup_ny_vare"', module_ui_html, fixed = TRUE),
  grepl('id="varer-save_ny_vare"', module_ui_html, fixed = TRUE),
  grepl('id="varer-popup_varer_rediger"', module_ui_html, fixed = TRUE),
  grepl('id="varer-varer_edit_enhed"', module_ui_html, fixed = TRUE),
  grepl('id="varer-varer_edit_kat1"', module_ui_html, fixed = TRUE),
  grepl('id="varer-varer_edit_kat2"', module_ui_html, fixed = TRUE),
  grepl('id="varer-save_varer_edit"', module_ui_html, fixed = TRUE)
)

table_widget <- varer_table_widget(
  varer_fixture(),
  shiny::NS("varer")
)
table_data <- table_widget$x$data

stopifnot(
  identical(
    names(table_data),
    c("Vare", "Enhed", "Rediger", "Slet")
  ),
  identical(
    attr(table_widget$x$options, "escapeIdx"),
    "\"1,2\""
  ),
  !any(c("Kategori 1", "Kategori 2") %in% names(table_data)),
  grepl(
    'id="varer-varer_edit_button_1"',
    table_data$Rediger[[1]],
    fixed = TRUE
  ),
  grepl(
    "varer-varer_editPressed",
    table_data$Rediger[[1]],
    fixed = TRUE
  ),
  grepl("Banan", table_data$Rediger[[1]], fixed = TRUE),
  grepl(
    'id="varer-varer_delete_button_1"',
    table_data$Slet[[1]],
    fixed = TRUE
  ),
  grepl(
    "varer-varer_deletePressed",
    table_data$Slet[[1]],
    fixed = TRUE
  ),
  grepl("Banan", table_data$Slet[[1]], fixed = TRUE)
)

unsafe_fixture <- varer_fixture()[1, , drop = FALSE]
unsafe_fixture$Indkobsliste <- "<img src=x onerror=alert(1)>"
unsafe_fixture$enhed <- "<b>stk</b>"
unsafe_fixture$kat_1 <- "<i>kategori</i>"
unsafe_fixture$kat_2 <- "<script>alert(2)</script>"
unsafe_widget <- varer_table_widget(
  unsafe_fixture,
  shiny::NS("varer")
)
escaped_widget <- unsafe_widget$preRenderHook(unsafe_widget)
stopifnot(
  identical(
    escaped_widget$x$data[[1]][[1]],
    "&lt;img src=x onerror=alert(1)&gt;"
  ),
  identical(
    escaped_widget$x$data[[2]][[1]],
    "&lt;b&gt;stk&lt;/b&gt;"
  ),
  !any(grepl("kategori|alert\\(2\\)", unlist(escaped_widget$x$data))),
  grepl("<button", escaped_widget$x$data[[3]][[1]], fixed = TRUE)
)

# Tilføjelse afleverer et komplet, sorteret forslag til root-commit. State
# ændres kun af commit-spyen, ligesom den rigtige app først publicerer efter
# en vellykket filskrivning.
add_state <- shiny::reactiveVal(varer_fixture())
recipe_only_row <- data.frame(
  Indkobsliste = "Opskriftsvare",
  maengde = 1,
  enhed = "stk",
  kat_1 = "konserves",
  kat_2 = "",
  stringsAsFactors = FALSE
)
add_all_current <- function() {
  rbind(add_state(), recipe_only_row)
}
add_commits <- list()
add_commit <- function(
  next_df,
  error_message = "",
  log_context = list()
) {
  add_commits[[length(add_commits) + 1L]] <<- list(
    data = next_df,
    error_message = error_message,
    log_context = log_context
  )
  add_state(next_df)
  TRUE
}

shiny::testServer(
  test_varer_module,
  args = list(
    varer_custom_current = add_state,
    varer_all_current = add_all_current,
    commit_varer = add_commit,
    kategori_1 = c("frugt og grønt", "mejeri"),
    kategori_2 = c("", "mælk")
  ),
  {
    session$setInputs(
      ny_vare_navn = "  Abrikos  ",
      ny_vare_enhed = "kg",
      ny_vare_kat1 = "frugt og grønt",
      ny_vare_kat2 = ""
    )
    session$setInputs(save_ny_vare = 1L)

    current <- add_state()
    stopifnot(
      length(add_commits) == 1L,
      identical(add_commits[[1L]]$log_context$action, "basis_item_add"),
      identical(add_commits[[1L]]$log_context$item_name, "Abrikos"),
      identical(
        add_commits[[1L]]$log_context$success_message,
        'Varen "Abrikos" blev tilføjet til bruttolisten.'
      ),
      identical(
        names(current),
        c("Indkobsliste", "maengde", "enhed", "kat_1", "kat_2")
      ),
      identical(current$Indkobsliste, c("Abrikos", "Banan", "Mælk"))
    )
    new_row <- current[current$Indkobsliste == "Abrikos", , drop = FALSE]
    stopifnot(
      nrow(new_row) == 1L,
      identical(new_row$maengde[[1]], 1),
      identical(new_row$enhed[[1]], "kg"),
      identical(new_row$kat_1[[1]], "frugt og grønt"),
      identical(new_row$kat_2[[1]], "")
    )

    state_before_duplicate <- current
    session$setInputs(ny_vare_navn = "  mÆLK ")
    session$setInputs(save_ny_vare = 2L)
    stopifnot(
      length(add_commits) == 1L,
      identical(add_state(), state_before_duplicate)
    )

    session$setInputs(ny_vare_navn = "opskriftsVARE")
    session$setInputs(save_ny_vare = 3L)
    stopifnot(
      length(add_commits) == 1L,
      identical(add_state(), state_before_duplicate)
    )

    session$setInputs(ny_vare_navn = "   ")
    session$setInputs(save_ny_vare = 4L)
    stopifnot(
      length(add_commits) == 1L,
      identical(add_state(), state_before_duplicate)
    )
  }
)

# Dialogens kategorier skal komme fra den aktuelle vareliste, selv når de
# valgfrie kategoriargumenter udelades, og kategorien først opstår efter
# modulstart.
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

  runtime_custom <- shiny::reactiveVal(varer_fixture())
  runtime_all <- shiny::reactiveVal(varer_fixture())
  shiny::testServer(
    test_varer_module,
    args = list(
      varer_custom_current = runtime_custom,
      varer_all_current = runtime_all,
      commit_varer = function(
        next_df,
        error_message = "",
        log_context = list()
      ) TRUE
    ),
    {
      runtime_rows <- runtime_all()
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
      runtime_all(runtime_rows)
      session$setInputs(open_ny_vare = 1L)

      stopifnot(
        "runtime-kategori" %in%
          select_updates$ny_vare_kat1$choices,
        "runtime-underkategori" %in%
          select_updates$ny_vare_kat2$choices
      )

      session$setInputs(varer_editPressed = "Mælk")
      stopifnot(
        "pose" %in% select_updates$varer_edit_enhed$choices,
        "runtime-kategori" %in%
          select_updates$varer_edit_kat1$choices,
        "runtime-underkategori" %in%
          select_updates$varer_edit_kat2$choices,
        identical(select_updates$varer_edit_enhed$selected, "liter"),
        identical(select_updates$varer_edit_kat1$selected, "mejeri"),
        identical(select_updates$varer_edit_kat2$selected, "mælk")
      )
    }
  )
})

# Redigering gemmer navn, enhed og kategorier samlet, men bevarer mængden.
edit_state <- shiny::reactiveVal(varer_fixture())
edit_commits <- list()
edit_log_contexts <- list()
edit_commit <- function(
  next_df,
  error_message = "",
  log_context = list()
) {
  edit_commits[[length(edit_commits) + 1L]] <<- next_df
  edit_log_contexts[[length(edit_log_contexts) + 1L]] <<- log_context
  edit_state(next_df)
  TRUE
}

shiny::testServer(
  test_varer_module,
  args = list(
    varer_custom_current = edit_state,
    varer_all_current = edit_state,
    commit_varer = edit_commit,
    kategori_1 = c("frugt og grønt", "mejeri"),
    kategori_2 = c("", "mælk")
  ),
  {
    session$flushReact()
    session$setInputs(varer_editPressed = "Mælk")
    session$setInputs(
      varer_edit_value = "Citronmælk",
      varer_edit_enhed = "kg",
      varer_edit_kat1 = "frugt og grønt",
      varer_edit_kat2 = ""
    )
    session$setInputs(save_varer_edit = 1L)

    current <- edit_state()
    edited_row <- current[
      current$Indkobsliste == "Citronmælk",
      ,
      drop = FALSE
    ]
    stopifnot(
      length(edit_commits) == 1L,
      identical(edit_log_contexts[[1L]]$action, "basis_item_update"),
      identical(edit_log_contexts[[1L]]$item_name, "Citronmælk"),
      identical(edit_log_contexts[[1L]]$previous_item_name, "Mælk"),
      identical(
        edit_log_contexts[[1L]]$success_message,
        paste(
          'Varen "Mælk" blev omdøbt til "Citronmælk"',
          "på bruttolisten."
        )
      ),
      identical(current$Indkobsliste, c("Banan", "Citronmælk")),
      nrow(edited_row) == 1L,
      identical(edited_row$maengde[[1]], 1),
      identical(edited_row$enhed[[1]], "kg"),
      identical(edited_row$kat_1[[1]], "frugt og grønt"),
      identical(edited_row$kat_2[[1]], "")
    )

    session$setInputs(varer_editPressed = "Banan")
    session$setInputs(
      varer_edit_value = "Banan",
      varer_edit_enhed = "stk",
      varer_edit_kat1 = "mejeri",
      varer_edit_kat2 = "mælk"
    )
    session$setInputs(save_varer_edit = 2L)

    category_only_row <- edit_state()[
      edit_state()$Indkobsliste == "Banan",
      ,
      drop = FALSE
    ]
    stopifnot(
      length(edit_commits) == 2L,
      identical(category_only_row$maengde[[1]], 1),
      identical(category_only_row$enhed[[1]], "stk"),
      identical(category_only_row$kat_1[[1]], "mejeri"),
      identical(category_only_row$kat_2[[1]], "mælk")
    )

    current <- edit_state()
    state_before_invalid_edit <- current
    session$setInputs(varer_editPressed = "Findes ikke")
    session$setInputs(varer_edit_value = "Må ikke gemmes")
    session$setInputs(save_varer_edit = 3L)
    stopifnot(
      length(edit_commits) == 2L,
      identical(edit_state(), state_before_invalid_edit)
    )
  }
)

# Sletning bruger rækken fra knappen, mens ugyldige eller forældede
# rækkenumre er sikre no-ops.
delete_state <- shiny::reactiveVal(varer_fixture())
delete_commits <- list()
delete_log_contexts <- list()
delete_commit <- function(
  next_df,
  error_message = "",
  log_context = list()
) {
  delete_commits[[length(delete_commits) + 1L]] <<- next_df
  delete_log_contexts[[length(delete_log_contexts) + 1L]] <<- log_context
  delete_state(next_df)
  TRUE
}

shiny::testServer(
  test_varer_module,
  args = list(
    varer_custom_current = delete_state,
    varer_all_current = delete_state,
    commit_varer = delete_commit,
    kategori_1 = c("frugt og grønt", "mejeri"),
    kategori_2 = c("", "mælk")
  ),
  {
    session$setInputs(varer_deletePressed = "Banan")
    stopifnot(
      length(delete_commits) == 1L,
      identical(delete_log_contexts[[1L]]$action, "basis_item_delete"),
      identical(delete_log_contexts[[1L]]$item_name, "Banan"),
      identical(
        delete_log_contexts[[1L]]$success_message,
        'Varen "Banan" blev slettet fra bruttolisten.'
      ),
      identical(delete_state()$Indkobsliste, "Mælk")
    )

    state_before_invalid_delete <- delete_state()
    session$setInputs(varer_deletePressed = "Gammel tabelvare")
    stopifnot(
      length(delete_commits) == 1L,
      identical(delete_state(), state_before_invalid_delete)
    )
  }
)

# En commitfejl må ikke publicere kandidat-data. Den åbne redigering bevares,
# så præcis samme handling kan prøves igen.
retry_state <- shiny::reactiveVal(varer_fixture())
retry_attempts <- list()
retry_log_contexts <- list()
fail_next_commit <- TRUE
retry_commit <- function(
  next_df,
  error_message = "",
  log_context = list()
) {
  retry_attempts[[length(retry_attempts) + 1L]] <<- next_df
  retry_log_contexts[[length(retry_log_contexts) + 1L]] <<- log_context
  if (fail_next_commit) {
    fail_next_commit <<- FALSE
    return(FALSE)
  }

  retry_state(next_df)
  TRUE
}

shiny::testServer(
  test_varer_module,
  args = list(
    varer_custom_current = retry_state,
    varer_all_current = retry_state,
    commit_varer = retry_commit,
    kategori_1 = c("frugt og grønt", "mejeri"),
    kategori_2 = c("", "mælk")
  ),
  {
    session$flushReact()
    state_before_failure <- retry_state()
    session$setInputs(varer_editPressed = "Banan")
    session$setInputs(
      varer_edit_value = "Ananas",
      varer_edit_enhed = "kg",
      varer_edit_kat1 = "mejeri",
      varer_edit_kat2 = "mælk"
    )
    session$setInputs(save_varer_edit = 1L)

    stopifnot(
      length(retry_attempts) == 1L,
      identical(retry_state(), state_before_failure)
    )

    session$setInputs(save_varer_edit = 2L)
    stopifnot(
      length(retry_attempts) == 2L,
      identical(retry_attempts[[2]], retry_attempts[[1]]),
      identical(retry_log_contexts[[2L]], retry_log_contexts[[1L]]),
      identical(retry_state()$Indkobsliste, c("Ananas", "Mælk")),
      identical(retry_state()$enhed, c("kg", "liter")),
      identical(retry_state()$kat_1, c("mejeri", "mejeri")),
      identical(retry_state()$kat_2, c("mælk", "mælk"))
    )
  }
)

message(
  paste(
    "Varemodulet er namespacet og håndterer tilføjelse, redigering,",
    "sletning og commitfejl korrekt."
  )
)
