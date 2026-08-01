# Shiny-modul for fanen Opskrifter -----------------------------------------
#
# Denne fil styrer fanens brugerflade, dialoger og reaktive arbejdsgange for
# oprettelse, redigering, arkivering og sletning. Selve dataændringerne og
# lagringen udføres gennem opskriftskatalogets regler og den indsprøjtede state.

library(stats)
library(htmltools)
library(DT)
library(shiny)
library(shinyMobile)
library(purrr)
library(dplyr)
library(shinyjs)

#' Byg brugerfladen til fanen Opskrifter
#'
#' Funktionen samler den synlige introduktion, knappen til at oprette en ret
#' og den dynamiske del af opskriftsfanen. Alle id'er får modulets namespace,
#' så fanens input og output ikke kolliderer med resten af appen.
#'
#' @param id Modul-id'et, som også bruges ved kaldet til servermodulet.
#'
#' @return En Shiny-tagliste, der kan indsættes direkte i appens UI.
#' @keywords internal
mod_opskrifter_ui <- function(id) {
  ns <- NS(id)

  tagList(
    f7BlockTitle(title = "Opskrifter"),
    f7Block(
      inset = TRUE,
      strong = TRUE,
      tags$p(
        "Alle opskrifter nedenfor er angivet med mængder svarende til ",
        tags$b("1 person"),
        "."
      ),
      tags$p(
        paste(
          "Du kan redigere og slette ingredienslinjer direkte.",
          "Ændringer gemmes automatisk."
        )
      ),
      f7Button(
        ns("open_ny_ret"),
        "Tilføj ny ret",
        fill = TRUE,
        color = "green"
      )
    ),
    uiOutput(ns("opskrifter_ui"))
  )
}

#' Byg dialogerne til opskriftsmodulet
#'
#' Dialogerne ligger samlet uden for den dynamiske opskriftsvisning. Dermed
#' findes de altid i DOM'en, mens serveren blot åbner og lukker den relevante
#' dialog ved redigering, tilføjelse, arkivering eller sletning.
#'
#' @param id Modul-id'et, som også bruges ved kaldet til servermodulet.
#'
#' @return En Shiny-tagliste med alle modulets dialogvinduer.
#' @keywords internal
mod_opskrifter_dialogs_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      id = ns("popup_opskrift_rediger"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Redigér ingrediens"),
        tags$p(textOutput(ns("opskrift_edit_context"))),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          nInput(
            ns("opskrift_edit_maengde"),
            "Mængde",
            value = 1
          ),
          sInput(
            ns("opskrift_edit_enhed"),
            "Enhed",
            choices = c(""),
            selected = ""
          ),
          sInput(
            ns("opskrift_edit_kat1"),
            "Kategori 1",
            choices = c(""),
            selected = ""
          ),
          sInput(
            ns("opskrift_edit_kat2"),
            "Kategori 2",
            choices = c(""),
            selected = ""
          ),
          br(),
          f7Button(
            ns("save_opskrift_row"),
            "Opdater række",
            fill = TRUE,
            color = "blue"
          ),
          br(),
          f7Button(
            ns("cancel_opskrift_row"),
            "Luk",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    ),
    tags$div(
      id = ns("popup_opskrift_tilfoej"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Tilføj ingrediens"),
        tags$p(textOutput(ns("opskrift_add_context"))),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          tInput(
            ns("opskrift_add_navn"),
            "Varenavn"
          ),
          nInput(
            ns("opskrift_add_maengde"),
            "Mængde",
            value = 1
          ),
          sInput(
            ns("opskrift_add_enhed"),
            "Enhed",
            choices = c(""),
            selected = ""
          ),
          sInput(
            ns("opskrift_add_kat1"),
            "Kategori 1",
            choices = c(""),
            selected = ""
          ),
          sInput(
            ns("opskrift_add_kat2"),
            "Kategori 2",
            choices = c(""),
            selected = ""
          ),
          br(),
          f7Button(
            ns("save_opskrift_new_row"),
            "Tilføj vare",
            fill = TRUE,
            color = "green"
          ),
          br(),
          f7Button(
            ns("cancel_opskrift_new_row"),
            "Luk",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    ),
    tags$div(
      id = ns("popup_opskrift_slet_bekraeft"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Slet ingrediens"),
        tags$p(textOutput(ns("opskrift_delete_context"))),
        tags$p(
          "Er du sikker på at du vil slette denne ingredienslinje?"
        ),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          f7Button(
            ns("confirm_delete_opskrift_row"),
            "Ja, slet",
            fill = TRUE,
            color = "red"
          ),
          br(),
          f7Button(
            ns("cancel_delete_opskrift_row"),
            "Nej",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    ),
    tags$div(
      id = ns("popup_ret_slet_bekraeft"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Arkiver ret"),
        tags$p(textOutput(ns("ret_delete_context"))),
        tags$p(
          "Retten flyttes til arkivet og kan gendannes igen senere."
        ),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          f7Button(
            ns("confirm_delete_ret"),
            "Ja, arkiver ret",
            fill = TRUE,
            color = "red"
          ),
          br(),
          f7Button(
            ns("cancel_delete_ret"),
            "Nej",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    ),
    tags$div(
      id = ns("popup_ret_slet_permanent_bekraeft"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Slet ret permanent"),
        tags$p(
          textOutput(ns("ret_permanent_delete_context"))
        ),
        tags$p(
          paste(
            "Retten, opskriftsfilen og linket slettes permanent",
            "og kan ikke gendannes."
          )
        ),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          f7Button(
            ns("confirm_delete_archived_ret"),
            "Ja, slet permanent",
            fill = TRUE,
            color = "red"
          ),
          br(),
          f7Button(
            ns("cancel_delete_archived_ret"),
            "Nej",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    ),
    tags$div(
      id = ns("popup_ny_ret"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Tilføj ny ret"),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          tInput(
            ns("ny_ret_navn"),
            "Rettens navn"
          ),
          sInput(
            ns("ny_ret_type"),
            "Type",
            choices = c("vegetar", "kylling", "gris", "okse", "fisk"),
            selected = "vegetar"
          ),
          tInput(
            ns("ny_ret_link"),
            "Link (valgfrit)"
          ),
          br(),
          f7Button(
            ns("save_ny_ret"),
            "Gem ret",
            fill = TRUE,
            color = "blue"
          ),
          br(),
          f7Button(
            ns("close_ny_ret"),
            "Luk",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    )
  )
}

#' Knyt logikken til fanen Opskrifter
#'
#' Servermodulet styrer dialoger og brugerhandlinger i opskriftsfanen. Selve
#' opskriftskataloget ejes fortsat af hovedappen: modulet læser kataloget gennem
#' de udleverede gettere og sender alle ændringer gennem én commit-funktion.
#' Det bevarer én kanonisk state og gør modulet lettere at teste.
#'
#' @param input Shiny-inputobjektet, som leveres af `callModule()`.
#' @param output Shiny-outputobjektet, som leveres af `callModule()`.
#' @param session Shiny-sessionen, som leveres af `callModule()`.
#' @param catalog_read Navngivet liste af reaktive getter-funktioner til
#'   katalogets snapshot, opskrifter, links, aktive og arkiverede retter samt
#'   revision.
#' @param commit_catalog Funktion, der gemmer et nyt katalog-snapshot.
#' @param varer_current Reaktiv getter til appens aktuelle varekartotek.
#' @param kategori_1 Valgfrie standardværdier til ingrediensens første
#'   kategori. Aktuelle kategorier hentes desuden fra `varer_current`, hver
#'   gang en ingrediensdialog åbnes.
#' @param kategori_2 Valgfrie standardværdier til ingrediensens anden
#'   kategori. Aktuelle kategorier hentes desuden fra `varer_current`, hver
#'   gang en ingrediensdialog åbnes.
#'
#' @return En navngivet liste med den reaktive hændelse
#'   `active_retter_changed`.
#' @keywords internal
mod_opskrifter_server <- function(
  input,
  output,
  session,
  catalog_read,
  commit_catalog,
  varer_current,
  kategori_1 = character(),
  kategori_2 = character()
) {
  validate_recipe_module_dependencies(
    catalog_read,
    commit_catalog,
    varer_current
  )

  ns <- session$ns

  rv_recipeEditState <- reactiveValues(
    key = NULL,
    row = NULL,
    revision = NULL
  )
  rv_recipeDeleteState <- reactiveValues(
    key = NULL,
    row = NULL,
    revision = NULL
  )
  rv_recipeArchiveState <- reactiveValues(key = NULL)
  rv_recipePermanentDeleteState <- reactiveValues(key = NULL)
  rv_recipeAddState <- reactiveValues(key = NULL)

  rv_activeRetterChanged <- reactiveVal(NULL)
  active_change_seq <- reactiveVal(0L)

  output$opskrift_edit_context <- renderText({
    recipe_row_context(
      rv_recipeEditState$key,
      rv_recipeEditState$row,
      catalog_read$recipes()
    )
  })

  output$opskrift_add_context <- renderText({
    key <- rv_recipeAddState$key
    req(!is.null(key))

    ops <- catalog_read$recipes()
    req(key %in% names(ops))

    sprintf("Tilføj ny ingrediens til '%s'", names(ops[[key]])[1])
  })

  output$opskrift_delete_context <- renderText({
    recipe_row_context(
      rv_recipeDeleteState$key,
      rv_recipeDeleteState$row,
      catalog_read$recipes()
    )
  })

  output$ret_delete_context <- renderText({
    key <- rv_recipeArchiveState$key
    req(!is.null(key))

    active <- catalog_read$active_retter()
    row <- match(key, active$key)
    req(!is.na(row))

    sprintf(
      'Er du sikker paa, at du vil arkivere "%s"?',
      active$retter[[row]]
    )
  })

  output$ret_permanent_delete_context <- renderText({
    key <- rv_recipePermanentDeleteState$key
    req(!is.null(key))

    archive <- catalog_read$archived_retter()
    row <- match(key, archive$key)
    req(!is.na(row))

    sprintf(
      'Er du sikker paa, at du vil slette "%s" permanent?',
      archive$retter[[row]]
    )
  })

  for (output_id in c(
    "opskrift_edit_context",
    "opskrift_add_context",
    "opskrift_delete_context",
    "ret_delete_context",
    "ret_permanent_delete_context"
  )) {
    outputOptions(
      output,
      output_id,
      suspendWhenHidden = FALSE
    )
  }

  observeEvent(input$open_ny_ret, {
    updateTextInput(session, "ny_ret_navn", value = "")
    updateSelectInput(
      session,
      "ny_ret_type",
      selected = "vegetar"
    )
    updateTextInput(session, "ny_ret_link", value = "")
    recipe_show_dialog("popup_ny_ret", ns)
  })

  observeEvent(input$close_ny_ret, {
    recipe_hide_dialog("popup_ny_ret", ns)
  })

  observeEvent(input$save_ny_ret, {
    change_attempt <- recipe_attempt_catalog_change(
      recipe_catalog_create(
        catalog_read$snapshot(),
        recipe_name = input$ny_ret_navn,
        recipe_type = input$ny_ret_type,
        link = input$ny_ret_link
      )
    )
    if (!change_attempt$ok) {
      showNotification(
        change_attempt$message,
        type = "warning"
      )
      return(invisible(NULL))
    }
    change <- change_attempt$value

    saved <- commit_catalog(
      change$catalog,
      delete_recipe_keys = change$delete_recipe_keys,
      error_message = "Retten kunne ikke oprettes."
    )
    if (!isTRUE(saved)) return(invisible(NULL))

    recipe_hide_dialog("popup_ny_ret", ns)
    updateSelectizeInput(
      session,
      "opskrift_valgt_key",
      choices = recipe_choices(
        change$catalog$active_retter,
        names(change$catalog$recipes)
      ),
      selected = change$event$key,
      options = opskrift_selectize_options
    )
    recipe_emit_active_change(
      rv_activeRetterChanged,
      active_change_seq,
      change$event$reason,
      catalog_read$revision()
    )
    showNotification(
      sprintf('Retten "%s" er oprettet.', change$event$recipe_name),
      type = "message"
    )
  })

  observeEvent(input$opskrift_editPressed, {
    info <- input$opskrift_editPressed
    req(!is.null(info$key), !is.null(info$row))

    key <- as.character(info$key)
    row <- suppressWarnings(as.integer(info$row))
    recipes <- catalog_read$recipes()
    req(key %in% names(recipes), !is.na(row))

    df <- recipes[[key]]
    req(nrow(df) >= row)

    rv_recipeEditState$key <- key
    rv_recipeEditState$row <- row
    rv_recipeEditState$revision <- catalog_read$revision()

    varer <- varer_current()
    enhed_choices <- sort(unique(c("", varer$enhed, df$enhed)))
    kat1_choices <- sort(unique(c(kategori_1, varer$kat_1, df$kat_1)))
    kat2_choices <- sort(unique(c("", kategori_2, varer$kat_2, df$kat_2)))

    updateNumericInput(
      session,
      "opskrift_edit_maengde",
      value = df$maengde[row]
    )
    updateSelectInput(
      session,
      "opskrift_edit_enhed",
      choices = enhed_choices,
      selected = df$enhed[row]
    )
    updateSelectInput(
      session,
      "opskrift_edit_kat1",
      choices = kat1_choices,
      selected = df$kat_1[row]
    )
    updateSelectInput(
      session,
      "opskrift_edit_kat2",
      choices = kat2_choices,
      selected = df$kat_2[row]
    )

    recipe_show_dialog("popup_opskrift_rediger", ns)
  })

  observeEvent(input$save_opskrift_row, {
    key <- rv_recipeEditState$key
    row <- rv_recipeEditState$row
    req(!is.null(key), !is.null(row))
    if (!recipe_row_action_is_current(
      rv_recipeEditState$revision,
      catalog_read$revision()
    )) {
      return(invisible(NULL))
    }

    change_attempt <- recipe_attempt_catalog_change(
      recipe_catalog_update_ingredient(
        catalog_read$snapshot(),
        key = key,
        row = row,
        amount = input$opskrift_edit_maengde,
        unit = input$opskrift_edit_enhed,
        category_1 = input$opskrift_edit_kat1,
        category_2 = input$opskrift_edit_kat2
      )
    )
    if (!change_attempt$ok) {
      showNotification(
        change_attempt$message,
        type = "error"
      )
      return(invisible(NULL))
    }
    change <- change_attempt$value

    saved <- commit_catalog(
      change$catalog,
      delete_recipe_keys = change$delete_recipe_keys,
      error_message = "Ingrediensen kunne ikke opdateres."
    )
    if (!isTRUE(saved)) return(invisible(NULL))

    recipe_hide_dialog("popup_opskrift_rediger", ns)
    rv_recipeEditState$key <- NULL
    rv_recipeEditState$row <- NULL
    rv_recipeEditState$revision <- NULL
    showNotification(
      "Ingrediensen er opdateret og gemt.",
      type = "message"
    )
  })

  observeEvent(input$cancel_opskrift_row, {
    recipe_hide_dialog("popup_opskrift_rediger", ns)
    rv_recipeEditState$key <- NULL
    rv_recipeEditState$row <- NULL
    rv_recipeEditState$revision <- NULL
  })

  observeEvent(input$opskrift_addPressed, {
    info <- input$opskrift_addPressed
    req(!is.null(info$key))

    key <- as.character(info$key)
    recipes <- catalog_read$recipes()
    req(key %in% names(recipes))

    rv_recipeAddState$key <- key
    df <- recipes[[key]]
    varer <- varer_current()

    enhed_choices <- sort(unique(c("", varer$enhed, df$enhed)))
    kat1_choices <- sort(unique(c(kategori_1, varer$kat_1, df$kat_1)))
    kat2_choices <- sort(unique(c("", kategori_2, varer$kat_2, df$kat_2)))

    updateTextInput(
      session,
      "opskrift_add_navn",
      value = ""
    )
    updateNumericInput(
      session,
      "opskrift_add_maengde",
      value = 1
    )
    updateSelectInput(
      session,
      "opskrift_add_enhed",
      choices = enhed_choices,
      selected = ""
    )
    updateSelectInput(
      session,
      "opskrift_add_kat1",
      choices = kat1_choices,
      selected = "konserves"
    )
    updateSelectInput(
      session,
      "opskrift_add_kat2",
      choices = kat2_choices,
      selected = ""
    )

    recipe_show_dialog("popup_opskrift_tilfoej", ns)
  })

  observeEvent(input$save_opskrift_new_row, {
    key <- rv_recipeAddState$key
    req(!is.null(key))

    change_attempt <- recipe_attempt_catalog_change(
      recipe_catalog_add_ingredient(
        catalog_read$snapshot(),
        key = key,
        name = input$opskrift_add_navn,
        amount = input$opskrift_add_maengde,
        unit = input$opskrift_add_enhed,
        category_1 = input$opskrift_add_kat1,
        category_2 = input$opskrift_add_kat2
      )
    )
    if (!change_attempt$ok) {
      showNotification(
        change_attempt$message,
        type = "error"
      )
      return(invisible(NULL))
    }
    change <- change_attempt$value

    saved <- commit_catalog(
      change$catalog,
      delete_recipe_keys = change$delete_recipe_keys,
      error_message = "Ingrediensen kunne ikke tilføjes."
    )
    if (!isTRUE(saved)) return(invisible(NULL))

    recipe_hide_dialog("popup_opskrift_tilfoej", ns)
    rv_recipeAddState$key <- NULL
    showNotification(
      sprintf(
        'Ingrediensen "%s" er tilføjet.',
        change$event$ingredient_name
      ),
      type = "message"
    )
  })

  observeEvent(input$cancel_opskrift_new_row, {
    recipe_hide_dialog("popup_opskrift_tilfoej", ns)
    rv_recipeAddState$key <- NULL
  })

  observeEvent(input$opskrift_deletePressed, {
    info <- input$opskrift_deletePressed
    req(!is.null(info$key), !is.null(info$row))

    key <- as.character(info$key)
    row <- suppressWarnings(as.integer(info$row))
    recipes <- catalog_read$recipes()
    req(key %in% names(recipes), !is.na(row))

    df <- recipes[[key]]
    req(!is.null(df), nrow(df) >= row)

    rv_recipeDeleteState$key <- key
    rv_recipeDeleteState$row <- row
    rv_recipeDeleteState$revision <- catalog_read$revision()
    recipe_show_dialog("popup_opskrift_slet_bekraeft", ns)
  })

  observeEvent(input$confirm_delete_opskrift_row, {
    key <- rv_recipeDeleteState$key
    row <- rv_recipeDeleteState$row
    req(!is.null(key), !is.null(row))
    if (!recipe_row_action_is_current(
      rv_recipeDeleteState$revision,
      catalog_read$revision()
    )) {
      return(invisible(NULL))
    }

    change_attempt <- recipe_attempt_catalog_change(
      recipe_catalog_delete_ingredient(
        catalog_read$snapshot(),
        key = key,
        row = row
      )
    )
    if (!change_attempt$ok) {
      showNotification(
        change_attempt$message,
        type = "error"
      )
      return(invisible(NULL))
    }
    change <- change_attempt$value

    saved <- commit_catalog(
      change$catalog,
      delete_recipe_keys = change$delete_recipe_keys,
      error_message = "Ingrediensen kunne ikke slettes."
    )
    if (!isTRUE(saved)) return(invisible(NULL))

    recipe_hide_dialog("popup_opskrift_slet_bekraeft", ns)
    rv_recipeDeleteState$key <- NULL
    rv_recipeDeleteState$row <- NULL
    rv_recipeDeleteState$revision <- NULL
    showNotification(
      sprintf(
        'Linjen "%s" er slettet permanent.',
        change$event$line
      ),
      type = "message"
    )
  })

  observeEvent(input$cancel_delete_opskrift_row, {
    recipe_hide_dialog("popup_opskrift_slet_bekraeft", ns)
    rv_recipeDeleteState$key <- NULL
    rv_recipeDeleteState$row <- NULL
    rv_recipeDeleteState$revision <- NULL
  })

  observeEvent(input$opskrift_archivePressed, {
    info <- input$opskrift_archivePressed
    req(!is.null(info$key))

    key <- as.character(info$key)
    active <- catalog_read$active_retter()
    req(key %in% active$key)

    rv_recipeArchiveState$key <- key
    recipe_show_dialog("popup_ret_slet_bekraeft", ns)
  })

  observeEvent(input$confirm_delete_ret, {
    key <- rv_recipeArchiveState$key
    req(!is.null(key))

    change_attempt <- recipe_attempt_catalog_change(
      recipe_catalog_archive(catalog_read$snapshot(), key)
    )
    if (!change_attempt$ok) {
      showNotification(
        change_attempt$message,
        type = "error"
      )
      return(invisible(NULL))
    }
    change <- change_attempt$value

    saved <- commit_catalog(
      change$catalog,
      delete_recipe_keys = change$delete_recipe_keys,
      error_message = "Retten kunne ikke arkiveres."
    )
    if (!isTRUE(saved)) return(invisible(NULL))

    recipe_hide_dialog("popup_ret_slet_bekraeft", ns)
    rv_recipeArchiveState$key <- NULL

    recipe_keys <- names(change$catalog$recipes)
    valid_active_new <- recipe_active_rows(
      change$catalog$active_retter,
      recipe_keys
    )
    valid_choices <- recipe_choices(valid_active_new, recipe_keys)
    updateSelectizeInput(
      session,
      "opskrift_valgt_key",
      choices = valid_choices,
      selected = if (length(valid_choices) > 0) {
        unname(valid_choices[[1]])
      } else {
        character(0)
      },
      options = opskrift_selectize_options
    )
    recipe_emit_active_change(
      rv_activeRetterChanged,
      active_change_seq,
      change$event$reason,
      catalog_read$revision()
    )

    showNotification(
      sprintf(
        'Retten "%s" er flyttet til arkivet.',
        change$event$recipe_name
      ),
      type = "message"
    )
  })

  observeEvent(input$cancel_delete_ret, {
    recipe_hide_dialog("popup_ret_slet_bekraeft", ns)
    rv_recipeArchiveState$key <- NULL
  })

  observeEvent(input$restore_ret, {
    key <- as.character(input$restore_ret %||% "")
    req(nzchar(key))

    change_attempt <- recipe_attempt_catalog_change(
      recipe_catalog_restore(catalog_read$snapshot(), key)
    )
    if (!change_attempt$ok) {
      showNotification(
        change_attempt$message,
        type = "error"
      )
      return(invisible(NULL))
    }
    change <- change_attempt$value

    saved <- commit_catalog(
      change$catalog,
      delete_recipe_keys = change$delete_recipe_keys,
      error_message = "Retten kunne ikke gendannes."
    )
    if (!isTRUE(saved)) return(invisible(NULL))

    recipe_keys <- names(change$catalog$recipes)
    valid_active_new <- recipe_active_rows(
      change$catalog$active_retter,
      recipe_keys
    )
    updateSelectizeInput(
      session,
      "opskrift_valgt_key",
      choices = recipe_choices(valid_active_new, recipe_keys),
      selected = change$event$key,
      options = opskrift_selectize_options
    )
    recipe_emit_active_change(
      rv_activeRetterChanged,
      active_change_seq,
      change$event$reason,
      catalog_read$revision()
    )

    showNotification(
      sprintf(
        'Retten "%s" er gendannet.',
        change$event$recipe_name
      ),
      type = "message"
    )
  })

  observeEvent(input$delete_archived_ret, {
    key <- as.character(input$delete_archived_ret %||% "")
    req(nzchar(key))

    archive <- catalog_read$archived_retter()
    row_idx <- match(key, archive$key)
    req(!is.na(row_idx))

    rv_recipePermanentDeleteState$key <- key
    recipe_show_dialog("popup_ret_slet_permanent_bekraeft", ns)
  })

  observeEvent(input$confirm_delete_archived_ret, {
    key <- rv_recipePermanentDeleteState$key
    req(!is.null(key), nzchar(key))

    change_attempt <- recipe_attempt_catalog_change(
      recipe_catalog_delete(catalog_read$snapshot(), key)
    )
    if (!change_attempt$ok) {
      showNotification(
        change_attempt$message,
        type = "error"
      )
      return(invisible(NULL))
    }
    change <- change_attempt$value

    saved <- commit_catalog(
      change$catalog,
      delete_recipe_keys = change$delete_recipe_keys,
      error_message = "Retten kunne ikke slettes permanent."
    )
    if (!isTRUE(saved)) return(invisible(NULL))

    recipe_hide_dialog("popup_ret_slet_permanent_bekraeft", ns)
    rv_recipePermanentDeleteState$key <- NULL
    showNotification(
      sprintf(
        'Retten "%s" er slettet permanent.',
        change$event$recipe_name
      ),
      type = "message"
    )
  })

  observeEvent(input$cancel_delete_archived_ret, {
    recipe_hide_dialog("popup_ret_slet_permanent_bekraeft", ns)
    rv_recipePermanentDeleteState$key <- NULL
  })

  selected_recipe_model <- reactive({
    key <- input$opskrift_valgt_key
    req(!is.null(key), nzchar(key))

    ops_local <- catalog_read$recipes()
    active_retter <- recipe_active_rows(
      catalog_read$active_retter(),
      names(ops_local)
    )
    req(
      key %in% active_retter$key,
      key %in% names(ops_local)
    )

    df <- ops_local[[key]]
    ret_navn <- names(df)[1]

    links_df <- catalog_read$links()
    link_url <- links_df$link[links_df$ret == ret_navn]
    link_url <- if (length(link_url) > 0) link_url[1] else ""

    list(
      key = key,
      df = df,
      ret_navn = ret_navn,
      link_url = recipe_normalize_link(link_url)
    )
  })

  selected_recipe_table_model <- reactive({
    model <- selected_recipe_model()
    key <- model$key
    df <- model$df

    ingredienslinje <- recipe_format_line(
      df$maengde,
      df$enhed,
      df[[1]]
    )
    df_vis <- data.frame(
      Ingrediens = htmlEscape(ingredienslinje),
      check.names = FALSE
    )

    edit_event_id <- ns("opskrift_editPressed")
    delete_event_id <- ns("opskrift_deletePressed")

    df_vis$Rediger <- vapply(
      seq_len(nrow(df_vis)),
      recipe_edit_button,
      "",
      key = key,
      ns = ns,
      event_id = edit_event_id
    )

    df_vis$Slet <- vapply(
      seq_len(nrow(df_vis)),
      recipe_delete_button,
      "",
      key = key,
      ns = ns,
      event_id = delete_event_id
    )

    list(key = key, rows = df_vis)
  })

  output$opskrift_tbl_valgt <- renderDT({
    table_model <- selected_recipe_table_model()

    themed_dt(
      table_model$rows,
      escape = c(FALSE, FALSE, FALSE),
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        searching = FALSE
      )
    )
  })

  output$opskrifter_ui <- renderUI({
    ops_local <- catalog_read$recipes()
    active_retter <- recipe_active_rows(
      catalog_read$active_retter(),
      names(ops_local)
    )

    keys <- active_retter$key
    titler <- active_retter$retter
    archive <- catalog_read$archived_retter()

    archive_ui <- NULL
    if (nrow(archive) > 0) {
      restore_event_id <- ns("restore_ret")
      delete_archived_event_id <- ns("delete_archived_ret")

      archive_ui <- f7Block(
        inset = TRUE,
        strong = TRUE,
        tags$h3("Arkiv"),
        tags$p(
          "Slettede retter ligger her og kan gendannes."
        ),
        tagList(lapply(
          seq_len(nrow(archive)),
          recipe_archive_row_ui,
          archive = archive,
          ns = ns,
          restore_event_id = restore_event_id,
          delete_event_id = delete_archived_event_id
        ))
      )
    }

    if (length(keys) == 0) {
      return(tagList(
        f7Block(
          inset = TRUE,
          strong = TRUE,
          tags$p("Der er ingen aktive opskrifter.")
        ),
        archive_ui
      ))
    }

    valgt <- isolate(input$opskrift_valgt_key)
    if (is.null(valgt) || !valgt %in% keys) valgt <- keys[1]

    tagList(
      f7Block(
        inset = TRUE,
        strong = TRUE,
        selectizeInput(
          ns("opskrift_valgt_key"),
          "Vælg opskrift",
          choices = setNames(keys, titler),
          selected = valgt,
          width = "100%",
          options = opskrift_selectize_options
        )
      ),
      uiOutput(ns("valgt_opskrift_ui")),
      archive_ui
    )
  })

  output$valgt_opskrift_ui <- renderUI({
    model <- selected_recipe_model()
    key <- model$key
    ret_navn <- model$ret_navn
    link_url <- model$link_url

    link_tag <- NULL
    if (!is.null(link_url) && nzchar(link_url)) {
      link_tag <- tags$p(
        class = "opskrift-link",
        "Link til opskriften: ",
        tags$a(
          href = link_url,
          target = "_blank",
          rel = "noopener noreferrer",
          class = "external opskrift-link-url",
          "Åbn opskriften"
        )
      )
    }

    add_event_id <- ns("opskrift_addPressed")
    archive_event_id <- ns("opskrift_archivePressed")

    tags$div(
      id = ns(paste0("opskrift_", key)),
      class = "opskrift-anchor",
      f7Block(
        inset = TRUE,
        strong = TRUE,
        tags$h3(ret_navn),
        tags$div(
          class = "recipe-action-bar",
          ga_js_button(
            inputId = ns(paste0("opskrift_add_btn_", key)),
            label = "Tilføj vare",
            class = "recipe-action-btn recipe-action-add",
            onclick = sprintf(
              paste0(
                "Shiny.setInputValue(",
                "'%s', {key: '%s'}, {priority:'event'}); ",
                "return false;"
              ),
              add_event_id,
              key
            )
          ),
          ga_js_button(
            inputId = ns(paste0("opskrift_archive_btn_", key)),
            label = "Arkiver ret",
            class = "recipe-action-btn recipe-action-archive",
            onclick = sprintf(
              paste0(
                "Shiny.setInputValue(",
                "'%s', {key: '%s'}, {priority:'event'}); ",
                "return false;"
              ),
              archive_event_id,
              key
            )
          )
        ),
        br(),
        DTOutput(ns("opskrift_tbl_valgt")),
        link_tag
      )
    )
  })

  list(active_retter_changed = rv_activeRetterChanged)
}

# Hjælpefunktioner --------------------------------------------------------

# Fælles indstillinger til opskriftsvælgeren. Objektet er placeret sammen med
# hjælpefunktionerne, så serverkoden kun beskriver modulets reaktive flow.
opskrift_selectize_options <- list(
  openOnFocus = TRUE,
  closeAfterSelect = TRUE,
  highlight = TRUE,
  diacritics = TRUE,
  create = FALSE,
  dropdownParent = "body",
  sortField = "label"
)

#' Kontrollér opskriftsmodulets afhængigheder
#'
#' Funktionen stopper tidligt med en tydelig fejl, hvis hovedappen ikke har
#' leveret alle katalog-gettere eller de callbacks, som modulet behøver. Det
#' gør konfigurationsfejl lettere at finde end en senere fejl midt i en
#' brugerhandling.
#'
#' @param catalog_read Navngivet liste med katalogets getter-funktioner.
#' @param commit_catalog Funktion, der gemmer et katalog-snapshot.
#' @param varer_current Reaktiv getter til appens aktuelle varekartotek.
#'
#' @return Usynligt `TRUE`, når alle afhængigheder er gyldige. Ellers stoppes
#'   udførelsen med en fejl.
#' @keywords internal
validate_recipe_module_dependencies <- function(
  catalog_read,
  commit_catalog,
  varer_current
) {
  required_getters <- c(
    "snapshot",
    "recipes",
    "links",
    "active_retter",
    "archived_retter",
    "revision"
  )
  getters_are_valid <- is.list(catalog_read) &&
    all(required_getters %in% names(catalog_read)) &&
    all(vapply(
      catalog_read[required_getters],
      is.function,
      logical(1)
    ))

  if (!getters_are_valid) {
    stop("Opskriftsmodulet mangler katalog-gettere.", call. = FALSE)
  }
  if (!is.function(commit_catalog) || !is.function(varer_current)) {
    stop("Opskriftsmodulet mangler sine callbacks.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Pak en fejl fra en katalogændring
#'
#' Funktionen omsætter en almindelig R-fejl til den faste resultatstruktur,
#' som servermodulet kan bruge til at vise en forståelig besked uden at lukke
#' appens session.
#'
#' @param condition Fejlen, som blev udløst af en katalogfunktion.
#'
#' @return En liste, der markerer forsøget som mislykket og indeholder fejlens
#'   besked.
#' @keywords internal
recipe_failed_catalog_attempt <- function(condition) {
  list(
    ok = FALSE,
    value = NULL,
    message = conditionMessage(condition)
  )
}

#' Udfør en katalogændring sikkert fra servermodulet
#'
#' De rene katalogfunktioner stopper med en letlæselig fejl, når brugerinput
#' eller kataloget er ugyldigt. Denne hjælper fanger fejlen og returnerer altid
#' samme form, så hver klik-handler kun skal vælge mellem succes og en besked.
#'
#' @param expression Kaldet til den rene katalogfunktion.
#'
#' @return En liste med `ok`, det eventuelle resultat i `value` og en eventuel
#'   fejltekst i `message`.
#' @keywords internal
recipe_attempt_catalog_change <- function(expression) {
  tryCatch(
    list(
      ok = TRUE,
      value = force(expression),
      message = ""
    ),
    error = recipe_failed_catalog_attempt
  )
}

#' Åbn en dialog i opskriftsmodulet
#'
#' Funktionen oversætter et lokalt dialog-id til modulets fulde, namespacede
#' id og viser dialogen med den samme fade-animation overalt i modulet.
#'
#' @param id Dialogens lokale id uden modul-prefix.
#' @param ns Modulets namespace-funktion, normalt `session$ns`.
#'
#' @return Resultatet fra `show()`, usynligt.
#' @keywords internal
recipe_show_dialog <- function(id, ns) {
  show(
    id = ns(id),
    anim = TRUE,
    animType = "fade",
    asis = TRUE
  )
}

#' Luk en dialog i opskriftsmodulet
#'
#' Funktionen oversætter et lokalt dialog-id til modulets fulde, namespacede
#' id og skjuler dialogen med den samme fade-animation overalt i modulet.
#'
#' @param id Dialogens lokale id uden modul-prefix.
#' @param ns Modulets namespace-funktion, normalt `session$ns`.
#'
#' @return Resultatet fra `hide()`, usynligt.
#' @keywords internal
recipe_hide_dialog <- function(id, ns) {
  hide(
    id = ns(id),
    anim = TRUE,
    animType = "fade",
    asis = TRUE
  )
}

#' Find aktive retter med en eksisterende opskrift
#'
#' Katalogets liste over aktive retter kan i sjældne tilfælde indeholde en
#' nøgle uden en tilhørende opskrift. Funktionen fjerner sådanne rækker og
#' sorterer de resterende retter alfabetisk til visning i brugerfladen.
#'
#' @param retter_df Data frame med aktive retter og kolonnerne `key` og
#'   `retter`.
#' @param recipe_keys Tegnvektor med nøgler på opskrifter, der faktisk findes.
#'
#' @return En filtreret og alfabetisk sorteret data frame.
#' @keywords internal
recipe_active_rows <- function(retter_df, recipe_keys) {
  retter_df |>
    filter(key %in% recipe_keys) |>
    arrange(tolower(retter))
}

#' Lav valgmuligheder til opskriftsvælgeren
#'
#' Funktionen omdanner aktive retter til den navngivne vektor, som Shiny
#' forventer: brugeren ser rettens navn, mens modulet modtager opskriftens
#' stabile nøgle.
#'
#' @param retter_df Data frame med aktive retter.
#' @param recipe_keys Tegnvektor med nøgler på opskrifter, der findes.
#'
#' @return En navngivet tegnvektor med nøgler som værdier og rettenavne som
#'   labels.
#' @keywords internal
recipe_choices <- function(retter_df, recipe_keys) {
  retter_df <- recipe_active_rows(retter_df, recipe_keys)
  setNames(retter_df$key, retter_df$retter)
}

#' Formatér en ingrediens som én læsbar linje
#'
#' Mængde, enhed og ingrediens samles med enkelte mellemrum. Manglende værdier
#' fjernes, så brugeren for eksempel ser `"2 stk tomater"` og ikke tekst med
#' `NA` eller dobbelte mellemrum.
#'
#' @param maengde Mængden for en eller flere ingredienser.
#' @param enhed Enheden for en eller flere ingredienser.
#' @param ingrediens Navnet på en eller flere ingredienser.
#'
#' @return En tegnvektor med færdigformaterede ingredienslinjer.
#' @keywords internal
recipe_format_line <- function(maengde, enhed, ingrediens) {
  linje <- paste(maengde, enhed, ingrediens)
  linje <- gsub("NA", "", linje)
  trimws(gsub("\\s+", " ", linje))
}

#' Normalisér et link til en opskrift
#'
#' Tomme og manglende links bliver til en tom tekst. Links uden protokol får
#' `https://`, mens komplette HTTP- og HTTPS-links bevares. Dermed kan linket
#' bruges direkte som `href` i brugerfladen.
#'
#' @param x Linket som tekst; kun den første værdi anvendes.
#'
#' @return Et normaliseret link eller en tom tekst.
#' @keywords internal
recipe_normalize_link <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) return("")

  x <- trimws(as.character(x[[1]]))
  if (!nzchar(x)) return("")
  if (grepl("^https?://", x, ignore.case = TRUE)) return(x)
  if (grepl("^//", x)) return(paste0("https:", x))

  paste0("https://", x)
}

#' Kontrollér at en åben dialog stadig bruger den aktuelle revision
#'
#' En opskrift kan være ændret, mens en redigerings- eller slette-dialog står
#' åben. Funktionen sammenligner revisionerne og advarer brugeren i stedet for
#' at overskrive en nyere ændring.
#'
#' @param opened_revision Katalogrevisionen da dialogen blev åbnet.
#' @param current_revision Katalogets aktuelle revision.
#'
#' @return `TRUE`, hvis handlingen stadig er sikker, ellers `FALSE`.
#' @keywords internal
recipe_row_action_is_current <- function(
  opened_revision,
  current_revision
) {
  if (
    !is.null(opened_revision) &&
      identical(opened_revision, current_revision)
  ) {
    return(TRUE)
  }

  showNotification(
    paste(
      "Opskriften er ændret, siden dialogen blev åbnet.",
      "Luk dialogen og prøv igen."
    ),
    type = "warning"
  )
  FALSE
}

#' Beskriv en ingrediensrække fra kataloget
#'
#' Funktionen validerer opskriftsnøglen og rækken, henter ingrediensen og
#' returnerer den formaterede tekst, som vises i redigerings- og
#' slette-dialogerne.
#'
#' @param key Nøglen til den valgte opskrift.
#' @param row Rækkenummeret i opskriftens data frame.
#' @param recipes Navngivet liste med alle opskrifter.
#'
#' @return En læsbar ingredienslinje.
#' @keywords internal
recipe_row_context <- function(key, row, recipes) {
  req(!is.null(key), !is.null(row))
  req(key %in% names(recipes))

  df <- recipes[[key]]
  req(
    length(row) == 1,
    !is.na(row),
    row >= 1,
    row <= nrow(df)
  )

  recipe_format_line(df$maengde[row], df$enhed[row], df[[1]][row])
}

#' Byg redigeringsknappen til en ingrediensrække
#'
#' Funktionen laver en namespacet knap, der sender opskriftsnøgle og
#' rækkenummer tilbage til Shiny. Den bruges som callback i `vapply()`, så der
#' ikke defineres anonyme funktioner inde i servermodulet.
#'
#' @param row Ingrediensens rækkenummer.
#' @param key Nøglen til den valgte opskrift.
#' @param ns Modulets namespace-funktion.
#' @param event_id Det fulde input-id, som skal modtage klik-hændelsen.
#'
#' @return Knappen som HTML-tekst.
#' @keywords internal
recipe_edit_button <- function(row, key, ns, event_id) {
  as.character(
    ga_js_button(
      inputId = ns(paste0(
        "opskrift_row_btn_",
        key,
        "_",
        row
      )),
      label = NULL,
      icon = icon("pen"),
      class = "edit-btn btn btn-sm",
      onclick = sprintf(
        paste0(
          "Shiny.setInputValue(",
          "'%s', {key: '%s', row: %d}, ",
          "{priority:'event'}); return false;"
        ),
        event_id,
        key,
        row
      ),
      style = paste(
        "background:#0ea5e9;",
        "color:#fff;",
        "border:1px solid #0284c7;",
        "border-radius:8px;",
        "padding:6px 1px;",
        "line-height:1;",
        "font-weight:600;",
        "box-shadow:none;",
        "background-image:none;"
      )
    )
  )
}

#' Byg sletteknappen til en ingrediensrække
#'
#' Funktionen laver en namespacet knap, der sender opskriftsnøgle og
#' rækkenummer tilbage til Shiny, når brugeren vil slette en ingrediens. Den
#' bruges som en navngivet callback i stedet for en anonym serverfunktion.
#'
#' @param row Ingrediensens rækkenummer.
#' @param key Nøglen til den valgte opskrift.
#' @param ns Modulets namespace-funktion.
#' @param event_id Det fulde input-id, som skal modtage klik-hændelsen.
#'
#' @return Knappen som HTML-tekst.
#' @keywords internal
recipe_delete_button <- function(row, key, ns, event_id) {
  as.character(
    ga_js_button(
      inputId = ns(paste0(
        "opskrift_row_del_",
        key,
        "_",
        row
      )),
      label = NULL,
      icon = icon("trash"),
      class = "delete-btn btn btn-sm",
      onclick = sprintf(
        paste0(
          "Shiny.setInputValue(",
          "'%s', {key: '%s', row: %d}, ",
          "{priority:'event'}); return false;"
        ),
        event_id,
        key,
        row
      ),
      style = paste(
        "background:#ef4444;",
        "color:#fff;",
        "border:1px solid #dc2626;",
        "border-radius:8px;",
        "padding:6px 1px;",
        "line-height:1;",
        "font-weight:600;",
        "box-shadow:none;",
        "background-image:none;"
      )
    )
  )
}

#' Byg én række i arkivet
#'
#' Funktionen viser navnet på en arkiveret ret og bygger knapperne til at
#' gendanne eller slette retten permanent. Alle id'er og events namespac'es,
#' så rækken kan bruges sikkert inde i modulet.
#'
#' @param row Rækkenummeret i arkivets data frame.
#' @param archive Data frame med arkiverede retter.
#' @param ns Modulets namespace-funktion.
#' @param restore_event_id Det fulde input-id til gendannelse.
#' @param delete_event_id Det fulde input-id til permanent sletning.
#'
#' @return Et Shiny `div`-tag med rettens navn og to handlingsknapper.
#' @keywords internal
recipe_archive_row_ui <- function(
  row,
  archive,
  ns,
  restore_event_id,
  delete_event_id
) {
  key <- archive$key[[row]]

  tags$div(
    class = "archive-recipe-row",
    tags$span(archive$retter[[row]]),
    tags$div(
      class = "archive-recipe-actions",
      ga_js_button(
        inputId = ns(paste0("restore_ret_btn_", key)),
        label = "Gendan",
        class = paste(
          "archive-action-btn",
          "archive-action-restore"
        ),
        onclick = sprintf(
          paste0(
            "Shiny.setInputValue(",
            "'%s', '%s', {priority:'event'}); ",
            "return false;"
          ),
          restore_event_id,
          key
        )
      ),
      ga_js_button(
        inputId = ns(paste0(
          "delete_archived_ret_btn_",
          key
        )),
        label = "Slet permanent",
        class = paste(
          "archive-action-btn",
          "archive-action-delete"
        ),
        onclick = sprintf(
          paste0(
            "Shiny.setInputValue(",
            "'%s', '%s', {priority:'event'}); ",
            "return false;"
          ),
          delete_event_id,
          key
        )
      )
    )
  )
}

#' Udsend en hændelse når listen over aktive retter ændres
#'
#' Funktionen øger et reaktivt løbenummer og gemmer en samlet hændelse med
#' årsag og katalogrevision. Løbenummeret sikrer, at to ens handlinger stadig
#' opfattes som to separate hændelser af hovedappen.
#'
#' @param event_setter En `reactiveVal`, som modtager den færdige hændelse.
#' @param sequence_state En `reactiveVal` med det aktuelle løbenummer.
#' @param reason Kort tekst, for eksempel `"created"` eller `"archived"`.
#' @param revision Katalogrevisionen efter den gemte ændring.
#'
#' @return Den udsendte hændelse, usynligt.
#' @keywords internal
recipe_emit_active_change <- function(
  event_setter,
  sequence_state,
  reason,
  revision
) {
  next_sequence <- isolate(sequence_state()) + 1L
  sequence_state(next_sequence)

  event <- list(
    seq = next_sequence,
    reason = reason,
    revision = revision
  )
  event_setter(event)
  invisible(event)
}
