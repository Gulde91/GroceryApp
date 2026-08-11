# Reaktiv styring af fanen Opskrifter --------------------------------------
#
# Denne fil styrer opskriftsfanens reaktive arbejdsgange for oprettelse,
# redigering, arkivering og sletning. Rene UI- og tabelbyggere ligger i
# recipe_view.R, mens dataændringer og lagring udføres gennem
# opskriftskatalogets regler og den indsprøjtede state.

library(DT)
library(shiny)
library(shinyjs)

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

  output$opskrift_tbl_valgt <- renderDT({
    model <- selected_recipe_model()
    rows <- recipe_ingredient_table_rows(
      model$df,
      model$key,
      ns
    )
    recipe_ingredient_table_widget(rows)
  })

  output$opskrifter_ui <- renderUI({
    recipes <- catalog_read$recipes()

    recipe_overview_ui(
      active_retter = catalog_read$active_retter(),
      recipe_keys = names(recipes),
      archive = catalog_read$archived_retter(),
      selected_key = isolate(input$opskrift_valgt_key),
      ns = ns
    )
  })

  output$valgt_opskrift_ui <- renderUI({
    recipe_selected_ui(selected_recipe_model(), ns)
  })

  list(active_retter_changed = rv_activeRetterChanged)
}

# Hjælpefunktioner --------------------------------------------------------

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
