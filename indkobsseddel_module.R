library(htmltools)
library(DT)
library(shiny)
library(shinyMobile)
library(dplyr)
library(shinyjs)

#' Byg brugerfladen til fanen Indkøbsseddel
#'
#' Funktionen samler genvejene til opskrifter, varekatalog og manuel
#' indtastning med selve indkøbssedlen, gem-knappen og forslag fra tidligere
#' indkøb. Alle input- og output-id'er får modulets namespace.
#'
#' @param id Modul-id'et, som også bruges ved kaldet til servermodulet.
#'
#' @return En Shiny-tagliste, der kan indsættes direkte i fanen
#'   Indkøbsseddel.
#' @keywords internal
mod_indkobsseddel_ui <- function(id) {
  ns <- NS(id)

  tagList(
    f7BlockTitle(title = "Vælg varer"),
    f7Block(
      inset = TRUE,
      strong = TRUE,
      tags$div(
        class = "ga-actions-grid",
        f7Button(
          ns("open_recipe"),
          "Opskrift",
          icon = f7Icon("book"),
          fill = FALSE,
          color = "green"
        ),
        f7Button(
          ns("open_catalog"),
          "Liste",
          icon = f7Icon("square_list"),
          fill = FALSE,
          color = "green"
        ),
        f7Button(
          ns("open_manual"),
          "Manuel",
          icon = f7Icon("hand_draw"),
          fill = FALSE,
          color = "green"
        )
      )
    ),
    br(),
    DTOutput(ns("cart_table")),
    f7Block(
      tags$div(
        class = "ga-save-cart",
        f7Button(
          ns("save_history"),
          "Gem indkøbssedlen til database",
          fill = TRUE,
          color = "blue"
        )
      )
    ),
    tags$h5(tags$strong("Forslag til manglende varer:")),
    tableOutput(ns("history_suggestions"))
  )
}

#' Byg dialogerne til fanen Indkøbsseddel
#'
#' Dialogerne har kun tomme startvalg. Servermodulet fylder dem med de
#' aktuelle reaktive varer og opskrifter, når brugeren åbner en dialog.
#' Dermed bliver nye varer og opskrifter synlige uden at genstarte appen.
#'
#' @param id Modul-id'et, som også bruges ved kaldet til servermodulet.
#' @param kategori_1 Valgfrie hovedkategorier til den første visning. Det
#'   aktuelle varekatalog overtager valgene, når servermodulet starter.
#' @param kategori_2 Valgfrie underkategorier til den første visning. Det
#'   aktuelle varekatalog overtager valgene, når servermodulet starter.
#' @param salat_choices Navnene på de salater, der kan vælges.
#' @param tilbehor_choices Navnene på det tilbehør, der kan vælges.
#'
#' @return En Shiny-tagliste med dialoger til opskrift, katalogvare, manuel
#'   vare og redigering af en linje.
#' @keywords internal
mod_indkobsseddel_dialogs_ui <- function(
  id,
  kategori_1 = character(),
  kategori_2 = character(),
  salat_choices = character(),
  tilbehor_choices = character()
) {
  ns <- NS(id)
  kategori_1 <- indkobsseddel_choice_values(
    kategori_1,
    defaults = "konserves",
    include_blank = FALSE
  )
  kategori_2 <- indkobsseddel_choice_values(
    kategori_2,
    defaults = "",
    include_blank = TRUE
  )

  tagList(
    tags$div(
      id = ns("catalog_dialog"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Tilføj varer fra liste"),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          selectizeInput(
            ns("catalog_item"),
            "Tilføj varer fra liste",
            choices = character(),
            width = "100%"
          ),
          br(),
          numericInput(
            ns("catalog_amount"),
            "Mængde",
            value = 1,
            width = "100%"
          ),
          br(),
          selectInput(
            ns("catalog_unit"),
            "Enhed",
            choices = character(),
            width = "100%"
          ),
          br(),
          f7Button(
            ns("add_catalog_item"),
            "Tilføj til indkøbssedlen",
            fill = TRUE,
            color = "green"
          ),
          br(),
          f7Button(
            ns("close_catalog"),
            "Luk",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    ),
    tags$div(
      id = ns("manual_dialog"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Tilføj vare manuelt"),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          textInput(
            ns("manual_name"),
            "Tilføj varer manuelt",
            width = "100%"
          ),
          br(),
          numericInput(
            ns("manual_amount"),
            "Mængde",
            value = 1,
            width = "100%"
          ),
          br(),
          selectInput(
            ns("manual_unit"),
            "Enhed",
            choices = character(),
            width = "100%"
          ),
          br(),
          selectInput(
            ns("manual_category_1"),
            "Kategori 1",
            choices = kategori_1,
            selected = indkobsseddel_preferred_choice(
              kategori_1,
              "konserves"
            ),
            width = "100%"
          ),
          br(),
          selectInput(
            ns("manual_category_2"),
            "Kategori 2",
            choices = kategori_2,
            selected = "",
            width = "100%"
          ),
          br(),
          f7Button(
            ns("add_manual_item"),
            "Tilføj til indkøbssedlen",
            fill = TRUE,
            color = "green"
          ),
          br(),
          f7Button(
            ns("close_manual"),
            "Luk",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    ),
    tags$div(
      id = ns("recipe_dialog"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Tilføj fra opskrift"),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          selectInput(
            ns("recipe_name"),
            "Vælg ret",
            choices = "",
            width = "100%"
          ),
          br(),
          numericInput(
            ns("recipe_persons"),
            "Vælg antal personer",
            value = 2,
            width = "100%"
          ),
          br(),
          selectInput(
            ns("salad_name"),
            "Vælg salat",
            choices = indkobsseddel_choice_values(
              salat_choices,
              defaults = "",
              include_blank = TRUE
            ),
            width = "100%"
          ),
          br(),
          selectInput(
            ns("accessory_name"),
            "Vælg tilbehør",
            choices = indkobsseddel_choice_values(
              tilbehor_choices,
              defaults = "",
              include_blank = TRUE
            ),
            width = "100%"
          ),
          br(),
          f7Button(
            ns("add_recipe"),
            "Tilføj til indkøbssedlen",
            fill = TRUE,
            color = "green"
          ),
          br(),
          f7Button(
            ns("close_recipe"),
            "Luk",
            fill = TRUE,
            color = "gray"
          )
        ),
        DTOutput(ns("recipe_preview"))
      )
    ),
    tags$div(
      id = ns("edit_dialog"),
      class = "ga-modal ga-edit-overlay",
      tags$div(
        class = "ga-dialog ga-edit-dialog",
        tags$h3("Redigér tekst"),
        tags$div(
          class = "ga-edit-input",
          textInput(
            ns("edit_value"),
            label = NULL,
            value = "",
            width = "100%"
          )
        ),
        tags$div(
          class = "ga-actions ga-edit-actions",
          actionButton(
            ns("cancel_edit"),
            "Annullér",
            class = "btn-flat"
          ),
          actionButton(
            ns("confirm_edit"),
            "Gem",
            class = "btn-flat btn-save"
          )
        )
      )
    )
  )
}

#' Kør serverlogikken til fanen Indkøbsseddel
#'
#' Modulet ejer hele indkøbssedlens skrivbare state i én intern
#' `rv_cart`. Root leverer kun læsefunktioner til vare- og
#' opskriftskataloger samt callbacks til historiske indkøbssedler. Derfor
#' kan kode uden for modulet ikke ændre carten direkte.
#'
#' `recipe_read` skal være en liste med funktionerne `recipes`,
#' `active_retter`, `links`, `salater`, `salater_opskrifter` og `tilbehor`.
#' De kaldes først inde i reaktive udtryk, så ændringer i katalogerne slår
#' igennem med det samme. De tre sidste getters må gerne lukke over statiske
#' tabeller og lister.
#'
#' `popular_items` skal returnere tidligere anvendte varenavne som
#' en tegnvektor eller en data frame med kolonnen `Indkøbsliste` eller
#' `Indkobsliste`. `save_cart` modtager en data frame med kolonnen
#' `Indkøbsliste` og skal returnere `TRUE` efter en vellykket gemning.
#'
#' @param input Modulets namespacede Shiny-input.
#' @param output Modulets namespacede Shiny-output.
#' @param session Modulets Shiny-session.
#' @param varer_current Reaktiv getter med det samlede, aktuelle varekatalog.
#' @param recipe_read Liste med read-only getters til opskrifter, aktive
#'   retter, links, salater, salatopskrifter og tilbehør.
#' @param save_cart Callback, der gemmer den færdige historik-data frame og
#'   returnerer `TRUE` ved succes.
#' @param popular_items Getter med varenavne fra tidligere
#'   indkøbssedler, sorteret efter relevans.
#'
#' @return En liste med tre read-only reaktive getters: `cart_current`,
#'   `visible_rows` og `copy_payload`.
#' @keywords internal
mod_indkobsseddel_server <- function(
  input,
  output,
  session,
  recipe_read,
  varer_current,
  save_cart,
  popular_items
) {
  stopifnot(is.function(varer_current))
  stopifnot(is.function(save_cart))
  stopifnot(is.function(popular_items))
  indkobsseddel_validate_recipe_read(recipe_read)

  ns <- session$ns
  rv_cart <- reactiveVal(new_cart_state())
  rv_edit_line_id <- reactiveVal(NULL)

  cart_current <- reactive({
    rv_cart()
  })

  visible_rows <- reactive({
    cart_visible(rv_cart())
  })

  copy_payload <- reactive({
    cart_copy_payload(rv_cart())
  })

  recipe_selection <- reactive({
    persons <- indkobsseddel_person_count(input$recipe_persons)

    indkobsseddel_prepare_recipe(
      recipes = recipe_read$recipes(),
      active_retter = recipe_read$active_retter(),
      links = recipe_read$links(),
      salater = recipe_read$salater(),
      salater_opskrifter = recipe_read$salater_opskrifter(),
      tilbehor = recipe_read$tilbehor(),
      selected_recipe = indkobsseddel_clean_text(input$recipe_name),
      selected_salad = indkobsseddel_clean_text(input$salad_name),
      persons = persons,
      selected_accessory = indkobsseddel_clean_text(input$accessory_name)
    )
  })

  observe({
    varer <- varer_current()
    choices <- indkobsseddel_item_names(varer)
    selected <- isolate(indkobsseddel_clean_text(input$catalog_item))
    if (!selected %in% choices) selected <- character()

    updateSelectizeInput(
      session,
      "catalog_item",
      choices = choices,
      selected = selected,
      server = TRUE
    )
  })

  observe({
    varer <- varer_current()
    selected_name <- indkobsseddel_clean_text(input$catalog_item)
    selected_row <- indkobsseddel_find_item(varer, selected_name)
    selected_unit <- if (nrow(selected_row) == 1L) {
      indkobsseddel_clean_text(selected_row$enhed[[1]])
    } else {
      ""
    }
    units <- indkobsseddel_choice_values(
      varer$enhed,
      defaults = "stk",
      include_blank = FALSE
    )

    updateSelectInput(
      session,
      "catalog_unit",
      choices = units,
      selected = selected_unit
    )
  })

  observe({
    varer <- varer_current()
    units <- indkobsseddel_choice_values(
      varer$enhed,
      defaults = "stk",
      include_blank = FALSE
    )
    selected_name <- indkobsseddel_clean_text(input$manual_name)
    selected_row <- indkobsseddel_find_item(varer, selected_name)
    selected_unit <- if (nrow(selected_row) == 1L) {
      indkobsseddel_clean_text(selected_row$enhed[[1L]])
    } else {
      isolate(indkobsseddel_clean_text(input$manual_unit))
    }
    if (!selected_unit %in% units) {
      selected_unit <- indkobsseddel_preferred_choice(units, "stk")
    }

    updateSelectInput(
      session,
      "manual_unit",
      choices = units,
      selected = selected_unit
    )
  })

  observe({
    categories <- indkobsseddel_manual_category_choices(varer_current())
    selected_category_1 <- isolate(
      indkobsseddel_preserved_choice(
        input$manual_category_1,
        categories$category_1,
        preferred = "konserves"
      )
    )
    selected_category_2 <- isolate(
      indkobsseddel_preserved_choice(
        input$manual_category_2,
        categories$category_2,
        preferred = ""
      )
    )

    updateSelectInput(
      session,
      "manual_category_1",
      choices = categories$category_1,
      selected = selected_category_1
    )
    updateSelectInput(
      session,
      "manual_category_2",
      choices = categories$category_2,
      selected = selected_category_2
    )
  })

  observeEvent(input$open_recipe, {
    dialog_values <- indkobsseddel_recipe_dialog_values(
      recipes = recipe_read$recipes(),
      active_retter = recipe_read$active_retter(),
      salater = recipe_read$salater(),
      tilbehor = recipe_read$tilbehor()
    )

    updateSelectInput(
      session,
      "recipe_name",
      choices = dialog_values$recipe_choices,
      selected = dialog_values$selected_recipe
    )
    updateNumericInput(
      session,
      "recipe_persons",
      value = dialog_values$persons
    )
    updateSelectInput(
      session,
      "salad_name",
      choices = dialog_values$salad_choices,
      selected = dialog_values$selected_salad
    )
    updateSelectInput(
      session,
      "accessory_name",
      choices = dialog_values$accessory_choices,
      selected = dialog_values$selected_accessory
    )
    indkobsseddel_show_dialog("recipe_dialog", ns)
  })

  observeEvent(input$close_recipe, {
    indkobsseddel_hide_dialog("recipe_dialog", ns)
  })

  observeEvent(input$add_recipe, {
    persons <- indkobsseddel_person_count(input$recipe_persons)
    if (is.na(persons)) {
      showNotification(
        "Antallet af personer skal være et tal større end 0.",
        type = "warning"
      )
      return(invisible(NULL))
    }

    selection <- recipe_selection()
    if (nrow(selection$rows) == 0L) {
      showNotification(
        "Vælg mindst én opskrift, salat eller ét tilbehør.",
        type = "warning"
      )
      return(invisible(NULL))
    }

    rv_cart(
      cart_add_recipe(
        rv_cart(),
        selection$rows,
        selection$sections
      )
    )
    indkobsseddel_hide_dialog("recipe_dialog", ns)
  })

  observeEvent(input$open_catalog, {
    updateSelectizeInput(
      session,
      "catalog_item",
      selected = character()
    )
    updateNumericInput(
      session,
      "catalog_amount",
      value = 1
    )
    updateSelectInput(
      session,
      "catalog_unit",
      selected = "stk"
    )
    indkobsseddel_show_dialog("catalog_dialog", ns)
  })

  observeEvent(input$close_catalog, {
    indkobsseddel_hide_dialog("catalog_dialog", ns)
  })

  observeEvent(input$add_catalog_item, {
    selected_name <- indkobsseddel_clean_text(input$catalog_item)
    if (!nzchar(selected_name)) {
      showNotification(
        "Vælg en vare fra listen.",
        type = "warning"
      )
      return(invisible(NULL))
    }

    selected_row <- indkobsseddel_find_item(
      varer_current(),
      selected_name
    )
    if (nrow(selected_row) != 1L) {
      showNotification(
        "Varen kunne ikke findes entydigt. Vælg den igen.",
        type = "warning"
      )
      return(invisible(NULL))
    }

    amount <- indkobsseddel_positive_number(input$catalog_amount)
    if (is.na(amount)) {
      showNotification(
        "Mængden skal være et tal større end 0.",
        type = "warning"
      )
      return(invisible(NULL))
    }

    unit <- indkobsseddel_clean_text(input$catalog_unit)
    if (!nzchar(unit)) {
      showNotification(
        "Vælg en enhed, før varen tilføjes.",
        type = "warning"
      )
      return(invisible(NULL))
    }

    base_amount <- suppressWarnings(
      as.numeric(selected_row$maengde[[1]])
    )
    if (
      length(base_amount) != 1L ||
        is.na(base_amount) ||
        !is.finite(base_amount)
    ) {
      showNotification(
        "Varen har ikke en gyldig standardmængde.",
        type = "warning"
      )
      return(invisible(NULL))
    }

    selected_row$maengde <- base_amount * amount
    selected_row$enhed <- unit
    rv_cart(cart_add_rows(rv_cart(), selected_row))
    indkobsseddel_hide_dialog("catalog_dialog", ns)
  })

  observeEvent(input$open_manual, {
    varer <- varer_current()
    units <- indkobsseddel_choice_values(
      varer$enhed,
      defaults = "stk",
      include_blank = FALSE
    )

    updateTextInput(
      session,
      "manual_name",
      value = ""
    )
    updateNumericInput(
      session,
      "manual_amount",
      value = 1
    )
    updateSelectInput(
      session,
      "manual_unit",
      choices = units,
      selected = indkobsseddel_preferred_choice(units, "stk")
    )
    indkobsseddel_show_dialog("manual_dialog", ns)
  })

  observeEvent(input$close_manual, {
    indkobsseddel_hide_dialog("manual_dialog", ns)
  })

  observeEvent(input$add_manual_item, {
    name <- indkobsseddel_clean_text(input$manual_name)
    amount <- indkobsseddel_positive_number(input$manual_amount)
    unit <- indkobsseddel_clean_text(input$manual_unit)

    if (!nzchar(name)) {
      showNotification(
        "Skriv et varenavn.",
        type = "warning"
      )
      return(invisible(NULL))
    }
    if (is.na(amount)) {
      showNotification(
        "Mængden skal være et tal større end 0.",
        type = "warning"
      )
      return(invisible(NULL))
    }
    if (!nzchar(unit)) {
      showNotification(
        "Vælg en enhed.",
        type = "warning"
      )
      return(invisible(NULL))
    }

    new_row <- data.frame(
      Indkobsliste = name,
      maengde = amount,
      enhed = unit,
      kat_1 = indkobsseddel_clean_text(input$manual_category_1),
      kat_2 = indkobsseddel_clean_text(input$manual_category_2),
      stringsAsFactors = FALSE
    )
    rv_cart(cart_add_rows(rv_cart(), new_row))
    indkobsseddel_hide_dialog("manual_dialog", ns)
  })

  observeEvent(input$delete_pressed, {
    line_id <- indkobsseddel_clean_text(input$delete_pressed)
    if (!nzchar(line_id)) return(invisible(NULL))

    rv_cart(cart_delete_line(rv_cart(), line_id))
  })

  observeEvent(input$edit_pressed, ignoreInit = TRUE, {
    line_id <- indkobsseddel_clean_text(input$edit_pressed)
    if (!nzchar(line_id)) return(invisible(NULL))

    rows <- visible_rows()
    row_index <- match(line_id, rows$line_id)
    if (is.na(row_index)) {
      showNotification(
        "Linjen findes ikke længere på indkøbssedlen.",
        type = "warning"
      )
      return(invisible(NULL))
    }

    rv_edit_line_id(line_id)
    updateTextInput(
      session,
      "edit_value",
      value = rows$display[[row_index]]
    )
    indkobsseddel_show_dialog("edit_dialog", ns)
  })

  observeEvent(input$confirm_edit, {
    line_id <- rv_edit_line_id()
    if (is.null(line_id)) return(invisible(NULL))

    value <- indkobsseddel_clean_text(input$edit_value)
    if (!nzchar(value)) {
      showNotification(
        "Teksten på indkøbssedlen må ikke være tom.",
        type = "warning"
      )
      return(invisible(NULL))
    }

    rv_cart(cart_edit_line(rv_cart(), line_id, value))
    rv_edit_line_id(NULL)
    indkobsseddel_hide_dialog("edit_dialog", ns)
  })

  observeEvent(input$cancel_edit, {
    rv_edit_line_id(NULL)
    indkobsseddel_hide_dialog("edit_dialog", ns)
  })

  observeEvent(input$save_history, {
    history_df <- indkobsseddel_history_frame(copy_payload())
    result <- indkobsseddel_try_save_history(
      save_cart,
      history_df
    )

    if (!isTRUE(result$ok)) {
      message <- result$error
      if (is.null(message) || !nzchar(message)) {
        message <- "Indkøbssedlen kunne ikke gemmes."
      }
      showNotification(
        message,
        type = "error",
        duration = NULL
      )
      return(invisible(NULL))
    }

    session$sendCustomMessage(
      "show_toast",
      list(
        text = "Indkøbsseddel gemt ✔",
        tone = "blue"
      )
    )
  })

  output$recipe_preview <- renderDT({
    selection <- recipe_selection()
    req(nrow(selection$rows) > 0L)
    indkobsseddel_recipe_preview_widget(selection$rows)
  })

  output$cart_table <- renderDT(
    {
      indkobsseddel_cart_widget(copy_payload(), ns)
    },
    server = FALSE
  )

  output$history_suggestions <- renderTable(
    {
      rows <- visible_rows()
      req(nrow(rows) > 0L)
      varer <- varer_current()
      units <- if (
        is.data.frame(varer) &&
          "enhed" %in% names(varer)
      ) {
        varer$enhed
      } else {
        character()
      }

      indkobsseddel_history_suggestions(
        popular_items(),
        rows,
        units = units,
        limit = 10L
      )
    },
    colnames = FALSE
  )

  list(
    cart_current = cart_current,
    visible_rows = visible_rows,
    copy_payload = copy_payload
  )
}

#' Kontrollér opskriftskatalogets read-only API
#'
#' @param recipe_read Liste med katalogernes read-only getter-funktioner.
#'
#' @return `TRUE` usynligt. Funktionen stopper med en tydelig fejl, hvis en
#'   getter mangler.
#' @keywords internal
indkobsseddel_validate_recipe_read <- function(recipe_read) {
  required <- c(
    "recipes",
    "active_retter",
    "links",
    "salater",
    "salater_opskrifter",
    "tilbehor"
  )

  if (
    !is.list(recipe_read) ||
      !all(required %in% names(recipe_read))
  ) {
    stop(
      paste(
        "recipe_read skal indeholde funktionerne:",
        paste(required, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  valid <- vapply(
    recipe_read[required],
    is.function,
    logical(1)
  )
  if (!all(valid)) {
    stop(
      "Alle værdier i recipe_read skal være funktioner.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Rens én tekstværdi fra et input
#'
#' @param value En vilkårlig værdi, typisk fra et Shiny-input.
#'
#' @return Første element som trimmet tekst eller `""`.
#' @keywords internal
indkobsseddel_clean_text <- function(value) {
  if (
    is.null(value) ||
      length(value) == 0L ||
      is.na(value[[1]])
  ) {
    return("")
  }

  trimws(as.character(value[[1]]))
}

#' Læs et positivt tal fra et input
#'
#' @param value En vilkårlig værdi, typisk fra et numeric input.
#'
#' @return Et endeligt tal større end nul eller `NA_real_`.
#' @keywords internal
indkobsseddel_positive_number <- function(value) {
  if (is.null(value) || length(value) != 1L) return(NA_real_)

  result <- suppressWarnings(as.numeric(value))
  if (
    length(result) != 1L ||
      is.na(result) ||
      !is.finite(result) ||
      result <= 0
  ) {
    return(NA_real_)
  }

  result
}

#' Læs antal personer med appens sikre standardværdi
#'
#' Et endnu ikke initialiseret input svarer til standardværdien 2. En
#' udtrykkeligt ugyldig værdi, eksempelvis 0 eller negativ tekst, afvises
#' fortsat som `NA_real_`.
#'
#' @param value Værdien fra opskriftsdialogens personfelt.
#'
#' @return Et positivt antal eller `NA_real_`.
#' @keywords internal
indkobsseddel_person_count <- function(value) {
  if (is.null(value) || length(value) == 0L) return(2)
  indkobsseddel_positive_number(value)
}

#' Opret en tom tabel med cartens fem datakolonner
#'
#' @return En tom data frame med navn, mængde, enhed og kategorier.
#' @keywords internal
indkobsseddel_empty_rows <- function() {
  data.frame(
    Indkobsliste = character(),
    maengde = numeric(),
    enhed = character(),
    kat_1 = character(),
    kat_2 = character(),
    stringsAsFactors = FALSE
  )
}

#' Gør opskriftsrækker klar til carten
#'
#' Opskriftsfiler bruger rettens navn som første kolonnenavn. Funktionen
#' omdøber denne kolonne og sikrer en ensartet femkolonnestruktur.
#'
#' @param rows Data frame med ingrediensrækker.
#' @param label Læsevenligt navn til eventuelle fejlbeskeder.
#'
#' @return En data frame med cartens fem inputkolonner.
#' @keywords internal
indkobsseddel_as_cart_rows <- function(rows, label = "Data") {
  expected_tail <- c("maengde", "enhed", "kat_1", "kat_2")

  if (is.null(rows) || nrow(rows) == 0L) {
    return(indkobsseddel_empty_rows())
  }
  if (
    !is.data.frame(rows) ||
      ncol(rows) != 5L ||
      !identical(names(rows)[2:5], expected_tail)
  ) {
    stop(
      paste(
        label,
        "skal have en varekolonne efterfulgt af",
        "maengde, enhed, kat_1 og kat_2."
      ),
      call. = FALSE
    )
  }

  result <- as.data.frame(rows, stringsAsFactors = FALSE)
  names(result)[1] <- "Indkobsliste"
  result$maengde <- suppressWarnings(as.numeric(result$maengde))

  character_columns <- c(
    "Indkobsliste",
    "enhed",
    "kat_1",
    "kat_2"
  )
  for (column in character_columns) {
    result[[column]] <- as.character(result[[column]])
    result[[column]][is.na(result[[column]])] <- ""
  }

  result$Indkobsliste <- trimws(result$Indkobsliste)
  result <- result[nzchar(result$Indkobsliste), , drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Hent og skalér én opskrift
#'
#' @param recipes Navngivet liste med opskrifter.
#' @param index Data frame med kolonnerne `retter` og `key`.
#' @param selected_name Rettens viste navn.
#' @param persons Antal personer.
#' @param label Beskrivelse til en eventuel valideringsfejl.
#'
#' @return Opskriftens ingredienser i cart-format. Hvis intet gyldigt navn er
#'   valgt, returneres en tom tabel.
#' @keywords internal
indkobsseddel_scaled_recipe <- function(
  recipes,
  index,
  selected_name,
  persons,
  label
) {
  selected_name <- indkobsseddel_clean_text(selected_name)
  if (!nzchar(selected_name)) return(indkobsseddel_empty_rows())
  if (
    !is.list(recipes) ||
      !is.data.frame(index) ||
      !all(c("retter", "key") %in% names(index))
  ) {
    return(indkobsseddel_empty_rows())
  }

  matches <- which(
    as.character(index$retter) == selected_name &
      as.character(index$key) %in% names(recipes)
  )
  if (length(matches) != 1L) return(indkobsseddel_empty_rows())

  key <- as.character(index$key[[matches[[1]]]])
  result <- indkobsseddel_as_cart_rows(recipes[[key]], label)
  result$maengde <- result$maengde * persons
  result
}

#' Hent og skalér et valgt tilbehør
#'
#' @param tilbehor Data frame med tilbehørsvarer.
#' @param selected_name Navnet på det valgte tilbehør.
#' @param persons Antal personer.
#'
#' @return Tilbehøret i cart-format eller en tom tabel.
#' @keywords internal
indkobsseddel_scaled_accessory <- function(
  tilbehor,
  selected_name,
  persons
) {
  selected_name <- indkobsseddel_clean_text(selected_name)
  if (
    !nzchar(selected_name) ||
      !is.data.frame(tilbehor) ||
      !"Indkobsliste" %in% names(tilbehor)
  ) {
    return(indkobsseddel_empty_rows())
  }

  rows <- tilbehor[
    as.character(tilbehor$Indkobsliste) == selected_name,
    ,
    drop = FALSE
  ]
  rows <- indkobsseddel_as_cart_rows(rows, "Tilbehør")
  rows$maengde <- round(rows$maengde * persons, 4)
  rows
}

#' Find et entydigt link til en opskrift
#'
#' @param links Data frame med kolonnerne `ret` og `link`.
#' @param name Rettens viste navn.
#'
#' @return Linket som tekst eller `NULL`, hvis der ikke er præcis ét match.
#' @keywords internal
indkobsseddel_recipe_link <- function(links, name) {
  name <- indkobsseddel_clean_text(name)
  if (
    !nzchar(name) ||
      !is.data.frame(links) ||
      !all(c("ret", "link") %in% names(links))
  ) {
    return(NULL)
  }

  matches <- links$link[as.character(links$ret) == name]
  matches <- as.character(matches)
  matches <- matches[!is.na(matches) & nzchar(matches)]
  if (length(matches) != 1L) return(NULL)

  matches[[1]]
}

#' Saml en opskriftsdialog til cart-rækker og kopinoter
#'
#' Funktionen beregner de synlige ingredienser og de skjulte opskriftsafsnit
#' fra det samme snapshot. Derfor kan preview og tilføjelse ikke komme ud af
#' takt, når et reaktivt katalog ændres.
#'
#' @param recipes Navngivet liste med almindelige opskrifter.
#' @param active_retter Data frame med aktive retter.
#' @param links Data frame med opskriftslinks.
#' @param salater Data frame med salatnavne og -nøgler.
#' @param salater_opskrifter Navngivet liste med salatopskrifter.
#' @param tilbehor Data frame med tilbehør.
#' @param selected_recipe Valgt almindelig ret.
#' @param selected_salad Valgt salat.
#' @param persons Antal personer.
#' @param selected_accessory Valgt tilbehør.
#'
#' @return En liste med `rows` til carten og `sections` til kopiteksten.
#' @keywords internal
indkobsseddel_prepare_recipe <- function(
  recipes,
  active_retter,
  links,
  salater,
  salater_opskrifter,
  tilbehor,
  selected_recipe,
  selected_salad,
  persons,
  selected_accessory
) {
  persons <- indkobsseddel_positive_number(persons)
  if (is.na(persons)) {
    return(list(
      rows = indkobsseddel_empty_rows(),
      sections = list()
    ))
  }

  selected_recipe <- indkobsseddel_clean_text(selected_recipe)
  selected_salad <- indkobsseddel_clean_text(selected_salad)
  selected_accessory <- indkobsseddel_clean_text(selected_accessory)

  recipe_rows <- indkobsseddel_scaled_recipe(
    recipes,
    active_retter,
    selected_recipe,
    persons,
    "Opskriften"
  )
  salad_rows <- indkobsseddel_scaled_recipe(
    salater_opskrifter,
    salater,
    selected_salad,
    persons,
    "Salaten"
  )
  accessory_rows <- indkobsseddel_scaled_accessory(
    tilbehor,
    selected_accessory,
    persons
  )
  rows <- bind_rows(recipe_rows, accessory_rows, salad_rows)
  sections <- list()

  if (nrow(recipe_rows) > 0L) {
    section_rows <- recipe_rows
    title <- selected_recipe
    link <- indkobsseddel_recipe_link(links, selected_recipe)

    if (nrow(salad_rows) > 0L) {
      section_rows <- bind_rows(recipe_rows, salad_rows)
      title <- paste0(selected_recipe, " m. ", selected_salad)
      if (is.null(link)) {
        link <- indkobsseddel_recipe_link(links, selected_salad)
      }
    }

    sections[[length(sections) + 1L]] <- list(
      title = title,
      pers = persons,
      df = section_rows,
      link = link
    )
  } else {
    if (nrow(salad_rows) > 0L) {
      sections[[length(sections) + 1L]] <- list(
        title = paste0("Salat: ", selected_salad),
        pers = persons,
        df = salad_rows,
        link = indkobsseddel_recipe_link(links, selected_salad)
      )
    }
    if (nrow(accessory_rows) > 0L) {
      sections[[length(sections) + 1L]] <- list(
        title = paste0("Tilbehør: ", selected_accessory),
        pers = persons,
        df = accessory_rows,
        link = NA_character_
      )
    }
  }

  list(rows = rows, sections = sections)
}

#' Filtrér aktive retter til opskrifter, der faktisk findes
#'
#' @param active_retter Data frame med aktive retter.
#' @param recipe_keys Navnene på de indlæste opskrifter.
#'
#' @return En alfabetisk sorteret data frame med gyldige aktive retter.
#' @keywords internal
indkobsseddel_active_recipe_rows <- function(
  active_retter,
  recipe_keys
) {
  if (
    !is.data.frame(active_retter) ||
      !all(c("retter", "key") %in% names(active_retter))
  ) {
    return(data.frame(
      retter = character(),
      key = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- active_retter[
    as.character(active_retter$key) %in% recipe_keys,
    ,
    drop = FALSE
  ]
  rows <- rows[
    order(tolower(as.character(rows$retter))),
    ,
    drop = FALSE
  ]
  rownames(rows) <- NULL
  rows
}

#' Hent valgmuligheder fra én navngivet kolonne
#'
#' @param data Data frame med valgmuligheder.
#' @param column Navnet på kolonnen.
#' @param include_blank Om et tomt valg skal stå først.
#'
#' @return En unik tegnvektor med valgmuligheder.
#' @keywords internal
indkobsseddel_named_choices <- function(
  data,
  column,
  include_blank = FALSE
) {
  if (!is.data.frame(data) || !column %in% names(data)) {
    return(if (isTRUE(include_blank)) "" else character())
  }

  values <- as.character(data[[column]])
  values <- trimws(values[!is.na(values)])
  values <- unique(values[nzchar(values)])
  values <- sort(values)
  if (isTRUE(include_blank)) values <- c("", values)
  values
}

#' Byg de aktuelle valg og standarder til opskriftsdialogen
#'
#' Funktionen samler dialogens værdier hver gang den åbnes. Derfor bliver
#' opskrifter, salater og tilbehør, som er ændret efter appstart, vist med det
#' samme, mens alle frivillige valg fortsat starter tomme.
#'
#' @param recipes Navngivet liste med de aktuelle opskrifter.
#' @param active_retter Data frame med de aktive opskrifter.
#' @param salater Data frame med de aktuelle salater.
#' @param tilbehor Data frame med det aktuelle tilbehør.
#'
#' @return En liste med valgmuligheder og dialogens sikre standardværdier.
#' @keywords internal
indkobsseddel_recipe_dialog_values <- function(
  recipes,
  active_retter,
  salater,
  tilbehor
) {
  active <- indkobsseddel_active_recipe_rows(
    active_retter,
    names(recipes)
  )

  list(
    recipe_choices = c("", as.character(active$retter)),
    selected_recipe = "",
    persons = 2,
    salad_choices = indkobsseddel_named_choices(
      salater,
      "retter",
      include_blank = TRUE
    ),
    selected_salad = "",
    accessory_choices = indkobsseddel_named_choices(
      tilbehor,
      "Indkobsliste",
      include_blank = TRUE
    ),
    selected_accessory = ""
  )
}

#' Saml og sortér generelle valgmuligheder
#'
#' @param values Værdier fra et aktuelt katalog.
#' @param defaults Standardværdier, som altid bør tilbydes.
#' @param include_blank Om et tomt valg skal medtages.
#'
#' @return En sorteret tegnvektor uden dubletter.
#' @keywords internal
indkobsseddel_choice_values <- function(
  values,
  defaults = character(),
  include_blank = FALSE
) {
  result <- as.character(c(defaults, values))
  result <- trimws(result[!is.na(result)])
  result <- unique(result)
  if (!isTRUE(include_blank)) result <- result[nzchar(result)]
  result <- sort(result)

  if (isTRUE(include_blank)) {
    result <- c("", result[nzchar(result)])
  }
  unique(result)
}

#' Hent kategorier sikkert fra det aktuelle varekatalog
#'
#' Funktionen samler hoved- og underkategorier fra varekataloget. Mangler
#' kataloget eller en af kolonnerne, returneres stadig brugbare standardvalg,
#' så dialogen til manuel indtastning ikke ender med et tomt select-input.
#'
#' @param varer Det aktuelle varekatalog, normalt en data frame med kolonnerne
#'   `kat_1` og `kat_2`.
#'
#' @return En liste med de to tegnvektorer `category_1` og `category_2`.
#' @keywords internal
indkobsseddel_manual_category_choices <- function(varer) {
  category_1 <- if (
    is.data.frame(varer) &&
      "kat_1" %in% names(varer)
  ) {
    varer$kat_1
  } else {
    character()
  }
  category_2 <- if (
    is.data.frame(varer) &&
      "kat_2" %in% names(varer)
  ) {
    varer$kat_2
  } else {
    character()
  }

  list(
    category_1 = indkobsseddel_choice_values(
      category_1,
      defaults = "konserves",
      include_blank = FALSE
    ),
    category_2 = indkobsseddel_choice_values(
      category_2,
      defaults = "",
      include_blank = TRUE
    )
  )
}

#' Bevar et gyldigt valg ved opdatering af valgmuligheder
#'
#' Et eksisterende valg beholdes, hvis det stadig findes i det aktuelle
#' katalog. Er valget forsvundet eller endnu ikke sat, bruges den foretrukne
#' standard og derefter det første tilgængelige valg.
#'
#' @param current Det aktuelle inputvalg.
#' @param choices De nye valgmuligheder.
#' @param preferred Den foretrukne standardværdi.
#'
#' @return En enkelt tegnværdi, der findes blandt `choices`, eller `""`.
#' @keywords internal
indkobsseddel_preserved_choice <- function(
  current,
  choices,
  preferred = ""
) {
  current <- indkobsseddel_clean_text(current)
  choices <- as.character(choices)
  if (current %in% choices) return(current)

  indkobsseddel_preferred_choice(choices, preferred)
}

#' Vælg en foretrukken værdi med sikker fallback
#'
#' @param choices De aktuelle valgmuligheder.
#' @param preferred Den ønskede standardværdi.
#'
#' @return Standardværdien, hvis den findes, ellers første valg eller `""`.
#' @keywords internal
indkobsseddel_preferred_choice <- function(
  choices,
  preferred
) {
  choices <- as.character(choices)
  preferred <- indkobsseddel_clean_text(preferred)
  if (preferred %in% choices) return(preferred)
  if (length(choices) > 0L) return(choices[[1]])
  ""
}

#' Kontrollér varekatalogets grundstruktur
#'
#' @param varer Det samlede varekatalog.
#'
#' @return `TRUE`, når de fem forventede kolonner findes.
#' @keywords internal
indkobsseddel_has_item_columns <- function(varer) {
  required <- c(
    "Indkobsliste",
    "maengde",
    "enhed",
    "kat_1",
    "kat_2"
  )

  is.data.frame(varer) && all(required %in% names(varer))
}

#' Hent de entydige varenavne fra kataloget
#'
#' @param varer Det samlede varekatalog.
#'
#' @return En alfabetisk sorteret tegnvektor.
#' @keywords internal
indkobsseddel_item_names <- function(varer) {
  if (!indkobsseddel_has_item_columns(varer)) return(character())

  names <- as.character(varer$Indkobsliste)
  names <- trimws(names[!is.na(names)])
  sort(unique(names[nzchar(names)]))
}

#' Find én vare uden forskel på store og små bogstaver
#'
#' @param varer Det samlede varekatalog.
#' @param name Varenavnet fra dialogen.
#'
#' @return Den entydige varerække eller en tom tabel med samme kolonner.
#' @keywords internal
indkobsseddel_find_item <- function(varer, name) {
  if (!indkobsseddel_has_item_columns(varer)) {
    return(indkobsseddel_empty_rows())
  }

  name <- tolower(indkobsseddel_clean_text(name))
  if (!nzchar(name)) return(varer[0, , drop = FALSE])

  matches <- which(
    tolower(trimws(as.character(varer$Indkobsliste))) == name
  )
  if (length(matches) != 1L) return(varer[0, , drop = FALSE])

  result <- varer[matches[[1]], , drop = FALSE]
  result[c(
    "Indkobsliste",
    "maengde",
    "enhed",
    "kat_1",
    "kat_2"
  )]
}

#' Vis en namespacet dialog
#'
#' @param id Dialogens lokale id.
#' @param ns Modulets namespace-funktion.
#'
#' @return Resultatet fra ShinyJS usynligt.
#' @keywords internal
indkobsseddel_show_dialog <- function(id, ns) {
  show(
    id = ns(id),
    anim = TRUE,
    animType = "fade",
    asis = TRUE
  )
}

#' Skjul en namespacet dialog
#'
#' @param id Dialogens lokale id.
#' @param ns Modulets namespace-funktion.
#'
#' @return Resultatet fra ShinyJS usynligt.
#' @keywords internal
indkobsseddel_hide_dialog <- function(id, ns) {
  hide(
    id = ns(id),
    anim = TRUE,
    animType = "fade",
    asis = TRUE
  )
}

#' Byg DT-tabellen til indkøbssedlen
#'
#' Synlige varelinjer får redigerings- og sletteknapper. Skjulte
#' opskriftsnoter lægges efter de synlige rækker, så DataTables' kopiknap kan
#' kopiere hele teksten uden at vise noterne på den første side.
#'
#' @param payload Resultatet fra `cart_copy_payload()`.
#' @param ns Modulets namespace-funktion.
#'
#' @return Et DT-widget-objekt.
#' @keywords internal
indkobsseddel_cart_widget <- function(payload, ns) {
  lines_visible <- as.character(payload$visible)
  lines_hidden <- as.character(payload$hidden)
  line_ids <- as.character(payload$line_ids)
  n_visible <- as.integer(payload$n_visible)

  if (length(n_visible) != 1L || is.na(n_visible) || n_visible < 0L) {
    stop("Cartens copy-payload har et ugyldigt rækkeantal.", call. = FALSE)
  }
  if (
    length(lines_visible) != n_visible ||
      length(line_ids) != n_visible
  ) {
    stop("Cartens copy-payload er inkonsistent.", call. = FALSE)
  }

  if (n_visible == 0L) {
    table_data <- data.frame(
      `Indkøbsliste` = character(),
      edit = character(),
      delete = character(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    page_length <- 1L
  } else {
    all_lines <- c(lines_visible, lines_hidden)
    edit_buttons <- ga_make_cart_edit_buttons(
      line_ids,
      event_id = ns("edit_pressed"),
      id_prefix = ns("edit_")
    )
    delete_buttons <- ga_make_cart_delete_buttons(
      line_ids,
      event_id = ns("delete_pressed"),
      id_prefix = ns("delete_")
    )
    hidden_count <- length(all_lines) - n_visible

    table_data <- data.frame(
      `Indkøbsliste` = all_lines,
      edit = c(edit_buttons, rep("", hidden_count)),
      delete = c(delete_buttons, rep("", hidden_count)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    page_length <- n_visible
  }

  datatable(
    table_data,
    rownames = FALSE,
    colnames = NULL,
    escape = 1,
    extensions = "Buttons",
    options = list(
      paging = TRUE,
      pageLength = max(1L, page_length),
      lengthChange = FALSE,
      info = FALSE,
      ordering = FALSE,
      searching = FALSE,
      dom = "Bft",
      buttons = list(
        list(
          extend = "copy",
          text = "Kopiér indkøbslisten",
          title = NULL,
          exportOptions = list(
            columns = 0,
            modifier = list(page = "all")
          ),
          attr = list(
            style = paste(
              "background:#22c55e;",
              "color:#fff;",
              "border:1px solid #16a34a;",
              "border-radius:100px;",
              "font-weight:500;"
            )
          ),
          action = JS("copyWithFeedback")
        )
      ),
      columnDefs = list(
        list(
          targets = 1,
          orderable = FALSE,
          searchable = FALSE
        ),
        list(
          targets = 2,
          orderable = FALSE,
          searchable = FALSE
        )
      ),
      language = list(
        emptyTable = "Ingen varer på indkøbslisten!"
      )
    )
  )
}

#' Byg preview-tabellen til en opskrift
#'
#' @param rows Ingrediensrækker i cart-format.
#'
#' @return Et kompakt DT-widget-objekt med navn, mængde og enhed.
#' @keywords internal
indkobsseddel_recipe_preview_widget <- function(rows) {
  datatable(
    rows[, c("Indkobsliste", "maengde", "enhed"), drop = FALSE],
    rownames = FALSE,
    options = list(
      dom = "t",
      ordering = FALSE,
      pageLength = max(1L, nrow(rows))
    )
  )
}

#' Byg data til historisk lagring
#'
#' @param payload Resultatet fra `cart_copy_payload()`.
#'
#' @return En data frame med både synlige varelinjer og skjulte
#'   opskriftsnoter i kolonnen `Indkøbsliste`.
#' @keywords internal
indkobsseddel_history_frame <- function(payload) {
  values <- trimws(as.character(c(
    payload$visible,
    payload$hidden
  )))

  data.frame(
    `Indkøbsliste` = values,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Udtræk varenavne fra historikkens forskellige formater
#'
#' @param history En tegnvektor eller data frame fra rootens historiklæser.
#'
#' @return En trimmet tegnvektor uden tomme værdier og dubletter.
#' @keywords internal
indkobsseddel_history_values <- function(history) {
  if (is.data.frame(history)) {
    candidates <- c("Indkøbsliste", "Indkobsliste")
    column <- candidates[candidates %in% names(history)]
    if (length(column) == 0L) return(character())
    values <- history[[column[[1]]]]
  } else {
    values <- history
  }

  values <- as.character(values)
  values <- trimws(values[!is.na(values)])
  values <- values[nzchar(values)]
  values[!duplicated(tolower(values))]
}

#' Find forslag, som endnu ikke står på indkøbssedlen
#'
#' Sammenligningen bruger cartens oprindelige varenavne frem for at forsøge
#' at parse den formatterede visning. Det gør filtreringen robust over for
#' mængder, enheder og brugerens egne redigeringstekster.
#'
#' @param history Historiske varenavne som tegnvektor eller data frame.
#' @param visible_rows Resultatet fra `cart_visible()`.
#' @param units Enheder fra det aktuelle varekatalog. De fjernes fra navnene
#'   på samme måde som i den eksisterende historikberegning.
#' @param limit Maksimalt antal forslag.
#'
#' @return En data frame med op til `limit` forslag.
#' @keywords internal
indkobsseddel_history_suggestions <- function(
  history,
  visible_rows,
  units = character(),
  limit = 10L
) {
  values <- indkobsseddel_history_values(history)
  current <- if (
    is.data.frame(visible_rows) &&
      "Indkobsliste" %in% names(visible_rows)
  ) {
    units <- as.character(units)
    units <- units[!is.na(units)]
    tolower(rens_varer(
      as.character(visible_rows$Indkobsliste),
      units
    ))
  } else {
    character()
  }

  values <- values[!tolower(values) %in% current]
  limit <- suppressWarnings(as.integer(limit))
  if (length(limit) != 1L || is.na(limit) || limit < 0L) limit <- 10L
  values <- head(values, limit)

  data.frame(
    Forslag = values,
    stringsAsFactors = FALSE
  )
}

#' Omsæt en callback-fejl til et almindeligt resultat
#'
#' @param error En condition fra `save_history`.
#'
#' @return En liste med `ok = FALSE` og den læsbare fejltekst.
#' @keywords internal
indkobsseddel_capture_save_error <- function(error) {
  list(
    ok = FALSE,
    error = conditionMessage(error)
  )
}

#' Kald historik-callbacken sikkert
#'
#' @param save_history Callbacken fra root.
#' @param history_df Den færdige data frame, der skal gemmes.
#'
#' @return En liste med `ok` og en eventuel fejltekst.
#' @keywords internal
indkobsseddel_try_save_history <- function(
  save_history,
  history_df
) {
  tryCatch(
    {
      ok <- isTRUE(save_history(history_df))
      list(
        ok = ok,
        error = if (ok) NULL else "Indkøbssedlen kunne ikke gemmes."
      )
    },
    error = indkobsseddel_capture_save_error
  )
}

#' Gem indkøbssedlen i appens historikmappe
#'
#' Filen beholder objekt-navnet `df`, fordi de eksisterende historikfunktioner
#' forventer dette navn, når en ældre indkøbsseddel indlæses. Callbacken
#' returnerer først succes, når filen faktisk er skrevet.
#'
#' @param history_df Data frame med kolonnen `Indkøbsliste`.
#' @param history_dir Mappen med appens historiske indkøbssedler.
#'
#' @return `TRUE` usynligt efter en vellykket gemning.
#' @keywords internal
indkobsseddel_save_history <- function(
  history_df,
  history_dir = "./data/indkobssedler"
) {
  if (
    !is.data.frame(history_df) ||
      !identical(names(history_df), "Indkøbsliste")
  ) {
    stop(
      "Indkøbssedlens historik skal have præcis kolonnen 'Indkøbsliste'.",
      call. = FALSE
    )
  }
  if (!dir.exists(history_dir)) {
    stop("Mappen til historiske indkøbssedler findes ikke.", call. = FALSE)
  }

  df <- history_df
  file_name <- paste0(
    "indkobsseddel_",
    format(Sys.Date(), "%Y%m%d"),
    ".rda"
  )
  save(df, file = file.path(history_dir, file_name))
  invisible(TRUE)
}
