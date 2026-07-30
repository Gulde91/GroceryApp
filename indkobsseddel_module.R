library(htmltools)
library(DT)
library(shiny)
library(shinyMobile)
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
