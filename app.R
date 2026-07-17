# ShinyMobile-skal for GroceryApp
library(shiny)
library(shinyMobile)
library(readr)
library(dplyr)
library(purrr)
library(fontawesome)
library(shinyjs)
library(ggplot2)
library(wordcloud2)

source("./data.R")
source("./funktioner.R")
source("./cart_state.R")


ui <- f7Page(
  
  # opsætning ----
  tags$head(
    includeCSS("www/styles.css"),
    fa_html_dependency(),
    htmltools::singleton(tags$script(src = "selectize-mobile.js")),
    htmltools::singleton(tags$script(src = "button-press.js")),
    htmltools::singleton(tags$script(src = "copy-helper.js")),
    htmltools::singleton(tags$script(src = "DT-copy-feedback.js")),
    htmltools::singleton(tags$script(src = "pwa-viewport-fix.js"))
  ),
  
  useShinyjs(),

  title = "IndkøbsApp",
  options = list(
    theme = "auto",
    dark = TRUE,
    preloader = TRUE
    ),
  
  f7TabLayout(
    navbar = f7Navbar(title = "IndkøbsApp"),
    f7Tabs(
      id = "main_tabs",
      animated = FALSE,
      swipeable = FALSE,
      # Indkøbsliste ----
      f7Tab(
        tabName = "Indkøbsseddel",
        icon = f7Icon("cart"),
        active = TRUE,
        f7BlockTitle(title = "Vælg varer"),
        f7Block(
          inset = TRUE, strong = TRUE,
          tags$div(class = "ga-actions-grid",
              f7Button("open_opskrift", "Opskrift", icon = f7Icon("book"), 
                       fill = FALSE, color = "green"),
              f7Button("open_varer", "Liste", icon = f7Icon("square_list"), 
                       fill = FALSE, color = "green"),
              f7Button("open_manuel", "Manuel", icon = f7Icon("hand_draw"), 
                       fill = FALSE, color = "green")
          )
        ),
        br(),
        DT::DTOutput("indkobsseddel"),
        f7Block(
          f7Button("gem_indkobsseddel", "Gem indkøbssedlen til database", 
                   fill = TRUE, color = "blue"),
        ),
        h5(strong("Forslag til manglende varer:")),
        tableOutput("tidl_kob"),
      ),
      # Varer (bruttoliste) ----
      f7Tab(
        tabName = "Varer",
        icon = f7Icon("square_list"),
        active = FALSE,
        f7BlockTitle(title = "Bruttoliste over varer"),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          f7Button("open_ny_vare", "Tilføj ny vare", fill = TRUE, color = "green")
        ),
        DT::DTOutput("varer_tbl")
      ),
      # Opskrifter ----
      f7Tab(
        tabName = "Opskrifter",
        icon = f7Icon("book"),
        active = FALSE,
        f7BlockTitle(title = "Opskrifter"),
      f7Block(inset = TRUE, strong = TRUE,
        tags$p(
          "Alle opskrifter nedenfor er angivet med mængder svarende til ",
          tags$b("1 person"), ".")
        ,
          tags$p("Du kan redigere og slette ingredienslinjer direkte. Ændringer gemmes automatisk."),
          f7Button("open_ny_ret", "Tilføj ny ret", fill = TRUE, color = "green")
        ),
        # Dynamisk indhold til alle opskrifter
        uiOutput("opskrifter_ui")
      ),
      # Inspiration----
      f7Tab(
        tabName = "Inspiration",
        icon = f7Icon("sparkles"),
        f7BlockTitle(title = "Inspiration"),
        f7Block(inset = TRUE, strong = TRUE,
          # Knap som åbner filter-sheet (Framework7 styret)
          tags$a(
            class = "sheet-open",
            `data-sheet` = "#plot_filters_sheet",
            f7Button(
              inputId = "open_filters",
              label = "Filtre",
              icon = f7Icon("slider_horizontal_3"),
              fill = TRUE,
              color = "blue"
            )
          ),
          br(),
          plotOutput("opskrifter_statistik_plot")
        ),
        f7Block(inset = TRUE, strong = TRUE,
                sInput("menu_type", "V\u00E6lg type",
                       c("Alle", "Vegetar", "Kylling", "Gris", "Okse", "Fisk")),
                wordcloud2Output("wordcloud_retter", height = "250px")
        )
      )
    ),
    
    # Modals ----
    # Custom "modal" (overlay) – skjult til at starte med. går igen flere steder
    tags$div(
      id = "edit-overlay",
      tags$div(
        id = "edit-dialog",
        tags$h3("Redigér tekst"),
        textInput("table_edit_value", label = NULL, value = "", width = "100%"),
        tags$div(
          id = "edit-actions",
          actionButton("cancel_edit", "Annullér", class = "btn-flat"),
          actionButton("confirm_edit", "Gem", class = "btn-flat btn-save")
        )
      )
    ),
    
    # POPUP: fra liste
    tags$div(
      id="popup_varer", class="ga-modal",
      tags$div(class="ga-dialog",
               tags$h3("Tilføj varer fra liste"),
               f7Block(inset=TRUE, strong=TRUE,
                       selectizeInput("basis_varer", "Tilf\u00F8j varer fra liste", NULL),
                       br(), nInput("antal_basis_varer", "M\u00E6ngde", value=1),
                       br(), sInput("enhed_alle_varer", "Enhed", "", "stk"),
                       br(),
                       f7Button("add_varer", "Tilføj til indkøbssedlen", fill=TRUE, color="green"),
                       br(),
                       f7Button("close_varer", "Luk", fill=TRUE, color="gray")
               )
      )
    ),
    
    # POPUP: manuelt
    tags$div(
      id="popup_manuel", class="ga-modal",
      tags$div(class="ga-dialog",
               tags$h3("Tilføj vare manuelt"),
               f7Block(inset=TRUE, strong=TRUE,
                 tInput("basis_varer_manuel", label="Tilf\u00F8j varer manuelt"),
                 br(), nInput("antal_basis_varer_manuel", "M\u00E6ngde", value=1),
                 br(), sInput("enhed_basis_varer_manuel", "Enhed", "", "stk"),
                 br(), sInput("add_kat_1", "Kategori 1", kategori_1, "konserves"),
                 br(), sInput("add_kat_2", "Kategori 2", kategori_2, "konserves"),
                 br(),
                 f7Button("add_varer_manuel", "Tilføj til indkøbssedlen", fill=TRUE, color="green"),
                 br(),
                 f7Button("close_manuel", "Luk", fill=TRUE, color="gray")
               )
      )
    ),
    
    # POPUP: opskrifter
    tags$div(
      id="popup_opskrift", class="ga-modal",
      tags$div(class="ga-dialog",
               tags$h3("Tilføj fra opskrift"),
               f7Block(inset=TRUE, strong=TRUE,
                 sInput("ret", "Vælg ret", c("", retter$retter[retter$key %in% names(opskrifter)])),
                 br(), nInput("pers", "Vælg antal personer", value=2),
                 br(), sInput("salat", "Vælg salat", salater$retter),
                 br(), sInput("tilbehor", "Vælg tilbehør", c("", tilbehor$Indkobsliste)),
                 br(),
                 f7Button("add_opskrift", "Tilføj til indkøbssedlen", fill=TRUE, color="green"),
                 br(),
                 f7Button("close_opskrift", "Luk", fill=TRUE, color="gray")
               ),
               DT::DTOutput("opskrift")
      )
    ),
    # POPUP: tilføj ny vare til bruttoliste
    tags$div(
      id = "popup_ny_vare", class = "ga-modal",
      tags$div(class = "ga-dialog",
               tags$h3("Tilføj ny basisvare"),
               f7Block(
                 inset = TRUE, strong = TRUE,
                 tInput("ny_vare_navn", "Varenavn"),
                 sInput("ny_vare_enhed", "Enhed", choices = NULL, ""),
                 sInput("ny_vare_kat1", "Kategori 1", choices = NULL),
                 sInput("ny_vare_kat2", "Kategori 2", choices = NULL),
                 br(),
                 f7Button("save_ny_vare",  "Gem vare", fill = TRUE, color = "blue"),
                 br(),
                 f7Button("close_ny_vare", "Luk", fill = TRUE, color = "gray")
               )
      )
    ),
    # POPUP: redigér ingrediens i opskrift
    tags$div(
      id = "popup_opskrift_rediger", class = "ga-modal",
      tags$div(class = "ga-dialog",
               tags$h3("Redigér ingrediens"),
               tags$p(textOutput("opskrift_edit_context")),
               f7Block(
                 inset = TRUE, strong = TRUE,
                 nInput("opskrift_edit_maengde", "Mængde", value = 1),
                 sInput("opskrift_edit_enhed", "Enhed", choices = c(""), selected = ""),
                 sInput("opskrift_edit_kat1", "Kategori 1", choices = c(""), selected = ""),
                 sInput("opskrift_edit_kat2", "Kategori 2", choices = c(""), selected = ""),
                 br(),
                 f7Button("save_opskrift_row", "Opdater række", fill = TRUE, color = "blue"),
                 br(),
                 f7Button("cancel_opskrift_row", "Luk", fill = TRUE, color = "gray")
               )
      )
    ),
    # POPUP: tilføj ingrediens til valgt opskrift
    tags$div(
      id = "popup_opskrift_tilfoej", class = "ga-modal",
      tags$div(class = "ga-dialog",
               tags$h3("Tilføj ingrediens"),
               tags$p(textOutput("opskrift_add_context")),
               f7Block(
                 inset = TRUE, strong = TRUE,
                 tInput("opskrift_add_navn", "Varenavn"),
                 nInput("opskrift_add_maengde", "Mængde", value = 1),
                 sInput("opskrift_add_enhed", "Enhed", choices = c(""), selected = ""),
                 sInput("opskrift_add_kat1", "Kategori 1", choices = c(""), selected = ""),
                 sInput("opskrift_add_kat2", "Kategori 2", choices = c(""), selected = ""),
                 br(),
                 f7Button("save_opskrift_new_row", "Tilføj vare", fill = TRUE, color = "green"),
                 br(),
                 f7Button("cancel_opskrift_new_row", "Luk", fill = TRUE, color = "gray")
               )
      )
    ),
    # POPUP: bekræft sletning af ingredienslinje
    tags$div(
      id = "popup_opskrift_slet_bekraeft", class = "ga-modal",
      tags$div(class = "ga-dialog",
               tags$h3("Slet ingrediens"),
               tags$p(textOutput("opskrift_delete_context")),
               tags$p("Er du sikker på at du vil slette denne ingredienslinje?"),
               f7Block(
                 inset = TRUE, strong = TRUE,
                 f7Button("confirm_delete_opskrift_row", "Ja, slet", fill = TRUE, color = "red"),
                 br(),
                 f7Button("cancel_delete_opskrift_row", "Nej", fill = TRUE, color = "gray")
               )
      )
    ),
    # POPUP: bekraeft sletning/arkivering af ret
    tags$div(
      id = "popup_ret_slet_bekraeft", class = "ga-modal",
      tags$div(class = "ga-dialog",
               tags$h3("Arkiver ret"),
               tags$p(textOutput("ret_delete_context")),
               tags$p("Retten flyttes til arkivet og kan gendannes igen senere."),
               f7Block(
                 inset = TRUE, strong = TRUE,
                 f7Button("confirm_delete_ret", "Ja, arkiver ret", fill = TRUE, color = "red"),
                 br(),
                 f7Button("cancel_delete_ret", "Nej", fill = TRUE, color = "gray")
               )
      )
    ),
    # POPUP: bekraeft permanent sletning af arkiveret ret
    tags$div(
      id = "popup_ret_slet_permanent_bekraeft", class = "ga-modal",
      tags$div(class = "ga-dialog",
               tags$h3("Slet ret permanent"),
               tags$p(textOutput("ret_permanent_delete_context")),
               tags$p("Retten, opskriftsfilen og linket slettes permanent og kan ikke gendannes."),
               f7Block(
                 inset = TRUE, strong = TRUE,
                 f7Button("confirm_delete_archived_ret", "Ja, slet permanent", fill = TRUE, color = "red"),
                 br(),
                 f7Button("cancel_delete_archived_ret", "Nej", fill = TRUE, color = "gray")
               )
      )
    ),
    # POPUP: tilføj ny ret
    tags$div(
      id = "popup_ny_ret", class = "ga-modal",
      tags$div(class = "ga-dialog",
               tags$h3("Tilføj ny ret"),
               f7Block(
                 inset = TRUE, strong = TRUE,
                 tInput("ny_ret_navn", "Rettens navn"),
                 sInput("ny_ret_type", "Type", choices = c("vegetar", "kylling", "gris", "okse", "fisk"), selected = "vegetar"),
                 tInput("ny_ret_link", "Link (valgfrit)"),
                 br(),
                 f7Button("save_ny_ret", "Gem ret", fill = TRUE, color = "blue"),
                 br(),
                 f7Button("close_ny_ret", "Luk", fill = TRUE, color = "gray")
               )
      )
    )
    
  ),
  
  # Sheet til filtre for plot med mest brugte opskrifter ----
  f7Sheet(
    id = "plot_filters_sheet",
    label = "Filtre for statistik",
    orientation = "bottom",
    swipeToClose = TRUE,
    backdrop = TRUE,
    f7Block(
      strong = TRUE,
      f7Slider("top_n", "Antal top-opskrifter", 1, 20, 10),
      f7DatePicker("date_from", "Fra dato"),
      f7DatePicker("date_to", "Til dato"),
      
      tags$a(
        class = "sheet-close",
        f7Button("close_filters", "Luk", fill = TRUE, color = "gray")
      )
    )
  ),
  
  uiOutput("edit_popup_ui")
)

server <- function(input, output, session) {

  # Sætter reaktive værdier ----
  rv_opskrift_tmp <- reactiveValues(df = NULL)
  rv_manuel_tilfoj <- reactiveValues(df = NULL)
  rv_cart <- reactiveVal(new_cart_state())
  rv_opskrifter_custom <- reactiveVal(opskrifter)
  rv_links_custom <- reactiveVal(links)
  rv_retter_custom <- reactiveVal(retter)
  rv_retter_arkiv <- reactiveVal(retter_arkiv)
  rv_recipeEditState <- reactiveValues(key = NULL, row = NULL)
  rv_recipeDeleteState <- reactiveValues(key = NULL, row = NULL)
  rv_recipeArchiveState <- reactiveValues(key = NULL)
  rv_recipePermanentDeleteState <- reactiveValues(key = NULL)
  rv_recipeAddState <- reactiveValues(key = NULL)

  rv_varer_custom <- reactiveVal(
    read.csv("./data/basis_varer.txt", fileEncoding = "UTF-8") |> 
      arrange(Indkobsliste)
    )
  
  # Én sandhed om hvad der redigeres (tabel + række) til brug for "Gem" i fælles overlay
  opskrift_selectize_options <- list(
    openOnFocus = TRUE,
    closeAfterSelect = TRUE,
    highlight = TRUE,
    diacritics = TRUE,
    create = FALSE,
    dropdownParent = "body",
    sortField = "label"
  )

  active_recipe_rows <- function(retter_df, ops_keys = names(rv_opskrifter_custom())) {
    retter_df <- retter_df |>
      filter(key %in% ops_keys) |>
      arrange(tolower(retter))

    retter_df
  }

  opskrift_choices <- function(retter_df, ops_keys = names(rv_opskrifter_custom())) {
    retter_df <- active_recipe_rows(retter_df, ops_keys)

    stats::setNames(retter_df$key, retter_df$retter)
  }

  rv_editState <- reactiveValues(table = NULL, row = NULL, line_id = NULL)
  
  # indlæser basis varer ved genload af appen
  session$onFlushed(function() {
    rv_varer_custom(read.csv("./data/basis_varer.txt", fileEncoding = "UTF-8"))
  }, once = TRUE)
  
  
  # laves som reactive (og ikke reactiveVal) fordi der ikke kan indgå
  # reactive elementer i en reactiveVal
  rv_varer <- reactive({
    opskrift_df_custom <- c(rv_opskrifter_custom(), salater_opskrifter) |>
      lapply(function(x) {names(x)[1] <- "Indkobsliste"; x}) |>
      bind_rows() |>
      arrange(Indkobsliste) |>
      mutate(maengde = 1) |>
      distinct()

    bind_rows(opskrift_df_custom, rv_varer_custom()) |>
      arrange(Indkobsliste) |>
      mutate(maengde = 1) |>
      distinct()
  }) 
  
  # Reaktive inputs ----
  observe({
    v_min <- max(
      as.Date("2025-12-01"), # kan ændres på sigt, så det bare er det seneste år
      lubridate::`%m-%`(Sys.Date(), lubridate::years(1))
      )

    updateF7DatePicker("date_from", v_min, dateFormat = "dd-mm-yyyy")
    }
  )
  
  observe(
    updateF7DatePicker("date_to", Sys.Date(), dateFormat = "dd-mm-yyyy")
  )

  observe(
    updateSelectizeInput(
      session, 
      inputId = "basis_varer", 
      choices = sort(rv_varer()$Indkobsliste)
      )
  )
  
  observe(
    updateSelectInput(
      session,
      inputId = "ny_vare_enhed",
      choices = sort(setdiff(unique(rv_varer()$enhed), ""))
    )
  )
  
  observe(
    updateSelectInput(
      session,
      inputId = "ny_vare_kat1",
      choices = sort(setdiff(unique(rv_varer()$kat_1), ""))
    )
  )
  
  observe(
    updateSelectInput(
      session,
      inputId = "ny_vare_kat2",
      choices = sort(setdiff(unique(rv_varer()$kat_2), ""))
    )
  )
  
  
  # Bruttoliste: vis, rediger og slet alle varer ----
  
  # Slet: træk rækkenummer ud af knap-ID når det skal slettes
  observeEvent(input$varer_deletePressed, {
    
    res <- safe_delete_by_click(
      click_id  = input$varer_deletePressed,
      df = rv_varer_custom(),
      label_col = "Indkobsliste"
    )
    
    # opdater reaktiv tilstand
    rv_varer_custom(res$df)
    
    # gemmer
    write.csv(res$df, file = "./data/basis_varer.txt", row.names = FALSE, fileEncoding = "UTF-8")
    
    # valgfri notifikation
    if (!is.null(res$label)) {
      showNotification(sprintf('"%s" er slettet fra bruttolisten.', res$label), type = "message")
    }
  })
  
  # rediger række i bruttoliste
  observeEvent(input$varer_editPressed, ignoreInit = TRUE, {
    r <- suppressWarnings(as.integer(input$varer_editPressed))
    req(!is.na(r))
    
    df <- rv_varer_custom()
    req(!is.null(df), nrow(df) >= r)
    
    # --- WHY: Fortæl fælles "Gem", at det er VARER tabel + hvilken række ---
    rv_editState$table <- "varer"
    rv_editState$row <- r
    rv_editState$line_id <- NULL
    
    updateTextInput(session, "table_edit_value", value = df$Indkobsliste[r])
    show(id = "edit-overlay", anim = TRUE, animType = "fade")
  })
  
  ## Tilføj varer til bruttoliste
  # Åbn/Luk popup
  observeEvent(input$open_ny_vare, {
    show(id = "popup_ny_vare",  anim = TRUE, animType = "fade")
    })
  
  observeEvent(input$close_ny_vare, {
    hide(id = "popup_ny_vare",  anim = TRUE, animType = "fade")
    })
  
  # Sync enheds/kategori-valg ved åbning (trækker aktuelle værdier)
  observeEvent(input$open_ny_vare, {
    df_all <- rv_varer_custom()
    enheder <- sort(unique(c(df_all$enhed, rv_varer()$enhed)))
    updateSelectInput(session, "ny_vare_enhed", choices = enheder, selected = "stk")
    
    kat1 <- sort(unique(c(kategori_1, df_all$kat_1, rv_varer()$kat_1)))
    kat2 <- sort(unique(c(kategori_2, df_all$kat_2, rv_varer()$kat_2)))
    updateSelectInput(session, "ny_vare_kat1", choices = kat1, selected = if (length(kat1)) kat1[1] else "")
    updateSelectInput(session, "ny_vare_kat2", choices = kat2, selected = "")
  })
  
  # Gem ny vare i bruttolisten
  observeEvent(input$save_ny_vare, {
    navn <- trimws(input$ny_vare_navn %||% "")
    validate(need(navn != "", "Skriv et varenavn"))
    
    df <- rv_varer_custom()
    
    # Undgå dubletter (case-insensitive trim)
    if (tolower(navn) %in% tolower(trimws(df$Indkobsliste))) {
      showNotification(sprintf('"%s" findes allerede på bruttolisten.', navn), type = "warning")
      return(invisible(NULL))
    }
    
    ny <- data.frame(
      Indkobsliste = navn,
      maengde = 1,
      enhed = input$ny_vare_enhed %||% "",
      kat_1 = input$ny_vare_kat1 %||% "",
      kat_2 = input$ny_vare_kat2 %||% "",
      stringsAsFactors = FALSE
    )
    
    df_new <- bind_rows(df, ny) |> arrange(Indkobsliste)
    
    # Opdater reaktiv + gem til fil
    rv_varer_custom(df_new)
    write.csv(df_new, "./data/basis_varer.txt", row.names = FALSE, fileEncoding = "UTF-8")
    
    showNotification(sprintf('"%s" er tilføjet til bruttolisten.', navn), type = "message")
    
    # Ryd felter og luk
    updateTextInput(session, "ny_vare_navn",  value = "")
    hide(id = "popup_ny_vare", anim = TRUE, animType = "fade")
  })
  
  
  ## Vis bruttoliste
  output$varer_tbl <- DT::renderDT({
    
    df <- rv_varer_custom()[c("Indkobsliste", "enhed")] |> 
      rename(Vare = Indkobsliste, Enhed = enhed)

    # redigér- og slet-knapper (genbruger dine helpers)
    edit_btns <- ga_make_edit_buttons(n = nrow(df), table_id = "varer")
     
    delete_btns <- vapply(
      seq_len(nrow(df)),
      function(i) add_slet_knap(i, id_prefix = "varer_delete_button", event_name = "varer_deletePressed"),
      FUN.VALUE = ""
    )
    
    DT::datatable(
      cbind(df, Rediger = edit_btns, Slet = delete_btns),
      rownames = FALSE, escape = FALSE,
      options = list(
        dom = "ft", pageLength = nrow(df), ordering = TRUE,
        columnDefs = list(
          list(targets = ncol(df),   orderable = FALSE, searchable = FALSE), # rediger
          list(targets = ncol(df)+1, orderable = FALSE, searchable = FALSE)  # slet
        ),
        language = list(
          search = "",
          searchPlaceholder = "Søg…",
          zeroRecords = "Ingen match", 
          info = "", 
          infoEmpty = "", 
          infoFiltered = ""
          )
      )
    )
  })

  
  # Tilføj opskrift ----
  observe({
    
    # sætter opskrift sammen
    rv_opskrift_tmp$df <- opskrift(
      rv_opskrifter_custom(), rv_retter_custom(), salater, salater_opskrifter, tilbehor,
      input$ret, input$salat, input$pers, input$tilbehor
    )
    
    # viser opskrift
    output$opskrift <- DT::renderDT({
      themed_dt(
        rv_opskrift_tmp$df[, 1:3],
        options = list(
          dom = "t", 
          ordering = FALSE, 
          pageLength = nrow(rv_opskrift_tmp$df)
          )
      )
    }) 
  })
  
  # tilføjer opskrift og rbinder de andre opskrifter
  observeEvent(input$add_opskrift, {
    recipe_rows <- rv_opskrift_tmp$df
    req(!is.null(recipe_rows))

    col_names <- c("Indkobsliste", "maengde", "enhed", "kat_1", "kat_2")
    names(recipe_rows) <- col_names

    # Copy-afsnit og synlige varelinjer gemmes i samme cart-state.
    recipe_sections <- list()
    har_ret <- !is.null(input$ret) && nzchar(input$ret)
    har_salat <- !is.null(input$salat) && nzchar(input$salat)
    har_tilh <- !is.null(input$tilbehor) && nzchar(input$tilbehor)
    
    if (har_ret) {
      # Brug reaktive data, så nyoprettede retter virker uden genstart.
      df_ret <- get_df_custom(ret = input$ret, pers = input$pers)
      if (har_salat) {
        df_sal <- get_df_custom(salat = input$salat, pers = input$pers)
        df_merged <- bind_rows(df_ret, df_sal)
        title <- paste0(input$ret, " m. ", input$salat)
        link  <- get_link_custom(rv_links_custom(), input$ret) %||%
          get_link_custom(rv_links_custom(), input$salat)
      } else {
        df_merged <- df_ret
        title <- input$ret
        link  <- get_link_custom(rv_links_custom(), input$ret)
      }
      recipe_sections <- c(
        recipe_sections,
        list(list(
          title = title,
          pers  = input$pers,
          df    = df_merged,
          link  = link
        ))
      )
    } else {
      # Ingen ret valgt → salat/tilbehør må gerne stå alene
      if (har_salat) {
        df_sal <- get_df_custom(salat = input$salat, pers = input$pers)
        recipe_sections <- c(
          recipe_sections,
          list(list(
            title = paste0("Salat: ", input$salat),
            pers  = input$pers,
            df    = df_sal,
            # Link slås op i de reaktive links (inkl. brugerens tilføjelser).
            link  = get_link_custom(rv_links_custom(), input$salat)
          ))
        )
      }
      if (har_tilh) {
        df_til <- get_df_custom(tilbeh = input$tilbehor, pers = input$pers)
        recipe_sections <- c(
          recipe_sections,
          list(list(
            title = paste0("Tilbehør: ", input$tilbehor),
            pers  = input$pers,
            df    = df_til,
            link  = NA_character_
          ))
        )
      }
    }

    rv_cart(cart_add_recipe(rv_cart(), recipe_rows, recipe_sections))
    rv_opskrift_tmp$df <- NULL
    
    hide(id = "popup_opskrift", anim = TRUE, animType = "fade")
    
  })

  observeEvent(input$open_opskrift, {
    valid_retter <- active_recipe_rows(rv_retter_custom())

    updateSelectInput(session = session, inputId = "ret", choices = c("", valid_retter$retter))
    updateSelectInput(session = session, inputId = "ret", selected = "")
    updateNumericInput(session = session, inputId = "pers", value = 2)
    updateSelectInput(session = session, inputId = "salat", selected = salater$retter[[1]])
    updateSelectInput(session = session, inputId = "tilbehor", selected = "")
    show(id = "popup_opskrift", anim = TRUE, animType = "fade")
  })
  observeEvent(input$close_opskrift, {hide(id = "popup_opskrift", anim = TRUE, animType = "fade")})
  
  # Tilføj varer fra liste ----
  
  # viser enhed på valgt vare
  observe({ 
    
    updateSelectInput(
      session = session,
      inputId = "enhed_alle_varer",
      choices = sort(setdiff(unique(rv_varer()$enhed), "")),
      selected = rv_varer()[rv_varer()$Indkobsliste == input$basis_varer, ]$enhed
    )
  })
  
  # mulighed for at tilføje varer
  observeEvent(input$add_varer, {
    
    if (is.null(input$enhed_alle_varer)) {
      showNotification("Vælg en enhed, før varen tilføjes.", type = "warning")
    } else {
      
      varer_tmp <- rv_varer()[rv_varer()$Indkobsliste == input$basis_varer, ]
      varer_tmp$maengde <- varer_tmp$maengde * input$antal_basis_varer
      varer_tmp$enhed <- input$enhed_alle_varer
      
      cat(input$basis_varer, "er tilføjet!\n")
      rv_cart(cart_add_rows(rv_cart(), varer_tmp))
      
      hide(id = "popup_varer", anim = TRUE, animType = "fade")
      
    }
    
  })
  
  observeEvent(input$open_varer, {
    updateSelectizeInput(session, inputId = "basis_varer", selected = character(0))
    updateNumericInput(session = session, inputId = "antal_basis_varer", value = 1)
    updateSelectInput(session = session, inputId = "enhed_alle_varer", selected = "stk")
    show(id = "popup_varer", anim = TRUE, animType = "fade")
  })
  observeEvent(input$close_varer, {hide(id = "popup_varer", anim = TRUE, animType = "fade")})
  
  # Tilføj varer manuel ----
  observe({ 
    
    updateSelectInput(
      session = session,
      inputId = "enhed_basis_varer_manuel",
      choices = sort(setdiff(unique(rv_varer()$enhed), "")),
      selected = rv_varer()[rv_varer()$Indkobsliste == input$basis_varer, ]$enhed
    )
  })
  
  observeEvent(input$add_varer_manuel, {
    
    varer_manuel_tmp <- data.frame(
      Indkobsliste = input$basis_varer_manuel,
      maengde = input$antal_basis_varer_manuel,
      enhed = input$enhed_basis_varer_manuel,
      kat_1 =  input$add_kat_1,
      kat_2 = input$add_kat_2
    )
    
    rv_cart(cart_add_rows(rv_cart(), varer_manuel_tmp))
    
    hide(id = "popup_manuel", anim = TRUE, animType = "fade")
  })
  
  observeEvent(input$gem_vare, {
    
    varer_manuel_tmp <- data.frame(
      Indkobsliste = input$basis_varer_manuel,
      maengde = input$antal_basis_varer_manuel,
      enhed = input$enhed_basis_varer_manuel,
      kat_1 =  input$add_kat_1,
      kat_2 = input$add_kat_2
    )
    
    rv_manuel_tilfoj$df <- bind_rows(rv_manuel_tilfoj$df, varer_manuel_tmp)
    varer_custom_new <- bind_rows(rv_varer_custom(), rv_manuel_tilfoj$df)
    
    write.csv(
      varer_custom_new, 
      file = "./data/basis_varer.txt", 
      row.names = FALSE,
      fileEncoding = "UTF-8"
      )
    
    message(input$basis_varer_manuel, " er nu gemt i basis_varer.txt")
    
  })
  
  observeEvent(input$open_manuel, {show(id = "popup_manuel", anim = TRUE, animType = "fade")})
  observeEvent(input$close_manuel, {hide(id = "popup_manuel", anim = TRUE, animType = "fade")})
  
  
  # Én kanonisk state; tabel og copy-payload er rene afledninger.
  cart_lines <- reactive(cart_visible(rv_cart()))
  combined_lines <- reactive(cart_copy_payload(rv_cart()))

  # mulighed for at slette række
  observeEvent(input$deletePressed, {
    line_id <- as.character(input$deletePressed %||% "")
    req(nzchar(line_id))
    rv_cart(cart_delete_line(rv_cart(), line_id))
  })
  
  # Åbn overlay når der klikkes på Redigér-knap i tabellen
  observeEvent(input$indkobsseddel_editPressed, ignoreInit = TRUE, {
    line_id <- as.character(input$indkobsseddel_editPressed %||% "")
    req(nzchar(line_id))

    view <- cart_lines()
    row_index <- match(line_id, view$line_id)
    req(!is.na(row_index))
    
    # Et stabilt line_id bruges i stedet for den aktuelle tabelposition.
    rv_editState$table <- "indkobsseddel"
    rv_editState$row <- NULL
    rv_editState$line_id <- line_id
    
    updateTextInput(session, "table_edit_value", value = view$display[[row_index]])
    show(id = "edit-overlay", anim = TRUE, animType = "fade")
  })
  
  # Gem ændringen og luk overlay
  observeEvent(input$confirm_edit, {
    tbl <- rv_editState$table
    req(!is.null(tbl))
    
    val <- input$table_edit_value
    
    if (tbl == "indkobsseddel") {
      line_id <- rv_editState$line_id
      req(!is.null(line_id))

      if (!nzchar(trimws(val %||% ""))) {
        showNotification("Teksten på indkøbssedlen må ikke være tom.", type = "warning")
        return(invisible(NULL))
      }

      rv_cart(cart_edit_line(rv_cart(), line_id, val))
      
    } else if (tbl == "varer") {
      r <- rv_editState$row
      req(!is.null(r))
      df <- rv_varer_custom()
      req(nrow(df) >= r)
      df$Indkobsliste[r] <- val
      
      df <- df |> arrange(Indkobsliste)
      rv_varer_custom(df)
      
      # --- WHY: Varer er vedvarende (basisliste) → skriv til fil ---
      write.csv(df, "./data/basis_varer.txt", row.names = FALSE, fileEncoding = "UTF-8")
      showNotification(sprintf('Varen er omdøbt til "%s".', val), type = "message")
    }
    
    # Ryd state og luk overlay (så næste redigering starter rent)
    rv_editState$table <- NULL
    rv_editState$row <- NULL
    rv_editState$line_id <- NULL
    hide(id = "edit-overlay", anim = TRUE, animType = "fade")
  })
  
  # Luk uden at gemme
  observeEvent(input$cancel_edit, {
    rv_editState$table <- NULL
    rv_editState$row <- NULL
    rv_editState$line_id <- NULL
    hide(id = "edit-overlay", anim = TRUE, animType = "fade")
  })


  # udstiller indkøbsseddel ----
  output$indkobsseddel <- DT::renderDT(server = FALSE, {
    
    payload <- combined_lines()
    lines_visible <- payload$visible
    lines_hidden  <- payload$hidden
    line_ids      <- payload$line_ids
    n_visible     <- payload$n_visible
    
    # Vis KUN varer i tabellen, hvis der ikke er nogen varer → vis tom tabel.
    if (n_visible == 0) {
      df_tbl <- data.frame(`Indkøbsliste` = character())
      edit_col <- delete_col <- character()
      page_len <- 1L
    } else {
      # læg de skjulte linjer bagpå, så copy kan tage dem via "page=all"
      all_lines <- c(lines_visible, lines_hidden)
      df_tbl <- data.frame(`Indkøbsliste` = all_lines, check.names = FALSE)
      
      # Knapper kun på de synlige (vare) rækker
      edit_btn <- ga_make_cart_edit_buttons(line_ids)
      del_btn  <- ga_make_cart_delete_buttons(line_ids)
      edit_col   <- c(edit_btn, rep("", length(all_lines) - n_visible))
      delete_col <- c(del_btn,  rep("", length(all_lines) - n_visible))
      page_len <- n_visible
    }
    
    DT::datatable(
      cbind(df_tbl, edit = edit_col, delete = delete_col),
      rownames = FALSE,
      colnames = NULL,
      escape   = c(1),
      extensions = "Buttons",
      options = list(
        paging = TRUE,
        pageLength = max(1, page_len),
        lengthChange = FALSE,
        info = FALSE,
        ordering = FALSE,
        searching = FALSE,
        dom = "Bft",
        buttons = list(
          list(
            extend = "copy",
            text   = "Kopiér indkøbslisten",
            title  = NULL,
            exportOptions = list(
              columns  = 0,
              modifier = list(page = "all")  # kopier alle rækker (inkl. opskrifter)
            ),
            attr = list(style = paste(
              "background:#22c55e;",
              "color:#fff;",
              "border:1px solid #16a34a;",
              "border-radius:100px;",
              "font-weight:500;"
            )),
            action = DT::JS("copyWithFeedback")
          )
        ),
        columnDefs = list(
          list(targets = 1, orderable = FALSE, searchable = FALSE),
          list(targets = 2, orderable = FALSE, searchable = FALSE)
        ),
        language = list(emptyTable = "Ingen varer på indkøbslisten!")
      )
    )
  })

  # gemmer indkøbsseddel ----
  observeEvent(input$gem_indkobsseddel, {
    
    samlet <- combined_lines()
    df <- data.frame(Indkøbsliste = c(samlet$visible, samlet$hidden))
    df$Indkøbsliste <- trimws(df$Indkøbsliste)
    
    path <- paste0("./data/indkobssedler/indkobsseddel_", gsub("-", "", Sys.Date()), ".rda")
    save(df, file = path)
    runjs("showCopyToast(\"Indkøbsseddel gemt ✔\", \"blue\")")
    
  })
  
  # mest populære varer ----
  # loader tidligere indkøbssedler
  tidl_kob <- reactive({
    mest_brugte_varer(c(rv_varer()$enhed, rv_varer_custom()$enhed))
  })
  
  
  output$tidl_kob <- renderTable({
    current_cart <- cart_display_data(rv_cart())
    req(nrow(current_cart) > 0)

    paa_listen <- medtag_kun_varer(current_cart)
    paa_listen <- rens_varer(
      paa_listen$Indkøbsliste,
      c(rv_varer()$enhed, rv_varer_custom()$enhed)
    )

    tidl_kob()[!tidl_kob()$Indkøbsliste %in% paa_listen, ] |>
      slice_head(n = 10)
  }, colnames = FALSE)
  
  ## Inspiration og statistik
  
  # wordcloud plot ----
  output$wordcloud_retter <- renderWordcloud2({
    
    retter_tmp <- rv_retter_custom()
    
    if (input$menu_type != "Alle") {
      retter_tmp <- filter(retter_tmp, grepl(tolower(input$menu_type), type))
    }
    
    farver <- c("#fde68a", "#bef264", "#6ee7b7", "#93c5fd", "#e5e7eb")
    
    retter_tmp %>%
      filter(retter != "V\u00E6lg ret") %>%
      select(retter) %>%
      mutate(count = sample(c(0.4, 0.45, 0.5), nrow(.), 
                            replace = TRUE, prob = c(0.6, 0.3, 0.1))) %>%
      wordcloud2(
        size = 0.1, 
        color = sample(farver, size = nrow(.), replace= TRUE), 
        backgroundColor = "#1c1c1e",
        shape = "circle",
        rotateRatio = 0)
  })

  # statistik over brugte opskrifter ----
  opskrifter_statistik <- brugte_opskrifter(retter$retter)
  
  output$opskrifter_statistik_plot <- renderPlot({
    plot_brugte_opskrifter(
      opskrifter_statistik,  
      dato_start = input$date_from,
      dato_slut = input$date_to,
      top_n = input$top_n
      )
  })

  # Opskrifter: redigering via modal pr. ingrediens ----
  format_recipe_line <- function(maengde, enhed, ingrediens) {
    linje <- paste(maengde, enhed, ingrediens)
    linje <- gsub("NA", "", linje)
    trimws(gsub("\\s+", " ", linje))
  }

  persist_recipe <- function(key) {
    df <- rv_opskrifter_custom()[[key]]
    req(!is.null(df))

    write.table(
      df,
      file = file.path("./data/opskrifter", paste0(key, ".txt")),
      sep = ";",
      row.names = FALSE,
      quote = FALSE,
      na = "",
      fileEncoding = "UTF-8"
    )
  }
  
  slugify_recipe_key <- function(x) {
    x_ascii <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    x_ascii <- tolower(x_ascii)
    x_ascii <- gsub("[^a-z0-9]+", "_", x_ascii)
    gsub("^_+|_+$", "", x_ascii)
  }

  normalize_recipe_link <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x[[1]])) return("")

    x <- trimws(as.character(x[[1]]))
    if (!nzchar(x)) return("")
    if (grepl("^https?://", x, ignore.case = TRUE)) return(x)
    if (grepl("^//", x)) return(paste0("https:", x))

    paste0("https://", x)
  }

  persist_retter <- function(df) {
    write.table(
      df,
      file = "./data/retter.txt",
      sep = ";",
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE,
      fileEncoding = "UTF-8"
    )
  }

  persist_retter_arkiv <- function(df) {
    write.table(
      df,
      file = "./data/retter_arkiv.txt",
      sep = ";",
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE,
      fileEncoding = "UTF-8"
    )
  }
  
  persist_links <- function(df) {
    write.table(
      df,
      file = "./data/links.txt",
      sep = ";",
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE,
      fileEncoding = "UTF-8"
    )
  }

  get_df_custom <- function(ret = "", salat = "", pers = 2, tilbeh = "") {
    out <- opskrift(
      rv_opskrifter_custom(), rv_retter_custom(), salater, salater_opskrifter, tilbehor,
      dag_ret = ret, dag_salat = salat, antal = pers, dag_tilbehor = tilbeh
    )
    if (!is.null(out)) {
      colnames(out) <- c("Indkobsliste", "maengde", "enhed", "kat_1", "kat_2")
    }
    out
  }

  observeEvent(input$open_ny_ret, {
    updateTextInput(session, "ny_ret_navn", value = "")
    updateSelectInput(session, "ny_ret_type", selected = "vegetar")
    updateTextInput(session, "ny_ret_link", value = "")
    show(id = "popup_ny_ret", anim = TRUE, animType = "fade")
  })

  observeEvent(input$close_ny_ret, {
    hide(id = "popup_ny_ret", anim = TRUE, animType = "fade")
  })

  observeEvent(input$save_ny_ret, {
    ret_navn <- trimws(input$ny_ret_navn %||% "")
    ret_type <- trimws(input$ny_ret_type %||% "")
    ret_link <- normalize_recipe_link(input$ny_ret_link)

    validate(need(ret_navn != "", "Skriv et navn til retten."))
    validate(need(ret_type != "", "Vælg en type."))

    ops <- rv_opskrifter_custom()
    eksisterende_navne <- vapply(ops, function(df) names(df)[1], "")
    if (tolower(ret_navn) %in% tolower(eksisterende_navne)) {
      showNotification(sprintf('Retten "%s" findes allerede.', ret_navn), type = "warning")
      return(invisible(NULL))
    }

    base_key <- paste0(slugify_recipe_key(ret_navn), "_opskr")
    key <- base_key
    i <- 1L
    while (key %in% names(ops)) {
      i <- i + 1L
      key <- paste0(base_key, "_", i)
    }

    ny_opskrift <- data.frame(
      temp = character(),
      maengde = numeric(),
      enhed = character(),
      kat_1 = character(),
      kat_2 = character(),
      stringsAsFactors = FALSE
    )
    names(ny_opskrift)[1] <- ret_navn

    ops[[key]] <- ny_opskrift
    rv_opskrifter_custom(ops)
    persist_recipe(key)

    retter_new <- bind_rows(
      rv_retter_custom(),
      data.frame(retter = ret_navn, key = key, type = ret_type, stringsAsFactors = FALSE)
    ) |>
      distinct(key, .keep_all = TRUE) |>
      arrange(retter)
    rv_retter_custom(retter_new)
    persist_retter(retter_new)

    links_new <- rv_links_custom()
    if (nzchar(ret_link)) {
      links_new <- bind_rows(
        links_new,
        data.frame(ret = ret_navn, link = ret_link, stringsAsFactors = FALSE)
      ) |>
        distinct(ret, .keep_all = TRUE) |>
        arrange(ret)
      persist_links(links_new)
    }
    rv_links_custom(links_new)

    hide(id = "popup_ny_ret", anim = TRUE, animType = "fade")
    updateSelectizeInput(
      session,
      "opskrift_valgt_key",
      choices = opskrift_choices(retter_new),
      selected = key,
      options = opskrift_selectize_options
    )
    showNotification(sprintf('Retten "%s" er oprettet.', ret_navn), type = "message")
  })

  observeEvent(input$opskrift_editPressed, {
    info <- input$opskrift_editPressed
    req(!is.null(info$key), !is.null(info$row))

    key <- as.character(info$key)
    row <- suppressWarnings(as.integer(info$row))
    req(key %in% names(rv_opskrifter_custom()), !is.na(row))

    df <- rv_opskrifter_custom()[[key]]
    req(nrow(df) >= row)

    rv_recipeEditState$key <- key
    rv_recipeEditState$row <- row

    enhed_choices <- sort(unique(c("", rv_varer()$enhed, df$enhed)))
    kat1_choices <- sort(unique(c(kategori_1, rv_varer()$kat_1, df$kat_1)))
    kat2_choices <- sort(unique(c("", kategori_2, rv_varer()$kat_2, df$kat_2)))

    updateNumericInput(session, "opskrift_edit_maengde", value = df$maengde[row])
    updateSelectInput(session, "opskrift_edit_enhed", choices = enhed_choices, selected = df$enhed[row])
    updateSelectInput(session, "opskrift_edit_kat1", choices = kat1_choices, selected = df$kat_1[row])
    updateSelectInput(session, "opskrift_edit_kat2", choices = kat2_choices, selected = df$kat_2[row])

    output$opskrift_edit_context <- renderText({
      format_recipe_line(df$maengde[row], df$enhed[row], df[[1]][row])
    })

    show(id = "popup_opskrift_rediger", anim = TRUE, animType = "fade")
  })

  observeEvent(input$save_opskrift_row, {
    key <- rv_recipeEditState$key
    row <- rv_recipeEditState$row
    req(!is.null(key), !is.null(row))

    df <- rv_opskrifter_custom()[[key]]
    req(!is.null(df), nrow(df) >= row)

    maengde <- suppressWarnings(as.numeric(input$opskrift_edit_maengde))
    if (length(maengde) == 0) maengde <- NA_real_
    enhed <- trimws(as.character(input$opskrift_edit_enhed %||% ""))
    kat1 <- trimws(as.character(input$opskrift_edit_kat1 %||% ""))
    kat2 <- trimws(as.character(input$opskrift_edit_kat2 %||% ""))

    if (!is.na(maengde) && maengde <= 0) {
      showNotification("Mængde skal være tom eller et tal større end 0.", type = "error")
      return(invisible(NULL))
    }
    if (kat1 == "") {
      showNotification("Kategori 1 må ikke være tom.", type = "error")
      return(invisible(NULL))
    }

    df$maengde[row] <- maengde
    df$enhed[row] <- enhed
    df$kat_1[row] <- kat1
    df$kat_2[row] <- kat2

    ops <- rv_opskrifter_custom()
    ops[[key]] <- df
    rv_opskrifter_custom(ops)

    persist_recipe(key)

    hide(id = "popup_opskrift_rediger", anim = TRUE, animType = "fade")
    showNotification("Ingrediensen er opdateret og gemt.", type = "message")
  })

  observeEvent(input$cancel_opskrift_row, {
    hide(id = "popup_opskrift_rediger", anim = TRUE, animType = "fade")
  })

  observeEvent(input$opskrift_addPressed, {
    info <- input$opskrift_addPressed
    req(!is.null(info$key))

    key <- as.character(info$key)
    req(key %in% names(rv_opskrifter_custom()))

    rv_recipeAddState$key <- key

    df <- rv_opskrifter_custom()[[key]]
    ret_navn <- names(df)[1]

    enhed_choices <- sort(unique(c("", rv_varer()$enhed, df$enhed)))
    kat1_choices <- sort(unique(c(kategori_1, rv_varer()$kat_1, df$kat_1)))
    kat2_choices <- sort(unique(c("", kategori_2, rv_varer()$kat_2, df$kat_2)))

    updateTextInput(session, "opskrift_add_navn", value = "")
    updateNumericInput(session, "opskrift_add_maengde", value = 1)
    updateSelectInput(session, "opskrift_add_enhed", choices = enhed_choices, selected = "")
    updateSelectInput(session, "opskrift_add_kat1", choices = kat1_choices, selected = "konserves")
    updateSelectInput(session, "opskrift_add_kat2", choices = kat2_choices, selected = "")

    output$opskrift_add_context <- renderText({
      sprintf("Tilføj ny ingrediens til '%s'", ret_navn)
    })

    show(id = "popup_opskrift_tilfoej", anim = TRUE, animType = "fade")
  })

  observeEvent(input$save_opskrift_new_row, {
    key <- rv_recipeAddState$key
    req(!is.null(key), key %in% names(rv_opskrifter_custom()))

    df <- rv_opskrifter_custom()[[key]]
    ret_navn <- names(df)[1]

    ingrediens <- trimws(as.character(input$opskrift_add_navn %||% ""))
    maengde <- suppressWarnings(as.numeric(input$opskrift_add_maengde))
    enhed <- trimws(as.character(input$opskrift_add_enhed %||% ""))
    kat1 <- trimws(as.character(input$opskrift_add_kat1 %||% ""))
    kat2 <- trimws(as.character(input$opskrift_add_kat2 %||% ""))

    validate(need(ingrediens != "", "Skriv et varenavn."))
    validate(need(!is.na(maengde), "Mængde skal være et tal."))
    validate(need(kat1 != "", "Vælg en kategori 1."))

    ny_linje <- data.frame(
      temp = ingrediens,
      maengde = maengde,
      enhed = enhed,
      kat_1 = kat1,
      kat_2 = kat2,
      stringsAsFactors = FALSE
    )
    names(ny_linje)[1] <- ret_navn

    df <- bind_rows(df, ny_linje)
    ops <- rv_opskrifter_custom()
    ops[[key]] <- df
    rv_opskrifter_custom(ops)

    persist_recipe(key)

    hide(id = "popup_opskrift_tilfoej", anim = TRUE, animType = "fade")
    rv_recipeAddState$key <- NULL

    showNotification(sprintf('Ingrediensen "%s" er tilføjet.', ingrediens), type = "message")
  })

  observeEvent(input$cancel_opskrift_new_row, {
    hide(id = "popup_opskrift_tilfoej", anim = TRUE, animType = "fade")
    rv_recipeAddState$key <- NULL
  })

  observeEvent(input$opskrift_deletePressed, {
    info <- input$opskrift_deletePressed
    req(!is.null(info$key), !is.null(info$row))

    key <- as.character(info$key)
    row <- suppressWarnings(as.integer(info$row))
    req(key %in% names(rv_opskrifter_custom()), !is.na(row))

    df <- rv_opskrifter_custom()[[key]]
    req(!is.null(df), nrow(df) >= row)

    rv_recipeDeleteState$key <- key
    rv_recipeDeleteState$row <- row

    output$opskrift_delete_context <- renderText({
      format_recipe_line(df$maengde[row], df$enhed[row], df[[1]][row])
    })

    show(id = "popup_opskrift_slet_bekraeft", anim = TRUE, animType = "fade")
  })

  observeEvent(input$confirm_delete_opskrift_row, {
    key <- rv_recipeDeleteState$key
    row <- rv_recipeDeleteState$row
    req(!is.null(key), !is.null(row))

    df <- rv_opskrifter_custom()[[key]]
    req(!is.null(df), nrow(df) >= row)

    slettet <- format_recipe_line(df$maengde[row], df$enhed[row], df[[1]][row])
    df <- df[-row, , drop = FALSE]

    ops <- rv_opskrifter_custom()
    ops[[key]] <- df
    rv_opskrifter_custom(ops)

    persist_recipe(key)

    hide(id = "popup_opskrift_slet_bekraeft", anim = TRUE, animType = "fade")
    rv_recipeDeleteState$key <- NULL
    rv_recipeDeleteState$row <- NULL

    showNotification(sprintf('Linjen "%s" er slettet permanent.', slettet), type = "message")
  })

  observeEvent(input$cancel_delete_opskrift_row, {
    hide(id = "popup_opskrift_slet_bekraeft", anim = TRUE, animType = "fade")
    rv_recipeDeleteState$key <- NULL
    rv_recipeDeleteState$row <- NULL
  })

  observeEvent(input$opskrift_archivePressed, {
    info <- input$opskrift_archivePressed
    req(!is.null(info$key))

    key <- as.character(info$key)
    active <- rv_retter_custom()
    req(key %in% active$key)

    ret_navn <- active$retter[match(key, active$key)]
    rv_recipeArchiveState$key <- key

    output$ret_delete_context <- renderText({
      sprintf('Er du sikker paa, at du vil arkivere "%s"?', ret_navn)
    })

    show(id = "popup_ret_slet_bekraeft", anim = TRUE, animType = "fade")
  })

  observeEvent(input$confirm_delete_ret, {
    key <- rv_recipeArchiveState$key
    req(!is.null(key))

    active <- rv_retter_custom()
    row_idx <- match(key, active$key)
    req(!is.na(row_idx))

    archived_row <- active[row_idx, , drop = FALSE]
    active_new <- active[-row_idx, , drop = FALSE] |>
      arrange(retter)
    archive_new <- bind_rows(rv_retter_arkiv(), archived_row) |>
      distinct(key, .keep_all = TRUE) |>
      arrange(retter)

    rv_retter_custom(active_new)
    rv_retter_arkiv(archive_new)
    persist_retter(active_new)
    persist_retter_arkiv(archive_new)

    hide(id = "popup_ret_slet_bekraeft", anim = TRUE, animType = "fade")
    rv_recipeArchiveState$key <- NULL

    valid_active_new <- active_recipe_rows(active_new)
    valid_choices <- opskrift_choices(valid_active_new)

    updateSelectInput(session = session, inputId = "ret", choices = c("", valid_active_new$retter), selected = "")
    updateSelectizeInput(
      session,
      "opskrift_valgt_key",
      choices = valid_choices,
      selected = if (length(valid_choices) > 0) unname(valid_choices[[1]]) else character(0),
      options = opskrift_selectize_options
    )

    showNotification(sprintf('Retten "%s" er flyttet til arkivet.', archived_row$retter[[1]]), type = "message")
  })

  observeEvent(input$cancel_delete_ret, {
    hide(id = "popup_ret_slet_bekraeft", anim = TRUE, animType = "fade")
    rv_recipeArchiveState$key <- NULL
  })

  observeEvent(input$restore_ret, {
    key <- as.character(input$restore_ret %||% "")
    req(nzchar(key))

    archive <- rv_retter_arkiv()
    row_idx <- match(key, archive$key)
    req(!is.na(row_idx))

    if (!key %in% names(rv_opskrifter_custom())) {
      showNotification("Opskriftsfilen mangler, saa retten kan ikke gendannes.", type = "error")
      return(invisible(NULL))
    }

    restored_row <- archive[row_idx, , drop = FALSE]
    active_new <- bind_rows(rv_retter_custom(), restored_row) |>
      distinct(key, .keep_all = TRUE) |>
      arrange(retter)
    archive_new <- archive[-row_idx, , drop = FALSE] |>
      arrange(retter)

    rv_retter_custom(active_new)
    rv_retter_arkiv(archive_new)
    persist_retter(active_new)
    persist_retter_arkiv(archive_new)

    valid_active_new <- active_recipe_rows(active_new)

    updateSelectInput(session = session, inputId = "ret", choices = c("", valid_active_new$retter), selected = "")
    updateSelectizeInput(
      session,
      "opskrift_valgt_key",
      choices = opskrift_choices(valid_active_new),
      selected = key,
      options = opskrift_selectize_options
    )

    showNotification(sprintf('Retten "%s" er gendannet.', restored_row$retter[[1]]), type = "message")
  })
  
  # dynamisk visning af én valgt opskrift i fanen "Opskrifter" ----
  observeEvent(input$delete_archived_ret, {
    key <- as.character(input$delete_archived_ret %||% "")
    req(nzchar(key))

    archive <- rv_retter_arkiv()
    row_idx <- match(key, archive$key)
    req(!is.na(row_idx))

    ret_navn <- archive$retter[[row_idx]]
    rv_recipePermanentDeleteState$key <- key

    output$ret_permanent_delete_context <- renderText({
      sprintf('Er du sikker paa, at du vil slette "%s" permanent?', ret_navn)
    })

    show(id = "popup_ret_slet_permanent_bekraeft", anim = TRUE, animType = "fade")
  })

  observeEvent(input$confirm_delete_archived_ret, {
    key <- rv_recipePermanentDeleteState$key
    req(!is.null(key), nzchar(key))

    archive <- rv_retter_arkiv()
    row_idx <- match(key, archive$key)
    req(!is.na(row_idx))

    deleted_row <- archive[row_idx, , drop = FALSE]
    ret_navn <- deleted_row$retter[[1]]
    recipe_file <- file.path("./data/opskrifter", paste0(key, ".txt"))

    if (file.exists(recipe_file) && !file.remove(recipe_file)) {
      showNotification("Opskriftsfilen kunne ikke slettes.", type = "error")
      return(invisible(NULL))
    }

    archive_new <- archive[-row_idx, , drop = FALSE] |>
      arrange(retter)

    ops <- rv_opskrifter_custom()
    ops[[key]] <- NULL

    links_new <- rv_links_custom() |>
      filter(ret != ret_navn) |>
      arrange(ret)

    rv_retter_arkiv(archive_new)
    rv_opskrifter_custom(ops)
    rv_links_custom(links_new)
    persist_retter_arkiv(archive_new)
    persist_links(links_new)

    hide(id = "popup_ret_slet_permanent_bekraeft", anim = TRUE, animType = "fade")
    rv_recipePermanentDeleteState$key <- NULL

    showNotification(sprintf('Retten "%s" er slettet permanent.', ret_navn), type = "message")
  })

  observeEvent(input$cancel_delete_archived_ret, {
    hide(id = "popup_ret_slet_permanent_bekraeft", anim = TRUE, animType = "fade")
    rv_recipePermanentDeleteState$key <- NULL
  })

  output$opskrifter_ui <- renderUI({
    ops_local <- rv_opskrifter_custom()
    active_retter <- active_recipe_rows(rv_retter_custom(), names(ops_local))

    keys <- active_retter$key
    titler <- active_retter$retter
    archive <- rv_retter_arkiv()

    archive_ui <- NULL
    if (nrow(archive) > 0) {
      archive_ui <- f7Block(
        inset = TRUE,
        strong = TRUE,
        tags$h3("Arkiv"),
        tags$p("Slettede retter ligger her og kan gendannes."),
        tagList(lapply(seq_len(nrow(archive)), function(i) {
          key <- archive$key[[i]]
          tags$div(
            class = "archive-recipe-row",
            tags$span(archive$retter[[i]]),
            tags$div(
              class = "archive-recipe-actions",
              ga_js_button(
                inputId = paste0("restore_ret_btn_", key),
                label = "Gendan",
                class = "archive-action-btn archive-action-restore",
                onclick = sprintf(
                  "Shiny.setInputValue('restore_ret', '%s', {priority:'event'}); return false;",
                  key
                )
              ),
              ga_js_button(
                inputId = paste0("delete_archived_ret_btn_", key),
                label = "Slet permanent",
                class = "archive-action-btn archive-action-delete",
                onclick = sprintf(
                  "Shiny.setInputValue('delete_archived_ret', '%s', {priority:'event'}); return false;",
                  key
                )
              )
            )
          )
        }))
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

    # isolate() forhindrer at selve skrivning i feltet triggere renderUI-genopbygning
    # (som ellers nulstiller tekst/søgetilstand i selectize-kontrollen).
    valgt <- isolate(input$opskrift_valgt_key)
    if (is.null(valgt) || !valgt %in% keys) valgt <- keys[1]

    tagList(
      f7Block(
        inset = TRUE,
        strong = TRUE,
        selectizeInput(
          "opskrift_valgt_key",
          "Vælg opskrift",
          choices = stats::setNames(keys, titler),
          selected = valgt,
          width = "100%",
          options = opskrift_selectize_options
        )
      ),
      uiOutput("valgt_opskrift_ui"),
      archive_ui
    )
  })

  output$valgt_opskrift_ui <- renderUI({
    key <- input$opskrift_valgt_key
    req(!is.null(key))

    req(key %in% active_recipe_rows(rv_retter_custom())$key)

    ops_local <- rv_opskrifter_custom()
    req(key %in% names(ops_local))

    df <- ops_local[[key]]
    ret_navn <- names(df)[1]

    links_df <- rv_links_custom()
    link_url <- links_df$link[links_df$ret == ret_navn]
    link_url <- if (length(link_url) > 0) link_url[1] else ""
    link_url <- normalize_recipe_link(link_url)

    ingredienslinje <- format_recipe_line(df$maengde, df$enhed, df[[ret_navn]])
    df_vis <- data.frame(
      Ingrediens = htmltools::htmlEscape(ingredienslinje),
      check.names = FALSE
    )

    edit_col <- vapply(
      seq_len(nrow(df_vis)),
      function(r) {
        as.character(
          ga_js_button(
            inputId = paste0("opskrift_row_btn_", key, "_", r),
            label = NULL,
            icon = icon("pen"),
            class = "edit-btn btn btn-sm",
            onclick = sprintf(
              "Shiny.setInputValue('opskrift_editPressed', {key: '%s', row: %d}, {priority:'event'}); return false;",
              key,
              r
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
      },
      ""
    )

    delete_col <- vapply(
      seq_len(nrow(df_vis)),
      function(r) {
        as.character(
          ga_js_button(
            inputId = paste0("opskrift_row_del_", key, "_", r),
            label = NULL,
            icon = icon("trash"),
            class = "delete-btn btn btn-sm",
            onclick = sprintf(
              "Shiny.setInputValue('opskrift_deletePressed', {key: '%s', row: %d}, {priority:'event'}); return false;",
              key,
              r
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
      },
      ""
    )

    df_vis$Rediger <- edit_col
    df_vis$Slet <- delete_col

    output$opskrift_tbl_valgt <- DT::renderDT({
      themed_dt(
        df_vis,
        escape = c(FALSE, FALSE, FALSE),
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          searching = FALSE
        )
      )
    })

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

    tags$div(
      id = paste0("opskrift_", key),
      class = "opskrift-anchor",
      f7Block(
        inset = TRUE,
        strong = TRUE,
        tags$h3(ret_navn),
        tags$div(
          class = "recipe-action-bar",
          ga_js_button(
            inputId = paste0("opskrift_add_btn_", key),
            label = "Tilføj vare",
            class = "recipe-action-btn recipe-action-add",
            onclick = sprintf(
              "Shiny.setInputValue('opskrift_addPressed', {key: '%s'}, {priority:'event'}); return false;",
              key
            )
          ),
          ga_js_button(
            inputId = paste0("opskrift_archive_btn_", key),
            label = "Arkiver ret",
            class = "recipe-action-btn recipe-action-archive",
            onclick = sprintf(
              "Shiny.setInputValue('opskrift_archivePressed', {key: '%s'}, {priority:'event'}); return false;",
              key
            )
          )
        ),
        br(),
        DT::DTOutput("opskrift_tbl_valgt"),
        link_tag
      )
    )
  })
  

}

shinyApp(ui = ui, server = server)
