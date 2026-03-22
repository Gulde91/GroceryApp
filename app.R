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


ui <- f7Page(
  
  # opsætning ----
  tags$head(
    # tags$meta(
    #   name = "viewport",
    #   content = "width=device-width, initial-scale=1, viewport-fit=cover"
    # ),
    # tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    # tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "black-translucent"),
    # tags$meta(name = "mobile-web-app-capable", content = "yes"),
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
          tags$p("Du kan redigere og slette ingredienslinjer direkte. Ændringer gemmes automatisk.")
          ,
          f7Button(
            "open_ny_opskrift",
            "Ny opskrift",
            icon = f7Icon("plus"),
            fill = TRUE,
            color = "green"
          )
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
                 sInput("ret", "Vælg ret", c("", retter$retter)),
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
    # POPUP: ny opskrift
    tags$div(
      id = "popup_ny_opskrift", class = "ga-modal",
      tags$div(class = "ga-dialog",
               tags$h3("Ny opskrift"),
               f7Block(
                 inset = TRUE, strong = TRUE,
                 tInput("ny_opskrift_titel", "Titel"),
                 tInput("ny_opskrift_link", "Link (valgfrit)"),
                 textAreaInput(
                   "ny_opskrift_ingredienser",
                   "Ingredienser (én linje pr. ingrediens)",
                   width = "100%",
                   rows = 8,
                   placeholder = paste(
                     "Format pr. linje: ingrediens;maengde;enhed;kat_1;kat_2",
                     "Eksempel: hakket oksekød;0.175;kg;kød;",
                     sep = "\n"
                   )
                 ),
                 textAreaInput(
                   "ny_opskrift_fremgangsmaade",
                   "Kort fremgangsmåde (valgfri)",
                   width = "100%",
                   rows = 4
                 ),
                 br(),
                 f7Button("save_ny_opskrift", "Gem", fill = TRUE, color = "blue"),
                 br(),
                 f7Button("close_ny_opskrift", "Annuller", fill = TRUE, color = "gray")
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
  rv_indk_liste <- reactiveValues(df = NULL)
  rv_opskrift_tmp <- reactiveValues(df = NULL)
  rv_opskrift_all <- reactiveValues(df = NULL)
  rv_indkobsseddel_samlet <- reactiveValues(df = NULL)
  rv_manuel_tilfoj <- reactiveValues(df = NULL)
  rv_valgte_opskrifter <- reactiveValues(items = list())
  rv_opskrifter_custom <- reactiveVal(opskrifter)
  rv_links_custom <- reactiveVal(links)
  rv_recipeEditState <- reactiveValues(key = NULL, row = NULL)
  rv_recipeDeleteState <- reactiveValues(key = NULL, row = NULL)

  rv_varer_custom <- reactiveVal(
    read.csv("./data/basis_varer.txt", fileEncoding = "UTF-8") |> 
      arrange(Indkobsliste)
    )
  
  # Én sandhed om hvad der redigeres (tabel + række) til brug for "Gem" i fælles overlay
  rv_editState <- reactiveValues(table = NULL, row = NULL)
  
  # indlæser basis varer ved genload af appen
  session$onFlushed(function() {
    rv_varer_custom(read.csv("./data/basis_varer.txt", fileEncoding = "UTF-8"))
  }, once = TRUE)
  
  
  # laves som reactive (og ikke reactiveVal) fordi der ikke kan indgå
  # reactive elementer i en reactiveVal
  rv_varer <- reactive({
    opskrift_df_custom <- c(rv_opskrifter_custom(), salater_opskrifter) |>
      lapply(function(x) {names(x)[1] <- "Indkobsliste"; x}) |>
      dplyr::bind_rows() |>
      dplyr::arrange(Indkobsliste) |>
      dplyr::mutate(maengde = 1) |>
      dplyr::distinct()

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
      opskrifter, retter, salater, salater_opskrifter, tilbehor,
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
    
    col_names <- c("Indkobsliste", "maengde", "enhed", "kat_1", "kat_2")
    names(rv_opskrift_tmp$df) <- col_names
    
    rv_opskrift_all$df <- bind_rows(rv_opskrift_all$df, rv_opskrift_tmp$df)
    rv_opskrift_tmp$df <- NULL

    # byg rv_valgte_opskrifter (ret + evt. salat; ignorér tilbehør hvis der er ret)
    har_ret <- !is.null(input$ret) && nzchar(input$ret)
    har_salat <- !is.null(input$salat) && nzchar(input$salat)
    har_tilh <- !is.null(input$tilbehor) && nzchar(input$tilbehor)
    
    if (har_ret) {
      df_ret <- get_df(ret = input$ret, pers = input$pers)
      if (har_salat) {
        df_sal <- get_df(salat = input$salat, pers = input$pers)
        df_merged <- bind_rows(df_ret, df_sal)
        title <- paste0(input$ret, " m. ", input$salat)
        link  <- get_link(input$ret) %||% get_link(input$salat)
      } else {
        df_merged <- df_ret
        title <- input$ret
        link  <- get_link(input$ret)
      }
      rv_valgte_opskrifter$items <- c(
        rv_valgte_opskrifter$items,
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
        df_sal <- get_df(salat = input$salat, pers = input$pers)
        rv_valgte_opskrifter$items <- c(
          rv_valgte_opskrifter$items,
          list(list(
            title = paste0("Salat: ", input$salat),
            pers  = input$pers,
            df    = df_sal,
            link  = get_link(input$salat)
          ))
        )
      }
      if (har_tilh) {
        df_til <- get_df(tilbeh = input$tilbehor, pers = input$pers)
        rv_valgte_opskrifter$items <- c(
          rv_valgte_opskrifter$items,
          list(list(
            title = paste0("Tilbehør: ", input$tilbehor),
            pers  = input$pers,
            df    = df_til,
            link  = NA_character_
          ))
        )
      }
    }
    
    hide(id = "popup_opskrift", anim = TRUE, animType = "fade")
    
  })

  observeEvent(input$open_opskrift, {
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
      rv_indk_liste$df <- bind_rows(rv_indk_liste$df, varer_tmp)
      
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
    
    rv_indk_liste$df <- bind_rows(rv_indk_liste$df, varer_manuel_tmp)
    
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
  
  
  # binder hele indkøbslisten ----
  # sætter indkøbslisten
  observe({
  
    if (!is.null(rv_indk_liste$df) | !is.null(rv_opskrift_all$df)) {
      
      indkob <- bind_rows(rv_indk_liste$df, rv_opskrift_all$df)
      
      # summerer indkøb
      indkob <- indkob %>%
        group_by(Indkobsliste, enhed, kat_1, kat_2) %>%
        summarise(maengde = sum(maengde, na.rm = TRUE), .groups = "drop") %>%
        sort_by_cat(first = c("frugt og grønt", "konserves"),
                    last = c("husholdning"))

      # runder op
      rund_op <- c("stk ", "d\u00E5se(r)", "pakke(r)", "rulle(r)")
      indkob$maengde <- ifelse(indkob$enhed %in% rund_op,
                               ceiling(indkob$maengde), indkob$maengde)

      indkob$Indkobsliste <- paste(indkob$maengde, indkob$enhed, indkob$Indkobsliste)
      indkob$Indkobsliste <- gsub("NA", "", indkob$Indkobsliste) %>% trimws()
      indkob <- indkob[, "Indkobsliste"]

      names(indkob) <- "Indk\u00F8bsliste"
      
      rv_indkobsseddel_samlet$df <- indkob

    }
  })
  
  # tilføjer opskrift + link 
  combined_lines <- reactive({
    # Synlige linjer = kun indkøbsvarer (før evt. tom-separator)
    vis_df <- rv_indkobsseddel_samlet$df
    vis <- character()
    
    if (!is.null(vis_df) && nrow(vis_df) > 0) {
      v <- vis_df[[1]]
      v <- v[nzchar(v)]
      vis <- v
    }
    
    n_visible <- length(vis)
    
    secs <- character()
    if (!is.null(rv_valgte_opskrifter) && length(rv_valgte_opskrifter$items) > 0) {
      for (it in rv_valgte_opskrifter$items) {
        secs <- c(secs, "", sprintf("%s (til %s pers.)", it$title, it$pers))
        if (!is.null(it$df) && nrow(it$df) > 0) {
          ing <- apply(it$df, 1, function(r){
            m <- r[["maengde"]]
            e <- r[["enhed"]]
            n <- r[["Indkobsliste"]]
            if (!is.na(m) && nzchar(as.character(m))) {
              paste0(m, if (nzchar(e)) paste0(" ", e) else "", " ", n)
            } else n
          })
          secs <- c(secs, ing)
        }
        if (!is.null(it$link) && nzchar(it$link)) {
          secs <- c(secs, paste0("Link: ", it$link))
        }
      }
    }
    
    list(
      visible = vis, # det, der vises
      hidden = secs, # kun til copy
      n_visible = n_visible
    )
  })

  # konstruerer "slet-knap" kolonne til indkøbsseddel ----
  deleteCol <- reactive({
    if (!is.null(rv_indkobsseddel_samlet$df)) {
      unlist(lapply(seq_len(nrow(rv_indkobsseddel_samlet$df)), add_slet_knap))
    }
  })
  
  # mulighed for at slette række
  observeEvent(input$deletePressed, {
    res <- safe_delete_by_click(input$deletePressed, rv_indkobsseddel_samlet$df, label_col = 1)
    rv_indkobsseddel_samlet$df <- res$df
  })
  
  # konstruerer "rediger-knap" til indkøbsseddel ----
  editCol <- reactive({
    df <- rv_indkobsseddel_samlet$df
    if (is.null(df) || nrow(df) == 0) return(character())
    ga_make_edit_buttons(n = nrow(df), table_id = "indkobsseddel")
  })
  
  # Åbn overlay når der klikkes på Redigér-knap i tabellen
  observeEvent(input$indkobsseddel_editPressed, ignoreInit = TRUE, {
    r <- suppressWarnings(as.integer(input$indkobsseddel_editPressed))
    req(!is.na(r))
    
    df <- rv_indkobsseddel_samlet$df
    req(!is.null(df), nrow(df) >= r)
    
    # --- WHY: Fortæl fælles "Gem", at det er INDKØBSSEDDEL + hvilken række ---
    rv_editState$table <- "indkobsseddel"
    rv_editState$row <- r
    
    updateTextInput(session, "table_edit_value", value = df[r, 1, drop = TRUE])
    show(id = "edit-overlay", anim = TRUE, animType = "fade")
  })
  
  # Gem ændringen og luk overlay
  observeEvent(input$confirm_edit, {
    r   <- rv_editState$row
    tbl <- rv_editState$table
    req(!is.null(r), !is.null(tbl))
    
    val <- input$table_edit_value
    
    if (tbl == "indkobsseddel") {
      df <- rv_indkobsseddel_samlet$df
      req(nrow(df) >= r)
      df[r, 1] <- val
      rv_indkobsseddel_samlet$df <- df
      
    } else if (tbl == "varer") {
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
    hide(id = "edit-overlay", anim = TRUE, animType = "fade")
  })
  
  # Luk uden at gemme
  observeEvent(input$cancel_edit, {
    hide(id = "edit-overlay", anim = TRUE, animType = "fade")
  })


  # udstiller indkøbsseddel ----
  output$indkobsseddel <- DT::renderDT(server = FALSE, {
    
    payload <- combined_lines()
    lines_visible <- payload$visible
    lines_hidden  <- payload$hidden
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
      edit_btn <- ga_make_edit_buttons(n_visible, table_id = "indkobsseddel")
      del_btn  <- vapply(seq_len(n_visible), function(i) add_slet_knap(i), "")
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
  
  
  observe({

    if (!is.null(rv_indkobsseddel_samlet$df)) {
      paa_listen <- medtag_kun_varer(rv_indkobsseddel_samlet$df)
      paa_listen <- rens_varer(
        paa_listen$Indkøbsliste,
        c(rv_varer()$enhed, rv_varer_custom()$enhed)
      )

      tidl_kob_out <- tidl_kob()[!tidl_kob()$Indkøbsliste %in% paa_listen, ] |> slice(1:10)

      output$tidl_kob <- renderTable(
        tidl_kob_out,
        colnames = FALSE
      )
    }

  })
  
  ## Inspiration og statistik
  
  # wordcloud plot ----
  output$wordcloud_retter <- renderWordcloud2({
    
    retter_tmp <- retter
    
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

  make_recipe_key <- function(titel) {
    key <- iconv(titel, from = "UTF-8", to = "ASCII//TRANSLIT")
    key <- tolower(key)
    key <- gsub("[^a-z0-9]+", "_", key)
    key <- gsub("^_+|_+$", "", key)
    paste0(key, "_opskrift")
  }

  parse_recipe_lines <- function(txt) {
    linjer <- unlist(strsplit(txt %||% "", "\n", fixed = TRUE))
    linjer <- trimws(linjer)
    linjer <- linjer[nzchar(linjer)]

    if (length(linjer) == 0) {
      return(list(ok = FALSE, msg = "Tilføj mindst én ingredienslinje.", df = NULL))
    }

    parsed <- lapply(seq_along(linjer), function(i) {
      felter <- trimws(strsplit(linjer[[i]], ";", fixed = TRUE)[[1]])
      if (length(felter) != 5) {
        return(list(ok = FALSE, msg = sprintf("Linje %d har ikke 5 felter adskilt af ';'.", i)))
      }
      if (!nzchar(felter[[1]])) {
        return(list(ok = FALSE, msg = sprintf("Linje %d mangler ingrediensnavn.", i)))
      }

      maengde_chr <- gsub(",", ".", felter[[2]], fixed = TRUE)
      maengde <- suppressWarnings(as.numeric(maengde_chr))
      if (nzchar(maengde_chr) && is.na(maengde)) {
        return(list(ok = FALSE, msg = sprintf("Linje %d har ugyldig mængde.", i)))
      }

      list(
        ok = TRUE,
        row = data.frame(
          Indkobsliste = felter[[1]],
          maengde = if (nzchar(maengde_chr)) maengde else NA_real_,
          enhed = felter[[3]],
          kat_1 = felter[[4]],
          kat_2 = felter[[5]],
          stringsAsFactors = FALSE
        )
      )
    })

    fejl <- purrr::keep(parsed, ~ !isTRUE(.x$ok))
    if (length(fejl) > 0) {
      return(list(ok = FALSE, msg = fejl[[1]]$msg, df = NULL))
    }

    out <- bind_rows(purrr::map(parsed, "row"))
    list(ok = TRUE, msg = NULL, df = out)
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

  observeEvent(input$open_ny_opskrift, {
    updateTextInput(session, "ny_opskrift_titel", value = "")
    updateTextInput(session, "ny_opskrift_link", value = "")
    updateTextAreaInput(session, "ny_opskrift_ingredienser", value = "")
    updateTextAreaInput(session, "ny_opskrift_fremgangsmaade", value = "")
    show(id = "popup_ny_opskrift", anim = TRUE, animType = "fade")
  })

  observeEvent(input$close_ny_opskrift, {
    hide(id = "popup_ny_opskrift", anim = TRUE, animType = "fade")
  })

  observeEvent(input$save_ny_opskrift, {
    titel <- trimws(input$ny_opskrift_titel %||% "")
    link <- trimws(input$ny_opskrift_link %||% "")
    ingrediens_txt <- input$ny_opskrift_ingredienser %||% ""

    if (!nzchar(titel)) {
      showNotification("Titel er påkrævet.", type = "error")
      return(invisible(NULL))
    }

    parsed <- parse_recipe_lines(ingrediens_txt)
    if (!isTRUE(parsed$ok)) {
      showNotification(parsed$msg, type = "error")
      return(invisible(NULL))
    }

    eksisterende_titler <- vapply(rv_opskrifter_custom(), function(x) names(x)[1], "")
    if (tolower(titel) %in% tolower(eksisterende_titler)) {
      showNotification("Der findes allerede en opskrift med den titel.", type = "warning")
      return(invisible(NULL))
    }

    key <- make_recipe_key(titel)
    if (!nzchar(key) || key == "_opskrift") {
      showNotification("Kunne ikke danne gyldigt filnavn fra titlen.", type = "error")
      return(invisible(NULL))
    }

    if (key %in% names(rv_opskrifter_custom())) {
      showNotification("Filnavnet findes allerede. Vælg en anden titel.", type = "warning")
      return(invisible(NULL))
    }

    filsti <- file.path("./data/opskrifter", paste0(key, ".txt"))
    if (file.exists(filsti)) {
      showNotification("Der findes allerede en opskriftsfil med dette navn.", type = "warning")
      return(invisible(NULL))
    }

    ny_df <- parsed$df
    names(ny_df)[1] <- titel

    # Tjek at slutformat matcher eksisterende tekstfiler
    format_ok <- is.data.frame(ny_df) &&
      ncol(ny_df) == 5 &&
      identical(names(ny_df)[2:5], c("maengde", "enhed", "kat_1", "kat_2"))
    if (!isTRUE(format_ok)) {
      showNotification("Opskriften kunne ikke gemmes i korrekt filformat.", type = "error")
      return(invisible(NULL))
    }

    write.table(
      ny_df,
      file = filsti,
      sep = ";",
      row.names = FALSE,
      quote = FALSE,
      na = "",
      fileEncoding = "UTF-8"
    )

    ops <- rv_opskrifter_custom()
    ops[[key]] <- ny_df
    rv_opskrifter_custom(ops)

    if (nzchar(link)) {
      links_df <- rv_links_custom()
      if (!(tolower(titel) %in% tolower(links_df$ret))) {
        links_df <- bind_rows(
          links_df,
          data.frame(ret = titel, link = link, stringsAsFactors = FALSE)
        ) |>
          arrange(ret)
        rv_links_custom(links_df)

        write.table(
          links_df,
          file = "./data/links.txt",
          sep = ";",
          row.names = FALSE,
          quote = FALSE,
          fileEncoding = "UTF-8"
        )
      }
    }

    updateSelectInput(session, "opskrift_valgt_key", selected = key)
    hide(id = "popup_ny_opskrift", anim = TRUE, animType = "fade")
    showNotification("Ny opskrift er gemt.", type = "message")
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
  
  # dynamisk visning af én valgt opskrift i fanen "Opskrifter" ----
  output$opskrifter_ui <- renderUI({
    ops_local <- rv_opskrifter_custom()
    ops_sorted <- ops_local[order(vapply(ops_local, function(x) names(x)[1], ""))]

    keys <- names(ops_sorted)
    titler <- vapply(ops_sorted, function(df) names(df)[1], "")
    req(length(keys) > 0)

    valgt <- input$opskrift_valgt_key
    if (is.null(valgt) || !valgt %in% keys) valgt <- keys[1]

    tagList(
      f7Block(
        inset = TRUE,
        strong = TRUE,
        sInput(
          "opskrift_valgt_key",
          "Vælg opskrift",
          choices = stats::setNames(keys, titler),
          selected = valgt
        )
      ),
      uiOutput("valgt_opskrift_ui")
    )
  })

  output$valgt_opskrift_ui <- renderUI({
    key <- input$opskrift_valgt_key
    req(!is.null(key))

    ops_local <- rv_opskrifter_custom()
    req(key %in% names(ops_local))

    df <- ops_local[[key]]
    ret_navn <- names(df)[1]

    links_df <- rv_links_custom()
    link_url <- links_df$link[links_df$ret == ret_navn]
    link_url <- if (length(link_url) > 0) link_url[1] else ""

    ingredienslinje <- format_recipe_line(df$maengde, df$enhed, df[[ret_navn]])
    df_vis <- data.frame(
      Ingrediens = htmltools::htmlEscape(ingredienslinje),
      check.names = FALSE
    )

    edit_col <- vapply(
      seq_len(nrow(df_vis)),
      function(r) {
        as.character(
          actionButton(
            inputId = paste0("opskrift_row_btn_", key, "_", r),
            label = NULL,
            icon = icon("pen"),
            class = "edit-btn btn btn-sm",
            onclick = sprintf(
              "Shiny.setInputValue('opskrift_editPressed', {key: '%s', row: %d}, {priority:'event'}); return false;",
              key,
              r
            ),
            type = "button",
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
          actionButton(
            inputId = paste0("opskrift_row_del_", key, "_", r),
            label = NULL,
            icon = icon("trash"),
            class = "delete-btn btn btn-sm",
            onclick = sprintf(
              "Shiny.setInputValue('opskrift_deletePressed', {key: '%s', row: %d}, {priority:'event'}); return false;",
              key,
              r
            ),
            type = "button",
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
        DT::DTOutput("opskrift_tbl_valgt"),
        link_tag
      )
    )
  })
  

}

shinyApp(ui = ui, server = server)
