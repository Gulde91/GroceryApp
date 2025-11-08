# ShinyMobile-skal for GroceryApp
library(shiny)
library(shinyMobile)
library(readr)
library(dplyr)
library(purrr)
library(fontawesome)
library(shinyjs)

source("./data.R")
source("./funktioner.R")


ui <- f7Page(
  
  tags$head(
    includeCSS("www/styles.css"),
    fa_html_dependency(),
    htmltools::singleton(tags$script(src = "selectize-mobile.js")),
    htmltools::singleton(tags$script(src = "button-press.js"))
  ),
  
  useShinyjs(),

  title = "IndkøbsApp",
  options = list(
    theme = "auto",
    dark = TRUE
    ),
  
  f7TabLayout(
    navbar = f7Navbar(title = "IndkøbsApp"),
    f7Tabs(
      id = "main_tabs",
      animated = FALSE,
      swipeable = TRUE,
      # indkøbsliste
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
      # Varer (bruttoliste)
      f7Tab(
        tabName = "Varer",
        icon = f7Icon("square_list"),
        active = FALSE,
        f7BlockTitle(title = "Bruttoliste over varer"),
        #p("Her kan du se alle varer, der kan vælges fra. (Søg i feltet nedenfor)"),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          f7Button("open_ny_vare", "Tilføj ny vare", fill = TRUE, color = "green")
        ),
        DT::DTOutput("varer_tbl")
      ),
      # Inspiration
      f7Tab(
        tabName = "Inspiration",
        icon = f7Icon("sparkles"),
        f7BlockTitle(title = "Indhold kommer snart"),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          "Her kan du senere samle idéer og inspiration."
        )
      )
    ),
    
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
                       #f7Button("gem_vare", "Gem som basisvare", fill=TRUE, color="blue"),
                       #br(),
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
  
  rv_varer_custom <- reactiveVal(
    read.csv("./data/basis_varer.txt", fileEncoding = "UTF-8") |> 
      arrange(Indkobsliste)
    )
  
  # indlæser basis varer ved genload af appen
  session$onFlushed(function() {
    rv_varer_custom(read.csv("./data/basis_varer.txt", fileEncoding = "UTF-8"))
  }, once = TRUE)
  
  
  # laves som reactive (og ikke reactiveVal) fordi der ikke kan indgå
  # reactive ellementer i en reactiveVal
  rv_varer <- reactive({
    bind_rows(opskrift_df, rv_varer_custom()) |>
      arrange(Indkobsliste) |>
      mutate(maengde = 1) |>
      distinct()
  }) 
  
  # Reaktive inputs ----
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
  
  # Én sandhed om hvad der redigeres (tabel + række) til brug for "Gem" i fælles overlay
  rv_editState <- reactiveValues(table = NULL, row = NULL)
  
  
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
    
    # nulstiller inputfelter
    updateSelectInput(
      session = session,
      inputId = "ret",
      choices = c("", retter$retter)
    )
    
    updateSelectInput(
      session = session,
      inputId = "salat",
      choices = c("", salater$retter)
    )
    
    updateSelectInput(
      session = session,
      inputId = "tilbehor",
      choices = c("", tilbehor$Indkobsliste)
    )
    
    hide(id = "popup_opskrift", anim = TRUE, animType = "fade")
    
  })

  observeEvent(input$open_opskrift, {show(id = "popup_opskrift", anim = TRUE, animType = "fade")})
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
    
    if (input$basis_varer != "V\u00E6lg vare") {
      varer_tmp <- rv_varer()[rv_varer()$Indkobsliste == input$basis_varer, ]
      varer_tmp$maengde <- varer_tmp$maengde * input$antal_basis_varer
      varer_tmp$enhed <- input$enhed_alle_varer
      
      cat(input$basis_varer, "er tilføjet!\n")
      rv_indk_liste$df <- bind_rows(rv_indk_liste$df, varer_tmp)
    }
    
    hide(id = "popup_varer", anim = TRUE, animType = "fade")
    
  })
  
  observeEvent(input$open_varer, {show(id = "popup_varer", anim = TRUE, animType = "fade")})
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
  
  observeEvent(input$open_manuel, {show(id = "popup_manuel",   anim = TRUE, animType = "fade")})
  observeEvent(input$close_manuel, {hide(id = "popup_manuel",   anim = TRUE, animType = "fade")})
  
  
  # binder hele indkøbslisten ----
  observe({
  
    if (!is.null(rv_indk_liste$df) | !is.null(rv_opskrift_all$df)) {
      
      indkob <- bind_rows(rv_indk_liste$df, rv_opskrift_all$df)
      
      # summerer indkøb
      indkob <- indkob %>%
        group_by(Indkobsliste, enhed, kat_1, kat_2) %>%
        summarise(maengde = sum(maengde, na.rm = TRUE), .groups = "drop") %>%
        arrange(kat_1, kat_2)

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
    
    page_len <- ifelse(is.null(rv_indkobsseddel_samlet$df), 1,
                ifelse(any(rv_indkobsseddel_samlet$df[["Indk\u00F8bsliste"]] == ""),
                       which(rv_indkobsseddel_samlet$df[["Indk\u00F8bsliste"]] == "")[1] - 1,
                       nrow(rv_indkobsseddel_samlet$df)))
    
    themed_dt(cbind(
                rv_indkobsseddel_samlet$df, 
                edit   = editCol(),
                delete = deleteCol()
              ),
              colnames = NULL, 
              escape = FALSE,
              editable = TRUE,
              extensions = "Buttons",
              options = list(
                dom = "B", ordering = FALSE, pageLength = page_len,
                buttons = list(
                  list(
                    extend = "copy",
                    text   = "Kopiér indkøbslisten",
                    title  = NULL,
                    exportOptions = list(columns = 0), # kopierer kun den 1. kolonne
                    attr = list( # styler knap
                      style = paste(
                        "background:#22c55e;"
                        ,"color:#fff;"
                        ,"border:1px solid #16a34a;"
                        ,"border-radius:100px;"
                        ,"font-weight:500;"
                      )
                    )
                    
                  )
                ), # Disable sorting for the delete column
                columnDefs = list(
                  list(targets = 1, sortable = FALSE)
                ),
                language = list(
                  emptyTable  = "Ingen varer på indkøbslisten!"
                )
              )
    )
  })
  
  
  
  # gemmer indkøbsseddel ----
  observeEvent(input$gem_indkobsseddel, {
    
    df <- rv_indkobsseddel_samlet$df
    path <- paste0("./data/indkobssedler/indkobsseddel_", gsub("-", "", Sys.Date()), ".rda")
    save(df, file = path)
    
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
  
}

shinyApp(ui = ui, server = server)