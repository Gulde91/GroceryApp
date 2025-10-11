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
        f7BlockTitle(title = "Din indkøbsseddel"),
        DT::DTOutput("indkobsseddel"),
        br(),
        f7Button("gem_indkobsseddel", "Gem indkøbssedlen", fill = TRUE, color = "blue"),
        h5(strong("Forslag til manglende varer:")),
        tableOutput("tidl_kob")
      ),

      # tilføj varer fra liste
      f7Tab(
        tabName = "Varer",
        icon = f7Icon("square_list"),
        f7BlockTitle(title = "Vælg vare, mængde og enhed"),
        f7Block(inset = TRUE, strong = TRUE,
          sInput("basis_varer", "Tilf\u00F8j varer fra liste", sort(varer$Indkobsliste)),
          br(),
          nInput("antal_basis_varer", "M\u00E6ngde", value = 1),
          br(),
          sInput("enhed_alle_varer", "Enhed", "", "stk"),
          br(),
          f7Button("add_varer", "Tilføj til indkøbssedlen", fill = TRUE, color = "green")
        )
      ),

      # tilføjer varer manuelt
      f7Tab(
        tabName = "Varer_manuel",
        icon = f7Icon("cube"),
        f7BlockTitle(title = "Tilføj vare, mængde, enhed og kategori"),
        f7Block(
          inset = TRUE, strong = TRUE,
          tInput("basis_varer_manuel", label = "Tilf\u00F8j varer manuelt"),
          br(),
          nInput("antal_basis_varer_manuel", "M\u00E6ngde", value = 1),
          br(),
          sInput("enhed_basis_varer_manuel", "Enhed", "", "stk"),
          br(),
          sInput("add_kat_1", "Kategori 1", kategori_1, "konserves"),
          br(),
          sInput("add_kat_2", "Kategori 2", kategori_2, "konserves"),
          br(),
          f7Button("add_varer_manuel", "Tilføj til indkøbssedlen", fill = TRUE, color = "green"),
          br(),
          f7Button("gem_vare", "Gem", fill = TRUE, color = "blue")
        )
      ),
      
      # tilføj varer fra opskrift
      f7Tab(
        tabName = "Opskrifter",
        icon = f7Icon("book"),
        active = FALSE,
        f7BlockTitle(title = "Vælg opskrift"),
        f7Block(
          inset = TRUE, strong = TRUE,
          sInput("ret", "Vælg ret", c("", retter$retter)),
          br(),
          nInput("pers", "Vælg antal personer", value = 2),
          br(),
          sInput("salat", "Vælg salat", salater$retter),
          br(),
          # TODO virker ikke med multiple = TRUE
          sInput("tilbehor", "Vælg tilbehør", c("", tilbehor$Indkobsliste)),
          br(),
          f7Button("add_opskrift", "Tilføj til indkøbssedlen", fill = TRUE, color = "green"),
          br()
        ),
        DT::DTOutput("opskrift")
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
    
    # Custom "modal" (overlay) – skjult til at starte med
    tags$div(
      id = "edit-overlay",
      tags$div(
        id = "edit-dialog",
        tags$h3("Redigér tekst"),
        textInput("indkobsseddel_edit_value", label = NULL, value = "", width = "100%"),
        tags$div(
          id = "edit-actions",
          actionButton("cancel_edit", "Annullér", class = "btn-flat"),
          actionButton("confirm_edit", "Gem",      class = "btn-flat btn-save")
        )
      )
    )
  ),
  uiOutput("edit_popup_ui")
)

server <- function(input, output, session) {

  # sætter reaktive værdier
  rv_indk_liste <- reactiveValues(df = NULL)
  rv_opskrift_tmp <- reactiveValues(df = NULL)
  rv_opskrift_all <- reactiveValues(df = NULL)
  rv_indkobsseddel_samlet <- reactiveValues(df = NULL)
  rv_manuel_tilfoj <- reactiveValues(df = NULL)
  
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
    
  })

  # Tilføj varer fra liste ----
  
  # viser enhed på valgt vare
  observe({ 
    
    updateSelectInput(
      session = session,
      inputId = "enhed_alle_varer",
      choices = sort(setdiff(unique(varer$enhed), "")),
      selected = varer[varer$Indkobsliste == input$basis_varer, ]$enhed
    )
  })
  
  # mulighed for at tilføje varer
  observeEvent(input$add_varer, {
    
    if (input$basis_varer != "V\u00E6lg vare") {
      varer_tmp <- varer[varer$Indkobsliste == input$basis_varer, ]
      varer_tmp$maengde <- varer_tmp$maengde * input$antal_basis_varer
      varer_tmp$enhed <- input$enhed_alle_varer
      
      cat(input$basis_varer, "er tilføjet!\n")
      rv_indk_liste$df <- bind_rows(rv_indk_liste$df, varer_tmp)
    }
    
  })
  
  # Tilføj varer manuel ----
  observe({ 
    
    updateSelectInput(
      session = session,
      inputId = "enhed_basis_varer_manuel",
      choices = sort(setdiff(unique(varer$enhed), "")),
      selected = varer[varer$Indkobsliste == input$basis_varer, ]$enhed
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
    varer_custom_new <- bind_rows(varer_custom, rv_manuel_tilfoj$df)
    
    write.csv(
      varer_custom_new, 
      file = "./data/basis_varer.txt", 
      row.names = FALSE,
      fileEncoding = "UTF-8"
      )
    
    message(input$basis_varer_manuel, " er nu gemt i basis_varer.txt")
    
  })
  
  
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
  
  # konstruerer "slet-knap" kolonne ----
  deleteCol <- reactive({
    if (!is.null(rv_indkobsseddel_samlet$df)) {
      unlist(lapply(seq_len(nrow(rv_indkobsseddel_samlet$df)), add_slet_knap))
    }
  })
  
  # mulighed for at slette række
  observeEvent(input$deletePressed, {
    rowNum <- parse_delete_event(input$deletePressed)
    rv_indkobsseddel_samlet$df <- rv_indkobsseddel_samlet$df[-rowNum,]
  })
  
  # konstruerer "rediger-knap" ----
  editCol <- reactive({
    df <- rv_indkobsseddel_samlet$df
    if (is.null(df) || nrow(df) == 0) return(character())
    ga_make_edit_buttons(n = nrow(df), table_id = "indkobsseddel")
  })
  
  # Gemmer aktuel rækkenummer der redigeres
  editRow <- reactiveVal(NULL)
  
  # Åbn overlay når der klikkes på Redigér-knap i tabellen
  observeEvent(input$indkobsseddel_editPressed, ignoreInit = TRUE, {
    r <- suppressWarnings(as.integer(input$indkobsseddel_editPressed))
    req(!is.na(r))
    
    df <- rv_indkobsseddel_samlet$df
    req(!is.null(df), nrow(df) >= r)
    
    editRow(r)
    updateTextInput(session, "indkobsseddel_edit_value",
                    value = df[r, 1, drop = TRUE])  # kolonne 1 = tekstkolonnen
    
    show(id = "edit-overlay", anim = TRUE, animType = "fade")
  })
  
  # Gem ændringen og luk overlay
  observeEvent(input$confirm_edit, {
    r <- editRow(); req(r)
    val <- input$indkobsseddel_edit_value
    
    df <- rv_indkobsseddel_samlet$df
    req(!is.null(df), nrow(df) >= r)
    
    df[r, 1] <- val
    rv_indkobsseddel_samlet$df <- df
    
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
                    text   = "Kopiér",
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
  tidl_kob <- mest_brugte_varer(c(varer$enhed, varer_custom$enhed))
  
  observe({
    
    if (!is.null(rv_indkobsseddel_samlet$df)) {
      paa_listen <- medtag_kun_varer(rv_indkobsseddel_samlet$df)
      paa_listen <- rens_varer(
        paa_listen$Indkøbsliste,
        c(varer$enhed, varer_custom$enhed)
      )
      
      tidl_kob_out <- tidl_kob[!tidl_kob$Indkøbsliste %in% paa_listen, ] |> slice(1:10)
      
      output$tidl_kob <- renderTable(
        tidl_kob_out,
        colnames = FALSE
      )
    }
    
  })
  
}

shinyApp(ui = ui, server = server)