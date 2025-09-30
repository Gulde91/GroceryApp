# ShinyMobile-skal for GroceryApp
library(shiny)
library(shinyMobile)
library(readr)
library(dplyr)
library(purrr)

source("./data.R")
source("./funktioner.R")


ui <- f7Page(
  
  # bruges til styling af input-knapper
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  title = "IndkøbsApp",
  options = list(theme = "auto"),
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
        active = FALSE,
        f7BlockTitle(title = "Din indkøbsseddel"),
        DT::DTOutput("indkøbsseddel"),
        f7Block(inset = TRUE, strong = TRUE,
          f7Button("clear_list", "Ryd indkøbssedlen", color = "red", fill = TRUE)
        )
      ),

      # tilføj varer fra liste
      f7Tab(
        tabName = "Varer",
        icon = f7Icon("cube"),
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

      # tilføj varer fra opskrift
      f7Tab(
        tabName = "Opskrifter",
        icon = f7Icon("book"),
        active = TRUE,
        f7BlockTitle(title = "Vælg opskrift"),
        f7Block(
          inset = TRUE, strong = TRUE,
          sInput("ret", "Vælg ret", c("", retter$retter)),
          br(),
          nInput("pers", "Vælg antal personer", value = 2),
          br(),
          sInput("salat", "Vælg salat", c("", salater$retter)),
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
    )
  )
)

server <- function(input, output, session) {

  # sætter reaktive værdier
  rv_indk_liste <- reactiveValues(df = NULL)
  
  rv_opskrift_tmp <- reactiveValues(df = NULL)
  rv_opskrift_all <- reactiveValues(df = NULL)

  rv_indkobsseddel_samlet <- reactiveValues(df = NULL)
  
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

      # output$indkøbsseddel <- DT::renderDT({
      #   themed_dt(
      #     indkob,
      #     options = list(
      #       dom = "t",
      #       ordering = FALSE,
      #       pageLength = nrow(indkob)
      #       )
      #   )
      # })
    }
  })
  
  # konstruerer "slet-knap" kolonne
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
  
  
  # udstiller indk
  output$indkøbsseddel <- DT::renderDT(server = FALSE, {
    
    page_len <- ifelse(is.null(rv_indkobsseddel_samlet$df), 1,
                ifelse(any(rv_indkobsseddel_samlet$df[["Indk\u00F8bsliste"]] == ""),
                       which(rv_indkobsseddel_samlet$df[["Indk\u00F8bsliste"]] == "")[1] - 1,
                       nrow(rv_indkobsseddel_samlet$df)))
    
    themed_dt(cbind(rv_indkobsseddel_samlet$df, delete = deleteCol()),
              colnames = NULL, extensions = "Buttons",
              escape = FALSE,
              editable = TRUE,
              options = list(
                dom = "B", ordering = FALSE, pageLength = page_len,
                buttons = list(
                  list(extend = "copy",
                       title = NULL,
                       exportOptions = list(columns = 0)
                  )
                ),
                # Disable sorting for the delete column
                columnDefs = list(
                  list(targets = 1, sortable = FALSE)
                )
              )
    )
  })
  
  
}

shinyApp(ui = ui, server = server)