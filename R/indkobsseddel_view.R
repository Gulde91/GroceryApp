# Visningsbyggere for indkøbssedlen ---------------------------------------
#
# Funktionerne i denne fil omsætter allerede beregnede data til DT-widgets.
# De registrerer ingen outputs og læser hverken input eller reaktiv state.

library(htmltools)
library(DT)
library(shiny)

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
  category_break_after <- as.integer(payload$category_break_after)

  if (length(n_visible) != 1L || is.na(n_visible) || n_visible < 0L) {
    stop("Cartens copy-payload har et ugyldigt rækkeantal.", call. = FALSE)
  }
  if (
    length(lines_visible) != n_visible ||
      length(line_ids) != n_visible
  ) {
    stop("Cartens copy-payload er inkonsistent.", call. = FALSE)
  }
  valid_category_breaks <- length(category_break_after) == 0L ||
    (
      !anyNA(category_break_after) &&
        all(category_break_after > 0L) &&
        all(category_break_after < n_visible) &&
        identical(
          category_break_after,
          sort(unique(category_break_after))
        )
    )
  if (!isTRUE(valid_category_breaks)) {
    stop("Cartens kategoriskift til kopiering er ugyldige.", call. = FALSE)
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
            modifier = list(page = "all"),
            customizeData = indkobsseddel_copy_group_customizer(
              category_break_after
            )
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

#' Indsæt blanklinjer mellem hovedkategorier ved kopiering
#'
#' Funktionen bygger en lille JavaScript-tilpasning til DataTables. Den føjer
#' et ekstra linjeskift til den sidste eksporterede vare i hver kategori 1-
#' gruppe. Dermed ændres kun teksten i udklipsholderen; tabellen og den gemte
#' indkøbshistorik beholder deres oprindelige rækker.
#'
#' @param category_break_after Énbaserede rækkenumre, som afslutter en
#'   kategori 1-gruppe.
#'
#' @return JavaScript, som tilpasser DataTables' eksporterede tabeldata.
#' @keywords internal
indkobsseddel_copy_group_customizer <- function(
  category_break_after
) {
  zero_based_rows <- as.integer(category_break_after) - 1L
  rows_javascript <- paste(zero_based_rows, collapse = ",")

  JS(paste0(
    "function(data) {",
    "[", rows_javascript, "].forEach(function(rowIndex) {",
    "if (data.body[rowIndex] && data.body[rowIndex][0] !== undefined) {",
    "data.body[rowIndex][0] = data.body[rowIndex][0] + '\\n';",
    "}",
    "});",
    "}"
  ))
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
