#' Opskrift Funktion
#'
#' Beregner mængden af ingredienser for en given ret, salat
#' og tilbehør baseret på antal personer.
#'
#' @param opskrifter Liste over opskrifter.
#' @param retter Data frame med retter.
#' @param salater Data frame med salater.
#' @param salater_opskrifter Liste over salatopskrifter.
#' @param tilbehor Data frame med tilbehør.
#' @param dag_ret Valgt ret for dagen.
#' @param dag_salat Valgt salat for dagen.
#' @param antal Antal personer.
#' @param dag_tilbehor Valgt tilbehør for dagen.
#' @return En data frame med de samlede ingredienser for den
#' valgte ret, salat og tilbehør.
opskrift <- function(opskrifter, retter, salater, salater_opskrifter, tilbehor,
                     dag_ret, dag_salat, antal, dag_tilbehor) {

  if (dag_ret != "") {
    ret <- opskrifter[[retter$key[retter$retter == dag_ret]]]
    ret$maengde <- ret$maengde * antal
  } else {
    ret <- NULL
  }

  if (dag_tilbehor != "") {
    tilbehor_all <- filter(tilbehor, Indkobsliste %in% dag_tilbehor)
    tilbehor_all$maengde <- tilbehor_all$maengde * antal / length(dag_tilbehor)
    tilbehor_all$maengde <- round(tilbehor_all$maengde, 4)
    if(!is.null(ret)) names(tilbehor_all)[1] <-  names(ret)[1] # Hmm
  } else {
    tilbehor_all <- NULL
  }

  if (dag_salat != "") {
    salat <- salater_opskrifter[[salater$key[salater$retter == dag_salat]]]
    salat$maengde <- salat$maengde * antal
  } else {
    salat <- NULL
  }

  if (!is.null(ret) & !is.null(salat)) {
    name_ret <- paste(names(ret)[1], "m.", names(salat)[1])
    names(ret)[1] <- name_ret
    names(salat)[1] <- name_ret
    if (!is.null(tilbehor_all)) names(tilbehor_all)[1] <- name_ret
  }

  rbind(ret, tilbehor_all, salat)

}

#' Display Opskrift
#'
#' Viser opskriften i en datatabel.
#'
#' @param ret_opskr Data frame med opskriften.
#' @return En datatabel med opskriften.
display_opskrift <- function(ret_opskr) {

  if (!is.null(ret_opskr)) {
    ret_opskr[[1]] <- paste(ret_opskr$maengde, ret_opskr$enhed, ret_opskr[[1]])
    ret_opskr[[1]] <- gsub("NA", "", ret_opskr[[1]]) %>% trimws()
    ret_opskr <- ret_opskr[, 1]
  } else {
    ret_opskr <- NULL
  }

  DT::datatable(ret_opskr, rownames = NULL,
                options = list(dom = "t",
                               ordering = FALSE,
                               pageLength = nrow(ret_opskr)))
}

#' Parse Delete Event
#'
#' Ekstraherer ID fra en slet-knap event.
#'
#' @param idstr ID-strengen fra eventet.
#' @return Det numeriske ID, hvis det findes, ellers NA.
parse_delete_event <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (!is.na(res)) res
}

#' Add Slet Knap
#'
#' Tilføjer en slet-knap til UI.
#'
#' @param i Indeks for knappen.
#' @return HTML-kode for slet-knappen.
add_slet_knap <- function(i) {
  as.character(
    actionButton(
      paste("delete_button", i, sep = "_"),
      label = NULL,
      icon = icon("trash"), # viser ikke noget ikon
      onclick = 'Shiny.setInputValue(\"deletePressed\", this.id, {priority: "event"})',
      style = paste(
        "background:#ef4444;",
        "color:#fff;", # skriftfarve
        "border:1px solid #dc2626;",
        "border-radius:100px;", # afrundede hjørner
        "font-weight:600;",
        "padding:6px 1px;", # styrer højde og bredde på knappen
        "line-height:1;",
        "box-shadow:none;",
        "background-image:none;"
      )
    )
  )
}

#' Add Links
#'
#' Tilføjer links til retter baseret på en links data frame.
#'
#' @param retter Liste over retter.
#' @param links Data frame med links.
#' @return Opdateret liste over retter med links.
add_links <- function(retter, links) {

  stopifnot(is.list(retter), is.data.frame(links))

  retter_navne <- names(retter)

  retter <- mapply(x = retter, y = retter_navne, SIMPLIFY = FALSE,
    function(x, y) {
      y_kor <- stringr::str_replace(y, " m\\..+$", "")
      if (y_kor %in% links$ret) {
        x[nrow(x) + 1, 1] <- links$link[links$ret == y_kor]
      }
      return(x)
    }
  )

  names(retter) <- retter_navne

  return(retter)

}

#' Mest Brugte Varer
#'
#' Finder de mest brugte varer fra indkøbssedler.
#'
#' @param enheder Liste over enheder.
#' @return Data frame med de mest brugte varer.
mest_brugte_varer <- function(enheder) {

  files <- list.files("./data/indkobssedler/")

  varer <- lapply(files, find_varer) |> bind_rows()

  varer$Indkøbsliste <- sub("\\((tilsmagning|tilbehør)\\)", "", varer$Indkøbsliste)
  varer$Indkøbsliste <- sub("\\d+\\.*\\d*", "", varer$Indkøbsliste, perl = TRUE)

  enhed <- setdiff(unique(enheder), "")
  enhed <- paste0(enhed, collapse = "|")
  enhed <- gsub("\\(", "\\\\(", enhed)
  enhed <- gsub("\\)", "\\\\)", enhed)

  varer$Indkøbsliste <- sub(enhed, "", varer$Indkøbsliste, perl = TRUE)
  varer$Indkøbsliste <- trimws(varer$Indkøbsliste)

  out <- varer |>
    group_by(Indkøbsliste) |>
    summarise(count = n()) |>
    arrange(desc(count)) |>
    select(Indkøbsliste)

  out
}

#' Find Varer
#'
#' Finder varer fra en given fil.
#'
#' @param x Filnavn.
#' @return Data frame med varer.
find_varer <- function(x) {

  load(paste0("./data/indkobssedler/", x))

  medtag_kun_varer(df)
}

#' Medtag Kun Varer
#'
#' Filtrerer varer fra en data frame.
#'
#' @param x Data frame med varer.
#' @return Filtreret data frame med varer.
medtag_kun_varer <- function(x) {

  index <- which(x$Indkøbsliste == "")[1] - 1

  if (is.na(index)) {
    index <- nrow(x)
  }

  x[1:index, ]

}

#' Rens Varer
#'
#' Renser en liste af varer for uønskede tegn og enheder.
#'
#' @param varer Liste over varer.
#' @param enheder Liste over enheder.
#' @return Renset liste over varer.
rens_varer <- function(varer, enheder) {

  varer <- sub("\\((tilsmagning|tilbehør)\\)", "", varer)
  varer <- sub("\\d+\\.*\\d*", "", varer, perl = TRUE)

  enhed <- setdiff(unique(enheder), "")
  enhed <- paste0(enhed, collapse = "|")
  enhed <- gsub("\\(", "\\\\(", enhed)
  enhed <- gsub("\\)", "\\\\)", enhed)

  varer <- sub(enhed, "", varer, perl = TRUE)
  varer <- trimws(varer)

  varer
}


sInput <- function(inputId, label, choices, selected = NULL,
                   placeholder = "Vælg...", ...) {
  
  if (length(choices) < 30) {
    return(selectInput(
      inputId  = inputId,
      label    = label,
      choices  = choices,
      selected = selected,
      width    = "100%",
      selectize = FALSE,
      ...
    ))
  }
  
  selectizeInput(
    inputId  = inputId,
    label    = label,
    choices  = choices,
    selected = selected,
    width    = "100%",
    options  = list(
      openOnFocus      = TRUE,   # dropdown åbner ved fokus/tryk
      closeAfterSelect = TRUE,   # luk efter valg (mobilvenligt)
      highlight        = TRUE,
      diacritics       = TRUE,
      create           = FALSE,
      dropdownParent   = "body"  # UNDGÅ at touch/click bliver “slugt” i f7
    ),
    ...
  )
}

nInput <- function(inputId, label, value, ...) {
  numericInput(
    inputId,
    label,
    value,
    width = "100%",
    ...
  )
}

tInput <- function(inputId, label, ...) {
  
  textInput(
    inputId = "basis_varer_manuel", 
    label = "Tilf\u00F8j varer manuelt",
    width = "100%",
    ...
    )

}


themed_dt <- function(data, ...) {
  
  w <- DT::datatable(
    data, 
    rownames = NULL,
    ...)
  
}

# Helper: lav en "Redigér"-knap pr. række til DT
ga_make_edit_buttons <- function(n, table_id = "indkobsseddel") {
  if (is.na(n) || n <= 0) return(character())
  vapply(
    seq_len(n),
    function(i) {
      as.character(
        actionButton(
          inputId = paste0("edit_button_", i),
          label   = NULL,
          icon    = icon("pen"),
          class   = "edit-btn btn btn-sm",
          # send rækkenummeret direkte til Shiny (undgår data-attributter)
          onclick = sprintf(
            'Shiny.setInputValue("%s_editPressed", %d, {priority:"event"}); return false;',
            table_id, i
          ),
          type  = "button",
          # lille, robust inline-styling så temaer ikke overstyrer
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
    FUN.VALUE = "",
    USE.NAMES = FALSE
  )
}
