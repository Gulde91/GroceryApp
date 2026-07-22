#' JavaScript-knap uden afhængighed af actionButton(...)-attributter
#'
#' Ældre Shiny-versioner accepterer ikke vilkårlige HTML-attributter som
#' \code{onclick} i \code{actionButton()}. Denne helper bygger derfor en
#' almindelig HTML-knap til de steder, hvor klik alligevel håndteres via
#' \code{Shiny.setInputValue()}.
ga_js_button <- function(inputId, label = NULL, icon = NULL, class = NULL,
                         onclick = NULL, type = "button", style = NULL) {
  children <- Filter(Negate(is.null), list(icon, label))

  do.call(
    tags$button,
    c(
      list(
        id = inputId,
        type = type,
        class = class,
        onclick = onclick,
        style = style
      ),
      children
    )
  )
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

  # må lave til df igen da subsetting af 1 col laves df om til character
  out <- as.data.frame(x[1:index, ])
  names(out) <- "Indkøbsliste"
  
  out

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

#' Smart select-input (selectInput/selectizeInput)
#'
#' Wrapper der vælger mellem \code{selectInput()} og \code{selectizeInput()}
#' afhængigt af antal valgmuligheder. Ved få valg bruges almindelig select for
#' enkelhed; ved mange valg bruges selectize med søgning.
#'
#' @param inputId Input-id til Shiny-kontrollen.
#' @param label Label vist over inputfeltet.
#' @param choices Mulige valg.
#' @param selected Forvalgt værdi.
#' @param placeholder Placeholder-tekst (bevares af hensyn til kompatibilitet).
#' @param ... Øvrige argumenter sendes videre til den underliggende input-funktion.
#'
#' @return En Shiny input-kontrol (\code{tag}).
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

#' Numeric input med fuld bredde
#'
#' Lille wrapper omkring \code{numericInput()} med \code{width = "100%"} som
#' standard.
#'
#' @param inputId Input-id til Shiny-kontrollen.
#' @param label Label vist over inputfeltet.
#' @param value Startværdi.
#' @param ... Øvrige argumenter sendt videre til \code{numericInput()}.
#'
#' @return En Shiny numeric input-kontrol.
nInput <- function(inputId, label, value, ...) {
  numericInput(
    inputId,
    label,
    value,
    width = "100%",
    ...
  )
}

#' Tekstinput med fuld bredde
#'
#' Wrapper omkring \code{textInput()} med \code{width = "100%"} som standard.
#'
#' @param inputId Input-id til Shiny-kontrollen.
#' @param label Label vist over inputfeltet.
#' @param ... Øvrige argumenter sendt videre til \code{textInput()}.
#'
#' @return En Shiny text input-kontrol.
tInput <- function(inputId, label, ...) {
  
  textInput(
    inputId = inputId, 
    label = label,
    width = "100%",
    ...
    )

}

#' Standardiseret DT-tabel med app-tema
#'
#' Opretter en \code{DT::datatable()} med fælles standardvalg for appen.
#'
#' @param data Data der skal vises i tabellen.
#' @param ... Øvrige argumenter sendt videre til \code{DT::datatable()}.
#'
#' @return Et DT-widget-objekt.
themed_dt <- function(data, ...) {
  
  w <- DT::datatable(
    data, 
    rownames = NULL,
    ...)
  
}

#' Lav cart-knapper med stabile linje-id'er
#'
#' Indkøbssedlen kan skifte rækkefølge, når nye varer tilføjes. Disse helpers
#' sender derfor cartens stabile \code{line_id} i stedet for et rækkenummer.
#'
#' @param line_ids Character-vektor med id'er fra \code{cart_visible()}.
#' @param event_id Det fulde Shiny-input-id, som knappen skal sende linje-id'et
#'   til. Et modul kan derfor sende et id, som allerede er namespacet.
#' @param id_prefix Præfiks til knappernes DOM-id'er. Også dette kan
#'   namespaces, så flere tabeller ikke opretter de samme HTML-id'er.
#' @return Character-vektor med HTML for knapperne.
ga_make_cart_edit_buttons <- function(
  line_ids,
  event_id = "indkobsseddel_editPressed",
  id_prefix = "cart_edit_"
) {
  line_ids <- as.character(line_ids)
  if (length(line_ids) == 0) return(character())
  stopifnot(all(grepl("^cart_[0-9]+$", line_ids)))

  vapply(
    line_ids,
    function(line_id) {
      as.character(
        ga_js_button(
          inputId = paste0(id_prefix, line_id),
          label = NULL,
          icon = icon("pen"),
          class = "edit-btn btn btn-sm",
          onclick = sprintf(
            'Shiny.setInputValue("%s", "%s", {priority:"event"}); return false;',
            event_id,
            line_id
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
    FUN.VALUE = "",
    USE.NAMES = FALSE
  )
}

#' @rdname ga_make_cart_edit_buttons
ga_make_cart_delete_buttons <- function(
  line_ids,
  event_id = "deletePressed",
  id_prefix = "cart_delete_"
) {
  line_ids <- as.character(line_ids)
  if (length(line_ids) == 0) return(character())
  stopifnot(all(grepl("^cart_[0-9]+$", line_ids)))

  vapply(
    line_ids,
    function(line_id) {
      as.character(
        ga_js_button(
          inputId = paste0(id_prefix, line_id),
          label = NULL,
          icon = icon("trash"),
          class = "delete-btn btn btn-sm",
          onclick = sprintf(
            'Shiny.setInputValue("%s", "%s", {priority:"event"}); return false;',
            event_id,
            line_id
          ),
          style = paste(
            "background:#ef4444;",
            "color:#fff;",
            "border:1px solid #dc2626;",
            "border-radius:100px;",
            "font-weight:600;",
            "padding:6px 1px;",
            "line-height:1;",
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
