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

#' Add Slet Knap (generaliseret)
#'
#' @param i Rækkeindeks.
#' @param id_prefix Prefix for inputId (default: "delete_button").
#' @param event_name Navn på Shiny input-event (default: "deletePressed").
#' 
#' @return HTML for slette-knappen.
#' 
add_slet_knap <- function(i, id_prefix = "delete_button", event_name = "deletePressed") {
  as.character(
    actionButton(
      paste(id_prefix, i, sep = "_"),
      label = NULL,
      icon  = icon("trash"),
      onclick = sprintf('Shiny.setInputValue("%s", this.id, {priority: "event"})', event_name),
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
}

#' Slet én række fra en DT-bundet data.frame baseret på klik-id
#'
#' Hjælperfunktion til slette-knapper i \pkg{shiny} + \pkg{DT}.
#' Givet et `click_id` (fra en slette-knap oprettet med fx `add_slet_knap()`),
#' finder funktionen rækkenummeret via `parse_delete_event()`, foretager
#' bounds-tjek og returnerer en *ny* data.frame uden den pågældende række.
#' Den returnerer også en label fra en ønsket kolonne (fx varenavn), som kan
#' bruges til notifikationer. Funktionens input ændres ikke in-place.
#'
#' @param click_id `character(1)`. ID fra den klikkede slette-knap
#'   (typisk noget i stil med `"varer_delete_button_12"`).
#' @param df `data.frame` eller \code{tibble}. Den aktuelle tabel, der vises i DT.
#' @param label_col Kolonnen som returneret label skal tages fra.
#'   Kan være kolonneindeks (heltal, default `1`) **eller**
#'   kolonnenavn (`character`). Hvis navnet ikke findes, eller indekset
#'   er ugyldigt, returneres `label = NULL`.
#'
#' @return `list` med to elementer:
#' \itemize{
#'   \item \code{df}: den opdaterede data.frame (samme struktur som input, men
#'         med den slettede række fjernet; hvis sletningen ikke kunne udføres,
#'         returneres originalen uændret).
#'   \item \code{label}: værdi fra \code{label_col} på den slettede række
#'         (typisk et varenavn). \code{NULL} hvis ikke tilgængelig.
#' }
#'
#' @details
#' Funktionen er “fail-safe”: Hvis \code{click_id} ikke kan parses,
#' \code{df} er tom, eller indekset er udenfor rækkevidde, returneres
#' den originale \code{df} uændret, og \code{label = NULL}. Det gør den
#' robust over for hurtige dobbeltklik, race conditions og events der
#' fyrer før tabellen er klar.
#'
safe_delete_by_click <- function(click_id, df, label_col = 1) {
  idx <- parse_delete_event(click_id)
  
  # blød beskyttelse mod NA, out-of-bounds og tomme tabeller
  if (is.na(idx) || is.null(df) || nrow(df) < 1 || idx < 1 || idx > nrow(df)) {
    return(list(df = df, label = NULL))
  }
  
  label <- tryCatch(
    df[[label_col]][idx],
    error = function(e) NULL
  )
  
  df <- df[-idx, , drop = FALSE]
  
  list(df = df, label = label)
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
    inputId = inputId, 
    label = label,
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

get_link <- function(navn) {
  L <- links$link[links$ret == navn]
  if (length(L) == 1) return(L)
  NULL
}

get_df <- function(ret = "", salat = "", pers = 2, tilbeh = "") {
  out <- opskrift(
    opskrifter, retter, salater, salater_opskrifter, tilbehor,
    dag_ret = ret, dag_salat = salat, antal = pers, dag_tilbehor = tilbeh
  )
  if (!is.null(out)) {
    colnames(out) <- c("Indkobsliste","maengde","enhed","kat_1","kat_2")
  }
  out
}


#' @title Udtræk af brugte opskrifter fra indkøbssedler
#'
#' @description Funktionen gennemgår alle filer i mappen `./data/indkobssedler/`,
#' udtrækker retter via `find_retter()` og returnerer et datasæt med én række 
#' per registreret ret samt tilhørende dato udledt af filnavnet.
#' Filnavnene forventes at følge mønsteret:
#' `"indkobsseddel_YYYYMMDD.rdaX"`, hvor X kan være et eller flere cifre.
#' 
#' @param alle_retter Alle retter der findes i løsningen
#' 
#' @return En data.frame med to kolonner: \code{retter} og \code{dato}.
#' 
brugte_opskrifter <- function(alle_retter) {
  
  files <- list.files("./data/indkobssedler/")
  
  retter_count <- sapply(files,function(x) find_retter(x, alle_retter)) |> unlist() |> sort()
  
  retter_count_df <- data.frame(retter = retter_count)
  retter_count_df$dato <- 
    sub("indkobsseddel_([0-9]+)\\.rda\\d*", "\\1", row.names(retter_count_df)) |> 
    as.Date(format = "%Y%m%d")
  row.names(retter_count_df) <- NULL
  
  retter_count_df
  
}

#' @title Udtræk af retter fra en indkøbsseddel-fil
#'
#' @description Funktionen indlæser en `.rda`-fil fra mappen `./data/indkobssedler/`, 
#' finder retter baseret på et starts-with match mod listen `retter$retter`, og 
#' renser teksten for uønskede tegn og tilføjelser.
#'
#' @param x Filnavn (streng) på en indkøbsseddel, som skal behandles.
#'          Filen skal ligge i mappen `./data/indkobssedler/`.
#' @param alle_retter Alle retter der findes i løsningen
#'          
#' @return En vektor af strenge, som repræsenterer de fundne og rensede retter.
#'          
find_retter <- function(x, alle_retter) {
  
  load(paste0("./data/indkobssedler/", x))
  
  pattern <- paste0("^(", paste(alle_retter, collapse = "|"), ")")
  resultat <- df$Indkøbsliste[grepl(pattern, df$Indkøbsliste)]
  resultat <- gsub(":", "", resultat)
  renset <- sub(" m\\..*$", "", resultat)
  
  renset
  
}

#' @title Plot af brugte opskrifter
#'
#' @description Funktionen filtrerer et datasæt af brugte opskrifter på 
#' baggrund af en startdato og plotter derefter hyppigheden af retter i 
#' faldende rækkefølge. 
#' 
#' @param df En data.frame som indeholder variablerne:
#'   \describe{
#'     \item{retter}{Navn på retten (streng eller faktor)}
#'     \item{dato}{Dato for hvornår retten indgik (klasse: Date)}
#'   }
#' @param dato_filter En tegnstreng eller Date-værdi der angiver den tidligste
#'   dato der skal medtages i plottet. Default er `"2024-01-01"`.
#' @param dato_start Startdato for hvornår brugte opskrifter skal med i plottet
#' @param dato_slut Slutdato for hvornår brugte opskrifter skal med i plottet
#' @param top_n integer med de n mest benyttede retter der skal vises
#'   
#' @return Et ggplot2‐objekt med et søjlediagram.
#' 
plot_brugte_opskrifter <- function(df, dato_start, dato_slut, top_n = 5) {
  
  if (nrow(df) == 0) {
    return(
      ggplot() +
        geom_blank() +
        labs(title = "Ingen data i valgt interval")
    )
  }
  
  # filtrér på datointerval
  df_plot <- df |> 
    subset(dato >= as.Date(dato_start) & dato <= as.Date(dato_slut))
  
  if (nrow(df_plot) == 0) {
    return(
      ggplot() +
        geom_blank() +
        labs(title = "Ingen data i dette datointerval")
    )
  } 
 
  # find top_n retter i dette interval
  top_n_brugte <- df_plot |>
    count(retter) |> 
    arrange(desc(n)) |> 
    slice_head(n = top_n) |>
    pull(retter)
    
  df_plot <- subset(df_plot, retter %in% top_n_brugte)
  
  # ggplot(df_plot, aes(x = fct_infreq(retter))) +
  #   geom_bar() + 
  #   labs(x = "Antal", y = "Retter", title = "Mest brugte opskrifter") +
  #   theme(
  #     axis.text.x = element_text(angle = 45, hjust = 1)
  #   )
  
  ggplot(df_plot, aes(x = fct_infreq(retter))) +
    geom_bar(fill = "#8EB5FF") +
    labs(x = "Retter", y = "Antal", title = "Mest brugte opskrifter") +
    theme_minimal(base_size = 16) +
    theme(
      plot.background = element_rect(fill = "#1c1c1e", color = NA),
      panel.background = element_rect(fill = "#1c1c1e", color = NA),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      plot.title = element_text(color = "white", face = "bold", size = 18),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
  
}
