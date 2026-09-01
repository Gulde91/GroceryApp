# Shiny-modul for fanen Varer ----------------------------------------------
#
# Denne fil styrer visning og dialoger for brugerens egne basisvarer. Modulet
# holder kun midlertidig dialogtilstand og sender permanente ændringer gennem
# den state-funktion, som appen giver det.

library(stats)
library(htmltools)
library(DT)
library(shiny)
library(shinyMobile)
library(dplyr)
library(shinyjs)

#' Byg brugerfladen til fanen Varer
#'
#' Funktionen samler overskriften, knappen til at oprette en basisvare og
#' tabellen med alle brugerens egne basisvarer. Alle id'er får modulets
#' namespace, så de ikke kan kollidere med input og output andre steder i
#' appen.
#'
#' @param id Modul-id'et, som også bruges ved kaldet til servermodulet.
#'
#' @return En Shiny-tagliste, der kan indsættes direkte i fanen Varer.
#' @keywords internal
mod_varer_ui <- function(id) {
  ns <- NS(id)

  tagList(
    f7BlockTitle(title = "Bruttoliste over varer"),
    f7Block(
      inset = TRUE,
      strong = TRUE,
      f7Button(
        ns("open_ny_vare"),
        "Tilføj ny vare",
        fill = TRUE,
        color = "green"
      )
    ),
    DTOutput(ns("varer_tbl"))
  )
}

#' Byg dialogerne til varemodulet
#'
#' Funktionen opretter én dialog til nye basisvarer og én separat dialog til
#' redigering af navn, enhed og kategorier for en eksisterende basisvare.
#' Dialogerne tilhører kun modulet og deler derfor ikke redigeringsfelter eller
#' intern tilstand med indkøbssedlen.
#'
#' @param id Modul-id'et, som også bruges ved kaldet til servermodulet.
#'
#' @return En Shiny-tagliste med modulets tilføjelses- og redigeringsdialog.
#' @keywords internal
mod_varer_dialogs_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      id = ns("popup_ny_vare"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Tilføj ny basisvare"),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          tInput(ns("ny_vare_navn"), "Varenavn"),
          sInput(
            ns("ny_vare_enhed"),
            "Enhed",
            choices = NULL,
            selected = ""
          ),
          sInput(
            ns("ny_vare_kat1"),
            "Kategori 1",
            choices = NULL
          ),
          sInput(
            ns("ny_vare_kat2"),
            "Kategori 2",
            choices = NULL
          ),
          br(),
          f7Button(
            ns("save_ny_vare"),
            "Gem vare",
            fill = TRUE,
            color = "blue"
          ),
          br(),
          f7Button(
            ns("close_ny_vare"),
            "Luk",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    ),
    tags$div(
      id = ns("popup_varer_rediger"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Redigér basisvare"),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          tInput(
            ns("varer_edit_value"),
            "Varenavn"
          ),
          sInput(
            ns("varer_edit_enhed"),
            "Enhed",
            choices = NULL
          ),
          sInput(
            ns("varer_edit_kat1"),
            "Kategori 1",
            choices = NULL
          ),
          sInput(
            ns("varer_edit_kat2"),
            "Kategori 2",
            choices = NULL
          ),
          br(),
          f7Button(
            ns("save_varer_edit"),
            "Gem ændring",
            fill = TRUE,
            color = "blue"
          ),
          br(),
          f7Button(
            ns("cancel_varer_edit"),
            "Annullér",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    )
  )
}

#' Kør serverlogikken til fanen Varer
#'
#' Modulet viser, opretter, redigerer og sletter basisvarer. Det ejer ikke
#' basisvarernes reaktive state og skriver heller ikke selv til en fil. I
#' stedet læser det gennem de to getter-funktioner og sender en færdig,
#' opdateret data frame til `commit_varer`. Dermed findes der fortsat kun ét
#' sted i hovedappen, som er ansvarligt for state og vedvarende lagring.
#'
#' Hvis et commit mislykkes, bliver dialogens felter og den interne
#' redigeringstilstand stående. Brugeren kan derfor rette eller prøve samme
#' handling igen uden at begynde forfra.
#'
#' @param input Modulets namespacede Shiny-input.
#' @param output Modulets namespacede Shiny-output.
#' @param session Modulets Shiny-session.
#' @param varer_custom_current Getter, der returnerer de brugerdefinerede
#'   basisvarer.
#' @param varer_all_current Getter, der returnerer alle varer, som kan bruges
#'   til aktuelle valg af enhed og kategori.
#' @param commit_varer Funktion med argumenterne `next_df`, `error_message` og
#'   `log_context`. Funktionen gemmer data, opdaterer hovedappens state og
#'   returnerer `TRUE` ved succes eller `FALSE` ved fejl.
#' @param kategori_1 Valgfri tegnvektor med faste hovedkategorier. Kategorier
#'   fra `varer_custom_current` og `varer_all_current` hentes desuden på ny,
#'   hver gang dialogen åbnes.
#' @param kategori_2 Valgfri tegnvektor med faste underkategorier. Kategorier
#'   fra `varer_custom_current` og `varer_all_current` hentes desuden på ny,
#'   hver gang dialogen åbnes.
#'
#' @return `NULL` usynligt. Modulet kommunikerer ændringer gennem
#'   `commit_varer`.
#' @keywords internal
mod_varer_server <- function(
  input,
  output,
  session,
  varer_custom_current,
  varer_all_current,
  commit_varer,
  kategori_1 = character(),
  kategori_2 = character()
) {
  stopifnot(is.function(varer_custom_current))
  stopifnot(is.function(varer_all_current))
  stopifnot(is.function(commit_varer))

  ns <- session$ns
  rv_varerEditState <- reactiveValues(
    original_name = NULL
  )

  observeEvent(input$open_ny_vare, {
    df_custom <- varer_custom_current()
    df_all <- varer_all_current()

    if (
      !varer_has_required_columns(df_custom) ||
        !varer_has_required_columns(df_all)
    ) {
      showNotification(
        "Varelisten mangler de forventede kolonner.",
        type = "error"
      )
      return(invisible(NULL))
    }

    enheder <- varer_choice_values(
      "stk",
      df_custom$enhed,
      df_all$enhed,
      include_blank = TRUE
    )
    kat1 <- varer_choice_values(
      kategori_1,
      df_custom$kat_1,
      df_all$kat_1
    )
    kat2 <- varer_choice_values(
      kategori_2,
      df_custom$kat_2,
      df_all$kat_2,
      include_blank = TRUE
    )
    selected_kat1 <- if (length(kat1) > 0) kat1[[1]] else ""

    updateSelectInput(
      session,
      "ny_vare_enhed",
      choices = enheder,
      selected = "stk"
    )
    updateSelectInput(
      session,
      "ny_vare_kat1",
      choices = kat1,
      selected = selected_kat1
    )
    updateSelectInput(
      session,
      "ny_vare_kat2",
      choices = kat2,
      selected = ""
    )
    varer_show_dialog("popup_ny_vare", ns)
  })

  observeEvent(input$close_ny_vare, {
    varer_hide_dialog("popup_ny_vare", ns)
  })

  observeEvent(input$save_ny_vare, {
    navn <- varer_clean_text(input$ny_vare_navn)
    if (!nzchar(navn)) {
      showNotification(
        "Skriv et varenavn.",
        type = "warning"
      )
      return(invisible(NULL))
    }

    df <- varer_custom_current()
    if (!varer_has_required_columns(df)) {
      showNotification(
        "Varelisten mangler de forventede kolonner.",
        type = "error"
      )
      return(invisible(NULL))
    }
    if (varer_name_exists(navn, varer_all_current())) {
      showNotification(
        sprintf('"%s" findes allerede i varekataloget.', navn),
        type = "warning"
      )
      return(invisible(NULL))
    }

    ny <- varer_new_row(
      navn = navn,
      enhed = input$ny_vare_enhed,
      kat_1 = input$ny_vare_kat1,
      kat_2 = input$ny_vare_kat2
    )
    next_df <- bind_rows(df, ny)
    next_df <- varer_sort_rows(next_df)

    saved <- commit_varer(
      next_df,
      error_message = "Varen kunne ikke tilføjes.",
      log_context = list(
        action = "basis_item_add",
        item_name = navn,
        success_message = sprintf(
          'Varen "%s" blev tilføjet til bruttolisten.',
          navn
        ),
        failure_message = sprintf(
          'Varen "%s" kunne ikke tilføjes til bruttolisten.',
          navn
        )
      )
    )
    if (!isTRUE(saved)) return(invisible(NULL))

    updateTextInput(
      session,
      "ny_vare_navn",
      value = ""
    )
    varer_hide_dialog("popup_ny_vare", ns)
    showNotification(
      sprintf('"%s" er tilføjet til bruttolisten.', navn),
      type = "message"
    )
  })

  observeEvent(input$varer_editPressed, ignoreInit = TRUE, {
    original_name <- varer_clean_text(input$varer_editPressed)
    df <- varer_custom_current()
    df_all <- varer_all_current()

    if (
      !varer_has_required_columns(df) ||
        !varer_has_required_columns(df_all)
    ) {
      showNotification(
        "Varelisten mangler de forventede kolonner.",
        type = "error"
      )
      return(invisible(NULL))
    }

    row <- varer_find_name_row(original_name, df)

    if (is.na(row)) {
      showNotification(
        "Varen kunne ikke findes. Prøv at genindlæse tabellen.",
        type = "warning"
      )
      return(invisible(NULL))
    }

    rv_varerEditState$original_name <- varer_clean_text(
      df$Indkobsliste[[row]]
    )
    updateTextInput(
      session,
      "varer_edit_value",
      value = df$Indkobsliste[[row]]
    )
    enheder <- varer_choice_values(
      "stk",
      df$enhed,
      df_all$enhed,
      include_blank = TRUE
    )
    kat1 <- varer_choice_values(
      kategori_1,
      df$kat_1,
      df_all$kat_1
    )
    kat2 <- varer_choice_values(
      kategori_2,
      df$kat_2,
      df_all$kat_2,
      include_blank = TRUE
    )
    updateSelectInput(
      session,
      "varer_edit_enhed",
      choices = enheder,
      selected = df$enhed[[row]]
    )
    updateSelectInput(
      session,
      "varer_edit_kat1",
      choices = kat1,
      selected = df$kat_1[[row]]
    )
    updateSelectInput(
      session,
      "varer_edit_kat2",
      choices = kat2,
      selected = df$kat_2[[row]]
    )
    varer_show_dialog("popup_varer_rediger", ns)
  })

  observeEvent(input$save_varer_edit, {
    original_name <- rv_varerEditState$original_name
    if (is.null(original_name)) {
      return(invisible(NULL))
    }

    navn <- varer_clean_text(input$varer_edit_value)
    if (!nzchar(navn)) {
      showNotification(
        "Varenavnet må ikke være tomt.",
        type = "warning"
      )
      return(invisible(NULL))
    }

    df <- varer_custom_current()
    row <- varer_find_name_row(original_name, df)
    if (is.na(row)) {
      showNotification(
        paste(
          "Varelisten er ændret, mens dialogen var åben.",
          "Luk dialogen og vælg varen igen."
        ),
        type = "warning"
      )
      return(invisible(NULL))
    }
    normalized_name <- varer_normalized_names(navn)
    normalized_original <- varer_normalized_names(original_name)
    name_changed <- !identical(
      normalized_name,
      normalized_original
    )
    if (
      name_changed &&
        varer_name_exists(navn, varer_all_current())
    ) {
      showNotification(
        sprintf('"%s" findes allerede i varekataloget.', navn),
        type = "warning"
      )
      return(invisible(NULL))
    }

    next_df <- varer_replace_values(
      df,
      row,
      navn,
      enhed = input$varer_edit_enhed,
      kat_1 = input$varer_edit_kat1,
      kat_2 = input$varer_edit_kat2
    )
    next_df <- varer_sort_rows(next_df)
    saved <- commit_varer(
      next_df,
      error_message = "Varen kunne ikke redigeres.",
      log_context = list(
        action = "basis_item_update",
        item_name = navn,
        previous_item_name = original_name,
        success_message = if (!identical(navn, original_name)) {
          sprintf(
            'Varen "%s" blev omdøbt til "%s" på bruttolisten.',
            original_name,
            navn
          )
        } else {
          sprintf('Varen "%s" blev opdateret på bruttolisten.', navn)
        },
        failure_message = sprintf(
          'Varen "%s" kunne ikke opdateres på bruttolisten.',
          original_name
        )
      )
    )
    if (!isTRUE(saved)) return(invisible(NULL))

    varer_clear_edit_state(rv_varerEditState)
    varer_hide_dialog("popup_varer_rediger", ns)
    showNotification(
      sprintf('Ændringerne til "%s" er gemt.', navn),
      type = "message"
    )
  })

  observeEvent(input$cancel_varer_edit, {
    varer_clear_edit_state(rv_varerEditState)
    varer_hide_dialog("popup_varer_rediger", ns)
  })

  observeEvent(input$varer_deletePressed, {
    label <- varer_clean_text(input$varer_deletePressed)
    df <- varer_custom_current()
    row <- varer_find_name_row(label, df)

    if (is.na(row)) {
      showNotification(
        "Varen kunne ikke findes. Prøv at genindlæse tabellen.",
        type = "warning"
      )
      return(invisible(NULL))
    }

    label <- varer_clean_text(df$Indkobsliste[[row]])
    next_df <- df[-row, , drop = FALSE]
    next_df <- varer_sort_rows(next_df)
    saved <- commit_varer(
      next_df,
      error_message = "Varen kunne ikke slettes.",
      log_context = list(
        action = "basis_item_delete",
        item_name = label,
        success_message = sprintf(
          'Varen "%s" blev slettet fra bruttolisten.',
          label
        ),
        failure_message = sprintf(
          'Varen "%s" kunne ikke slettes fra bruttolisten.',
          label
        )
      )
    )
    if (!isTRUE(saved)) return(invisible(NULL))

    showNotification(
      sprintf('"%s" er slettet fra bruttolisten.', label),
      type = "message"
    )
  })

  output$varer_tbl <- renderDT({
    df <- varer_custom_current()
    validate(
      need(
        varer_has_required_columns(df),
        "Varelisten mangler de forventede kolonner."
      )
    )
    varer_table_widget(df, ns)
  })

  invisible(NULL)
}

#' Vis en dialog, som tilhører varemodulet
#'
#' Funktionen tilføjer modulets namespace til dialog-id'et og åbner derefter
#' dialogen med den samme fade-effekt som resten af appen.
#'
#' @param id Dialogens lokale id uden namespace.
#' @param ns Modulets namespace-funktion.
#'
#' @return Resultatet fra ShinyJS usynligt.
#' @keywords internal
varer_show_dialog <- function(id, ns) {
  show(
    id = ns(id),
    anim = TRUE,
    animType = "fade",
    asis = TRUE
  )
}

#' Skjul en dialog, som tilhører varemodulet
#'
#' Funktionen tilføjer modulets namespace til dialog-id'et og lukker
#' dialogen. `asis = TRUE` forhindrer, at ShinyJS tilføjer namespace endnu en
#' gang.
#'
#' @param id Dialogens lokale id uden namespace.
#' @param ns Modulets namespace-funktion.
#'
#' @return Resultatet fra ShinyJS usynligt.
#' @keywords internal
varer_hide_dialog <- function(id, ns) {
  hide(
    id = ns(id),
    anim = TRUE,
    animType = "fade",
    asis = TRUE
  )
}

#' Rens én tekstværdi fra et input
#'
#' Funktionen gør manglende værdier til tom tekst og fjerner mellemrum før
#' og efter indholdet. Det giver ens validering af både nye og redigerede
#' varenavne.
#'
#' @param x En værdi, typisk fra et Shiny-input.
#'
#' @return Den første værdi som trimmet tekst eller `""`.
#' @keywords internal
varer_clean_text <- function(x) {
  if (
    is.null(x) ||
      length(x) == 0 ||
      is.na(x[[1]])
  ) {
    return("")
  }

  trimws(as.character(x[[1]]))
}

#' Kontrollér varelistens grundlæggende struktur
#'
#' En basisvare skal have navn, mængde, enhed og to kategorier. Funktionen
#' bruges før visning og ændringer, så en ødelagt eller forkert indlæst fil
#' ikke overskrives med ufuldstændige data.
#'
#' @param df Det data frame, der skal kontrolleres.
#'
#' @return `TRUE`, hvis de forventede kolonner findes i præcis den aftalte
#'   rækkefølge, ellers `FALSE`.
#' @keywords internal
varer_has_required_columns <- function(df) {
  required <- c(
    "Indkobsliste",
    "maengde",
    "enhed",
    "kat_1",
    "kat_2"
  )

  is.data.frame(df) && identical(names(df), required)
}

#' Normalisér varenavne til sammenligning
#'
#' Funktionen fjerner yderste mellemrum og bruger små bogstaver. Derfor
#' betragtes eksempelvis `"Agurk"` og `" agurk "` som det samme varenavn.
#'
#' @param x En vektor med varenavne.
#'
#' @return En tegnvektor med normaliserede varenavne.
#' @keywords internal
varer_normalized_names <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  tolower(trimws(x))
}

#' Undersøg om et varenavn allerede findes
#'
#' Funktionen foretager en sammenligning uden forskel på store og små
#' bogstaver. Ved omdøbning kan den aktuelle række ignoreres, så det er
#' tilladt at gemme samme navn med ændret brug af store bogstaver.
#'
#' @param navn Det nye eller redigerede varenavn.
#' @param df Data frame med brugerens basisvarer.
#' @param ignore_row Et eventuelt rækkenummer, som ikke skal sammenlignes.
#'
#' @return `TRUE`, hvis navnet allerede bruges af en anden relevant række.
#' @keywords internal
varer_name_exists <- function(
  navn,
  df,
  ignore_row = NA_integer_
) {
  existing <- varer_normalized_names(df$Indkobsliste)
  if (
    length(ignore_row) == 1 &&
      !is.na(ignore_row) &&
      ignore_row >= 1 &&
      ignore_row <= length(existing)
  ) {
    existing <- existing[-ignore_row]
  }

  varer_normalized_names(navn) %in% existing
}

#' Sortér basisvarer efter navn
#'
#' Funktionen laver en ny data frame, hvor varenavnene sorteres uden forskel
#' på store og små bogstaver. Kolonnerne og deres øvrige værdier bevares.
#'
#' @param df Data frame med basisvarer.
#'
#' @return Den sorterede data frame med nulstillede rækkenavne.
#' @keywords internal
varer_sort_rows <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    rownames(df) <- NULL
    return(df)
  }

  sort_key <- varer_normalized_names(df$Indkobsliste)
  df <- df[order(sort_key, na.last = TRUE), , drop = FALSE]
  rownames(df) <- NULL
  df
}

#' Opret én ny basisvarerække
#'
#' Funktionen samler værdierne fra tilføjelsesdialogen i den faste struktur,
#' som resten af appen forventer. Mængden sættes til 1, fordi basislisten
#' beskriver standardvarer og ikke en konkret indkøbsmængde.
#'
#' @param navn Varens viste navn.
#' @param enhed Varens standardenhed.
#' @param kat_1 Varens hovedkategori.
#' @param kat_2 Varens eventuelle underkategori.
#'
#' @return En data frame med præcis én basisvarerække.
#' @keywords internal
varer_new_row <- function(
  navn,
  enhed,
  kat_1,
  kat_2
) {
  data.frame(
    Indkobsliste = varer_clean_text(navn),
    maengde = 1,
    enhed = varer_clean_text(enhed),
    kat_1 = varer_clean_text(kat_1),
    kat_2 = varer_clean_text(kat_2),
    stringsAsFactors = FALSE
  )
}

#' Find en basisvare ud fra dens stabile navn
#'
#' En tabel kan nå at blive sorteret eller genberegnet, før et klik fra
#' browseren behandles. Derfor findes varen igen via det navn, som knappen
#' blev bygget med, i stedet for at stole på et flytbart rækkenummer.
#'
#' @param navn Varenavnet fra den dynamiske tabelknap.
#' @param df Data frame med basisvarer.
#'
#' @return Varens entydige rækkenummer eller `NA_integer_`, hvis navnet ikke
#'   længere findes præcis én gang.
#' @keywords internal
varer_find_name_row <- function(navn, df) {
  if (!varer_has_required_columns(df)) return(NA_integer_)

  normalized_name <- varer_normalized_names(navn)
  if (length(normalized_name) != 1L || !nzchar(normalized_name)) {
    return(NA_integer_)
  }

  matches <- which(
    varer_normalized_names(df$Indkobsliste) ==
      normalized_name
  )
  if (length(matches) != 1L) return(NA_integer_)

  as.integer(matches[[1]])
}

#' Erstat navn, enhed og kategorier i én basisvarerække
#'
#' Funktionen kopierer varelisten og sikrer, at de redigerede kolonner er
#' tekst, før den ønskede række ændres. Mængden røres ikke. Dermed kan navn,
#' enhed og kategorier afleveres samlet til ét commit, også hvis en ældre
#' datakilde har indlæst en af tekstkolonnerne som faktor.
#'
#' @param df Data frame med basisvarer.
#' @param row Rækkenummeret, der skal ændres.
#' @param navn Det nye varenavn.
#' @param enhed Den nye standardenhed.
#' @param kat_1 Den nye hovedkategori.
#' @param kat_2 Den nye underkategori, som gerne må være tom.
#'
#' @return En ny data frame med det opdaterede navn, enhed og kategorier.
#' @keywords internal
varer_replace_values <- function(df, row, navn, enhed, kat_1, kat_2) {
  next_df <- df
  next_df$Indkobsliste <- as.character(next_df$Indkobsliste)
  next_df$enhed <- as.character(next_df$enhed)
  next_df$kat_1 <- as.character(next_df$kat_1)
  next_df$kat_2 <- as.character(next_df$kat_2)
  next_df$Indkobsliste[[row]] <- varer_clean_text(navn)
  next_df$enhed[[row]] <- varer_clean_text(enhed)
  next_df$kat_1[[row]] <- varer_clean_text(kat_1)
  next_df$kat_2[[row]] <- varer_clean_text(kat_2)
  next_df
}

#' Saml og sortér valgmuligheder til en dialog
#'
#' Funktionen kombinerer appens standardværdier med værdier fra den
#' brugerdefinerede og den samlede vareliste. Manglende værdier fjernes, og
#' dubletter vises kun én gang.
#'
#' @param defaults Appens faste standardværdier.
#' @param custom_values Værdier fra brugerens egne basisvarer.
#' @param all_values Værdier fra den samlede vareliste.
#' @param include_blank Om en tom valgmulighed skal medtages.
#'
#' @return En alfabetisk sorteret tegnvektor med unikke valgmuligheder.
#' @keywords internal
varer_choice_values <- function(
  defaults,
  custom_values,
  all_values,
  include_blank = FALSE
) {
  values <- c(defaults, custom_values, all_values)
  values <- as.character(values)
  values <- trimws(values[!is.na(values)])
  if (!isTRUE(include_blank)) {
    values <- values[nzchar(values)]
  }
  values <- unique(values)

  sort(values)
}

#' Nulstil modulets redigeringstilstand
#'
#' Funktionen rydder både rækkenummeret og det oprindelige navn. Den kaldes
#' først efter et vellykket commit eller når brugeren selv annullerer, så en
#' commitfejl kan forsøges igen.
#'
#' @param state Et `reactiveValues`-objekt med redigeringstilstanden.
#'
#' @return `NULL` usynligt.
#' @keywords internal
varer_clear_edit_state <- function(state) {
  state$original_name <- NULL
  invisible(NULL)
}

#' Kod en tekstværdi sikkert til JavaScript
#'
#' Dynamiske tabelknapper sender varenavnet tilbage til Shiny som stabil
#' identitet. Tekstkodningen sørger for, at blandt andet anførselstegn og
#' backslashes i et varenavn ikke kan ødelægge knappens JavaScript.
#'
#' @param x Den tekstværdi, som skal indgå i JavaScript.
#'
#' @return En citeret og escaped tekstværdi til JavaScript.
#' @keywords internal
varer_js_string <- function(x) {
  encodeString(
    varer_clean_text(x),
    quote = '"',
    na.encode = FALSE
  )
}

#' Byg en namespacet redigeringsknap til en varerække
#'
#' Knappen sender varenavnet til varemodulets eget input-event. Både
#' knappens id og eventets id er namespacet, så klikket ikke kan ramme andre
#' tabeller i appen. Navnet bruges som stabil identitet, mens rækkenummeret
#' kun indgår i knappens DOM-id.
#'
#' @param row Varens rækkenummer.
#' @param navne Vektoren med tabellens varenavne.
#' @param ns Modulets namespace-funktion.
#' @param event_id Det fulde input-id, som skal modtage varenavnet.
#'
#' @return Knappen som HTML-tekst til brug i en DT-tabel.
#' @keywords internal
varer_edit_button <- function(row, navne, ns, event_id) {
  event_value <- varer_js_string(navne[[row]])
  event_name <- varer_js_string(event_id)

  as.character(
    ga_js_button(
      inputId = ns(paste0("varer_edit_button_", row)),
      label = NULL,
      icon = icon("pen"),
      class = "edit-btn btn btn-sm",
      onclick = sprintf(
        paste0(
          "Shiny.setInputValue(",
          "%s, %s, {priority:'event'}); ",
          "return false;"
        ),
        event_name,
        event_value
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
}

#' Byg en namespacet sletteknap til en varerække
#'
#' Knappen sender varenavnet til varemodulets slette-event. Den dynamiske
#' knap og eventet får begge namespace, så de kan eksistere sikkert sammen
#' med indkøbssedlens og opskriftsmodulets knapper. Et klik fra en forældet
#' tabel kan derfor ikke komme til at slette en anden vare, som har overtaget
#' samme rækkenummer.
#'
#' @param row Varens rækkenummer.
#' @param navne Vektoren med tabellens varenavne.
#' @param ns Modulets namespace-funktion.
#' @param event_id Det fulde input-id, som skal modtage varenavnet.
#'
#' @return Knappen som HTML-tekst til brug i en DT-tabel.
#' @keywords internal
varer_delete_button <- function(row, navne, ns, event_id) {
  event_value <- varer_js_string(navne[[row]])
  event_name <- varer_js_string(event_id)

  as.character(
    ga_js_button(
      inputId = ns(paste0("varer_delete_button_", row)),
      label = NULL,
      icon = icon("trash"),
      class = "delete-btn btn btn-sm",
      onclick = sprintf(
        paste0(
          "Shiny.setInputValue(",
          "%s, %s, {priority:'event'}); ",
          "return false;"
        ),
        event_name,
        event_value
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
}

#' Byg den viste tabel med basisvarer
#'
#' Funktionen udvælger navn og enhed, tilføjer de dynamiske redigerings- og
#' sletteknapper og anvender tabellens danske søgetekster. Kategorierne vises
#' ikke i tabellen, men kan fortsat ændres gennem redigeringsdialogen. Selve
#' dataene ændres ikke; tabellen er kun en visning af hovedappens aktuelle
#' state.
#'
#' @param df Data frame med brugerens basisvarer.
#' @param ns Modulets namespace-funktion.
#'
#' @return Et DT-widget-objekt, som kan returneres fra `renderDT`.
#' @keywords internal
varer_table_widget <- function(df, ns) {
  table_df <- df[c("Indkobsliste", "enhed")]
  names(table_df) <- c("Vare", "Enhed")
  rows <- seq_len(nrow(table_df))
  edit_buttons <- vapply(
    rows,
    varer_edit_button,
    navne = table_df$Vare,
    ns = ns,
    event_id = ns("varer_editPressed"),
    FUN.VALUE = "",
    USE.NAMES = FALSE
  )
  delete_buttons <- vapply(
    rows,
    varer_delete_button,
    navne = table_df$Vare,
    ns = ns,
    event_id = ns("varer_deletePressed"),
    FUN.VALUE = "",
    USE.NAMES = FALSE
  )
  result <- cbind(
    table_df,
    Rediger = edit_buttons,
    Slet = delete_buttons
  )

  datatable(
    result,
    rownames = FALSE,
    escape = seq_len(ncol(table_df)),
    options = list(
      dom = "ft",
      pageLength = max(1L, nrow(table_df)),
      ordering = TRUE,
      columnDefs = list(
        list(
          targets = ncol(table_df),
          orderable = FALSE,
          searchable = FALSE
        ),
        list(
          targets = ncol(table_df) + 1,
          orderable = FALSE,
          searchable = FALSE
        )
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
}
