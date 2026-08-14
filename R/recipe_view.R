# Visningsbyggere for opskriftsfanen --------------------------------------
#
# Denne fil samler opskriftsfanens statiske brugerflade og de rene byggere,
# som omsætter allerede indlæste opskriftsdata til HTML og DT-widgets.
# Funktionerne registrerer ingen Shiny-outputs, læser ingen input og ændrer
# ingen reaktiv state. Det reaktive flow bliver derfor fortsat styret samlet i
# recipe_module.R.

library(stats)
library(htmltools)
library(DT)
library(shiny)
library(shinyMobile)
library(dplyr)

# Fælles indstillinger til opskriftsvælgeren. Objektet ligger sammen med
# visningsbyggerne, fordi det alene beskriver selectize-komponentens udseende
# og opførsel.
opskrift_selectize_options <- list(
  openOnFocus = TRUE,
  closeAfterSelect = TRUE,
  highlight = TRUE,
  diacritics = TRUE,
  create = FALSE,
  dropdownParent = "body",
  sortField = "label"
)

# Ingrediensfeltet bruger samme mobilvenlige opsætning som opskriftsvælgeren,
# men tillader samtidig, at brugeren opretter en ny tekstværdi. Dermed kan ét
# felt både søge i varekataloget og modtage en ingrediens, som endnu ikke
# findes på listen.
opskrift_ingredient_selectize_options <- modifyList(
  opskrift_selectize_options,
  list(
    create = TRUE,
    createOnBlur = TRUE,
    persist = FALSE,
    placeholder = "Søg i listen eller skriv en ny vare"
  )
)

#' Byg brugerfladen til fanen Opskrifter
#'
#' Funktionen samler den synlige introduktion, knappen til at oprette en ret
#' og den dynamiske del af opskriftsfanen. Alle id'er får modulets namespace,
#' så fanens input og output ikke kolliderer med resten af appen.
#'
#' @param id Modul-id'et, som også bruges ved kaldet til servermodulet.
#'
#' @return En Shiny-tagliste, der kan indsættes direkte i appens UI.
#' @keywords internal
mod_opskrifter_ui <- function(id) {
  ns <- NS(id)

  tagList(
    f7BlockTitle(title = "Opskrifter"),
    f7Block(
      inset = TRUE,
      strong = TRUE,
      tags$p(
        "Alle opskrifter nedenfor er angivet med mængder svarende til ",
        tags$b("1 person"),
        "."
      ),
      tags$p(
        paste(
          "Du kan redigere og slette ingredienslinjer direkte.",
          "Ændringer gemmes automatisk."
        )
      ),
      f7Button(
        ns("open_ny_ret"),
        "Tilføj ny ret",
        fill = TRUE,
        color = "green"
      )
    ),
    uiOutput(ns("opskrifter_ui"))
  )
}

#' Byg dialogerne til opskriftsmodulet
#'
#' Dialogerne ligger samlet uden for den dynamiske opskriftsvisning. Dermed
#' findes de altid i DOM'en, mens serveren blot åbner og lukker den relevante
#' dialog ved redigering, tilføjelse, arkivering eller sletning.
#'
#' @param id Modul-id'et, som også bruges ved kaldet til servermodulet.
#'
#' @return En Shiny-tagliste med alle modulets dialogvinduer.
#' @keywords internal
mod_opskrifter_dialogs_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      id = ns("popup_opskrift_rediger"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Redigér ingrediens"),
        tags$p(textOutput(ns("opskrift_edit_context"))),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          nInput(
            ns("opskrift_edit_maengde"),
            "Mængde",
            value = 1
          ),
          sInput(
            ns("opskrift_edit_enhed"),
            "Enhed",
            choices = c(""),
            selected = ""
          ),
          sInput(
            ns("opskrift_edit_kat1"),
            "Kategori 1",
            choices = c(""),
            selected = ""
          ),
          sInput(
            ns("opskrift_edit_kat2"),
            "Kategori 2",
            choices = c(""),
            selected = ""
          ),
          br(),
          f7Button(
            ns("save_opskrift_row"),
            "Opdater række",
            fill = TRUE,
            color = "blue"
          ),
          br(),
          f7Button(
            ns("cancel_opskrift_row"),
            "Luk",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    ),
    tags$div(
      id = ns("popup_opskrift_tilfoej"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Tilføj ingrediens"),
        tags$p(textOutput(ns("opskrift_add_context"))),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          selectizeInput(
            ns("opskrift_add_navn"),
            "Varenavn (vælg eller skriv)",
            choices = character(),
            selected = character(),
            width = "100%",
            options = opskrift_ingredient_selectize_options
          ),
          nInput(
            ns("opskrift_add_maengde"),
            "Mængde",
            value = 1
          ),
          sInput(
            ns("opskrift_add_enhed"),
            "Enhed",
            choices = c(""),
            selected = ""
          ),
          sInput(
            ns("opskrift_add_kat1"),
            "Kategori 1",
            choices = c(""),
            selected = ""
          ),
          sInput(
            ns("opskrift_add_kat2"),
            "Kategori 2",
            choices = c(""),
            selected = ""
          ),
          br(),
          f7Button(
            ns("save_opskrift_new_row"),
            "Tilføj vare",
            fill = TRUE,
            color = "green"
          ),
          br(),
          f7Button(
            ns("cancel_opskrift_new_row"),
            "Luk",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    ),
    tags$div(
      id = ns("popup_opskrift_slet_bekraeft"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Slet ingrediens"),
        tags$p(textOutput(ns("opskrift_delete_context"))),
        tags$p(
          "Er du sikker på at du vil slette denne ingredienslinje?"
        ),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          f7Button(
            ns("confirm_delete_opskrift_row"),
            "Ja, slet",
            fill = TRUE,
            color = "red"
          ),
          br(),
          f7Button(
            ns("cancel_delete_opskrift_row"),
            "Nej",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    ),
    tags$div(
      id = ns("popup_ret_slet_bekraeft"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Arkiver ret"),
        tags$p(textOutput(ns("ret_delete_context"))),
        tags$p(
          "Retten flyttes til arkivet og kan gendannes igen senere."
        ),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          f7Button(
            ns("confirm_delete_ret"),
            "Ja, arkiver ret",
            fill = TRUE,
            color = "red"
          ),
          br(),
          f7Button(
            ns("cancel_delete_ret"),
            "Nej",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    ),
    tags$div(
      id = ns("popup_ret_slet_permanent_bekraeft"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Slet ret permanent"),
        tags$p(
          textOutput(ns("ret_permanent_delete_context"))
        ),
        tags$p(
          paste(
            "Retten, opskriftsfilen og linket slettes permanent",
            "og kan ikke gendannes."
          )
        ),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          f7Button(
            ns("confirm_delete_archived_ret"),
            "Ja, slet permanent",
            fill = TRUE,
            color = "red"
          ),
          br(),
          f7Button(
            ns("cancel_delete_archived_ret"),
            "Nej",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    ),
    tags$div(
      id = ns("popup_ny_ret"),
      class = "ga-modal",
      tags$div(
        class = "ga-dialog",
        tags$h3("Tilføj ny ret"),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          tInput(
            ns("ny_ret_navn"),
            "Rettens navn"
          ),
          sInput(
            ns("ny_ret_type"),
            "Type",
            choices = c("vegetar", "kylling", "gris", "okse", "fisk"),
            selected = "vegetar"
          ),
          tInput(
            ns("ny_ret_link"),
            "Link (valgfrit)"
          ),
          br(),
          f7Button(
            ns("save_ny_ret"),
            "Gem ret",
            fill = TRUE,
            color = "blue"
          ),
          br(),
          f7Button(
            ns("close_ny_ret"),
            "Luk",
            fill = TRUE,
            color = "gray"
          )
        )
      )
    )
  )
}

#' Find aktive retter med en eksisterende opskrift
#'
#' Katalogets liste over aktive retter kan i sjældne tilfælde indeholde en
#' nøgle uden en tilhørende opskrift. Funktionen fjerner sådanne rækker og
#' sorterer de resterende retter alfabetisk til visning i brugerfladen.
#'
#' @param retter_df Data frame med aktive retter og kolonnerne `key` og
#'   `retter`.
#' @param recipe_keys Tegnvektor med nøgler på opskrifter, der faktisk findes.
#'
#' @return En filtreret og alfabetisk sorteret data frame.
#' @keywords internal
recipe_active_rows <- function(retter_df, recipe_keys) {
  arrange(
    filter(retter_df, key %in% recipe_keys),
    tolower(retter)
  )
}

#' Lav valgmuligheder til opskriftsvælgeren
#'
#' Funktionen omdanner aktive retter til den navngivne vektor, som Shiny
#' forventer: brugeren ser rettens navn, mens modulet modtager opskriftens
#' stabile nøgle.
#'
#' @param retter_df Data frame med aktive retter.
#' @param recipe_keys Tegnvektor med nøgler på opskrifter, der findes.
#'
#' @return En navngivet tegnvektor med nøgler som værdier og rettenavne som
#'   labels.
#' @keywords internal
recipe_choices <- function(retter_df, recipe_keys) {
  retter_df <- recipe_active_rows(retter_df, recipe_keys)
  setNames(retter_df$key, retter_df$retter)
}

#' Formatér en ingrediens som én læsbar linje
#'
#' Mængde, enhed og ingrediens samles med enkelte mellemrum. Manglende værdier
#' fjernes, så brugeren for eksempel ser `"2 stk tomater"` og ikke tekst med
#' `NA` eller dobbelte mellemrum.
#'
#' @param maengde Mængden for en eller flere ingredienser.
#' @param enhed Enheden for en eller flere ingredienser.
#' @param ingrediens Navnet på en eller flere ingredienser.
#'
#' @return En tegnvektor med færdigformaterede ingredienslinjer.
#' @keywords internal
recipe_format_line <- function(maengde, enhed, ingrediens) {
  linje <- paste(maengde, enhed, ingrediens)
  linje <- gsub("NA", "", linje)
  trimws(gsub("\\s+", " ", linje))
}

#' Normalisér et link til en opskrift
#'
#' Tomme og manglende links bliver til en tom tekst. Links uden protokol får
#' `https://`, mens komplette HTTP- og HTTPS-links bevares. Dermed kan linket
#' bruges direkte som `href` i brugerfladen.
#'
#' @param x Linket som tekst; kun den første værdi anvendes.
#'
#' @return Et normaliseret link eller en tom tekst.
#' @keywords internal
recipe_normalize_link <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) return("")

  x <- trimws(as.character(x[[1]]))
  if (!nzchar(x)) return("")
  if (grepl("^https?://", x, ignore.case = TRUE)) return(x)
  if (grepl("^//", x)) return(paste0("https:", x))

  paste0("https://", x)
}

#' Byg redigeringsknappen til en ingrediensrække
#'
#' Funktionen laver en namespacet knap, der sender opskriftsnøgle og
#' rækkenummer tilbage til Shiny. Den bruges som callback i `vapply()`, så der
#' ikke defineres anonyme funktioner inde i servermodulet.
#'
#' @param row Ingrediensens rækkenummer.
#' @param key Nøglen til den valgte opskrift.
#' @param ns Modulets namespace-funktion.
#' @param event_id Det fulde input-id, som skal modtage klik-hændelsen.
#'
#' @return Knappen som HTML-tekst.
#' @keywords internal
recipe_edit_button <- function(row, key, ns, event_id) {
  as.character(
    ga_js_button(
      inputId = ns(paste0(
        "opskrift_row_btn_",
        key,
        "_",
        row
      )),
      label = NULL,
      icon = icon("pen"),
      class = "edit-btn btn btn-sm",
      onclick = sprintf(
        paste0(
          "Shiny.setInputValue(",
          "'%s', {key: '%s', row: %d}, ",
          "{priority:'event'}); return false;"
        ),
        event_id,
        key,
        row
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

#' Byg sletteknappen til en ingrediensrække
#'
#' Funktionen laver en namespacet knap, der sender opskriftsnøgle og
#' rækkenummer tilbage til Shiny, når brugeren vil slette en ingrediens. Den
#' bruges som en navngivet callback i stedet for en anonym serverfunktion.
#'
#' @param row Ingrediensens rækkenummer.
#' @param key Nøglen til den valgte opskrift.
#' @param ns Modulets namespace-funktion.
#' @param event_id Det fulde input-id, som skal modtage klik-hændelsen.
#'
#' @return Knappen som HTML-tekst.
#' @keywords internal
recipe_delete_button <- function(row, key, ns, event_id) {
  as.character(
    ga_js_button(
      inputId = ns(paste0(
        "opskrift_row_del_",
        key,
        "_",
        row
      )),
      label = NULL,
      icon = icon("trash"),
      class = "delete-btn btn btn-sm",
      onclick = sprintf(
        paste0(
          "Shiny.setInputValue(",
          "'%s', {key: '%s', row: %d}, ",
          "{priority:'event'}); return false;"
        ),
        event_id,
        key,
        row
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
}

#' Byg én række i arkivet
#'
#' Funktionen viser navnet på en arkiveret ret og bygger knapperne til at
#' gendanne eller slette retten permanent. Alle id'er og events namespac'es,
#' så rækken kan bruges sikkert inde i modulet.
#'
#' @param row Rækkenummeret i arkivets data frame.
#' @param archive Data frame med arkiverede retter.
#' @param ns Modulets namespace-funktion.
#' @param restore_event_id Det fulde input-id til gendannelse.
#' @param delete_event_id Det fulde input-id til permanent sletning.
#'
#' @return Et Shiny `div`-tag med rettens navn og to handlingsknapper.
#' @keywords internal
recipe_archive_row_ui <- function(
  row,
  archive,
  ns,
  restore_event_id,
  delete_event_id
) {
  key <- archive$key[[row]]

  tags$div(
    class = "archive-recipe-row",
    tags$span(archive$retter[[row]]),
    tags$div(
      class = "archive-recipe-actions",
      ga_js_button(
        inputId = ns(paste0("restore_ret_btn_", key)),
        label = "Gendan",
        class = paste(
          "archive-action-btn",
          "archive-action-restore"
        ),
        onclick = sprintf(
          paste0(
            "Shiny.setInputValue(",
            "'%s', '%s', {priority:'event'}); ",
            "return false;"
          ),
          restore_event_id,
          key
        )
      ),
      ga_js_button(
        inputId = ns(paste0(
          "delete_archived_ret_btn_",
          key
        )),
        label = "Slet permanent",
        class = paste(
          "archive-action-btn",
          "archive-action-delete"
        ),
        onclick = sprintf(
          paste0(
            "Shiny.setInputValue(",
            "'%s', '%s', {priority:'event'}); ",
            "return false;"
          ),
          delete_event_id,
          key
        )
      )
    )
  )
}

#' Byg visningsrækkerne til en opskrifts ingredienstabel
#'
#' Funktionen formaterer og HTML-sikrer hver ingredienslinje og tilføjer de
#' to handlingsknapper, som sender rækkenummer og opskriftsnøgle tilbage til
#' servermodulet.
#'
#' @param recipe Data frame med den valgte opskrifts ingredienser.
#' @param key Den valgte opskrifts stabile nøgle.
#' @param ns Modulets namespace-funktion.
#'
#' @return En data frame med kolonnerne `Ingrediens`, `Rediger` og `Slet`.
#' @keywords internal
recipe_ingredient_table_rows <- function(recipe, key, ns) {
  ingredienslinje <- recipe_format_line(
    recipe$maengde,
    recipe$enhed,
    recipe[[1]]
  )
  rows <- data.frame(
    Ingrediens = htmlEscape(ingredienslinje),
    check.names = FALSE
  )

  edit_event_id <- ns("opskrift_editPressed")
  delete_event_id <- ns("opskrift_deletePressed")

  rows$Rediger <- vapply(
    seq_len(nrow(rows)),
    recipe_edit_button,
    "",
    key = key,
    ns = ns,
    event_id = edit_event_id
  )
  rows$Slet <- vapply(
    seq_len(nrow(rows)),
    recipe_delete_button,
    "",
    key = key,
    ns = ns,
    event_id = delete_event_id
  )

  rows
}

#' Byg DT-widgetten til den valgte opskrifts ingredienser
#'
#' Funktionen anvender appens fælles tabeltema og viser alle ingredienser uden
#' søgning, sortering eller sideinddeling. Ingredienslinjen er allerede
#' HTML-sikret, mens handlingsknapperne skal fortolkes som HTML.
#'
#' @param rows Resultatet fra `recipe_ingredient_table_rows()`.
#'
#' @return Et DT-widget-objekt med ingredienser og handlingsknapper.
#' @keywords internal
recipe_ingredient_table_widget <- function(rows) {
  themed_dt(
    rows,
    escape = c(FALSE, FALSE, FALSE),
    options = list(
      dom = "t",
      paging = FALSE,
      ordering = FALSE,
      searching = FALSE
    )
  )
}

#' Byg oversigten over aktive og arkiverede opskrifter
#'
#' Funktionen filtrerer aktive retter uden en eksisterende opskriftsfil fra,
#' vælger en gyldig opskrift og bygger både opskriftsvælgeren og den valgfrie
#' arkivblok. Den modtager kun almindelige værdier og kender derfor ikke til
#' modulets reaktive state.
#'
#' @param active_retter Data frame med aktive retter.
#' @param recipe_keys Tegnvektor med nøglerne på eksisterende opskrifter.
#' @param archive Data frame med arkiverede retter.
#' @param selected_key Den aktuelt valgte opskriftsnøgle eller `NULL`.
#' @param ns Modulets namespace-funktion.
#'
#' @return En Shiny-tagliste med vælger, valgt opskrift og eventuelt arkiv.
#' @keywords internal
recipe_overview_ui <- function(
  active_retter,
  recipe_keys,
  archive,
  selected_key,
  ns
) {
  active_retter <- recipe_active_rows(active_retter, recipe_keys)
  keys <- active_retter$key
  titles <- active_retter$retter

  archive_ui <- NULL
  if (nrow(archive) > 0) {
    restore_event_id <- ns("restore_ret")
    delete_archived_event_id <- ns("delete_archived_ret")

    archive_ui <- f7Block(
      inset = TRUE,
      strong = TRUE,
      tags$h3("Arkiv"),
      tags$p("Slettede retter ligger her og kan gendannes."),
      tagList(lapply(
        seq_len(nrow(archive)),
        recipe_archive_row_ui,
        archive = archive,
        ns = ns,
        restore_event_id = restore_event_id,
        delete_event_id = delete_archived_event_id
      ))
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

  if (is.null(selected_key) || !selected_key %in% keys) {
    selected_key <- keys[[1]]
  }

  tagList(
    f7Block(
      inset = TRUE,
      strong = TRUE,
      selectizeInput(
        ns("opskrift_valgt_key"),
        "Vælg opskrift",
        choices = setNames(keys, titles),
        selected = selected_key,
        width = "100%",
        options = opskrift_selectize_options
      )
    ),
    uiOutput(ns("valgt_opskrift_ui")),
    archive_ui
  )
}

#' Byg panelet til den valgte opskrift
#'
#' Funktionen viser rettens navn, knapperne til at tilføje og arkivere samt
#' ingredienstabellen. Hvis modellen indeholder et link, bygges det som et
#' eksternt link med sikre browser-attributter.
#'
#' @param model Navngivet liste med `key`, `ret_navn` og `link_url` fra
#'   serverens valgte opskriftsmodel.
#' @param ns Modulets namespace-funktion.
#'
#' @return Et Shiny `div`-tag med hele panelet for den valgte opskrift.
#' @keywords internal
recipe_selected_ui <- function(model, ns) {
  key <- model$key
  recipe_name <- model$ret_navn
  link_url <- model$link_url

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

  add_event_id <- ns("opskrift_addPressed")
  archive_event_id <- ns("opskrift_archivePressed")

  tags$div(
    id = ns(paste0("opskrift_", key)),
    class = "opskrift-anchor",
    f7Block(
      inset = TRUE,
      strong = TRUE,
      tags$h3(recipe_name),
      tags$div(
        class = "recipe-action-bar",
        ga_js_button(
          inputId = ns(paste0("opskrift_add_btn_", key)),
          label = "Tilføj vare",
          class = "recipe-action-btn recipe-action-add",
          onclick = sprintf(
            paste0(
              "Shiny.setInputValue(",
              "'%s', {key: '%s'}, {priority:'event'}); ",
              "return false;"
            ),
            add_event_id,
            key
          )
        ),
        ga_js_button(
          inputId = ns(paste0("opskrift_archive_btn_", key)),
          label = "Arkiver ret",
          class = "recipe-action-btn recipe-action-archive",
          onclick = sprintf(
            paste0(
              "Shiny.setInputValue(",
              "'%s', {key: '%s'}, {priority:'event'}); ",
              "return false;"
            ),
            archive_event_id,
            key
          )
        )
      ),
      br(),
      DTOutput(ns("opskrift_tbl_valgt")),
      link_tag
    )
  )
}
