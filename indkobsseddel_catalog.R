library(dplyr)

# Rene katalog- og valgregler for indkøbssedlen --------------------------
#
# Funktionerne i denne fil arbejder kun med de data, de får udleveret.
# De kender hverken Shiny-sessioner, reaktiv state eller filer og kan derfor
# testes direkte uden at starte appen.

#' Rens én tekstværdi fra et input
#'
#' @param value En vilkårlig værdi, typisk fra et Shiny-input.
#'
#' @return Første element som trimmet tekst eller `""`.
#' @keywords internal
indkobsseddel_clean_text <- function(value) {
  if (
    is.null(value) ||
      length(value) == 0L ||
      is.na(value[[1]])
  ) {
    return("")
  }

  trimws(as.character(value[[1]]))
}

#' Læs et positivt tal fra et input
#'
#' @param value En vilkårlig værdi, typisk fra et numeric input.
#'
#' @return Et endeligt tal større end nul eller `NA_real_`.
#' @keywords internal
indkobsseddel_positive_number <- function(value) {
  if (is.null(value) || length(value) != 1L) return(NA_real_)

  result <- suppressWarnings(as.numeric(value))
  if (
    length(result) != 1L ||
      is.na(result) ||
      !is.finite(result) ||
      result <= 0
  ) {
    return(NA_real_)
  }

  result
}

#' Læs antal personer med appens sikre standardværdi
#'
#' Et endnu ikke initialiseret input svarer til standardværdien 2. En
#' udtrykkeligt ugyldig værdi, eksempelvis 0 eller negativ tekst, afvises
#' fortsat som `NA_real_`.
#'
#' @param value Værdien fra opskriftsdialogens personfelt.
#'
#' @return Et positivt antal eller `NA_real_`.
#' @keywords internal
indkobsseddel_person_count <- function(value) {
  if (is.null(value) || length(value) == 0L) return(2)
  indkobsseddel_positive_number(value)
}

#' Opret en tom tabel med cartens fem datakolonner
#'
#' @return En tom data frame med navn, mængde, enhed og kategorier.
#' @keywords internal
indkobsseddel_empty_rows <- function() {
  data.frame(
    Indkobsliste = character(),
    maengde = numeric(),
    enhed = character(),
    kat_1 = character(),
    kat_2 = character(),
    stringsAsFactors = FALSE
  )
}

#' Gør opskriftsrækker klar til carten
#'
#' Opskriftsfiler bruger rettens navn som første kolonnenavn. Funktionen
#' omdøber denne kolonne og sikrer en ensartet femkolonnestruktur.
#'
#' @param rows Data frame med ingrediensrækker.
#' @param label Læsevenligt navn til eventuelle fejlbeskeder.
#'
#' @return En data frame med cartens fem inputkolonner.
#' @keywords internal
indkobsseddel_as_cart_rows <- function(rows, label = "Data") {
  expected_tail <- c("maengde", "enhed", "kat_1", "kat_2")

  if (is.null(rows) || nrow(rows) == 0L) {
    return(indkobsseddel_empty_rows())
  }
  if (
    !is.data.frame(rows) ||
      ncol(rows) != 5L ||
      !identical(names(rows)[2:5], expected_tail)
  ) {
    stop(
      paste(
        label,
        "skal have en varekolonne efterfulgt af",
        "maengde, enhed, kat_1 og kat_2."
      ),
      call. = FALSE
    )
  }

  result <- as.data.frame(rows, stringsAsFactors = FALSE)
  names(result)[1] <- "Indkobsliste"
  result$maengde <- suppressWarnings(as.numeric(result$maengde))

  character_columns <- c(
    "Indkobsliste",
    "enhed",
    "kat_1",
    "kat_2"
  )
  for (column in character_columns) {
    result[[column]] <- as.character(result[[column]])
    result[[column]][is.na(result[[column]])] <- ""
  }

  result$Indkobsliste <- trimws(result$Indkobsliste)
  result <- result[nzchar(result$Indkobsliste), , drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Hent og skalér én opskrift
#'
#' @param recipes Navngivet liste med opskrifter.
#' @param index Data frame med kolonnerne `retter` og `key`.
#' @param selected_name Rettens viste navn.
#' @param persons Antal personer.
#' @param label Beskrivelse til en eventuel valideringsfejl.
#'
#' @return Opskriftens ingredienser i cart-format. Hvis intet gyldigt navn er
#'   valgt, returneres en tom tabel.
#' @keywords internal
indkobsseddel_scaled_recipe <- function(
  recipes,
  index,
  selected_name,
  persons,
  label
) {
  selected_name <- indkobsseddel_clean_text(selected_name)
  if (!nzchar(selected_name)) return(indkobsseddel_empty_rows())
  if (
    !is.list(recipes) ||
      !is.data.frame(index) ||
      !all(c("retter", "key") %in% names(index))
  ) {
    return(indkobsseddel_empty_rows())
  }

  matches <- which(
    as.character(index$retter) == selected_name &
      as.character(index$key) %in% names(recipes)
  )
  if (length(matches) != 1L) return(indkobsseddel_empty_rows())

  key <- as.character(index$key[[matches[[1]]]])
  result <- indkobsseddel_as_cart_rows(recipes[[key]], label)
  result$maengde <- result$maengde * persons
  result
}

#' Hent og skalér et valgt tilbehør
#'
#' @param tilbehor Data frame med tilbehørsvarer.
#' @param selected_name Navnet på det valgte tilbehør.
#' @param persons Antal personer.
#'
#' @return Tilbehøret i cart-format eller en tom tabel.
#' @keywords internal
indkobsseddel_scaled_accessory <- function(
  tilbehor,
  selected_name,
  persons
) {
  selected_name <- indkobsseddel_clean_text(selected_name)
  if (
    !nzchar(selected_name) ||
      !is.data.frame(tilbehor) ||
      !"Indkobsliste" %in% names(tilbehor)
  ) {
    return(indkobsseddel_empty_rows())
  }

  rows <- tilbehor[
    as.character(tilbehor$Indkobsliste) == selected_name,
    ,
    drop = FALSE
  ]
  rows <- indkobsseddel_as_cart_rows(rows, "Tilbehør")
  rows$maengde <- round(rows$maengde * persons, 4)
  rows
}

#' Find et entydigt link til en opskrift
#'
#' @param links Data frame med kolonnerne `ret` og `link`.
#' @param name Rettens viste navn.
#'
#' @return Linket som tekst eller `NULL`, hvis der ikke er præcis ét match.
#' @keywords internal
indkobsseddel_recipe_link <- function(links, name) {
  name <- indkobsseddel_clean_text(name)
  if (
    !nzchar(name) ||
      !is.data.frame(links) ||
      !all(c("ret", "link") %in% names(links))
  ) {
    return(NULL)
  }

  matches <- links$link[as.character(links$ret) == name]
  matches <- as.character(matches)
  matches <- matches[!is.na(matches) & nzchar(matches)]
  if (length(matches) != 1L) return(NULL)

  matches[[1]]
}

#' Saml en opskriftsdialog til cart-rækker og kopinoter
#'
#' Funktionen beregner de synlige ingredienser og de skjulte opskriftsafsnit
#' fra det samme snapshot. Derfor kan preview og tilføjelse ikke komme ud af
#' takt, når et reaktivt katalog ændres.
#'
#' @param recipes Navngivet liste med almindelige opskrifter.
#' @param active_retter Data frame med aktive retter.
#' @param links Data frame med opskriftslinks.
#' @param salater Data frame med salatnavne og -nøgler.
#' @param salater_opskrifter Navngivet liste med salatopskrifter.
#' @param tilbehor Data frame med tilbehør.
#' @param selected_recipe Valgt almindelig ret.
#' @param selected_salad Valgt salat.
#' @param persons Antal personer.
#' @param selected_accessory Valgt tilbehør.
#'
#' @return En liste med `rows` til carten og `sections` til kopiteksten.
#' @keywords internal
indkobsseddel_prepare_recipe <- function(
  recipes,
  active_retter,
  links,
  salater,
  salater_opskrifter,
  tilbehor,
  selected_recipe,
  selected_salad,
  persons,
  selected_accessory
) {
  persons <- indkobsseddel_positive_number(persons)
  if (is.na(persons)) {
    return(list(
      rows = indkobsseddel_empty_rows(),
      sections = list()
    ))
  }

  selected_recipe <- indkobsseddel_clean_text(selected_recipe)
  selected_salad <- indkobsseddel_clean_text(selected_salad)
  selected_accessory <- indkobsseddel_clean_text(selected_accessory)

  recipe_rows <- indkobsseddel_scaled_recipe(
    recipes,
    active_retter,
    selected_recipe,
    persons,
    "Opskriften"
  )
  salad_rows <- indkobsseddel_scaled_recipe(
    salater_opskrifter,
    salater,
    selected_salad,
    persons,
    "Salaten"
  )
  accessory_rows <- indkobsseddel_scaled_accessory(
    tilbehor,
    selected_accessory,
    persons
  )
  rows <- bind_rows(recipe_rows, accessory_rows, salad_rows)
  sections <- list()

  if (nrow(recipe_rows) > 0L) {
    section_rows <- recipe_rows
    title <- selected_recipe
    link <- indkobsseddel_recipe_link(links, selected_recipe)

    if (nrow(salad_rows) > 0L) {
      section_rows <- bind_rows(recipe_rows, salad_rows)
      title <- paste0(selected_recipe, " m. ", selected_salad)
      if (is.null(link)) {
        link <- indkobsseddel_recipe_link(links, selected_salad)
      }
    }

    sections[[length(sections) + 1L]] <- list(
      title = title,
      pers = persons,
      df = section_rows,
      link = link
    )
  } else {
    if (nrow(salad_rows) > 0L) {
      sections[[length(sections) + 1L]] <- list(
        title = paste0("Salat: ", selected_salad),
        pers = persons,
        df = salad_rows,
        link = indkobsseddel_recipe_link(links, selected_salad)
      )
    }
    if (nrow(accessory_rows) > 0L) {
      sections[[length(sections) + 1L]] <- list(
        title = paste0("Tilbehør: ", selected_accessory),
        pers = persons,
        df = accessory_rows,
        link = NA_character_
      )
    }
  }

  list(rows = rows, sections = sections)
}

#' Filtrér aktive retter til opskrifter, der faktisk findes
#'
#' @param active_retter Data frame med aktive retter.
#' @param recipe_keys Navnene på de indlæste opskrifter.
#'
#' @return En alfabetisk sorteret data frame med gyldige aktive retter.
#' @keywords internal
indkobsseddel_active_recipe_rows <- function(
  active_retter,
  recipe_keys
) {
  if (
    !is.data.frame(active_retter) ||
      !all(c("retter", "key") %in% names(active_retter))
  ) {
    return(data.frame(
      retter = character(),
      key = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- active_retter[
    as.character(active_retter$key) %in% recipe_keys,
    ,
    drop = FALSE
  ]
  rows <- rows[
    order(tolower(as.character(rows$retter))),
    ,
    drop = FALSE
  ]
  rownames(rows) <- NULL
  rows
}

#' Hent valgmuligheder fra én navngivet kolonne
#'
#' @param data Data frame med valgmuligheder.
#' @param column Navnet på kolonnen.
#' @param include_blank Om et tomt valg skal stå først.
#'
#' @return En unik tegnvektor med valgmuligheder.
#' @keywords internal
indkobsseddel_named_choices <- function(
  data,
  column,
  include_blank = FALSE
) {
  if (!is.data.frame(data) || !column %in% names(data)) {
    return(if (isTRUE(include_blank)) "" else character())
  }

  values <- as.character(data[[column]])
  values <- trimws(values[!is.na(values)])
  values <- unique(values[nzchar(values)])
  values <- sort(values)
  if (isTRUE(include_blank)) values <- c("", values)
  values
}

#' Byg de aktuelle valg og standarder til opskriftsdialogen
#'
#' Funktionen samler dialogens værdier hver gang den åbnes. Derfor bliver
#' opskrifter, salater og tilbehør, som er ændret efter appstart, vist med det
#' samme, mens alle frivillige valg fortsat starter tomme.
#'
#' @param recipes Navngivet liste med de aktuelle opskrifter.
#' @param active_retter Data frame med de aktive opskrifter.
#' @param salater Data frame med de aktuelle salater.
#' @param tilbehor Data frame med det aktuelle tilbehør.
#'
#' @return En liste med valgmuligheder og dialogens sikre standardværdier.
#' @keywords internal
indkobsseddel_recipe_dialog_values <- function(
  recipes,
  active_retter,
  salater,
  tilbehor
) {
  active <- indkobsseddel_active_recipe_rows(
    active_retter,
    names(recipes)
  )

  list(
    recipe_choices = c("", as.character(active$retter)),
    selected_recipe = "",
    persons = 2,
    salad_choices = indkobsseddel_named_choices(
      salater,
      "retter",
      include_blank = TRUE
    ),
    selected_salad = "",
    accessory_choices = indkobsseddel_named_choices(
      tilbehor,
      "Indkobsliste",
      include_blank = TRUE
    ),
    selected_accessory = ""
  )
}

#' Saml og sortér generelle valgmuligheder
#'
#' @param values Værdier fra et aktuelt katalog.
#' @param defaults Standardværdier, som altid bør tilbydes.
#' @param include_blank Om et tomt valg skal medtages.
#'
#' @return En sorteret tegnvektor uden dubletter.
#' @keywords internal
indkobsseddel_choice_values <- function(
  values,
  defaults = character(),
  include_blank = FALSE
) {
  result <- as.character(c(defaults, values))
  result <- trimws(result[!is.na(result)])
  result <- unique(result)
  if (!isTRUE(include_blank)) result <- result[nzchar(result)]
  result <- sort(result)

  if (isTRUE(include_blank)) {
    result <- c("", result[nzchar(result)])
  }
  unique(result)
}

#' Hent kategorier sikkert fra det aktuelle varekatalog
#'
#' Funktionen samler hoved- og underkategorier fra varekataloget. Mangler
#' kataloget eller en af kolonnerne, returneres stadig brugbare standardvalg,
#' så dialogen til manuel indtastning ikke ender med et tomt select-input.
#'
#' @param varer Det aktuelle varekatalog, normalt en data frame med kolonnerne
#'   `kat_1` og `kat_2`.
#'
#' @return En liste med de to tegnvektorer `category_1` og `category_2`.
#' @keywords internal
indkobsseddel_manual_category_choices <- function(varer) {
  category_1 <- if (
    is.data.frame(varer) &&
      "kat_1" %in% names(varer)
  ) {
    varer$kat_1
  } else {
    character()
  }
  category_2 <- if (
    is.data.frame(varer) &&
      "kat_2" %in% names(varer)
  ) {
    varer$kat_2
  } else {
    character()
  }

  list(
    category_1 = indkobsseddel_choice_values(
      category_1,
      defaults = "konserves",
      include_blank = FALSE
    ),
    category_2 = indkobsseddel_choice_values(
      category_2,
      defaults = "",
      include_blank = TRUE
    )
  )
}

#' Bevar et gyldigt valg ved opdatering af valgmuligheder
#'
#' Et eksisterende valg beholdes, hvis det stadig findes i det aktuelle
#' katalog. Er valget forsvundet eller endnu ikke sat, bruges den foretrukne
#' standard og derefter det første tilgængelige valg.
#'
#' @param current Det aktuelle inputvalg.
#' @param choices De nye valgmuligheder.
#' @param preferred Den foretrukne standardværdi.
#'
#' @return En enkelt tegnværdi, der findes blandt `choices`, eller `""`.
#' @keywords internal
indkobsseddel_preserved_choice <- function(
  current,
  choices,
  preferred = ""
) {
  current <- indkobsseddel_clean_text(current)
  choices <- as.character(choices)
  if (current %in% choices) return(current)

  indkobsseddel_preferred_choice(choices, preferred)
}

#' Vælg en foretrukken værdi med sikker fallback
#'
#' @param choices De aktuelle valgmuligheder.
#' @param preferred Den ønskede standardværdi.
#'
#' @return Standardværdien, hvis den findes, ellers første valg eller `""`.
#' @keywords internal
indkobsseddel_preferred_choice <- function(
  choices,
  preferred
) {
  choices <- as.character(choices)
  preferred <- indkobsseddel_clean_text(preferred)
  if (preferred %in% choices) return(preferred)
  if (length(choices) > 0L) return(choices[[1]])
  ""
}

#' Kontrollér varekatalogets grundstruktur
#'
#' @param varer Det samlede varekatalog.
#'
#' @return `TRUE`, når de fem forventede kolonner findes.
#' @keywords internal
indkobsseddel_has_item_columns <- function(varer) {
  required <- c(
    "Indkobsliste",
    "maengde",
    "enhed",
    "kat_1",
    "kat_2"
  )

  is.data.frame(varer) && all(required %in% names(varer))
}

#' Hent de entydige varenavne fra kataloget
#'
#' @param varer Det samlede varekatalog.
#'
#' @return En alfabetisk sorteret tegnvektor.
#' @keywords internal
indkobsseddel_item_names <- function(varer) {
  if (!indkobsseddel_has_item_columns(varer)) return(character())

  names <- as.character(varer$Indkobsliste)
  names <- trimws(names[!is.na(names)])
  sort(unique(names[nzchar(names)]))
}

#' Find én vare uden forskel på store og små bogstaver
#'
#' @param varer Det samlede varekatalog.
#' @param name Varenavnet fra dialogen.
#'
#' @return Den entydige varerække eller en tom tabel med samme kolonner.
#' @keywords internal
indkobsseddel_find_item <- function(varer, name) {
  if (!indkobsseddel_has_item_columns(varer)) {
    return(indkobsseddel_empty_rows())
  }

  name <- tolower(indkobsseddel_clean_text(name))
  if (!nzchar(name)) return(varer[0, , drop = FALSE])

  matches <- which(
    tolower(trimws(as.character(varer$Indkobsliste))) == name
  )
  if (length(matches) != 1L) return(varer[0, , drop = FALSE])

  result <- varer[matches[[1]], , drop = FALSE]
  result[c(
    "Indkobsliste",
    "maengde",
    "enhed",
    "kat_1",
    "kat_2"
  )]
}


