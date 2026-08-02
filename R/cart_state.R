# Kanonisk state for indkøbssedlen -----------------------------------------
#
# Denne fil indeholder indkøbssedlens autoritative data og de rene regler,
# der tilføjer, samler, redigerer og sletter varelinjer. Funktionerne kender
# hverken Shiny, dialoger eller filer og kan derfor testes direkte.

.cart_group_columns <- c("Indkobsliste", "enhed", "kat_1", "kat_2")
.cart_input_columns <- c("Indkobsliste", "maengde", "enhed", "kat_1", "kat_2")

#' Opret en tom tabel med indkøbsseddelens varelinjer
#'
#' Funktionen samler kolonnenavne og datatyper for indkøbsseddelens kanoniske
#' varelinjer. Den bruges, når en ny state oprettes, og når en tom visning skal
#' returneres.
#'
#' @return En tom data frame med alle kolonner, som en varelinje skal have.
empty_cart_rows <- function() {
  data.frame(
    line_id = character(),
    Indkobsliste = character(),
    maengde = numeric(),
    enhed = character(),
    kat_1 = character(),
    kat_2 = character(),
    display_override = character(),
    locked = logical(),
    stringsAsFactors = FALSE
  )
}

#' Opret en ny og tom indkøbsseddel-state
#'
#' En state indeholder varelinjer, noter fra opskrifter og det næste id, der
#' skal bruges til en ny varelinje.
#'
#' @return En tom liste med klassen `grocery_cart_state`.
new_cart_state <- function() {
  structure(
    list(
      rows = empty_cart_rows(),
      recipe_notes = list(),
      next_line_id = 1L
    ),
    class = "grocery_cart_state"
  )
}

#' Kontrollér at en værdi er en gyldig indkøbsseddel-state
#'
#' Funktionen stopper med en forklarende fejl, hvis staten eller dens tabel
#' mangler de nødvendige dele.
#'
#' @param state Den indkøbsseddel-state, der skal kontrolleres.
#'
#' @return Den kontrollerede state, usynligt.
#' @keywords internal
.assert_cart_state <- function(state) {
  required <- c("rows", "recipe_notes", "next_line_id")
  if (!is.list(state) || !all(required %in% names(state))) {
    stop("Ugyldig cart-state.", call. = FALSE)
  }

  row_columns <- names(empty_cart_rows())
  if (!is.data.frame(state$rows) || !all(row_columns %in% names(state$rows))) {
    stop("Cart-state mangler de forventede rækkekolonner.", call. = FALSE)
  }

  invisible(state)
}

#' Normalisér indkommende varelinjer
#'
#' Funktionen udvælger de forventede inputkolonner, konverterer mængden til
#' tal og sikrer, at tekstkolonner ikke indeholder `NA`.
#'
#' @param rows En data frame med varelinjer, eller `NULL`.
#'
#' @return En data frame med de fem normaliserede inputkolonner.
#' @keywords internal
.normalize_cart_rows <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) {
    return(data.frame(
      Indkobsliste = character(),
      maengde = numeric(),
      enhed = character(),
      kat_1 = character(),
      kat_2 = character(),
      stringsAsFactors = FALSE
    ))
  }

  missing_columns <- setdiff(.cart_input_columns, names(rows))
  if (length(missing_columns) > 0) {
    stop(
      sprintf("Cart-rækker mangler kolonner: %s", paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }

  out <- as.data.frame(rows[, .cart_input_columns, drop = FALSE], stringsAsFactors = FALSE)
  out$maengde <- suppressWarnings(as.numeric(out$maengde))

  character_columns <- setdiff(.cart_input_columns, "maengde")
  for (column in character_columns) {
    out[[column]] <- as.character(out[[column]])
    out[[column]][is.na(out[[column]])] <- ""
  }

  out
}

#' Læg mængder fra ens varelinjer sammen
#'
#' Manglende værdier ignoreres, medmindre alle mængder mangler.
#'
#' @param amounts En numerisk vektor med mængder.
#'
#' @return Summen som ét tal, eller `NA_real_` hvis alle værdier er `NA`.
#' @keywords internal
.sum_cart_amounts <- function(amounts) {
  if (all(is.na(amounts))) return(NA_real_)
  sum(amounts, na.rm = TRUE)
}

#' Saml ens indkommende varelinjer
#'
#' Linjer med samme varenavn, enhed og kategorier samles, og deres mængder
#' lægges sammen, før de føjes til indkøbssedlen.
#'
#' @param rows En data frame med indkommende varelinjer.
#'
#' @return En normaliseret data frame med én række pr. unik varegruppe.
#' @keywords internal
.aggregate_cart_rows <- function(rows) {
  rows <- .normalize_cart_rows(rows)
  if (nrow(rows) == 0) return(rows)

  rows |>
    dplyr::group_by(dplyr::across(dplyr::all_of(.cart_group_columns))) |>
    dplyr::summarise(
      maengde = .sum_cart_amounts(.data$maengde),
      .groups = "drop"
    ) |>
    dplyr::select(dplyr::all_of(.cart_input_columns))
}

#' Find varelinjer, der tilhører samme gruppe som en ny vare
#'
#' To varer regnes som ens, når varenavn, enhed og begge kategorier matcher.
#'
#' @param rows Indkøbsseddelens eksisterende varelinjer.
#' @param incoming_row Én normaliseret varelinje, der skal sammenlignes med.
#'
#' @return En logisk vektor med én værdi for hver eksisterende varelinje.
#' @keywords internal
.same_cart_group <- function(rows, incoming_row) {
  if (nrow(rows) == 0) return(logical())

  matches <- rep(TRUE, nrow(rows))
  for (column in .cart_group_columns) {
    matches <- matches & rows[[column]] == incoming_row[[column]][[1]]
  }
  matches
}

#' Føj varelinjer til indkøbssedlen
#'
#' Nye varer samles med en eksisterende, ulåst varelinje, når varegruppen er
#' den samme. Ellers oprettes en ny varelinje med et stabilt id.
#'
#' @param state Den nuværende indkøbsseddel-state.
#' @param incoming_rows En data frame med de varer, der skal tilføjes.
#'
#' @return Den opdaterede indkøbsseddel-state.
cart_add_rows <- function(state, incoming_rows) {
  .assert_cart_state(state)
  incoming_rows <- .aggregate_cart_rows(incoming_rows)
  if (nrow(incoming_rows) == 0) return(state)

  for (i in seq_len(nrow(incoming_rows))) {
    incoming <- incoming_rows[i, , drop = FALSE]
    matching_row <- which(
      !state$rows$locked & .same_cart_group(state$rows, incoming)
    )

    if (length(matching_row) > 0) {
      row_index <- matching_row[[1]]
      state$rows$maengde[[row_index]] <-
        .sum_cart_amounts(c(state$rows$maengde[[row_index]], incoming$maengde[[1]]))
      next
    }

    new_row <- data.frame(
      line_id = sprintf("cart_%d", state$next_line_id),
      Indkobsliste = incoming$Indkobsliste[[1]],
      maengde = incoming$maengde[[1]],
      enhed = incoming$enhed[[1]],
      kat_1 = incoming$kat_1[[1]],
      kat_2 = incoming$kat_2[[1]],
      display_override = NA_character_,
      locked = FALSE,
      stringsAsFactors = FALSE
    )

    state$rows <- rbind(state$rows, new_row)
    state$next_line_id <- state$next_line_id + 1L
  }

  state
}

#' Føj en opskrift til indkøbssedlen
#'
#' Funktionen tilføjer opskriftens varer og gemmer eventuelle afsnit som
#' noter, der senere kan komme med i den kopierede tekst.
#'
#' @param state Den nuværende indkøbsseddel-state.
#' @param incoming_rows En data frame med opskriftens varer.
#' @param recipe_sections En liste med opskriftsafsnit. Hvert afsnit kan
#'   indeholde `title`, `pers`, `df` og `link`.
#'
#' @return Den opdaterede indkøbsseddel-state.
cart_add_recipe <- function(state, incoming_rows, recipe_sections = list()) {
  state <- cart_add_rows(state, incoming_rows)

  if (length(recipe_sections) > 0) {
    notes <- lapply(recipe_sections, .recipe_section_to_note)
    state$recipe_notes <- c(state$recipe_notes, notes)
  }

  state
}

#' Sortér indkøbsseddelens varelinjer
#'
#' Varelinjerne sorteres først stabilt efter deres varegruppe og derefter i
#' den kategoriorden, som appens indkøbsseddel bruger.
#'
#' @param rows En data frame med indkøbsseddelens varelinjer.
#'
#' @return De samme varelinjer i visningsrækkefølge.
#' @keywords internal
.sort_cart_rows <- function(rows) {
  if (nrow(rows) == 0) return(rows)

  # Radix giver samme bytebaserede rækkefølge på Windows og Raspberry Pi og
  # matcher den rækkefølge, som den tidligere dplyr-gruppering producerede.
  rows <- rows[
    order(rows$Indkobsliste, rows$enhed, rows$kat_1, rows$kat_2, method = "radix"),
    ,
    drop = FALSE
  ]

  first <- c("frugt og grønt", "konserves")
  last <- "husholdning"
  categories <- unique(rows$kat_1)
  first <- intersect(first, categories)
  last <- intersect(last, setdiff(categories, first))
  middle <- sort(setdiff(categories, c(first, last)), method = "radix")
  category_order <- c(first, middle, last)

  rows$.sort_category <- factor(rows$kat_1, levels = category_order, ordered = TRUE)
  rows <- rows[order(rows$.sort_category, rows$kat_2, method = "radix"), , drop = FALSE]
  rows$.sort_category <- NULL
  row.names(rows) <- NULL
  rows
}

#' Formatér varelinjer som letlæselig tekst
#'
#' Mængde, enhed og varenavn samles, og overflødige mellemrum fjernes.
#'
#' @param maengde En vektor med varernes mængder.
#' @param enhed En vektor med varernes enheder.
#' @param varenavn En vektor med varernes navne.
#'
#' @return En tegnvektor med én formateret tekst for hver varelinje.
#' @keywords internal
.format_cart_lines <- function(maengde, enhed, varenavn) {
  amount_text <- ifelse(is.na(maengde), "", as.character(maengde))
  trimws(gsub("\\s+", " ", paste(amount_text, enhed, varenavn)))
}

#' Opret indkøbsseddelens fulde visningsdata
#'
#' Funktionen sorterer varelinjerne, formaterer deres visningstekst og
#' respekterer tekst, som brugeren selv har redigeret og låst.
#'
#' @param state Den indkøbsseddel-state, der skal vises.
#'
#' @return En data frame med `line_id` og `display` først, efterfulgt af
#'   varelinjens øvrige data.
cart_view <- function(state) {
  .assert_cart_state(state)
  if (nrow(state$rows) == 0) {
    out <- empty_cart_rows()
    out$display <- character()
    return(out[, c("line_id", "display", setdiff(names(out), c("line_id", "display"))), drop = FALSE])
  }

  out <- .sort_cart_rows(state$rows)

  # Bevarer den nuværende afrundingsadfærd. "stk " har med vilje samme
  # afsluttende mellemrum som i den eksisterende kode.
  round_up_units <- c("stk ", "dåse(r)", "pakke(r)", "rulle(r)")
  display_amount <- ifelse(
    out$enhed %in% round_up_units,
    ceiling(out$maengde),
    out$maengde
  )

  out$display <- .format_cart_lines(display_amount, out$enhed, out$Indkobsliste)
  use_override <- out$locked & !is.na(out$display_override)
  out$display[use_override] <- out$display_override[use_override]

  out[, c("line_id", "display", setdiff(names(out), c("line_id", "display"))), drop = FALSE]
}

#' Hent de synlige varelinjer fra indkøbssedlen
#'
#' Varelinjer uden en gyldig visningstekst sorteres fra.
#'
#' @param state Den indkøbsseddel-state, der skal læses.
#'
#' @return En data frame med indkøbsseddelens synlige varelinjer.
cart_visible <- function(state) {
  out <- cart_view(state)
  keep <- !is.na(out$display) & nzchar(out$display)
  out[keep, , drop = FALSE]
}

#' Redigér den viste tekst på én varelinje
#'
#' En gyldig, ikke-tom tekst gemmes som en manuel visningsoverstyring, og
#' varelinjen låses, så den ikke senere samles automatisk med andre varer.
#'
#' @param state Den nuværende indkøbsseddel-state.
#' @param line_id Id'et på den varelinje, der skal redigeres.
#' @param display_text Den tekst, der fremover skal vises for varelinjen.
#'
#' @return Den opdaterede state. Ved et ukendt id eller en tom tekst returneres
#'   staten uændret.
cart_edit_line <- function(state, line_id, display_text) {
  .assert_cart_state(state)
  if (is.null(line_id) || length(line_id) == 0 || is.na(line_id[[1]])) return(state)
  line_id <- as.character(line_id[[1]])
  row_index <- match(line_id, state$rows$line_id)
  if (is.na(row_index)) return(state)

  display_text <- if (is.null(display_text) || length(display_text) == 0 || is.na(display_text[[1]])) {
    ""
  } else {
    as.character(display_text[[1]])
  }

  # En tom override ville gøre rækken usynlig og umulig at redigere/slette.
  if (!nzchar(trimws(display_text))) return(state)

  state$rows$display_override[[row_index]] <- display_text
  state$rows$locked[[row_index]] <- TRUE
  state
}

#' Slet én varelinje fra indkøbssedlen
#'
#' @param state Den nuværende indkøbsseddel-state.
#' @param line_id Id'et på den varelinje, der skal slettes.
#'
#' @return Den opdaterede state. Ved et ukendt eller manglende id returneres
#'   staten uændret.
cart_delete_line <- function(state, line_id) {
  .assert_cart_state(state)
  if (is.null(line_id) || length(line_id) == 0 || is.na(line_id[[1]])) return(state)
  line_id <- as.character(line_id[[1]])
  row_index <- match(line_id, state$rows$line_id)
  if (is.na(row_index)) return(state)

  state$rows <- state$rows[-row_index, , drop = FALSE]
  row.names(state$rows) <- NULL
  state
}

#' Formatér én opskriftsingrediens som tekst
#'
#' Funktionen samler en ingrediens' mængde, enhed og varenavn. Hvis mængden
#' mangler, returneres kun varenavnet.
#'
#' @param row En navngivet opskriftsrække med `maengde`, `enhed` og
#'   `Indkobsliste`.
#'
#' @return Én tekststreng med den formaterede ingrediens.
#' @keywords internal
.format_recipe_ingredient <- function(row) {
  amount <- row[["maengde"]]
  unit <- row[["enhed"]]
  name <- row[["Indkobsliste"]]
  unit <- if (is.na(unit)) "" else as.character(unit)
  name <- if (is.na(name)) "" else as.character(name)

  if (!is.na(amount) && nzchar(as.character(amount))) {
    paste0(amount, if (nzchar(unit)) paste0(" ", unit) else "", " ", name)
  } else {
    name
  }
}

#' Omdan et opskriftsafsnit til en note til indkøbssedlen
#'
#' Ingredienstabellen omdannes til tekstlinjer, mens titel, antal personer og
#' link bevares til den senere kopiering.
#'
#' @param section En liste med `title`, `pers`, `df` og `link`.
#'
#' @return En liste med `title`, `pers`, `ingredient_lines` og `link`.
#' @keywords internal
.recipe_section_to_note <- function(section) {
  ingredient_lines <- character()
  if (!is.null(section$df) && nrow(section$df) > 0) {
    ingredient_lines <- apply(section$df, 1, .format_recipe_ingredient)
  }

  list(
    title = section$title,
    pers = section$pers,
    ingredient_lines = ingredient_lines,
    link = section$link
  )
}

#' Klargør indkøbssedlen til kopiering
#'
#' Resultatet adskiller de synlige varelinjer fra de skjulte opskriftsnoter og
#' angiver, hvor kopiteksten skal have en blank linje mellem hovedkategorier.
#'
#' @param state Den indkøbsseddel-state, der skal kopieres.
#'
#' @return En liste med `visible`, `hidden`, `line_ids`, `n_visible` og
#'   `category_break_after`.
cart_copy_payload <- function(state) {
  visible_rows <- cart_visible(state)
  visible <- visible_rows$display
  category_break_after <- if (nrow(visible_rows) < 2L) {
    integer()
  } else {
    which(
      visible_rows$kat_1[-nrow(visible_rows)] !=
        visible_rows$kat_1[-1L]
    )
  }
  hidden <- character()

  for (note in state$recipe_notes) {
    hidden <- c(
      hidden,
      "",
      sprintf("%s (til %s pers.)", note$title, note$pers),
      note$ingredient_lines
    )

    link <- note$link
    has_link <- !is.null(link) && length(link) > 0 && !is.na(link[[1]]) && nzchar(link[[1]])
    if (has_link) hidden <- c(hidden, paste0("Link: ", link[[1]]))
  }

  # En helt tom indkøbsseddel kopierer og gemmer heller ikke løse opskriftsnoter.
  if (length(visible) == 0) hidden <- character()

  list(
    visible = visible,
    hidden = hidden,
    line_ids = visible_rows$line_id,
    n_visible = length(visible),
    category_break_after = category_break_after
  )
}

#' Opret den enkle tabel, der vises som indkøbsseddel
#'
#' @param state Den indkøbsseddel-state, der skal vises.
#'
#' @return En data frame med én kolonne, `Indkøbsliste`, og én række pr.
#'   synlig varelinje.
cart_display_data <- function(state) {
  data.frame(
    `Indkøbsliste` = cart_visible(state)$display,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
