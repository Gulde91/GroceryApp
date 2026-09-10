# Browserkladde for indkøbssedlen -----------------------------------------
#
# Browserens localStorage er klientstyret data og må derfor ikke gendannes
# direkte som et R-objekt. Denne fil omsætter den kanoniske cart-state til et
# lille, versionsstyret JSON-format og bygger kun en state igen efter streng
# validering.

.cart_draft_schema_version <- 1L
.cart_draft_max_bytes <- 2L * 1024L * 1024L
.cart_draft_max_rows <- 2000L
.cart_draft_max_notes <- 500L
.cart_draft_max_note_lines <- 5000L
.cart_draft_max_text_chars <- 10000L

#' Stop intern validering af en browserkladde
#'
#' @param message Fejlteksten, som kun bruges internt ved encoding.
#'
#' @return Funktionen returnerer ikke, men rejser en fejl uden call-stack.
#' @keywords internal
.cart_draft_abort <- function(message = "Browserkladden er ugyldig.") {
  stop(message, call. = FALSE)
}

#' Kontrollér at en liste har præcis de forventede feltnavne
#'
#' @param value Listen, der skal kontrolleres.
#' @param expected De tilladte feltnavne.
#'
#' @return Én logisk værdi.
#' @keywords internal
.cart_draft_has_exact_names <- function(value, expected) {
  actual <- names(value)
  !is.null(actual) &&
    length(actual) == length(expected) &&
    !anyDuplicated(actual) &&
    setequal(actual, expected)
}

#' Validér et tekstfelt fra browserkladden
#'
#' @param value Feltets værdi.
#' @param field Feltnavnet til en intern fejlbesked.
#' @param allow_empty Om tom tekst er gyldig.
#' @param allow_null Om `NULL` eller `NA` skal normaliseres til `NA_character_`.
#'
#' @return Én valideret tekstværdi.
#' @keywords internal
.cart_draft_text <- function(
  value,
  field,
  allow_empty = TRUE,
  allow_null = FALSE
) {
  if (is.null(value)) {
    if (isTRUE(allow_null)) return(NA_character_)
    .cart_draft_abort(sprintf("Browserkladden mangler feltet '%s'.", field))
  }
  if (
    is.character(value) &&
      length(value) == 1L &&
      is.na(value) &&
      isTRUE(allow_null)
  ) {
    return(NA_character_)
  }
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value)
  ) {
    .cart_draft_abort(sprintf("Browserkladdens felt '%s' er ugyldigt.", field))
  }

  text_length <- nchar(value, type = "chars", allowNA = TRUE)
  if (
    is.na(text_length) ||
      text_length > .cart_draft_max_text_chars ||
      (!isTRUE(allow_empty) && !nzchar(trimws(value)))
  ) {
    .cart_draft_abort(sprintf("Browserkladdens felt '%s' er ugyldigt.", field))
  }

  value
}

#' Validér et numerisk felt fra browserkladden
#'
#' @param value Feltets værdi.
#' @param field Feltnavnet til en intern fejlbesked.
#' @param allow_null Om `NULL` eller `NA` skal normaliseres til `NA_real_`.
#' @param positive Om tallet skal være større end nul.
#'
#' @return Én valideret numerisk værdi.
#' @keywords internal
.cart_draft_number <- function(
  value,
  field,
  allow_null = FALSE,
  positive = FALSE
) {
  if (is.null(value) || length(value) == 1L && is.numeric(value) && is.na(value)) {
    if (isTRUE(allow_null)) return(NA_real_)
    .cart_draft_abort(sprintf("Browserkladden mangler feltet '%s'.", field))
  }
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      !is.finite(value) ||
      (isTRUE(positive) && value <= 0)
  ) {
    .cart_draft_abort(sprintf("Browserkladdens felt '%s' er ugyldigt.", field))
  }

  as.numeric(value)
}

#' Validér et positivt heltal fra browserkladden
#'
#' @param value Feltets værdi.
#' @param field Feltnavnet til en intern fejlbesked.
#'
#' @return Én valideret integer-værdi.
#' @keywords internal
.cart_draft_integer <- function(value, field) {
  number <- .cart_draft_number(value, field)
  if (
    number < 1 ||
      number > .Machine$integer.max ||
      number != floor(number)
  ) {
    .cart_draft_abort(sprintf("Browserkladdens felt '%s' er ugyldigt.", field))
  }

  as.integer(number)
}

#' Validér et boolesk felt fra browserkladden
#'
#' @param value Feltets værdi.
#' @param field Feltnavnet til en intern fejlbesked.
#'
#' @return Én valideret logisk værdi.
#' @keywords internal
.cart_draft_boolean <- function(value, field) {
  if (
    !is.logical(value) ||
      length(value) != 1L ||
      is.na(value)
  ) {
    .cart_draft_abort(sprintf("Browserkladdens felt '%s' er ugyldigt.", field))
  }

  value
}

#' Validér en flad tekstvektor fra browserkladden
#'
#' @param value En tegnvektor fra intern state eller en JSON-array som liste.
#' @param field Feltnavnet til en intern fejlbesked.
#'
#' @return En valideret tegnvektor uden navne.
#' @keywords internal
.cart_draft_text_vector <- function(value, field) {
  if (is.character(value)) {
    values <- as.list(value)
  } else if (is.list(value) && is.null(names(value))) {
    values <- value
  } else {
    .cart_draft_abort(sprintf("Browserkladdens felt '%s' er ugyldigt.", field))
  }

  if (length(values) > .cart_draft_max_note_lines) {
    .cart_draft_abort(sprintf("Browserkladdens felt '%s' er for stort.", field))
  }

  vapply(
    values,
    .cart_draft_text,
    character(1),
    field = field,
    USE.NAMES = FALSE
  )
}

#' Normalisér og validér én varelinje fra browserkladden
#'
#' @param row En navngivet liste med felterne fra `empty_cart_rows()`.
#'
#' @return En valideret liste med varelinjens kanoniske værdier.
#' @keywords internal
.cart_draft_normalize_row <- function(row) {
  expected <- names(empty_cart_rows())
  if (!is.list(row) || !.cart_draft_has_exact_names(row, expected)) {
    .cart_draft_abort("Browserkladden indeholder en ugyldig varelinje.")
  }

  line_id <- .cart_draft_text(
    row$line_id,
    "line_id",
    allow_empty = FALSE
  )
  if (!grepl("^cart_[1-9][0-9]*$", line_id)) {
    .cart_draft_abort("Browserkladden indeholder et ugyldigt linje-id.")
  }
  numeric_id <- suppressWarnings(as.numeric(sub("^cart_", "", line_id)))
  if (!is.finite(numeric_id) || numeric_id > .Machine$integer.max) {
    .cart_draft_abort("Browserkladden indeholder et ugyldigt linje-id.")
  }

  amount <- .cart_draft_number(
    row$maengde,
    "maengde",
    allow_null = TRUE,
    positive = TRUE
  )
  locked <- .cart_draft_boolean(row$locked, "locked")
  display_override <- .cart_draft_text(
    row$display_override,
    "display_override",
    allow_empty = FALSE,
    allow_null = TRUE
  )
  if (
    isTRUE(locked) && is.na(display_override) ||
      !isTRUE(locked) && !is.na(display_override)
  ) {
    .cart_draft_abort(
      "Browserkladdens lås og manuelle visningstekst stemmer ikke overens."
    )
  }

  list(
    line_id = line_id,
    Indkobsliste = .cart_draft_text(
      row$Indkobsliste,
      "Indkobsliste",
      allow_empty = FALSE
    ),
    maengde = amount,
    enhed = .cart_draft_text(row$enhed, "enhed"),
    kat_1 = .cart_draft_text(row$kat_1, "kat_1"),
    kat_2 = .cart_draft_text(row$kat_2, "kat_2"),
    display_override = display_override,
    locked = locked,
    numeric_id = numeric_id
  )
}

#' Normalisér og validér én opskriftsnote fra browserkladden
#'
#' @param note En navngivet liste med titel, personantal, linjer og link.
#'
#' @return En valideret opskriftsnote.
#' @keywords internal
.cart_draft_normalize_note <- function(note) {
  expected <- c("title", "pers", "ingredient_lines", "link")
  if (!is.list(note) || !.cart_draft_has_exact_names(note, expected)) {
    .cart_draft_abort("Browserkladden indeholder en ugyldig opskriftsnote.")
  }

  list(
    title = .cart_draft_text(note$title, "title", allow_empty = FALSE),
    pers = .cart_draft_number(note$pers, "pers", positive = TRUE),
    ingredient_lines = .cart_draft_text_vector(
      note$ingredient_lines,
      "ingredient_lines"
    ),
    link = .cart_draft_text(note$link, "link", allow_null = TRUE)
  )
}

#' Omdan den kanoniske cart-state til kladdens JSON-payload
#'
#' @param state En gyldig `grocery_cart_state`.
#'
#' @return En JSON-kompatibel liste uden R-specifikke attributter.
#' @keywords internal
.cart_draft_state_payload <- function(state) {
  .assert_cart_state(state)
  if (
    !.cart_draft_has_exact_names(
      state,
      c("rows", "recipe_notes", "next_line_id")
    ) ||
      !is.data.frame(state$rows) ||
      nrow(state$rows) > .cart_draft_max_rows ||
      !is.list(state$recipe_notes) ||
      length(state$recipe_notes) > .cart_draft_max_notes
  ) {
    .cart_draft_abort("Indkøbssedlen kan ikke gemmes som browserkladde.")
  }

  rows <- lapply(seq_len(nrow(state$rows)), function(index) {
    row <- .cart_draft_normalize_row(
      as.list(state$rows[index, , drop = FALSE])
    )
    list(
      line_id = row$line_id,
      Indkobsliste = row$Indkobsliste,
      maengde = if (is.na(row$maengde)) NULL else row$maengde,
      enhed = row$enhed,
      kat_1 = row$kat_1,
      kat_2 = row$kat_2,
      display_override = if (is.na(row$display_override)) {
        NULL
      } else {
        row$display_override
      },
      locked = row$locked
    )
  })
  notes <- lapply(state$recipe_notes, function(note) {
    note <- .cart_draft_normalize_note(note)
    list(
      title = note$title,
      pers = note$pers,
      ingredient_lines = as.list(note$ingredient_lines),
      link = if (is.na(note$link)) NULL else note$link
    )
  })

  next_line_id <- .cart_draft_integer(state$next_line_id, "next_line_id")
  numeric_ids <- vapply(rows, function(row) {
    as.numeric(sub("^cart_", "", row$line_id))
  }, numeric(1))
  if (
    anyDuplicated(vapply(rows, `[[`, character(1), "line_id")) ||
      length(numeric_ids) > 0L && next_line_id <= max(numeric_ids)
  ) {
    .cart_draft_abort("Indkøbssedlens linje-id'er er ugyldige.")
  }

  list(
    rows = rows,
    recipe_notes = notes,
    next_line_id = next_line_id
  )
}

#' Byg en kanonisk cart-state fra en valideret kladde-payload
#'
#' @param payload Den afkodede `cart`-del af JSON-envelope'en.
#'
#' @return En valideret `grocery_cart_state`.
#' @keywords internal
.cart_draft_payload_state <- function(payload) {
  if (
    !is.list(payload) ||
      !.cart_draft_has_exact_names(
        payload,
        c("rows", "recipe_notes", "next_line_id")
      ) ||
      !is.list(payload$rows) ||
      !is.null(names(payload$rows)) ||
      length(payload$rows) > .cart_draft_max_rows ||
      !is.list(payload$recipe_notes) ||
      !is.null(names(payload$recipe_notes)) ||
      length(payload$recipe_notes) > .cart_draft_max_notes
  ) {
    .cart_draft_abort()
  }

  normalized_rows <- lapply(payload$rows, .cart_draft_normalize_row)
  line_ids <- vapply(
    normalized_rows,
    `[[`,
    character(1),
    "line_id"
  )
  numeric_ids <- vapply(
    normalized_rows,
    `[[`,
    numeric(1),
    "numeric_id"
  )
  next_line_id <- .cart_draft_integer(
    payload$next_line_id,
    "next_line_id"
  )
  if (
    anyDuplicated(line_ids) ||
      length(numeric_ids) > 0L && next_line_id <= max(numeric_ids)
  ) {
    .cart_draft_abort()
  }

  rows <- empty_cart_rows()
  if (length(normalized_rows) > 0L) {
    rows <- do.call(rbind, lapply(normalized_rows, function(row) {
      data.frame(
        line_id = row$line_id,
        Indkobsliste = row$Indkobsliste,
        maengde = row$maengde,
        enhed = row$enhed,
        kat_1 = row$kat_1,
        kat_2 = row$kat_2,
        display_override = row$display_override,
        locked = row$locked,
        stringsAsFactors = FALSE
      )
    }))
    row.names(rows) <- NULL
  }

  notes <- lapply(payload$recipe_notes, .cart_draft_normalize_note)
  structure(
    list(
      rows = rows,
      recipe_notes = notes,
      next_line_id = next_line_id
    ),
    class = "grocery_cart_state"
  )
}

#' Kod en indkøbsseddel som en versionsstyret browserkladde
#'
#' Kun den kanoniske cart-state gemmes. JSON-formatet indeholder ingen
#' sessions-id'er, netværksoplysninger eller andre hemmeligheder.
#'
#' @param state En gyldig `grocery_cart_state`.
#' @param saved_at Tidspunktet, som skal skrives i kladdens envelope.
#'
#' @return En JSON-streng, der kan gemmes atomisk i browserens localStorage.
cart_draft_encode <- function(state, saved_at = Sys.time()) {
  if (
    length(saved_at) != 1L ||
      !inherits(saved_at, "POSIXt") ||
      is.na(saved_at)
  ) {
    .cart_draft_abort("Browserkladdens gemmetidspunkt er ugyldigt.")
  }

  payload <- list(
    schema_version = .cart_draft_schema_version,
    saved_at = format(
      as.POSIXct(saved_at),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC",
      usetz = FALSE
    ),
    cart = .cart_draft_state_payload(state)
  )
  encoded <- as.character(jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = FALSE
  ))
  if (nchar(encoded, type = "bytes") > .cart_draft_max_bytes) {
    .cart_draft_abort("Indkøbssedlen er for stor til browserens kladdelager.")
  }

  encoded
}

#' Gendan en valideret indkøbsseddel fra browserens kladde
#'
#' Browserdata betragtes altid som upålidelige. Ugyldig JSON, ukendte
#' schema-versioner, for store payloads og ugyldige felter returnerer derfor
#' `NULL` i stedet for at kunne stoppe Shiny-sessionen.
#'
#' @param raw JSON-strengen fra browserens localStorage.
#'
#' @return En valideret `grocery_cart_state`, eller `NULL` ved ugyldige data.
cart_draft_decode <- function(raw) {
  tryCatch(
    {
      if (
        !is.character(raw) ||
          length(raw) != 1L ||
          is.na(raw) ||
          !nzchar(raw) ||
          nchar(raw, type = "bytes") > .cart_draft_max_bytes
      ) {
        .cart_draft_abort()
      }

      envelope <- jsonlite::fromJSON(raw, simplifyVector = FALSE)
      if (
        !is.list(envelope) ||
          !.cart_draft_has_exact_names(
            envelope,
            c("schema_version", "saved_at", "cart")
          ) ||
          !is.numeric(envelope$schema_version) ||
          length(envelope$schema_version) != 1L ||
          is.na(envelope$schema_version) ||
          envelope$schema_version != .cart_draft_schema_version
      ) {
        .cart_draft_abort()
      }

      saved_at <- .cart_draft_text(
        envelope$saved_at,
        "saved_at",
        allow_empty = FALSE
      )
      if (!grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
        saved_at
      )) {
        .cart_draft_abort()
      }
      parsed_time <- as.POSIXct(
        saved_at,
        format = "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      )
      if (
        is.na(parsed_time) ||
          !identical(
            format(
              parsed_time,
              "%Y-%m-%dT%H:%M:%SZ",
              tz = "UTC",
              usetz = FALSE
            ),
            saved_at
          )
      ) {
        .cart_draft_abort()
      }

      .cart_draft_payload_state(envelope$cart)
    },
    error = function(error) NULL
  )
}
