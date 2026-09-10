suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(shiny)
})

source(file.path("R", "cart_state.R"), encoding = "UTF-8")

cart_draft_source <- file.path("R", "cart_draft.R")
stopifnot(file.exists(cart_draft_source))
source(cart_draft_source, encoding = "UTF-8")

cart_draft_expect_null <- function(raw) {
  result <- tryCatch(
    cart_draft_decode(raw),
    error = function(error) {
      stop(
        "Ugyldig browser-state må ikke lukke Shiny-sessionen: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
  stopifnot(is.null(result))
}

cart_draft_row <- function(
  name,
  amount = 1,
  unit = "stk",
  category_1 = "konserves",
  category_2 = ""
) {
  data.frame(
    Indkobsliste = name,
    maengde = amount,
    enhed = unit,
    kat_1 = category_1,
    kat_2 = category_2,
    stringsAsFactors = FALSE
  )
}

# En realistisk kladde skal kunne gå gennem JSON og tilbage uden at miste
# Unicode, NA-mængder, manuelle tekstændringer, opskriftsnoter eller id'er.
draft_state <- new_cart_state()
draft_state <- cart_add_rows(
  draft_state,
  bind_rows(
    cart_draft_row(
      "Æbler & pærer",
      1.5,
      "kg",
      "frugt og grønt"
    ),
    cart_draft_row(
      "Citronsaft (tilbehør)",
      NA_real_,
      "",
      "konserves"
    )
  )
)
first_line_id <- draft_state$rows$line_id[[1L]]
draft_state <- cart_edit_line(
  draft_state,
  first_line_id,
  "Husk økologiske æbler & pærer"
)
draft_state <- cart_add_recipe(
  draft_state,
  cart_draft_row(
    "Crème fraîche",
    2,
    "dl",
    "mejeri"
  ),
  recipe_sections = list(list(
    title = "Tærte med porrer",
    pers = 3,
    df = bind_rows(
      cart_draft_row("Porrer", 2, "stk", "frugt og grønt"),
      cart_draft_row("Crème fraîche", 2, "dl", "mejeri")
    ),
    link = "https://example.com/opskrift?a=1&b=æ"
  ))
)

fixed_time <- as.POSIXct(
  "2026-09-03 10:11:12",
  tz = "UTC"
)
encoded_draft <- cart_draft_encode(
  draft_state,
  saved_at = fixed_time
)
stopifnot(
  is.character(encoded_draft),
  length(encoded_draft) == 1L,
  !is.na(encoded_draft),
  nzchar(encoded_draft)
)

decoded_envelope <- fromJSON(
  encoded_draft,
  simplifyVector = FALSE
)
stopifnot(
  identical(as.integer(decoded_envelope$schema_version), 1L),
  is.character(decoded_envelope$saved_at),
  length(decoded_envelope$saved_at) == 1L,
  is.list(decoded_envelope$cart),
  identical(
    sort(names(decoded_envelope$cart)),
    sort(c("rows", "recipe_notes", "next_line_id"))
  ),
  is.list(decoded_envelope$cart$rows),
  length(decoded_envelope$cart$rows) == nrow(draft_state$rows),
  all(vapply(
    decoded_envelope$cart$rows,
    function(row) {
      is.list(row) &&
        all(names(empty_cart_rows()) %in% names(row))
    },
    logical(1)
  ))
)

restored_draft <- cart_draft_decode(encoded_draft)
stopifnot(
  inherits(restored_draft, "grocery_cart_state"),
  identical(restored_draft, draft_state),
  identical(restored_draft$rows$line_id, draft_state$rows$line_id),
  identical(restored_draft$rows$locked, draft_state$rows$locked),
  identical(
    restored_draft$rows$display_override,
    draft_state$rows$display_override
  ),
  anyNA(restored_draft$rows$maengde),
  identical(restored_draft$recipe_notes, draft_state$recipe_notes),
  identical(restored_draft$next_line_id, draft_state$next_line_id)
)

# En gyldig, tom kladde er forskellig fra en ugyldig payload: den første
# gendannes som en typet tom state, mens den anden afvises som NULL.
empty_roundtrip <- cart_draft_decode(
  cart_draft_encode(new_cart_state(), saved_at = fixed_time)
)
stopifnot(
  inherits(empty_roundtrip, "grocery_cart_state"),
  identical(empty_roundtrip, new_cart_state())
)

# Alt fra localStorage er klientinput. Fejlformat, andre skemaversioner og
# strukturelt ugyldige states skal derfor blive sikre NULL-resultater.
invalid_payloads <- list(
  NULL,
  character(),
  NA_character_,
  c("{}", "{}"),
  42,
  "",
  "ikke-json",
  "null",
  "[]",
  "{}",
  '{"schema_version":2,"saved_at":"2026-09-03T10:11:12Z","cart":{}}',
  '{"schema_version":1,"saved_at":"ikke-en-dato","cart":{}}',
  paste0(
    '{"schema_version":1,"saved_at":"2026-09-03T10:11:12Z",',
    '"cart":{"rows":[],"recipe_notes":[],"next_line_id":0}}'
  )
)
invisible(lapply(invalid_payloads, cart_draft_expect_null))

mutate_draft_json <- function(mutator) {
  value <- fromJSON(encoded_draft, simplifyVector = FALSE)
  value <- mutator(value)
  toJSON(value, auto_unbox = TRUE, null = "null", na = "null")
}

invalid_structures <- list(
  # Rækkekolonner må ikke kunne mangle eller få forkerte typer.
  mutate_draft_json(function(value) {
    value$cart$rows[[1L]]$line_id <- NULL
    value
  }),
  mutate_draft_json(function(value) {
    value$cart$rows[[1L]]$locked <- "ja"
    value
  }),
  mutate_draft_json(function(value) {
    value$cart$rows[[1L]]$maengde <- "mange"
    value
  }),
  mutate_draft_json(function(value) {
    value$cart$rows[[1L]]$line_id <- "cart_1');alert(1)//"
    value
  }),
  mutate_draft_json(function(value) {
    value$cart$rows[[1L]]$locked <- FALSE
    value
  }),
  # Line-id'er skal være unikke, og next_line_id må ikke genbruge et id.
  mutate_draft_json(function(value) {
    value$cart$rows[[2L]]$line_id <- value$cart$rows[[1L]]$line_id
    value
  }),
  mutate_draft_json(function(value) {
    value$cart$next_line_id <- 1
    value
  }),
  # Opskriftsnoter har deres egen faste struktur.
  mutate_draft_json(function(value) {
    value$cart$recipe_notes[[1L]]$ingredient_lines <- list(list("ikke", "tekst"))
    value
  })
)
invisible(lapply(invalid_structures, cart_draft_expect_null))

# En syntaktisk gyldig payload må heller ikke kunne omgå størrelsesgrænsen.
oversized_payload <- mutate_draft_json(function(value) {
  value$cart$rows[[1L]]$Indkobsliste <- strrep("x", 2L * 1024L * 1024L)
  value
})
cart_draft_expect_null(oversized_payload)

# Encode arbejder kun på intern, allerede valideret state og skal derfor
# fejle tydeligt frem for at gemme et dokument, som ikke kan gendannes.
invalid_internal_state <- draft_state
invalid_internal_state$rows$locked[[1L]] <- "ja"
encode_failed <- tryCatch(
  {
    cart_draft_encode(invalid_internal_state, saved_at = fixed_time)
    FALSE
  },
  error = function(error) TRUE
)
stopifnot(encode_failed)

# Servermodulet skal vente på browserens load-svar, matche svarets request-id
# og derefter gemme hver reel cart-mutation med det samme.
suppressPackageStartupMessages({
  source(file.path("R", "funktioner.R"), encoding = "UTF-8")
  source(
    file.path("R", "indkobsseddel_catalog.R"),
    encoding = "UTF-8"
  )
  source(
    file.path("R", "indkobsseddel_view.R"),
    encoding = "UTF-8"
  )
  source(
    file.path("R", "indkobsseddel_module.R"),
    encoding = "UTF-8"
  )
})

cart_draft_recipe_read <- function() {
  list(
    recipes = function() list(),
    active_retter = function() data.frame(
      retter = character(),
      key = character(),
      type = character(),
      stringsAsFactors = FALSE
    ),
    links = function() data.frame(
      ret = character(),
      link = character(),
      stringsAsFactors = FALSE
    ),
    salater = function() data.frame(
      retter = "",
      key = "",
      type = "",
      stringsAsFactors = FALSE
    ),
    salater_opskrifter = function() list(),
    tilbehor = function() indkobsseddel_empty_rows()
  )
}

cart_draft_catalog <- function() {
  data.frame(
    Indkobsliste = "Testvare",
    maengde = 1,
    enhed = "stk",
    kat_1 = "konserves",
    kat_2 = "",
    stringsAsFactors = FALSE
  )
}

cart_draft_test_server <- function(id, save_cart = function(value) TRUE) {
  moduleServer(id, function(input, output, session) {
    module_api <- mod_indkobsseddel_server(
      input = input,
      output = output,
      session = session,
      recipe_read = cart_draft_recipe_read(),
      varer_current = cart_draft_catalog,
      save_cart = save_cart,
      popular_items = function() character()
    )
  })
}

cart_draft_capture_session <- function() {
  captured <- new.env(parent = emptyenv())
  captured$messages <- list()
  session <- shiny::MockShinySession$new()
  session$sendCustomMessage <- function(type, message) {
    captured$messages[[length(captured$messages) + 1L]] <- list(
      type = type,
      message = message
    )
    invisible(NULL)
  }
  list(session = session, captured = captured)
}

cart_draft_messages <- function(captured, type) {
  Filter(
    function(entry) identical(entry$type, type),
    captured$messages
  )
}

cart_draft_set_manual_item <- function(
  session,
  name,
  button_value
) {
  session$setInputs(
    manual_name = name,
    manual_amount = 1,
    manual_unit = "stk",
    manual_category_1 = "konserves",
    manual_category_2 = ""
  )
  session$setInputs(add_manual_item = button_value)
  session$flushReact()
}

normal_case <- cart_draft_capture_session()
shiny::testServer(
  cart_draft_test_server,
  session = normal_case$session,
  {
    session$flushReact()
    load_messages <- cart_draft_messages(
      normal_case$captured,
      "groceryapp_cart_draft_load"
    )
    stopifnot(
      length(load_messages) == 1L,
      length(cart_draft_messages(
        normal_case$captured,
        "groceryapp_cart_draft_save"
      )) == 0L
    )
    load_message <- load_messages[[1L]]$message
    stopifnot(
      is.character(load_message$input_id),
      grepl("cart_draft_restore$", load_message$input_id),
      is.character(load_message$request_id),
      length(load_message$request_id) == 1L,
      startsWith(load_message$request_id, "cart-draft-")
    )

    # Et gammelt svar fra en tidligere serversession må ikke åbne
    # save-gaten eller overskrive den nye sessions state.
    session$setInputs(cart_draft_restore = list(
      status = "found",
      request_id = "cart-draft-forkert-session",
      value = encoded_draft
    ))
    session$flushReact()
    stopifnot(
      identical(module_api$cart_current(), new_cart_state()),
      length(cart_draft_messages(
        normal_case$captured,
        "groceryapp_cart_draft_save"
      )) == 0L
    )

    session$setInputs(cart_draft_restore = list(
      status = "found",
      request_id = load_message$request_id,
      value = encoded_draft
    ))
    session$flushReact()
    stopifnot(identical(module_api$cart_current(), draft_state))

    saves_after_restore <- cart_draft_messages(
      normal_case$captured,
      "groceryapp_cart_draft_save"
    )
    stopifnot(length(saves_after_restore) == 1L)
    restored_save <- saves_after_restore[[1L]]$message
    stopifnot(
      identical(restored_save$request_id, load_message$request_id),
      identical(
        cart_draft_decode(restored_save$value),
        draft_state
      )
    )

    cart_draft_set_manual_item(session, "Efter restore", 1L)
    state_after_mutation <- module_api$cart_current()
    stopifnot(
      "Efter restore" %in% state_after_mutation$rows$Indkobsliste,
      length(cart_draft_messages(
        normal_case$captured,
        "groceryapp_cart_draft_save"
      )) == length(saves_after_restore) + 1L
    )
    latest_save <- tail(
      cart_draft_messages(
        normal_case$captured,
        "groceryapp_cart_draft_save"
      ),
      1L
    )[[1L]]$message
    stopifnot(identical(
      cart_draft_decode(latest_save$value),
      state_after_mutation
    ))

    # Restore accepteres præcis én gang. Et forsinket svar må ikke rulle en
    # efterfølgende brugerhandling tilbage.
    saves_before_replay <- length(cart_draft_messages(
      normal_case$captured,
      "groceryapp_cart_draft_save"
    ))
    session$setInputs(cart_draft_restore = list(
      status = "empty",
      request_id = load_message$request_id
    ))
    session$flushReact()
    stopifnot(
      identical(module_api$cart_current(), state_after_mutation),
      length(cart_draft_messages(
        normal_case$captured,
        "groceryapp_cart_draft_save"
      )) == saves_before_replay
    )

    # Også status-svar er sessionsbundne. Et gammelt conflict-svar må ikke
    # deaktivere denne sessions efterfølgende autosaves.
    session$setInputs(cart_draft_status = list(
      status = "conflict",
      request_id = "cart-draft-forkert-session"
    ))
    session$flushReact()
    cart_draft_set_manual_item(session, "Efter stale status", 2L)
    saves_after_stale_status <- length(cart_draft_messages(
      normal_case$captured,
      "groceryapp_cart_draft_save"
    ))
    stopifnot(saves_after_stale_status == saves_before_replay + 1L)

    # Et ægte konfliktsvar stopper derimod yderligere browseroverskrivning,
    # mens brugerens aktuelle cart fortsat virker i server-sessionen.
    session$setInputs(cart_draft_status = list(
      status = "conflict",
      request_id = load_message$request_id
    ))
    session$flushReact()
    cart_draft_set_manual_item(session, "Efter ægte konflikt", 3L)
    stopifnot(
      "Efter ægte konflikt" %in%
        module_api$cart_current()$rows$Indkobsliste,
      length(cart_draft_messages(
        normal_case$captured,
        "groceryapp_cart_draft_save"
      )) == saves_after_stale_status
    )
  }
)

# Hvis brugeren når at ændre sedlen, før localStorage svarer, vinder den nye
# brugerhandling. Den må hverken tabes eller blandes med den ældre kladde.
early_change_case <- cart_draft_capture_session()
shiny::testServer(
  cart_draft_test_server,
  session = early_change_case$session,
  {
    session$flushReact()
    load_message <- cart_draft_messages(
      early_change_case$captured,
      "groceryapp_cart_draft_load"
    )[[1L]]$message

    cart_draft_set_manual_item(session, "Tilføjet straks", 1L)
    early_state <- module_api$cart_current()
    stopifnot(
      identical(early_state$rows$Indkobsliste, "Tilføjet straks"),
      length(cart_draft_messages(
        early_change_case$captured,
        "groceryapp_cart_draft_save"
      )) == 0L
    )

    session$setInputs(cart_draft_restore = list(
      status = "found",
      request_id = load_message$request_id,
      value = encoded_draft
    ))
    session$flushReact()
    stopifnot(
      identical(module_api$cart_current(), early_state),
      !any(
        draft_state$rows$Indkobsliste %in%
          module_api$cart_current()$rows$Indkobsliste
      )
    )
    early_saves <- cart_draft_messages(
      early_change_case$captured,
      "groceryapp_cart_draft_save"
    )
    stopifnot(
      length(early_saves) == 1L,
      identical(
        cart_draft_decode(early_saves[[1L]]$message$value),
        early_state
      )
    )
  }
)

# Når den sidste vare slettes, er sedlen reelt afsluttet. Den lokale kladde
# skal fjernes, og eventuelle gamle opskriftsnoter/id'er må ikke følge med
# videre til en ny seddel i samme session.
empty_cart_case <- cart_draft_capture_session()
shiny::testServer(
  cart_draft_test_server,
  session = empty_cart_case$session,
  {
    session$flushReact()
    load_message <- cart_draft_messages(
      empty_cart_case$captured,
      "groceryapp_cart_draft_load"
    )[[1L]]$message

    session$setInputs(cart_draft_restore = list(
      status = "found",
      request_id = load_message$request_id,
      value = encoded_draft
    ))
    session$flushReact()
    stopifnot(length(module_api$cart_current()$recipe_notes) > 0L)

    for (line_id in draft_state$rows$line_id) {
      session$setInputs(delete_pressed = line_id)
      session$flushReact()
    }

    stopifnot(identical(module_api$cart_current(), new_cart_state()))
    final_save <- tail(
      cart_draft_messages(
        empty_cart_case$captured,
        "groceryapp_cart_draft_save"
      ),
      1L
    )[[1L]]$message
    stopifnot(
      isTRUE(final_save$clear),
      is.null(final_save$value),
      identical(final_save$request_id, load_message$request_id)
    )
  }
)

# En vellykket browserkopiering afslutter sessionen. Kun et svar fra den
# aktuelle serversession må nulstille carten og fjerne localStorage-kladden.
copy_completion_case <- cart_draft_capture_session()
shiny::testServer(
  cart_draft_test_server,
  session = copy_completion_case$session,
  {
    session$flushReact()
    load_message <- cart_draft_messages(
      copy_completion_case$captured,
      "groceryapp_cart_draft_load"
    )[[1L]]$message

    session$setInputs(cart_draft_restore = list(
      status = "found",
      request_id = load_message$request_id,
      value = encoded_draft
    ))
    session$flushReact()
    saves_before_copy <- length(cart_draft_messages(
      copy_completion_case$captured,
      "groceryapp_cart_draft_save"
    ))

    session$setInputs(cart_copy_done = list(
      request_id = "cart-draft-forkert-session",
      nonce = 1
    ))
    session$flushReact()
    stopifnot(
      identical(module_api$cart_current(), draft_state),
      length(cart_draft_messages(
        copy_completion_case$captured,
        "groceryapp_cart_draft_save"
      )) == saves_before_copy
    )

    session$setInputs(cart_copy_done = list(
      request_id = load_message$request_id,
      nonce = 2
    ))
    session$flushReact()
    stopifnot(identical(module_api$cart_current(), new_cart_state()))
    final_save <- tail(
      cart_draft_messages(
        copy_completion_case$captured,
        "groceryapp_cart_draft_save"
      ),
      1L
    )[[1L]]$message
    stopifnot(
      isTRUE(final_save$clear),
      is.null(final_save$value),
      identical(final_save$request_id, load_message$request_id)
    )
  }
)

# Kontrakten mellem app, modul og browser ligger fast, selv om browserens
# localStorage ikke er tilgængelig i Shiny's R-baserede testmiljø.
module_source_path <- file.path("R", "indkobsseddel_module.R")
browser_source_path <- file.path("www", "cart-persistence.js")
copy_feedback_source_path <- file.path("www", "DT-copy-feedback.js")
app_source_path <- "app.R"
stopifnot(
  file.exists(module_source_path),
  file.exists(browser_source_path),
  file.exists(copy_feedback_source_path),
  file.exists(app_source_path)
)

module_source <- paste(
  readLines(module_source_path, encoding = "UTF-8", warn = FALSE),
  collapse = "\n"
)
browser_source <- paste(
  readLines(browser_source_path, encoding = "UTF-8", warn = FALSE),
  collapse = "\n"
)
copy_feedback_source <- paste(
  readLines(copy_feedback_source_path, encoding = "UTF-8", warn = FALSE),
  collapse = "\n"
)
app_source <- paste(
  readLines(app_source_path, encoding = "UTF-8", warn = FALSE),
  collapse = "\n"
)

cart_draft_call_name <- function(node) {
  if (!is.call(node)) return("")
  head <- node[[1L]]
  if (is.symbol(head)) return(as.character(head))
  if (
    is.call(head) &&
      is.symbol(head[[1L]]) &&
      as.character(head[[1L]]) %in% c("::", ":::")
  ) {
    return(as.character(head[[3L]]))
  }
  ""
}

cart_draft_collect_calls <- function(node) {
  if (!is.call(node)) return(list())
  descendants <- unlist(
    lapply(as.list(node), cart_draft_collect_calls),
    recursive = FALSE
  )
  c(list(node), descendants)
}

cart_draft_input_name <- function(node) {
  if (
    !is.call(node) ||
      !cart_draft_call_name(node) %in% c("$", "[[") ||
      length(node) < 3L ||
      !is.symbol(node[[2L]]) ||
      !identical(as.character(node[[2L]]), "input")
  ) {
    return("")
  }
  member <- node[[3L]]
  if (!is.symbol(member) && !is.character(member)) return("")
  as.character(member)[[1L]]
}

module_calls <- unlist(
  lapply(
    as.list(parse(module_source_path, encoding = "UTF-8")),
    cart_draft_collect_calls
  ),
  recursive = FALSE
)
observe_event_calls <- Filter(
  function(node) identical(cart_draft_call_name(node), "observeEvent"),
  module_calls
)
mutation_events <- c(
  "add_recipe",
  "add_catalog_item",
  "add_manual_item",
  "delete_pressed",
  "confirm_edit",
  "save_history"
)
mutation_observers_ignore_initial_replay <- vapply(
  mutation_events,
  function(event_name) {
    matching_calls <- Filter(
      function(node) {
        length(node) >= 2L &&
          identical(cart_draft_input_name(node[[2L]]), event_name)
      },
      observe_event_calls
    )
    if (length(matching_calls) != 1L) return(FALSE)

    call_arguments <- as.list(matching_calls[[1L]])
    ignore_index <- which(names(call_arguments) == "ignoreInit")
    length(ignore_index) == 1L &&
      isTRUE(call_arguments[[ignore_index]])
  },
  logical(1)
)

stopifnot(
  grepl("input\\$cart_draft_restore", module_source),
  grepl('ns\\(["\']cart_draft_restore["\']\\)', module_source),
  grepl('ns\\(["\']cart_draft_status["\']\\)', module_source),
  grepl("groceryapp_cart_draft_load", module_source, fixed = TRUE),
  grepl("groceryapp_cart_draft_save", module_source, fixed = TRUE),
  grepl("cart_draft_decode", module_source, fixed = TRUE),
  grepl("cart_draft_encode", module_source, fixed = TRUE),
  grepl("cart-persistence.js", app_source, fixed = TRUE),
  grepl("groceryapp_cart_draft_load", browser_source, fixed = TRUE),
  grepl("groceryapp_cart_draft_save", browser_source, fixed = TRUE),
  grepl("localStorage.getItem", browser_source, fixed = TRUE),
  grepl("localStorage.setItem", browser_source, fixed = TRUE),
  grepl("localStorage.removeItem", browser_source, fixed = TRUE),
  grepl("Shiny.setInputValue", browser_source, fixed = TRUE),
  grepl("request_id: message.request_id", browser_source, fixed = TRUE),
  grepl("currentValue !== lastKnownValue", browser_source, fixed = TRUE),
  grepl("status: 'conflict'", browser_source, fixed = TRUE),
  grepl("try", browser_source, fixed = TRUE),
  grepl("catch", browser_source, fixed = TRUE)
)
stopifnot(
  grepl("input$cart_copy_done", module_source, fixed = TRUE),
  grepl("copyInputId", copy_feedback_source, fixed = TRUE),
  grepl("copyRequestId", copy_feedback_source, fixed = TRUE)
)
stopifnot(all(mutation_observers_ignore_initial_replay))

message(
  paste(
    "Cart-draft bestod tests for JSON-roundtrip, klientvalidering,",
    "restore/save-handshake og kontrakten med localStorage."
  )
)
