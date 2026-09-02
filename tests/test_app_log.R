suppressPackageStartupMessages(
  source(file.path("R", "app_log.R"), encoding = "UTF-8")
)

fixed_clock <- function() {
  as.POSIXct("2026-08-31 12:34:56", tz = "UTC")
}

written_lines <- character()
capture_writer <- function(line) {
  written_lines <<- c(written_lines, line)
  invisible(TRUE)
}

context <- list(
  runtime_id = "process-test",
  session_id = "session-test"
)
conflict <- structure(
  list(
    message = "Data er ændret i en anden session.\nPrøv igen.",
    call = NULL
  ),
  class = c("basis_varer_store_conflict", "error", "condition")
)

result <- app_log_event(
  level = "WARN",
  event = "Commit Conflict",
  component = "Basis Varer State",
  fields = list(
    message = 'Varen "Kaffe" blev tilføjet til bruttolisten.',
    row_count = 12L,
    refresh = "succeeded",
    unsafe_text = "linje 1\nlinje 2"
  ),
  error = conflict,
  context = context,
  threshold = "INFO",
  clock = fixed_clock,
  writer = capture_writer
)

stopifnot(
  isTRUE(result),
  length(written_lines) == 1L,
  startsWith(written_lines[[1L]], "groceryapp "),
  grepl(
    'timestamp="2026-08-31T12:34:56.000Z"',
    written_lines[[1L]],
    fixed = TRUE
  ),
  grepl('level="WARN"', written_lines[[1L]], fixed = TRUE),
  grepl(
    'event="commit_conflict"',
    written_lines[[1L]],
    fixed = TRUE
  ),
  grepl(
    'component="basis_varer_state"',
    written_lines[[1L]],
    fixed = TRUE
  ),
  grepl('runtime_id="process-test"', written_lines[[1L]], fixed = TRUE),
  grepl('session_id="session-test"', written_lines[[1L]], fixed = TRUE),
  grepl(
    'message="Varen \\"Kaffe\\" blev tilføjet til bruttolisten."',
    written_lines[[1L]],
    fixed = TRUE
  ),
  grepl('row_count="12"', written_lines[[1L]], fixed = TRUE),
  grepl(
    'error_code="concurrency_conflict"',
    written_lines[[1L]],
    fixed = TRUE
  ),
  grepl(
    'error_class="basis_varer_store_conflict"',
    written_lines[[1L]],
    fixed = TRUE
  ),
  !grepl("\n", written_lines[[1L]], fixed = TRUE),
  grepl("linje 1 linje 2", written_lines[[1L]], fixed = TRUE)
)

stale_candidate <- simpleError(
  "Opskriftskataloget er ændret, siden handlingen begyndte."
)
stopifnot(
  identical(
    app_log_error_code(stale_candidate),
    "concurrency_conflict"
  )
)

# Niveauet kan styres uden at ændre kode eller skrive tomme events.
lines_before_filter <- length(written_lines)
filtered <- app_log_event(
  level = "INFO",
  event = "filtered",
  component = "test",
  threshold = "ERROR",
  clock = fixed_clock,
  writer = capture_writer
)
stopifnot(
  identical(filtered, FALSE),
  length(written_lines) == lines_before_filter
)

# Writer-, clock- og formatteringsfejl må aldrig slippe ud af loggeren.
writer_failure <- app_log_event(
  level = "ERROR",
  event = "writer_failure",
  component = "test",
  clock = fixed_clock,
  writer = function(line) stop("Writer er nede.", call. = FALSE)
)
clock_failure <- app_log_event(
  level = "ERROR",
  event = "clock_failure",
  component = "test",
  clock = function() stop("Uret er nede.", call. = FALSE),
  writer = capture_writer
)
invalid_fields <- app_log_event(
  level = "ERROR",
  event = "invalid_fields",
  component = "test",
  fields = list("mangler navn"),
  clock = fixed_clock,
  writer = capture_writer
)
stopifnot(
  identical(writer_failure, FALSE),
  identical(clock_failure, FALSE),
  identical(invalid_fields, FALSE)
)

# Interne sessions-id'er er entydige uden at bruge Shiny-token eller klientdata.
session_one <- app_log_new_session_context()
session_two <- app_log_new_session_context()
stopifnot(
  identical(names(session_one), c("runtime_id", "session_id")),
  identical(session_one$runtime_id, session_two$runtime_id),
  !identical(session_one$session_id, session_two$session_id),
  grepl("^session-[0-9]{6}$", session_one$session_id)
)

# En bundet logger videresender konteksten og forbliver no-throw.
bound <- app_log_bind(context)
old_level <- Sys.getenv("GROCERYAPP_LOG_LEVEL", unset = NA_character_)
Sys.setenv(GROCERYAPP_LOG_LEVEL = "ERROR")
bound_result <- bound("INFO", "suppressed", "test")
if (is.na(old_level)) {
  Sys.unsetenv("GROCERYAPP_LOG_LEVEL")
} else {
  Sys.setenv(GROCERYAPP_LOG_LEVEL = old_level)
}
stopifnot(identical(bound_result, FALSE))

message("Driftsloggeren formaterer, filtrerer og fejler sikkert.")
