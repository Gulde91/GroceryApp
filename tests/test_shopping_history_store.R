source("shopping_history_store.R", encoding = "UTF-8")

# Manglende og tomme mapper giver samme, typede tomme snapshot.
missing_dir <- tempfile("groceryapp-missing-history-")
empty_dir <- tempfile("groceryapp-empty-history-")
dir.create(empty_dir)

missing_snapshot <- shopping_history_store_read(missing_dir)
empty_snapshot <- shopping_history_store_read(empty_dir)
stopifnot(
  identical(names(missing_snapshot), c("entries", "revision")),
  identical(missing_snapshot, empty_snapshot),
  identical(missing_snapshot$revision, "empty"),
  identical(
    names(missing_snapshot$entries),
    c("filename", "date", "line_number", "Indkøbsliste")
  ),
  is.character(missing_snapshot$entries$filename),
  inherits(missing_snapshot$entries$date, "Date"),
  is.integer(missing_snapshot$entries$line_number),
  is.character(missing_snapshot$entries$Indkøbsliste)
)

# Det gamle .rda-format, suffikser og flere filer læses samlet. En fil må
# gerne have gamle ekstrakolonner; kun Indkøbsliste publiceres i snapshot'et.
history_dir <- tempfile("groceryapp-history-read-")
dir.create(history_dir)

df <- data.frame(
  Indkøbsliste = c("2 stk Mælk", "", "Pizza (2 pers.)"),
  gammel_kolonne = c("a", "b", "c"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
save(df, file = file.path(history_dir, "indkobsseddel_20240101.rda"))

df <- data.frame(
  Indkøbsliste = c("1 kg Ris", "3 stk Æbler"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
save(df, file = file.path(history_dir, "indkobsseddel_20240102.rda1"))

df <- data.frame(
  Indkøbsliste = "Suppe (4 pers.)",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
save(df, file = file.path(history_dir, "indkobsseddel_20240102.rda12"))

# En gyldig RData-fil med et uvedkommende navn skal ignoreres.
df <- data.frame(
  Indkøbsliste = "Må ikke læses",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
save(df, file = file.path(history_dir, "anden_fil.rda"))

# Alle følgende filer matcher navnemønsteret, men skal springes over lydløst.
writeLines(
  "dette er ikke en RData-fil",
  file.path(history_dir, "indkobsseddel_20240103.rda"),
  useBytes = TRUE
)
andet_objekt <- data.frame(Indkøbsliste = "Forkert objektnavn")
save(
  andet_objekt,
  file = file.path(history_dir, "indkobsseddel_20240104.rda")
)
df <- data.frame(forkert_kolonne = "Forkert schema")
save(df, file = file.path(history_dir, "indkobsseddel_20240105.rda"))
df <- data.frame(
  Indkøbsliste = "Ugyldig kalenderdato",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
save(df, file = file.path(history_dir, "indkobsseddel_20240230.rda"))

emitted_warnings <- character()
snapshot <- withCallingHandlers(
  shopping_history_store_read(history_dir),
  warning = function(warning) {
    emitted_warnings <<- c(emitted_warnings, conditionMessage(warning))
    invokeRestart("muffleWarning")
  }
)

expected_filenames <- c(
  rep("indkobsseddel_20240101.rda", 3L),
  rep("indkobsseddel_20240102.rda1", 2L),
  "indkobsseddel_20240102.rda12"
)
stopifnot(
  length(emitted_warnings) == 0L,
  identical(names(snapshot), c("entries", "revision")),
  is.character(snapshot$revision),
  length(snapshot$revision) == 1L,
  !is.na(snapshot$revision),
  identical(snapshot$entries$filename, expected_filenames),
  identical(
    snapshot$entries$date,
    as.Date(c(
      rep("2024-01-01", 3L),
      rep("2024-01-02", 3L)
    ))
  ),
  identical(snapshot$entries$line_number, c(1:3, 1:2, 1L)),
  identical(
    snapshot$entries$Indkøbsliste,
    c(
      "2 stk Mælk",
      "",
      "Pizza (2 pers.)",
      "1 kg Ris",
      "3 stk Æbler",
      "Suppe (4 pers.)"
    )
  )
)

# Revisionen påvirkes kun af filer med historiknavnet og ændrer sig, når
# bytes i en relevant fil ændres, også selv om filen er korrupt og ignoreres.
revision_before <- shopping_history_store_revision(history_dir)
writeLines(
  "ændret uvedkommende fil",
  file.path(history_dir, "anden_fil.rda"),
  useBytes = TRUE
)
stopifnot(identical(
  shopping_history_store_revision(history_dir),
  revision_before
))
writeLines(
  c("dette er ikke en RData-fil", "ændret indhold"),
  file.path(history_dir, "indkobsseddel_20240103.rda"),
  useBytes = TRUE
)
stopifnot(!identical(
  shopping_history_store_revision(history_dir),
  revision_before
))

# Gemning bevarer filnavn og objektet df, returnerer et friskt snapshot og
# overskriver samme dags fil i stedet for at oprette endnu et format.
save_dir <- tempfile("groceryapp-history-save-")
dir.create(save_dir)
first_history <- data.frame(
  Indkøbsliste = c("2 stk Rugbrød", "", "Burger (2 pers.)"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
first_saved <- shopping_history_store_save(
  first_history,
  history_dir = save_dir,
  date = as.Date("2024-06-07")
)
saved_path <- file.path(save_dir, "indkobsseddel_20240607.rda")
loaded_environment <- new.env(parent = emptyenv())
loaded_names <- load(saved_path, envir = loaded_environment)
stopifnot(
  identical(list.files(save_dir), "indkobsseddel_20240607.rda"),
  identical(loaded_names, "df"),
  identical(loaded_environment$df, first_history),
  identical(first_saved$entries$Indkøbsliste, first_history$Indkøbsliste)
)

first_revision <- first_saved$revision
second_history <- data.frame(
  Indkøbsliste = c("1 stk Kaffe", "", "Tortellini (3 pers.)"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
second_saved <- shopping_history_store_save(
  second_history,
  history_dir = save_dir,
  date = "2024-06-07"
)
loaded_environment <- new.env(parent = emptyenv())
loaded_names <- load(saved_path, envir = loaded_environment)
stopifnot(
  identical(list.files(save_dir), "indkobsseddel_20240607.rda"),
  identical(loaded_names, "df"),
  identical(loaded_environment$df, second_history),
  identical(second_saved$entries$Indkøbsliste, second_history$Indkøbsliste),
  !identical(second_saved$revision, first_revision),
  identical(
    second_saved$revision,
    shopping_history_store_revision(save_dir)
  )
)

# Nye filer skal følge det præcise schema, og destinationsmappen skal findes.
extra_column <- data.frame(
  Indkøbsliste = "Mælk",
  ekstra = "må ikke gemmes",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
factor_column <- data.frame(
  Indkøbsliste = factor("Mælk"),
  check.names = FALSE
)
stopifnot(
  inherits(
    try(
      shopping_history_store_save(
        extra_column,
        history_dir = save_dir,
        date = "2024-06-08"
      ),
      silent = TRUE
    ),
    "try-error"
  ),
  inherits(
    try(
      shopping_history_store_save(
        factor_column,
        history_dir = save_dir,
        date = "2024-06-08"
      ),
      silent = TRUE
    ),
    "try-error"
  ),
  inherits(
    try(
      shopping_history_store_save(
        second_history,
        history_dir = tempfile("groceryapp-no-history-dir-"),
        date = "2024-06-08"
      ),
      silent = TRUE
    ),
    "try-error"
  ),
  inherits(
    try(
      shopping_history_store_save(
        second_history,
        history_dir = save_dir,
        date = "ikke-en-dato"
      ),
      silent = TRUE
    ),
    "try-error"
  )
)

# Populære varer bruger kun linjer før første blanke separator i hver fil.
popular_entries <- data.frame(
  filename = c(rep("a.rda", 4L), rep("b.rda", 3L)),
  date = as.Date(c(rep("2024-01-01", 4L), rep("2024-01-02", 3L))),
  line_number = c(1:4, 1:3),
  Indkøbsliste = c(
    "2 stk Rugbrød",
    "1 liter Mælk (tilbehør)",
    "",
    "Burger (2 pers.)",
    "1 stk Rugbrød",
    "3 dåse(r) Tomat (tilsmagning)",
    "2 kg Ris"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
popular_items <- shopping_history_popular_items(
  popular_entries,
  units = c("stk", "liter", "dåse(r)", "kg", "g")
)
stopifnot(
  identical(names(popular_items), "Indkøbsliste"),
  identical(popular_items$Indkøbsliste[[1L]], "Rugbrød"),
  identical(
    sort(popular_items$Indkøbsliste[-1L]),
    sort(c("Mælk", "Tomat", "Ris"))
  ),
  !"Burger" %in% popular_items$Indkøbsliste
)

# Opskriftsmatch er regex-sikkert, bruger naturlige ordgrænser og vælger det
# længste navn, når flere navne er præfikser for samme linje.
recipe_entries <- data.frame(
  filename = c(rep("a.rda", 4L), rep("b.rda", 3L)),
  date = as.Date(c(rep("2024-02-01", 4L), rep("2024-02-02", 3L))),
  line_number = c(1:4, 1:3),
  Indkøbsliste = c(
    "Pizza surdej (4 pers.)",
    "Fisk (citron)+ (2 pers.)",
    "Burgerboller (2 pers.)",
    "Fisk (citron)+special",
    "C++ gryde: stærk",
    "Pizza (2 pers.)",
    "En almindelig vare"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
recipe_usage <- shopping_history_recipe_usage(
  recipe_entries,
  recipe_names = c(
    "Pizza",
    "Pizza surdej",
    "Fisk (citron)+",
    "Burger",
    "C++ gryde",
    "",
    NA_character_,
    "Pizza"
  )
)
stopifnot(
  identical(names(recipe_usage), c("retter", "dato")),
  inherits(recipe_usage$dato, "Date"),
  nrow(recipe_usage) == 4L,
  identical(
    sort(recipe_usage$retter),
    sort(c("Pizza surdej", "Fisk (citron)+", "C++ gryde", "Pizza"))
  ),
  !"Burger" %in% recipe_usage$retter,
  identical(
    recipe_usage$dato[recipe_usage$retter == "Pizza surdej"],
    as.Date("2024-02-01")
  ),
  identical(
    recipe_usage$dato[recipe_usage$retter == "C++ gryde"],
    as.Date("2024-02-02")
  )
)

empty_usage <- shopping_history_recipe_usage(
  missing_snapshot$entries,
  recipe_names = "Pizza"
)
empty_popular <- shopping_history_popular_items(
  missing_snapshot$entries,
  units = character()
)
stopifnot(
  identical(names(empty_usage), c("retter", "dato")),
  nrow(empty_usage) == 0L,
  inherits(empty_usage$dato, "Date"),
  identical(names(empty_popular), "Indkøbsliste"),
  nrow(empty_popular) == 0L
)

# Store-filen består af dokumenterede top-level-funktioner og bruger ikke
# dobbelte koloner til pakkefunktioner.
store_lines <- readLines("shopping_history_store.R", encoding = "UTF-8")
function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  store_lines
)
stopifnot(
  length(function_lines) > 0L,
  all(function_lines > 1L),
  all(grepl("^#'", store_lines[function_lines - 1L])),
  !any(grepl("::", store_lines, fixed = TRUE))
)

unlink(empty_dir, recursive = TRUE, force = TRUE)
unlink(history_dir, recursive = TRUE, force = TRUE)
unlink(save_dir, recursive = TRUE, force = TRUE)

message(
  paste(
    "Shopping-history-store bestod tests for legacyfiler, sikker læsning,",
    "revision, gemning, vareforslag og opskriftsstatistik."
  )
)
