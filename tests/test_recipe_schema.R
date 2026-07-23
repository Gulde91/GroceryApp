source("recipe_schema.R", encoding = "UTF-8")

expect_recipe_schema_error <- function(expression, pattern) {
  error <- tryCatch(
    {
      force(expression)
      NULL
    },
    error = identity
  )

  stopifnot(
    inherits(error, "error"),
    grepl(pattern, conditionMessage(error), ignore.case = TRUE)
  )
}

active_retter <- data.frame(
  retter = "Burger",
  key = "burger_opskr",
  type = "kød",
  stringsAsFactors = FALSE
)
archived_retter <- data.frame(
  retter = "Ældre suppe",
  key = "aeldre_suppe_opskr",
  type = "vegetar",
  stringsAsFactors = FALSE
)
links <- data.frame(
  ret = "Ret uden metadata",
  link = "https://example.com/opskrift",
  stringsAsFactors = FALSE
)
empty_recipe <- data.frame(
  Burger = character(),
  maengde = numeric(),
  enhed = character(),
  kat_1 = character(),
  kat_2 = character(),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
recipes <- list(
  burger_opskr = empty_recipe,
  opskrift_uden_metadata = empty_recipe
)
names(recipes$opskrift_uden_metadata)[1] <- "Opskrift uden metadata"

stopifnot(
  isTRUE(recipe_schema_validate_table(
    active_retter,
    c("retter", "key", "type"),
    "Aktive retter"
  )),
  isTRUE(recipe_schema_validate_keys(
    active_retter$key,
    "Aktive retter"
  )),
  isTRUE(recipe_schema_validate_recipe(
    empty_recipe,
    "burger_opskr"
  )),
  isTRUE(recipe_schema_validate_recipes(recipes)),
  isTRUE(recipe_schema_validate_recipes(list())),
  isTRUE(recipe_schema_validate_catalog_tables(
    active_retter,
    archived_retter,
    links
  )),
  isTRUE(recipe_schema_validate_catalog_tables(
    links = links
  ))
)

# Skemaet beskriver datastrukturen, ikke relationerne mellem filerne. Derfor
# er en arkiveret ret uden opskrift, et link uden ret og en opskrift uden
# metadata fortsat gyldige, ligesom i de eksisterende data.
stopifnot(
  !"aeldre_suppe_opskr" %in% names(recipes),
  !"Ret uden metadata" %in% c(
    active_retter$retter,
    archived_retter$retter
  ),
  !"opskrift_uden_metadata" %in% c(
    active_retter$key,
    archived_retter$key
  )
)

expect_recipe_schema_error(
  recipe_schema_validate_table(
    "ikke en tabel",
    c("retter", "key", "type"),
    "Aktive retter"
  ),
  "data frame"
)
wrong_columns <- active_retter
names(wrong_columns)[1] <- "navn"
expect_recipe_schema_error(
  recipe_schema_validate_catalog_tables(
    active_retter = wrong_columns
  ),
  "kolonnerne"
)

expect_recipe_schema_error(
  recipe_schema_validate_keys(
    c("burger_opskr", ""),
    "Aktive retter"
  ),
  "tom nøgle"
)
expect_recipe_schema_error(
  recipe_schema_validate_keys(
    c("Burger_opskr", "burger_OPSKR"),
    "Aktive retter"
  ),
  "dublerede"
)
overlapping_archive <- archived_retter
overlapping_archive$key[[1]] <- "BURGER_OPSKR"
expect_recipe_schema_error(
  recipe_schema_validate_catalog_tables(
    active_retter,
    overlapping_archive,
    links
  ),
  "både aktiv og arkiveret"
)

expect_recipe_schema_error(
  recipe_schema_validate_recipes(data.frame()),
  "navngivet liste"
)
expect_recipe_schema_error(
  recipe_schema_validate_recipes(list(empty_recipe)),
  "unikke"
)
duplicate_recipes <- list(empty_recipe, empty_recipe)
names(duplicate_recipes) <- c("Burger_opskr", "burger_OPSKR")
expect_recipe_schema_error(
  recipe_schema_validate_recipes(duplicate_recipes),
  "unikke"
)

wrong_recipe_columns <- empty_recipe
names(wrong_recipe_columns)[2] <- "antal"
expect_recipe_schema_error(
  recipe_schema_validate_recipe(
    wrong_recipe_columns,
    "burger_opskr"
  ),
  "maengde"
)
missing_recipe_name <- empty_recipe
names(missing_recipe_name)[1] <- ""
expect_recipe_schema_error(
  recipe_schema_validate_recipe(
    missing_recipe_name,
    "burger_opskr"
  ),
  "mangler rettens navn"
)
text_amount <- empty_recipe
text_amount$maengde <- character()
expect_recipe_schema_error(
  recipe_schema_validate_recipe(
    text_amount,
    "burger_opskr"
  ),
  "numeriske"
)

# Semikolon og linjeskift er et filformatproblem og hører derfor fortsat til
# i recipe_store.R, ikke i det fælles skema.
links_with_semicolon <- links
links_with_semicolon$link[[1]] <- "https://example.com/a;b"
stopifnot(isTRUE(recipe_schema_validate_catalog_tables(
  links = links_with_semicolon
)))

message(paste(
  "Det fælles opskriftsskema validerer tabeller, nøgler og opskrifter",
  "uden at overtage fillagerets filspecifikke sikkerhedsregler."
))
