suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

reference_data_env <- new.env(parent = globalenv())
recipe_store_read_called <- FALSE
reference_data_env$recipe_store_read <- function(...) {
  recipe_store_read_called <<- TRUE
  stop("data.R må ikke læse det dynamiske opskriftslager.", call. = FALSE)
}

source(
  "data.R",
  local = reference_data_env,
  encoding = "UTF-8"
)

legacy_names <- c(
  "recipe_store_data",
  "retter",
  "retter_arkiv",
  "opskrifter",
  "links",
  "opskrift_df",
  "kategori_1",
  "kategori_2"
)

static_recipe_names <- c(
  "revet_gulerodder_opskr",
  "broccoli_salat_opskr",
  "spidskaal_agurk_opskr",
  "hytteost_salat_opskr",
  "broccoli_opskr",
  "boenne_salat_opskr",
  "groenne_boenner_opskr"
)

stopifnot(
  !recipe_store_read_called,
  !any(vapply(
    legacy_names,
    exists,
    logical(1),
    envir = reference_data_env,
    inherits = FALSE
  )),
  identical(
    names(reference_data_env$tilbehor),
    c("Indkobsliste", "maengde", "enhed", "kat_1", "kat_2")
  ),
  nrow(reference_data_env$tilbehor) == 8L,
  identical(
    names(reference_data_env$salater),
    c("retter", "key", "type")
  ),
  nrow(reference_data_env$salater) == 8L,
  identical(
    names(reference_data_env$salater_opskrifter),
    static_recipe_names
  ),
  setequal(
    reference_data_env$salater$key[
      nzchar(reference_data_env$salater$key)
    ],
    static_recipe_names
  ),
  all(vapply(
    reference_data_env$salater_opskrifter,
    function(recipe) {
      is.data.frame(recipe) &&
        ncol(recipe) == 5L &&
        identical(
          names(recipe)[2:5],
          c("maengde", "enhed", "kat_1", "kat_2")
        ) &&
        is.numeric(recipe$maengde)
    },
    logical(1)
  ))
)

data_lines <- readLines("data.R", encoding = "UTF-8")
stopifnot(!any(grepl("recipe_store_read[[:space:]]*\\(", data_lines)))

message(
  paste(
    "Reference-data indeholder kun statiske salater og tilbehør",
    "med de forventede schemaer."
  )
)
