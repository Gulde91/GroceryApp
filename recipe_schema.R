# Fælles skemaregler for opskriftsdata --------------------------------------
#
# Funktionerne i denne fil beskriver kun, hvordan gyldige opskriftsdata ser
# ud. De kender hverken Shiny eller filer og kan derfor bruges ens af både
# katalogreglerne i recipe_catalog.R og fillageret i recipe_store.R.

#' Kontrollér kolonnerne i en opskriftstabel
#'
#' Funktionen kontrollerer, at værdien er en data frame, og at kolonnerne
#' findes i præcis den forventede rækkefølge.
#'
#' @param value Tabellen, der skal kontrolleres.
#' @param expected_names De forventede kolonnenavne i rigtig rækkefølge.
#' @param label Det danske navn, som bruges i en eventuel fejl.
#'
#' @return Usynligt `TRUE`, hvis tabellen har det forventede skema.
#' @keywords internal
recipe_schema_validate_table <- function(
  value,
  expected_names,
  label
) {
  if (!is.data.frame(value)) {
    stop(sprintf("%s skal være en data frame.", label), call. = FALSE)
  }
  if (!identical(names(value), expected_names)) {
    stop(
      sprintf(
        "%s skal have kolonnerne: %s.",
        label,
        paste(expected_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Kontrollér nøglerne i en tabel med retter
#'
#' Hver nøgle skal være udfyldt, og samme nøgle må kun forekomme én gang.
#' Sammenligningen ignorerer store og små bogstaver.
#'
#' @param keys Tegnvektor med opskriftsnøgler.
#' @param label Navnet på tabellen til fejlbeskeder.
#'
#' @return Usynligt `TRUE`, hvis nøglerne er gyldige.
#' @keywords internal
recipe_schema_validate_keys <- function(keys, label) {
  keys <- as.character(keys)

  if (any(is.na(keys)) || any(!nzchar(keys))) {
    stop(sprintf("%s indeholder en tom nøgle.", label), call. = FALSE)
  }
  if (anyDuplicated(tolower(keys))) {
    stop(sprintf("%s indeholder dublerede nøgler.", label), call. = FALSE)
  }

  invisible(TRUE)
}

#' Kontrollér ingrediensskemaet for én opskrift
#'
#' Rettens navn ligger i første kolonnenavn. De fire øvrige kolonner har
#' faste navne, og mængden skal være numerisk. Funktionen kontrollerer kun
#' datastrukturen; tegn, der er særlige for filformatet, kontrolleres fortsat
#' af fillageret.
#'
#' @param recipe Opskriftens data frame.
#' @param key Opskriftens interne nøgle, kun brugt i fejlbeskeder.
#'
#' @return Usynligt `TRUE`, hvis opskriften har det forventede skema.
#' @keywords internal
recipe_schema_validate_recipe <- function(recipe, key) {
  label <- sprintf("Opskriften '%s'", key)

  if (!is.data.frame(recipe)) {
    stop(sprintf("%s skal være en data frame.", label), call. = FALSE)
  }
  if (
    ncol(recipe) != 5L ||
      !identical(
        names(recipe)[2:5],
        c("maengde", "enhed", "kat_1", "kat_2")
      )
  ) {
    stop(
      paste(
        label,
        "skal have rettens navn som første kolonne og derefter",
        "maengde, enhed, kat_1 og kat_2."
      ),
      call. = FALSE
    )
  }
  if (is.na(names(recipe)[1]) || !nzchar(names(recipe)[1])) {
    stop(sprintf("%s mangler rettens navn.", label), call. = FALSE)
  }
  if (!is.numeric(recipe$maengde)) {
    stop(sprintf("%s skal have numeriske mængder.", label), call. = FALSE)
  }

  invisible(TRUE)
}

#' Kontrollér en navngivet liste med opskrifter
#'
#' Funktionen sikrer, at listen har unikke, udfyldte nøgler, og at hver
#' opskrift følger det fælles ingrediensskema. Listen må gerne være tom.
#'
#' @param recipes En navngivet liste med opskriftstabeller.
#' @param label Listens navn til fejlbeskeder.
#'
#' @return Usynligt `TRUE`, hvis listen og alle opskrifter er gyldige.
#' @keywords internal
recipe_schema_validate_recipes <- function(
  recipes,
  label = "Opskrifter"
) {
  if (!is.list(recipes) || is.data.frame(recipes)) {
    stop(sprintf("%s skal være en navngivet liste.", label), call. = FALSE)
  }

  if (length(recipes) == 0L) return(invisible(TRUE))

  recipe_keys <- names(recipes)
  if (
    is.null(recipe_keys) ||
      any(is.na(recipe_keys)) ||
      any(!nzchar(recipe_keys)) ||
      anyDuplicated(tolower(recipe_keys))
  ) {
    stop(
      sprintf(
        "%s skal have unikke, ikke-tomme nøgler.",
        label
      ),
      call. = FALSE
    )
  }

  for (key in recipe_keys) {
    recipe_schema_validate_recipe(recipes[[key]], key)
  }

  invisible(TRUE)
}

#' Kontrollér metadata-tabellerne i opskriftskataloget
#'
#' Aktive retter, arkiverede retter og links kontrolleres efter de samme
#' regler, uanset om kaldet kommer fra kataloget eller fillageret. Et `NULL`
#' betyder, at tabellen ikke indgår i den aktuelle delvise lagerændring.
#'
#' @param active_retter Tabel med aktive retter eller `NULL`.
#' @param archived_retter Tabel med arkiverede retter eller `NULL`.
#' @param links Tabel med opskriftslinks eller `NULL`.
#'
#' @return Usynligt `TRUE`, hvis de medsendte tabeller er gyldige.
#' @keywords internal
recipe_schema_validate_catalog_tables <- function(
  active_retter = NULL,
  archived_retter = NULL,
  links = NULL
) {
  if (!is.null(active_retter)) {
    recipe_schema_validate_table(
      active_retter,
      c("retter", "key", "type"),
      "Aktive retter"
    )
    recipe_schema_validate_keys(
      active_retter$key,
      "Aktive retter"
    )
  }

  if (!is.null(archived_retter)) {
    recipe_schema_validate_table(
      archived_retter,
      c("retter", "key", "type"),
      "Arkiverede retter"
    )
    recipe_schema_validate_keys(
      archived_retter$key,
      "Arkiverede retter"
    )
  }

  if (!is.null(active_retter) && !is.null(archived_retter)) {
    overlap <- intersect(
      tolower(active_retter$key),
      tolower(archived_retter$key)
    )
    if (length(overlap) > 0L) {
      stop(
        "Den samme nøgle må ikke være både aktiv og arkiveret.",
        call. = FALSE
      )
    }
  }

  if (!is.null(links)) {
    recipe_schema_validate_table(
      links,
      c("ret", "link"),
      "Opskriftslinks"
    )
  }

  invisible(TRUE)
}
