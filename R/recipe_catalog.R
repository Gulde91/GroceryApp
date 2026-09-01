# Rene katalogregler for opskrifter -----------------------------------------
#
# Funktionerne i denne fil ændrer kun det snapshot, de får udleveret, og
# returnerer et nyt. De kender derfor hverken Shiny, filer eller den database-
# lignende lagring i recipe_store.R.

#' Kontrollér strukturen på et opskriftskatalog
#'
#' Funktionen sikrer, at et katalog har de fire dele, som katalogreglerne
#' arbejder med: aktive retter, arkiverede retter, links og opskrifter. Den
#' kontrollerer også tabellernes kolonner og hver opskrifts ingrediensskema.
#' Et arkiv må gerne pege på en manglende opskrift; det kan forekomme i ældre
#' data og håndteres særskilt ved gendannelse eller permanent sletning.
#'
#' @param catalog Et katalog-snapshot som navngivet liste.
#'
#' @return Usynligt `TRUE`, hvis kataloget er gyldigt. Ellers stoppes med en
#'   letlæselig fejl på dansk.
#'   
#' @keywords internal
recipe_catalog_validate <- function(catalog) {
  required_names <- c(
    "active_retter",
    "archived_retter",
    "links",
    "recipes"
  )

  if (!is.list(catalog) || is.data.frame(catalog)) {
    stop("Kataloget skal være en navngivet liste.", call. = FALSE)
  }
  if (!all(required_names %in% names(catalog))) {
    missing_names <- setdiff(required_names, names(catalog))
    stop(
      sprintf(
        "Kataloget mangler: %s.",
        paste(missing_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  recipe_schema_validate_catalog_tables(
    catalog$active_retter,
    catalog$archived_retter,
    catalog$links
  )

  recipe_schema_validate_recipes(
    catalog$recipes,
    "Katalogets opskrifter"
  )

  invisible(TRUE)
}

#' Forbered kataloget til en ændring
#'
#' R kopierer automatisk en liste eller data frame, når funktionen ændrer den.
#' Derfor kan kataloget bindes til et nyt navn her, mens hver mutation stadig
#' lader kalderens snapshot være urørt. Samtidig bevares interne attributter på
#' uændrede `readr`-tabeller, så lagerlaget korrekt kan se, hvilke katalogdele
#' der faktisk er ændret.
#'
#' @param catalog Et valideret katalog-snapshot.
#'
#' @return Kataloget, klar til R's automatiske kopi-ved-ændring.
#' @keywords internal
recipe_catalog_copy <- function(catalog) {
  catalog
}

#' Hent og kontrollér en tekstværdi
#'
#' Brugerinput trimmes, og funktionen afviser manglende eller flerdelte værdier
#' med en konkret dansk fejl.
#'
#' @param value Værdien, der skal kontrolleres.
#' @param label Feltets navn til fejlbeskeden.
#' @param allow_empty Om en tom tekst er tilladt.
#'
#' @return Den trimmede tekstværdi.
#' @keywords internal
recipe_catalog_scalar_text <- function(
  value,
  label,
  allow_empty = FALSE
) {
  if (
    is.null(value) ||
      length(value) != 1L ||
      is.list(value) ||
      is.na(value)
  ) {
    stop(sprintf("%s skal være én tekstværdi.", label), call. = FALSE)
  }

  value <- trimws(as.character(value))
  if (!allow_empty && !nzchar(value)) {
    stop(sprintf("%s må ikke være tom.", label), call. = FALSE)
  }

  value
}

#' Hent og kontrollér en ingrediensmængde
#'
#' Funktionen accepterer både tal og tekst, der kan læses som et tal. En tom
#' mængde bliver til `NA`, når det er tilladt ved redigering.
#'
#' @param value Mængden fra brugerinput.
#' @param allow_missing Om en tom mængde er tilladt.
#'
#' @return Ét numerisk tal eller `NA_real_`.
#' @keywords internal
recipe_catalog_amount <- function(value, allow_missing) {
  if (is.null(value) || length(value) != 1L || is.list(value)) {
    stop("Mængde skal være én værdi.", call. = FALSE)
  }

  is_blank <- is.character(value) && !is.na(value) && !nzchar(trimws(value))
  if (is.na(value) || is_blank) {
    if (!allow_missing) {
      stop("Mængde skal være et tal.", call. = FALSE)
    }
    return(NA_real_)
  }

  amount <- suppressWarnings(as.numeric(value))
  if (is.na(amount) || !is.finite(amount)) {
    stop("Mængde skal være et tal.", call. = FALSE)
  }

  amount
}

#' Hent en opskrift eller stop med en tydelig fejl
#'
#' @param catalog Et valideret katalog-snapshot.
#' @param key Opskriftens interne nøgle.
#'
#' @return Opskriftens data frame.
#' @keywords internal
recipe_catalog_get_recipe <- function(catalog, key) {
  key <- recipe_catalog_scalar_text(key, "Opskriftsnøglen")
  if (!key %in% names(catalog$recipes)) {
    stop(
      sprintf("Opskriften med nøglen '%s' findes ikke.", key),
      call. = FALSE
    )
  }

  catalog$recipes[[key]]
}

#' Kontrollér et rækkenummer i en opskrift
#'
#' @param row Rækkenummeret, der skal bruges.
#' @param recipe Opskriftens data frame.
#'
#' @return Rækkenummeret som ét heltal.
#' @keywords internal
recipe_catalog_row_number <- function(row, recipe) {
  if (is.null(row) || length(row) != 1L || is.list(row) || is.na(row)) {
    stop("Rækkenummeret skal være ét heltal.", call. = FALSE)
  }

  numeric_row <- suppressWarnings(as.numeric(row))
  if (
    is.na(numeric_row) ||
      !is.finite(numeric_row) ||
      numeric_row != floor(numeric_row) ||
      numeric_row < 1 ||
      numeric_row > nrow(recipe)
  ) {
    stop("Rækkenummeret findes ikke i opskriften.", call. = FALSE)
  }

  as.integer(numeric_row)
}

#' Hent rettens viste navn fra en opskrift
#'
#' I katalogformatet er rettens navn navnet på opskriftens første kolonne.
#'
#' @param recipe Opskriftens data frame.
#'
#' @return Rettens navn som tekst.
#' @keywords internal
recipe_catalog_display_name <- function(recipe) {
  names(recipe)[1]
}

#' Formatér en ingrediens som en læsbar linje
#'
#' Manglende mængder fjernes, og overflødige mellemrum samles. Eksempelvis
#' bliver delene `2`, `"stk"` og `"tomater"` til `"2 stk tomater"`.
#'
#' @param amount Ingrediensens mængde.
#' @param unit Ingrediensens enhed.
#' @param ingredient Ingrediensens navn.
#'
#' @return En færdig ingredienslinje som tekst.
#' @keywords internal
recipe_catalog_format_line <- function(amount, unit, ingredient) {
  line <- paste(amount, unit, ingredient)
  line <- gsub("NA", "", line, fixed = TRUE)
  trimws(gsub("\\s+", " ", line))
}

#' Normalisér et opskriftslink
#'
#' Et tomt eller manglende link bliver til tom tekst. Links uden protokol får
#' `https://`, mens eksisterende HTTP- og HTTPS-links bevares.
#'
#' @param link Linket fra brugerinput.
#'
#' @return Et normaliseret link eller tom tekst.
#' @keywords internal
recipe_catalog_normalize_link <- function(link) {
  if (is.null(link) || length(link) == 0L) return("")
  if (length(link) != 1L || is.list(link)) {
    stop("Link skal være én tekstværdi.", call. = FALSE)
  }
  if (is.na(link)) return("")

  link <- trimws(as.character(link))
  if (!nzchar(link)) return("")
  if (grepl("^https?://", link, ignore.case = TRUE)) return(link)
  if (grepl("^//", link)) return(paste0("https:", link))

  paste0("https://", link)
}

#' Dan en intern nøgle ud fra rettens navn
#'
#' Accenter fjernes, bogstaver gøres små, og andre tegn erstattes med
#' understregninger. `"Bøf med løg"` bliver således til `"bof_med_log"`.
#'
#' @param recipe_name Rettens navn.
#'
#' @return En nøgle med små ASCII-bogstaver, tal og understregninger.
#' @keywords internal
recipe_catalog_slugify_key <- function(recipe_name) {
  recipe_name <- recipe_catalog_scalar_text(recipe_name, "Rettens navn")
  ascii_name <- iconv(
    recipe_name,
    from = "UTF-8",
    to = "ASCII//TRANSLIT"
  )
  if (is.na(ascii_name)) {
    stop("Rettens navn kan ikke bruges som intern nøgle.", call. = FALSE)
  }

  key <- tolower(ascii_name)
  key <- gsub("[^a-z0-9]+", "_", key)
  key <- gsub("^_+|_+$", "", key)
  if (!nzchar(key)) {
    stop(
      "Rettens navn skal indeholde mindst ét bogstav eller tal.",
      call. = FALSE
    )
  }

  key
}

#' Sortér en katalogtabel alfabetisk
#'
#' @param value Tabellen, der skal sorteres.
#' @param column Kolonnen, som bestemmer rækkefølgen.
#'
#' @return Tabellen i alfabetisk rækkefølge og med nulstillede rækkenavne.
#' @keywords internal
recipe_catalog_sort_table <- function(value, column) {
  if (nrow(value) > 1L) {
    value <- value[order(value[[column]], na.last = TRUE), , drop = FALSE]
  }
  rownames(value) <- NULL
  value
}

#' Fjern dubletter og behold den første række
#'
#' @param value Tabellen, der skal renses.
#' @param column Kolonnen, hvor dubletter sammenlignes.
#'
#' @return Tabellen uden senere dubletter.
#' @keywords internal
recipe_catalog_distinct_first <- function(value, column) {
  value[!duplicated(value[[column]]), , drop = FALSE]
}

#' Byg metadata om en udført katalogændring
#'
#' Alle hændelser har samme felter. Felter, der ikke er relevante for en given
#' handling, indeholder en tom tekst eller et manglende rækkenummer.
#'
#' @param reason Handlingens maskinlæsbare navn.
#' @param key Opskriftens interne nøgle.
#' @param recipe_name Rettens viste navn.
#' @param row Et eventuelt ingrediens-rækkenummer.
#' @param ingredient_name Et eventuelt ingrediensnavn.
#' @param line En eventuel færdig ingredienslinje.
#'
#' @return En navngivet liste med ensartede hændelsesfelter.
#' @keywords internal
recipe_catalog_event <- function(
  reason,
  key,
  recipe_name,
  row = NA_integer_,
  ingredient_name = "",
  line = ""
) {
  list(
    reason = reason,
    key = key,
    recipe_name = recipe_name,
    row = as.integer(row),
    ingredient_name = ingredient_name,
    line = line
  )
}

#' Pak resultatet af en katalogændring
#'
#' Resultatet har samme form for alle handlinger. Kun permanent sletning
#' udfylder `delete_recipe_keys`; lagerlaget bruger feltet til at fjerne selve
#' opskriftsfilen.
#'
#' @param catalog Det ændrede katalog-snapshot.
#' @param event Metadata om ændringen.
#' @param delete_recipe_keys Nøgler på opskriftsfiler, der skal slettes.
#'
#' @return En liste med `catalog`, `event` og `delete_recipe_keys`.
#' @keywords internal
recipe_catalog_result <- function(
  catalog,
  event,
  delete_recipe_keys = character()
) {
  recipe_catalog_validate(catalog)
  list(
    catalog = catalog,
    event = event,
    delete_recipe_keys = as.character(delete_recipe_keys)
  )
}

#' Opret en ny tom opskrift i kataloget
#'
#' Funktionen afviser et eksisterende navn uden hensyn til store og små
#' bogstaver. Den danner en stabil nøgle af navnet og tilføjer `_2`, `_3` og
#' så videre, hvis nøglen allerede bruges. Retten sorteres ind blandt de aktive
#' retter, og et udfyldt link normaliseres og tilføjes.
#'
#' @param catalog Det aktuelle katalog-snapshot.
#' @param recipe_name Navnet på den nye ret.
#' @param recipe_type Rettens type, eksempelvis `"vegetar"`.
#' @param link Et valgfrit link til opskriften.
#'
#' @return Et ensartet katalogresultat med den nye nøgle i `event$key`.
recipe_catalog_create <- function(
  catalog,
  recipe_name,
  recipe_type,
  link = ""
) {
  recipe_catalog_validate(catalog)
  recipe_name <- recipe_catalog_scalar_text(recipe_name, "Rettens navn")
  recipe_type <- recipe_catalog_scalar_text(recipe_type, "Rettens type")
  link <- recipe_catalog_normalize_link(link)

  existing_names <- vapply(
    catalog$recipes,
    recipe_catalog_display_name,
    character(1)
  )
  if (tolower(recipe_name) %in% tolower(existing_names)) {
    stop(
      sprintf('Retten "%s" findes allerede.', recipe_name),
      call. = FALSE
    )
  }

  recipes <- catalog$recipes
  base_key <- paste0(recipe_catalog_slugify_key(recipe_name), "_opskr")
  key <- base_key
  suffix <- 1L
  while (key %in% names(recipes)) {
    suffix <- suffix + 1L
    key <- paste0(base_key, "_", suffix)
  }

  recipe <- data.frame(
    ingredient = character(),
    maengde = numeric(),
    enhed = character(),
    kat_1 = character(),
    kat_2 = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(recipe)[1] <- recipe_name
  recipes[[key]] <- recipe

  next_catalog <- recipe_catalog_copy(catalog)
  next_catalog$recipes <- recipes
  next_catalog$active_retter <- rbind(
    next_catalog$active_retter,
    data.frame(
      retter = recipe_name,
      key = key,
      type = recipe_type,
      stringsAsFactors = FALSE
    )
  )
  next_catalog$active_retter <- recipe_catalog_distinct_first(
    next_catalog$active_retter,
    "key"
  )
  next_catalog$active_retter <- recipe_catalog_sort_table(
    next_catalog$active_retter,
    "retter"
  )

  if (nzchar(link)) {
    next_catalog$links <- rbind(
      next_catalog$links,
      data.frame(
        ret = recipe_name,
        link = link,
        stringsAsFactors = FALSE
      )
    )
    next_catalog$links <- recipe_catalog_distinct_first(
      next_catalog$links,
      "ret"
    )
    next_catalog$links <- recipe_catalog_sort_table(
      next_catalog$links,
      "ret"
    )
  }

  recipe_catalog_result(
    next_catalog,
    recipe_catalog_event("created", key, recipe_name)
  )
}

#' Opdatér mængde og kategorier for en ingrediens
#'
#' Ingrediensens navn ændres ikke. En mængde må være tom eller større end nul,
#' og den første kategori skal være udfyldt. Det svarer til den nuværende
#' redigeringsdialog i appen.
#'
#' @param catalog Det aktuelle katalog-snapshot.
#' @param key Nøglen til opskriften.
#' @param row Ingrediensens rækkenummer.
#' @param amount Ny mængde; `NA` betyder tom mængde.
#' @param unit Ny enhed, som gerne må være tom.
#' @param category_1 Ny første kategori.
#' @param category_2 Ny anden kategori, som gerne må være tom.
#'
#' @return Et ensartet katalogresultat med den opdaterede opskrift.
recipe_catalog_update_ingredient <- function(
  catalog,
  key,
  row,
  amount,
  unit,
  category_1,
  category_2
) {
  recipe_catalog_validate(catalog)
  key <- recipe_catalog_scalar_text(key, "Opskriftsnøglen")
  recipe <- recipe_catalog_get_recipe(catalog, key)
  row <- recipe_catalog_row_number(row, recipe)
  ingredient_name <- as.character(recipe[[1]][[row]])
  amount <- recipe_catalog_amount(amount, allow_missing = TRUE)
  unit <- recipe_catalog_scalar_text(unit, "Enhed", allow_empty = TRUE)
  category_1 <- recipe_catalog_scalar_text(category_1, "Kategori 1")
  category_2 <- recipe_catalog_scalar_text(
    category_2,
    "Kategori 2",
    allow_empty = TRUE
  )
  if (!is.na(amount) && amount <= 0) {
    stop(
      "Mængde skal være tom eller et tal større end 0.",
      call. = FALSE
    )
  }

  recipe$maengde[[row]] <- amount
  recipe$enhed[[row]] <- unit
  recipe$kat_1[[row]] <- category_1
  recipe$kat_2[[row]] <- category_2

  next_catalog <- recipe_catalog_copy(catalog)
  next_catalog$recipes[[key]] <- recipe
  recipe_name <- recipe_catalog_display_name(recipe)

  recipe_catalog_result(
    next_catalog,
    recipe_catalog_event(
      "ingredient_updated",
      key,
      recipe_name,
      row = row,
      ingredient_name = ingredient_name
    )
  )
}

#' Tilføj en ingrediens til en opskrift
#'
#' Den nye linje får samme skema og samme første kolonnenavn som resten af
#' opskriften og indsættes nederst. For at bevare appens hidtidige regler skal
#' mængden være et tal, men den begrænses ikke yderligere i denne handling.
#'
#' @param catalog Det aktuelle katalog-snapshot.
#' @param key Nøglen til opskriften.
#' @param name Ingrediensens navn.
#' @param amount Ingrediensens mængde som tal.
#' @param unit Ingrediensens enhed, som gerne må være tom.
#' @param category_1 Ingrediensens første kategori.
#' @param category_2 Ingrediensens anden kategori, som gerne må være tom.
#'
#' @return Et ensartet katalogresultat med den tilføjede linje.
recipe_catalog_add_ingredient <- function(
  catalog,
  key,
  name,
  amount,
  unit,
  category_1,
  category_2
) {
  recipe_catalog_validate(catalog)
  key <- recipe_catalog_scalar_text(key, "Opskriftsnøglen")
  recipe <- recipe_catalog_get_recipe(catalog, key)
  ingredient_name <- recipe_catalog_scalar_text(name, "Ingrediensens navn")
  amount <- recipe_catalog_amount(amount, allow_missing = FALSE)
  unit <- recipe_catalog_scalar_text(unit, "Enhed", allow_empty = TRUE)
  category_1 <- recipe_catalog_scalar_text(category_1, "Kategori 1")
  category_2 <- recipe_catalog_scalar_text(
    category_2,
    "Kategori 2",
    allow_empty = TRUE
  )
  recipe_name <- recipe_catalog_display_name(recipe)

  new_row <- data.frame(
    ingredient = ingredient_name,
    maengde = amount,
    enhed = unit,
    kat_1 = category_1,
    kat_2 = category_2,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(new_row)[1] <- recipe_name
  recipe <- rbind(recipe, new_row)
  rownames(recipe) <- NULL
  row <- nrow(recipe)

  next_catalog <- recipe_catalog_copy(catalog)
  next_catalog$recipes[[key]] <- recipe

  recipe_catalog_result(
    next_catalog,
    recipe_catalog_event(
      "ingredient_added",
      key,
      recipe_name,
      row = row,
      ingredient_name = ingredient_name,
      line = recipe_catalog_format_line(amount, unit, ingredient_name)
    )
  )
}

#' Slet en ingredienslinje permanent
#'
#' @param catalog Det aktuelle katalog-snapshot.
#' @param key Nøglen til opskriften.
#' @param row Rækkenummeret, der skal slettes.
#'
#' @return Et ensartet katalogresultat. `event$line` indeholder den læsbare
#'   ingredienslinje, som brugerfladen kan vise i sin kvittering.
recipe_catalog_delete_ingredient <- function(catalog, key, row) {
  recipe_catalog_validate(catalog)
  key <- recipe_catalog_scalar_text(key, "Opskriftsnøglen")
  recipe <- recipe_catalog_get_recipe(catalog, key)
  row <- recipe_catalog_row_number(row, recipe)
  recipe_name <- recipe_catalog_display_name(recipe)
  ingredient_name <- as.character(recipe[[1]][[row]])
  line <- recipe_catalog_format_line(
    recipe$maengde[[row]],
    recipe$enhed[[row]],
    ingredient_name
  )

  recipe <- recipe[-row, , drop = FALSE]
  rownames(recipe) <- NULL
  next_catalog <- recipe_catalog_copy(catalog)
  next_catalog$recipes[[key]] <- recipe

  recipe_catalog_result(
    next_catalog,
    recipe_catalog_event(
      "ingredient_deleted",
      key,
      recipe_name,
      row = row,
      ingredient_name = ingredient_name,
      line = line
    )
  )
}

#' Flyt en aktiv ret til arkivet
#'
#' Opskriften og eventuelle links bevares. Kun rækken flyttes fra aktive til
#' arkiverede retter, hvorefter begge tabeller sorteres alfabetisk.
#'
#' @param catalog Det aktuelle katalog-snapshot.
#' @param key Nøglen til den aktive ret.
#'
#' @return Et ensartet katalogresultat med hændelsen `"archived"`.
recipe_catalog_archive <- function(catalog, key) {
  recipe_catalog_validate(catalog)
  key <- recipe_catalog_scalar_text(key, "Opskriftsnøglen")
  row <- match(key, catalog$active_retter$key)
  if (is.na(row)) {
    stop(
      sprintf("Retten med nøglen '%s' er ikke aktiv.", key),
      call. = FALSE
    )
  }

  next_catalog <- recipe_catalog_copy(catalog)
  archived_row <- next_catalog$active_retter[row, , drop = FALSE]
  next_catalog$active_retter <- next_catalog$active_retter[
    -row,
    ,
    drop = FALSE
  ]
  next_catalog$active_retter <- recipe_catalog_sort_table(
    next_catalog$active_retter,
    "retter"
  )
  next_catalog$archived_retter <- rbind(
    next_catalog$archived_retter,
    archived_row
  )
  next_catalog$archived_retter <- recipe_catalog_distinct_first(
    next_catalog$archived_retter,
    "key"
  )
  next_catalog$archived_retter <- recipe_catalog_sort_table(
    next_catalog$archived_retter,
    "retter"
  )

  recipe_name <- as.character(archived_row$retter[[1]])
  recipe_catalog_result(
    next_catalog,
    recipe_catalog_event("archived", key, recipe_name)
  )
}

#' Gendan en arkiveret ret
#'
#' Retten flyttes kun tilbage, hvis den tilhørende opskrift stadig findes.
#' Dermed bliver en gammel arkivrække uden opskriftsfil ikke gjort aktiv.
#'
#' @param catalog Det aktuelle katalog-snapshot.
#' @param key Nøglen til den arkiverede ret.
#'
#' @return Et ensartet katalogresultat med hændelsen `"restored"`.
recipe_catalog_restore <- function(catalog, key) {
  recipe_catalog_validate(catalog)
  key <- recipe_catalog_scalar_text(key, "Opskriftsnøglen")
  row <- match(key, catalog$archived_retter$key)
  if (is.na(row)) {
    stop(
      sprintf("Retten med nøglen '%s' findes ikke i arkivet.", key),
      call. = FALSE
    )
  }
  if (!key %in% names(catalog$recipes)) {
    stop(
      sprintf(
        "Opskriften til den arkiverede ret '%s' mangler.",
        key
      ),
      call. = FALSE
    )
  }

  next_catalog <- recipe_catalog_copy(catalog)
  restored_row <- next_catalog$archived_retter[row, , drop = FALSE]
  next_catalog$archived_retter <- next_catalog$archived_retter[
    -row,
    ,
    drop = FALSE
  ]
  next_catalog$archived_retter <- recipe_catalog_sort_table(
    next_catalog$archived_retter,
    "retter"
  )
  next_catalog$active_retter <- rbind(
    next_catalog$active_retter,
    restored_row
  )
  next_catalog$active_retter <- recipe_catalog_distinct_first(
    next_catalog$active_retter,
    "key"
  )
  next_catalog$active_retter <- recipe_catalog_sort_table(
    next_catalog$active_retter,
    "retter"
  )

  recipe_name <- as.character(restored_row$retter[[1]])
  recipe_catalog_result(
    next_catalog,
    recipe_catalog_event("restored", key, recipe_name)
  )
}

#' Slet en arkiveret ret permanent
#'
#' Funktionen fjerner arkivets række, opskriften og alle links med rettens
#' navn. Den returnerer også nøglen i `delete_recipe_keys`, så lagerlaget kan
#' fjerne opskriftsfilen. En gammel arkivrække uden opskrift kan stadig ryddes
#' op på denne måde.
#'
#' @param catalog Det aktuelle katalog-snapshot.
#' @param key Nøglen til den arkiverede ret.
#'
#' @return Et ensartet katalogresultat med slettenøglen udfyldt.
recipe_catalog_delete <- function(catalog, key) {
  recipe_catalog_validate(catalog)
  key <- recipe_catalog_scalar_text(key, "Opskriftsnøglen")
  row <- match(key, catalog$archived_retter$key)
  if (is.na(row)) {
    stop(
      sprintf("Retten med nøglen '%s' findes ikke i arkivet.", key),
      call. = FALSE
    )
  }

  next_catalog <- recipe_catalog_copy(catalog)
  deleted_row <- next_catalog$archived_retter[row, , drop = FALSE]
  recipe_name <- as.character(deleted_row$retter[[1]])
  next_catalog$archived_retter <- next_catalog$archived_retter[
    -row,
    ,
    drop = FALSE
  ]
  next_catalog$archived_retter <- recipe_catalog_sort_table(
    next_catalog$archived_retter,
    "retter"
  )
  next_catalog$recipes[[key]] <- NULL

  matching_link <- !is.na(next_catalog$links$ret) &
    next_catalog$links$ret == recipe_name
  if (any(matching_link)) {
    next_catalog$links <- next_catalog$links[
      !matching_link,
      ,
      drop = FALSE
    ]
    next_catalog$links <- recipe_catalog_sort_table(
      next_catalog$links,
      "ret"
    )
  }

  recipe_catalog_result(
    next_catalog,
    recipe_catalog_event("deleted", key, recipe_name),
    delete_recipe_keys = key
  )
}
