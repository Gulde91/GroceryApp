library(tools)

#' Opret en tom, kanonisk indkøbshistorik
#'
#' Den faste kolonnerækkefølge og de faste typer gør, at resten af appen kan
#' bruge samme kontrakt, uanset om historikmappen mangler, er tom eller kun
#' indeholder filer, der ikke kan læses.
#'
#' @return En tom data frame med filnavn, dato, linjenummer og tekst.
#' @keywords internal
.shopping_history_empty_entries <- function() {
  data.frame(
    filename = character(),
    date = as.Date(character()),
    line_number = integer(),
    Indkøbsliste = character(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Find de filer, som tilhører indkøbshistorikken
#'
#' Kun det eksisterende navneformat accepteres. Andre filer i mappen bliver
#' dermed ikke opfattet som indkøbssedler ved en fejl.
#'
#' @param history_dir Mappen med gemte indkøbssedler.
#'
#' @return En alfabetisk sorteret tegnvektor med filnavne uden mappesti.
#' @keywords internal
.shopping_history_files <- function(history_dir) {
  if (
    length(history_dir) != 1L ||
      is.na(history_dir) ||
      !dir.exists(history_dir)
  ) {
    return(character())
  }

  list.files(
    history_dir,
    pattern = "^indkobsseddel_[0-9]{8}\\.rda[0-9]*$",
    full.names = FALSE
  )
}

#' Læs datoen fra et historikfilnavn
#'
#' Filnavnet valideres både mod navnemønsteret og som en rigtig kalenderdato.
#' Eksempelvis bliver en fil med datoen 30. februar derfor afvist.
#'
#' @param filename Filnavnet uden mappesti.
#'
#' @return Datoen som én `Date`-værdi.
#' @keywords internal
.shopping_history_date_from_filename <- function(filename) {
  if (
    length(filename) != 1L ||
      is.na(filename) ||
      !grepl(
        "^indkobsseddel_[0-9]{8}\\.rda[0-9]*$",
        basename(filename)
      )
  ) {
    stop("Historikfilens navn er ugyldigt.", call. = FALSE)
  }

  date_text <- sub(
    "^indkobsseddel_([0-9]{8})\\.rda[0-9]*$",
    "\\1",
    basename(filename)
  )
  parsed <- suppressWarnings(as.Date(date_text, format = "%Y%m%d"))
  if (
    length(parsed) != 1L ||
      is.na(parsed) ||
      !identical(format(parsed, "%Y%m%d"), date_text)
  ) {
    stop("Historikfilens navn indeholder ikke en gyldig dato.", call. = FALSE)
  }

  parsed
}

#' Valider den gemte data frame ved indlæsning
#'
#' Ældre filer må gerne indeholde ekstra kolonner, men de skal indeholde netop
#' én kolonne med navnet `Indkøbsliste`. Teksten normaliseres til character i
#' det kanoniske snapshot.
#'
#' @param history Objektet `df` fra en historikfil.
#'
#' @return Kolonnen `Indkøbsliste` som en tegnvektor.
#' @keywords internal
.shopping_history_lines_from_frame <- function(history) {
  if (
    !is.data.frame(history) ||
      sum(names(history) == "Indkøbsliste") != 1L
  ) {
    stop(
      "Historikfilens 'df' skal indeholde kolonnen 'Indkøbsliste'.",
      call. = FALSE
    )
  }

  lines <- history[["Indkøbsliste"]]
  if (!is.atomic(lines) || is.object(lines) && !is.factor(lines)) {
    stop(
      "Historikfilens kolonne 'Indkøbsliste' har en ugyldig type.",
      call. = FALSE
    )
  }

  as.character(lines)
}

#' Indlæs én historikfil i et isoleret miljø
#'
#' Det isolerede miljø forhindrer, at objektet `df` eller andre objekter fra en
#' gammel fil overskriver værdier i appens globale miljø.
#'
#' @param filename Filnavnet uden mappesti.
#' @param history_dir Mappen med historikfilen.
#'
#' @return Kanoniske rækker fra filen.
#' @keywords internal
.shopping_history_file_entries <- function(filename, history_dir) {
  date <- .shopping_history_date_from_filename(filename)
  loaded <- new.env(parent = emptyenv())
  load(file.path(history_dir, filename), envir = loaded)
  if (!exists("df", envir = loaded, inherits = FALSE)) {
    stop("Historikfilen indeholder ikke objektet 'df'.", call. = FALSE)
  }

  lines <- .shopping_history_lines_from_frame(
    get("df", envir = loaded, inherits = FALSE)
  )
  if (length(lines) == 0L) {
    return(.shopping_history_empty_entries())
  }

  data.frame(
    filename = rep(filename, length(lines)),
    date = rep(date, length(lines)),
    line_number = seq_along(lines),
    Indkøbsliste = lines,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Omsæt en fejl i én historikfil til et tomt resultat
#'
#' Callbacken bruges ved sikker indlæsning. En beskadiget fil skal ikke
#' forhindre de øvrige indkøbssedler i at blive læst.
#'
#' @param error Den fangede fejl fra indlæsningen.
#'
#' @return `NULL`, så filen kan udelades fra det samlede snapshot.
#' @keywords internal
.shopping_history_ignore_file_error <- function(error) {
  NULL
}

#' Indlæs én historikfil uden at stoppe hele læsningen
#'
#' Alle fejl og advarsler fra netop denne fil undertrykkes. Det omfatter blandt
#' andet korrupte RData-filer, forkert objektnavn og ugyldige datoer.
#'
#' @param filename Filnavnet uden mappesti.
#' @param history_dir Mappen med historikfilen.
#'
#' @return Kanoniske rækker eller `NULL`, hvis filen ikke kan bruges.
#' @keywords internal
.shopping_history_file_entries_safe <- function(filename, history_dir) {
  suppressWarnings(
    tryCatch(
      .shopping_history_file_entries(filename, history_dir),
      error = .shopping_history_ignore_file_error
    )
  )
}

#' Beregn revisionen for indkøbshistorikkens filer
#'
#' Revisionen indeholder filnavn og MD5-værdi for hver fil, som følger det
#' kanoniske navnemønster. Den ændrer sig derfor, når en relevant fil bliver
#' tilføjet, fjernet, omdøbt eller får ændret sit indhold. Uvedkommende filer
#' påvirker ikke revisionen.
#'
#' @param history_dir Mappen med gemte indkøbssedler.
#'
#' @return Revisionen som præcis én character-værdi.
#' @keywords internal
shopping_history_store_revision <- function(
  history_dir = "./data/indkobssedler"
) {
  files <- .shopping_history_files(history_dir)
  if (length(files) == 0L) {
    return("empty")
  }

  hashes <- suppressWarnings(
    unname(md5sum(file.path(history_dir, files)))
  )
  hashes[is.na(hashes)] <- "unavailable"

  paste(
    paste(files, hashes, sep = "="),
    collapse = "|"
  )
}

#' Læs et kanonisk snapshot af indkøbshistorikken
#'
#' Funktionen læser kun historikfiler med det eksisterende navneformat og
#' samler deres linjer i én data frame. Rækkefølgen inde i hver fil bevares.
#' En enkelt ugyldig eller beskadiget fil bliver sprunget over uden at blokere
#' resten af historikken.
#'
#' @param history_dir Mappen med gemte indkøbssedler.
#'
#' @return En liste med præcis elementerne `entries` og `revision`.
#' @keywords internal
shopping_history_store_read <- function(
  history_dir = "./data/indkobssedler"
) {
  files <- .shopping_history_files(history_dir)
  rows <- vector("list", length(files))

  if (length(files) > 0L) {
    for (index in seq_along(files)) {
      rows[[index]] <- .shopping_history_file_entries_safe(
        files[[index]],
        history_dir
      )
    }
  }

  rows <- Filter(.shopping_history_has_rows, rows)
  entries <- if (length(rows) == 0L) {
    .shopping_history_empty_entries()
  } else {
    result <- do.call(rbind, rows)
    row.names(result) <- NULL
    result
  }

  list(
    entries = entries,
    revision = shopping_history_store_revision(history_dir)
  )
}

#' Afgør om et filudtræk indeholder historikrækker
#'
#' Funktionen er en navngivet callback til `Filter`, så alle selvdefinerede
#' funktioner i scriptet ligger på topniveau og kan testes isoleret.
#'
#' @param value Et muligt data frame-udtræk fra én fil.
#'
#' @return `TRUE`, når udtrækket indeholder mindst én række.
#' @keywords internal
.shopping_history_has_rows <- function(value) {
  is.data.frame(value) && nrow(value) > 0L
}

#' Normaliser en dato til brug i et historikfilnavn
#'
#' Kun én gyldig kalenderdato accepteres, så en gemning aldrig opretter en fil
#' med et tvetydigt eller ugyldigt navn.
#'
#' @param date En `Date` eller en værdi, der kan konverteres til `Date`.
#'
#' @return Datoen som én `Date`-værdi.
#' @keywords internal
.shopping_history_normalize_date <- function(date) {
  parsed <- suppressWarnings(
    tryCatch(as.Date(date), error = .shopping_history_ignore_file_error)
  )
  if (length(parsed) != 1L || is.na(parsed)) {
    stop("Datoen for indkøbssedlen er ugyldig.", call. = FALSE)
  }

  parsed
}

#' Valider en indkøbsseddel før gemning
#'
#' Nye filer har en streng kontrakt: en almindelig data frame med præcis én
#' character-kolonne ved navn `Indkøbsliste`. Det forhindrer, at nye filer
#' tilfører endnu et historikformat.
#'
#' @param history_df Den data frame, der ønskes gemt.
#'
#' @return Den validerede data frame uændret.
#' @keywords internal
.shopping_history_validate_save_frame <- function(history_df) {
  if (
    !is.data.frame(history_df) ||
      !identical(names(history_df), "Indkøbsliste") ||
      !is.character(history_df[["Indkøbsliste"]])
  ) {
    stop(
      paste(
        "Indkøbssedlens historik skal være en data frame med præcis",
        "character-kolonnen 'Indkøbsliste'."
      ),
      call. = FALSE
    )
  }

  history_df
}

#' Gem en indkøbsseddel og returner den friske historik
#'
#' Filnavnet og objektnavnet `df` bevarer det gamle `.rda`-format, så både nye
#' og eksisterende indkøbssedler kan læses gennem samme lager. En ny gemning
#' på samme dato overskriver dagens tidligere fil, ligesom appen hidtil har
#' gjort.
#'
#' @param history_df En data frame med præcis character-kolonnen
#'   `Indkøbsliste`.
#' @param history_dir En eksisterende mappe til historikfilerne.
#' @param date Datoen, der skal indgå i filnavnet.
#'
#' @return Et frisk snapshot med elementerne `entries` og `revision`.
#' @keywords internal
shopping_history_store_save <- function(
  history_df,
  history_dir = "./data/indkobssedler",
  date = Sys.Date()
) {
  history_df <- .shopping_history_validate_save_frame(history_df)
  if (
    length(history_dir) != 1L ||
      is.na(history_dir) ||
      !dir.exists(history_dir)
  ) {
    stop("Mappen til historiske indkøbssedler findes ikke.", call. = FALSE)
  }
  date <- .shopping_history_normalize_date(date)

  df <- history_df
  filename <- paste0(
    "indkobsseddel_",
    format(date, "%Y%m%d"),
    ".rda"
  )
  save(df, file = file.path(history_dir, filename))

  shopping_history_store_read(history_dir)
}

#' Valider et kanonisk historikudtræk
#'
#' Analysefunktionerne modtager samme faste kontrakt som lagerets snapshots.
#' En tydelig fejl her gør programmeringsfejl lettere at finde end et delvist
#' eller misvisende statistikresultat.
#'
#' @param entries En data frame fra `shopping_history_store_read()`.
#'
#' @return `entries` uændret efter validering.
#' @keywords internal
.shopping_history_validate_entries <- function(entries) {
  valid <- is.data.frame(entries) &&
    identical(
      names(entries),
      c("filename", "date", "line_number", "Indkøbsliste")
    ) &&
    is.character(entries$filename) &&
    inherits(entries$date, "Date") &&
    is.integer(entries$line_number) &&
    is.character(entries$Indkøbsliste)

  if (!isTRUE(valid)) {
    stop("Indkøbshistorikken har ikke det kanoniske schema.", call. = FALSE)
  }

  entries
}

#' Udvælg varelinjerne før den første separator i hver fil
#'
#' En tom linje adskiller varerne øverst på indkøbssedlen fra opskrifterne
#' nederst. Kun linjer med et lavere linjenummer end den første tomme linje
#' i samme fil bliver derfor taget med i varestatistikken.
#'
#' @param entries Et kanonisk historikudtræk.
#'
#' @return En tegnvektor med de rå varelinjer.
#' @keywords internal
.shopping_history_item_lines <- function(entries) {
  entries <- .shopping_history_validate_entries(entries)
  if (nrow(entries) == 0L) {
    return(character())
  }

  keep <- rep(FALSE, nrow(entries))
  filenames <- unique(entries$filename)
  for (filename in filenames) {
    file_rows <- which(entries$filename == filename)
    blank_rows <- file_rows[
      !is.na(entries$Indkøbsliste[file_rows]) &
        trimws(entries$Indkøbsliste[file_rows]) == ""
    ]
    first_blank <- if (length(blank_rows) == 0L) {
      Inf
    } else {
      min(entries$line_number[blank_rows])
    }
    keep[file_rows] <- entries$line_number[file_rows] < first_blank
  }

  entries$Indkøbsliste[keep]
}

#' Escape specialtegn i tekst til et regulært udtryk
#'
#' Enheder som `pakke(r)` skal behandles som almindelig tekst og ikke som et
#' regulært udtryk. Funktionen beskytter alle tegn med særlig regex-betydning.
#'
#' @param values Tegnvektor med tekst, der skal bruges bogstaveligt.
#'
#' @return Tegnvektoren med escapede regex-specialtegn.
#' @keywords internal
.shopping_history_escape_regex <- function(values) {
  gsub(
    "([][{}()+*^$|\\\\?.])",
    "\\\\\\1",
    values
  )
}

#' Rens historiske varelinjer på samme måde som den gamle statistik
#'
#' Mængder, kendte enheder og markørerne `(tilsmagning)` og `(tilbehør)`
#' fjernes. Enhederne sorteres længst først, så eksempelvis `kg` ikke bliver
#' delvist ramt af den kortere enhed `g`.
#'
#' @param lines Tegnvektor med rå varelinjer.
#' @param units Tegnvektor med appens kendte enheder.
#'
#' @return Rensede varenavne som en tegnvektor.
#' @keywords internal
.shopping_history_clean_items <- function(lines, units) {
  lines <- as.character(lines)
  lines <- sub("\\((tilsmagning|tilbehør)\\)", "", lines)
  lines <- sub("\\d+\\.*\\d*", "", lines, perl = TRUE)

  units <- unique(trimws(as.character(units)))
  units <- units[!is.na(units) & nzchar(units)]
  if (length(units) > 0L) {
    units <- units[order(nchar(units), decreasing = TRUE)]
    unit_pattern <- paste(
      .shopping_history_escape_regex(units),
      collapse = "|"
    )
    lines <- sub(unit_pattern, "", lines, perl = TRUE)
  }

  trimws(lines)
}

#' Find de mest brugte varer i det kanoniske historikudtræk
#'
#' Kun vareafsnittet før første tomme linje i hver indkøbsseddel indgår.
#' Linjerne renses for mængder, enheder og tilbehørsmarkører, optælles og
#' sorteres med de hyppigste varer først. Antalskolonnen er bevidst skjult for
#' at bevare den eksisterende kontrakt til forslagstabellen.
#'
#' @param entries Et kanonisk historikudtræk.
#' @param units Tegnvektor med appens kendte enheder.
#'
#' @return En data frame med character-kolonnen `Indkøbsliste`.
#' @keywords internal
shopping_history_popular_items <- function(entries, units) {
  lines <- .shopping_history_item_lines(entries)
  if (length(lines) == 0L) {
    return(data.frame(
      Indkøbsliste = character(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  items <- .shopping_history_clean_items(lines, units)
  items <- items[!is.na(items) & nzchar(items)]
  if (length(items) == 0L) {
    return(data.frame(
      Indkøbsliste = character(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  counts <- table(items)
  item_names <- names(counts)
  item_names <- item_names[
    order(-as.integer(counts), item_names)
  ]

  data.frame(
    Indkøbsliste = item_names,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Find det længste naturlige opskriftspræfiks i en linje
#'
#' `startsWith()` gør matchningen sikker for navne med parenteser, plus-tegn
#' og andre regex-specialtegn. Når eksempelvis både `Pizza` og `Pizza surdej`
#' matcher, vælges det længste navn. Efter navnet kræves slutningen af linjen
#' eller en naturlig grænse som mellemrum, kolon eller parentes.
#'
#' @param line En linje fra en gemt indkøbsseddel.
#' @param recipe_names Tegnvektor med kendte opskriftsnavne.
#'
#' @return Det matchede navn eller `NA_character_`.
#' @keywords internal
.shopping_history_recipe_for_line <- function(line, recipe_names) {
  if (length(line) != 1L || is.na(line)) {
    return(NA_character_)
  }

  line <- trimws(as.character(line))
  candidates <- recipe_names[startsWith(line, recipe_names)]
  if (length(candidates) == 0L) {
    return(NA_character_)
  }

  suffixes <- substring(line, nchar(candidates) + 1L)
  natural_boundary <- !nzchar(suffixes) |
    grepl("^[[:space:]:\\(]", suffixes)
  candidates <- candidates[natural_boundary]
  if (length(candidates) == 0L) {
    return(NA_character_)
  }

  candidates[[which.max(nchar(candidates))]]
}

#' Udtræk opskriftsbrug fra den fælles indkøbshistorik
#'
#' Hver historiklinje sammenlignes med de kendte opskriftsnavne. Resultatet
#' bevarer den eksisterende statistik-kontrakt med kolonnerne `retter` og
#' `dato` og sorteres efter ret og dato.
#'
#' @param entries Et kanonisk historikudtræk.
#' @param recipe_names Tegnvektor med kendte opskriftsnavne.
#'
#' @return En data frame med kolonnerne `retter` og `dato`.
#' @keywords internal
shopping_history_recipe_usage <- function(entries, recipe_names) {
  entries <- .shopping_history_validate_entries(entries)
  recipe_names <- unique(trimws(as.character(recipe_names)))
  recipe_names <- recipe_names[
    !is.na(recipe_names) & nzchar(recipe_names)
  ]
  if (nrow(entries) == 0L || length(recipe_names) == 0L) {
    return(data.frame(
      retter = character(),
      dato = as.Date(character()),
      stringsAsFactors = FALSE
    ))
  }

  matches <- vapply(
    entries$Indkøbsliste,
    .shopping_history_recipe_for_line,
    character(1),
    recipe_names = recipe_names
  )
  matched_rows <- which(!is.na(matches))
  if (length(matched_rows) == 0L) {
    return(data.frame(
      retter = character(),
      dato = as.Date(character()),
      stringsAsFactors = FALSE
    ))
  }

  result <- data.frame(
    retter = unname(matches[matched_rows]),
    dato = entries$date[matched_rows],
    stringsAsFactors = FALSE
  )
  result <- result[order(result$retter, result$dato), , drop = FALSE]
  row.names(result) <- NULL
  result
}
