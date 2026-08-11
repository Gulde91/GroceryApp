suppressPackageStartupMessages({
  source(file.path("R", "store_lock.R"), encoding = "UTF-8")
  source(file.path("R", "recipe_schema.R"), encoding = "UTF-8")
  source(file.path("R", "recipe_store.R"), encoding = "UTF-8")
})

expect_error <- function(code, pattern = NULL) {
  error <- tryCatch(
    {
      force(code)
      NULL
    },
    error = identity
  )

  stopifnot(inherits(error, "error"))
  if (!is.null(pattern)) {
    stopifnot(grepl(pattern, conditionMessage(error), fixed = TRUE))
  }

  invisible(error)
}

file_hash <- function(path) {
  if (!file.exists(path)) return("<missing>")
  unname(tools::md5sum(path))
}

read_store_table <- function(path) {
  utils::read.delim(
    path,
    sep = ";",
    header = TRUE,
    quote = "",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

run_recipe_store_tests <- function() {
  root <- tempfile("groceryapp-recipe-store-")
  dir.create(root)
  dir.create(file.path(root, "opskrifter"))

  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  temp_root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  stopifnot(startsWith(tolower(root), paste0(tolower(temp_root), "/")))
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  active <- data.frame(
    retter = c("Burger", "Kyllingespyd"),
    key = c("burger_opskr", "kyllingespyd_opsk"),
    type = c("okse|gris", "kylling"),
    stringsAsFactors = FALSE
  )
  archive_empty <- data.frame(
    retter = character(),
    key = character(),
    type = character(),
    stringsAsFactors = FALSE
  )
  links <- data.frame(
    ret = c("Burger", "Burger", "Kyllingespyd", "Kylling kiev"),
    link = c(
      "https://example.com/burger",
      "https://example.com/burger-alt",
      "https://example.com/kyllingespyd",
      "https://example.com/orphan"
    ),
    stringsAsFactors = FALSE
  )

  burger <- data.frame(
    temp = c("hakket oksekød", "bacon i skiver"),
    maengde = c(0.175, 50),
    enhed = c("kg", "gram"),
    kat_1 = c("kød", "kød"),
    kat_2 = c("", "pålæg"),
    stringsAsFactors = FALSE
  )
  names(burger)[1] <- "Burger"

  kyllingespyd <- data.frame(
    temp = "kyllingebryst",
    maengde = 0.2,
    enhed = "kg",
    kat_1 = "kød",
    kat_2 = "",
    stringsAsFactors = FALSE
  )
  names(kyllingespyd)[1] <- "Kyllingespyd"

  empty_recipe <- data.frame(
    temp = character(),
    maengde = numeric(),
    enhed = character(),
    kat_1 = character(),
    kat_2 = character(),
    stringsAsFactors = FALSE
  )
  names(empty_recipe)[1] <- "Tom opskrift"

  revision_empty <- recipe_store_revision(root)
  revision_initial <- recipe_store_commit(
    data_dir = root,
    active_retter = active,
    archived_retter = archive_empty,
    links = links,
    recipes = list(
      burger_opskr = burger,
      kyllingespyd_opsk = kyllingespyd,
      tom_opskr = empty_recipe
    ),
    expected_revision = revision_empty
  )

  stopifnot(
    !identical(revision_initial, revision_empty),
    identical(readLines(file.path(root, "retter.txt"), n = 1L), "retter;key;type"),
    identical(readLines(file.path(root, "retter_arkiv.txt"), n = 1L), "retter;key;type"),
    identical(readLines(file.path(root, "links.txt"), n = 1L), "ret;link"),
    identical(
      readLines(file.path(root, "opskrifter", "tom_opskr.txt"), n = 1L),
      "Tom opskrift;maengde;enhed;kat_1;kat_2"
    ),
    nrow(read_store_table(file.path(root, "retter_arkiv.txt"))) == 0L,
    nrow(read_store_table(file.path(root, "opskrifter", "tom_opskr.txt"))) == 0L
  )

  initial_snapshot <- recipe_store_read(root)
  stopifnot(
    identical(initial_snapshot$revision, revision_initial),
    identical(initial_snapshot$active_retter$key, active$key),
    nrow(initial_snapshot$archived_retter) == 0L,
    identical(names(initial_snapshot$recipes), c(
      "burger_opskr",
      "kyllingespyd_opsk",
      "tom_opskr"
    )),
    nrow(initial_snapshot$recipes$tom_opskr) == 0L,
    identical(initial_snapshot$links$ret, links$ret)
  )

  retter_bytes <- readBin(
    file.path(root, "retter.txt"),
    what = "raw",
    n = file.info(file.path(root, "retter.txt"))$size
  )
  stopifnot(
    !identical(retter_bytes[seq_len(min(3L, length(retter_bytes)))], as.raw(c(0xef, 0xbb, 0xbf))),
    identical(tail(retter_bytes, 1L), as.raw(0x0a))
  )

  burger_file <- file.path(root, "opskrifter", "burger_opskr.txt")
  links_file <- file.path(root, "links.txt")
  orphan_recipe_file <- file.path(root, "opskrifter", "tom_opskr.txt")
  burger_hash <- file_hash(burger_file)
  links_hash <- file_hash(links_file)
  orphan_recipe_hash <- file_hash(orphan_recipe_file)

  burger_row <- active[active$key == "burger_opskr", , drop = FALSE]
  active_after_archive <- active[active$key != "burger_opskr", , drop = FALSE]
  archive_with_burger <- burger_row

  revision_archived <- recipe_store_commit(
    data_dir = root,
    active_retter = active_after_archive,
    archived_retter = archive_with_burger,
    expected_revision = revision_initial
  )

  stored_active <- read_store_table(file.path(root, "retter.txt"))
  stored_archive <- read_store_table(file.path(root, "retter_arkiv.txt"))
  stopifnot(
    identical(stored_active$key, "kyllingespyd_opsk"),
    identical(stored_archive$key, "burger_opskr"),
    identical(file_hash(burger_file), burger_hash),
    identical(file_hash(links_file), links_hash),
    identical(file_hash(orphan_recipe_file), orphan_recipe_hash)
  )

  revision_restored <- recipe_store_commit(
    data_dir = root,
    active_retter = active,
    archived_retter = archive_empty,
    expected_revision = revision_archived
  )
  stopifnot(
    identical(read_store_table(file.path(root, "retter.txt"))$key, active$key),
    nrow(read_store_table(file.path(root, "retter_arkiv.txt"))) == 0L
  )

  expect_error(
    recipe_store_commit(
      data_dir = root,
      delete_recipe_keys = "burger_opskr",
      expected_revision = revision_restored
    ),
    "aktiv ret"
  )

  expect_error(
    recipe_store_commit(
      data_dir = root,
      active_retter = active_after_archive,
      archived_retter = archive_with_burger,
      expected_revision = revision_restored,
      .crash_at = "after_backup_1"
    ),
    "Simuleret processtop"
  )
  stopifnot(
    file.exists(file.path(root, "recipe-store-lock.sqlite")),
    !dir.exists(file.path(root, ".recipe-store-lock")),
    file.exists(file.path(root, ".recipe-store-transaction.rds"))
  )
  recovered_revision <- recipe_store_revision(root)
  recovered_snapshot <- recipe_store_read(root)
  stopifnot(
    identical(recovered_revision, revision_restored),
    identical(recovered_snapshot$revision, revision_restored),
    identical(recovered_snapshot$active_retter$key, active$key),
    nrow(recovered_snapshot$archived_retter) == 0L,
    !dir.exists(file.path(root, ".recipe-store-lock")),
    !file.exists(file.path(root, ".recipe-store-transaction.rds"))
  )

  before_rollback_revision <- recipe_store_revision(root)
  expect_error(
    recipe_store_commit(
      data_dir = root,
      active_retter = active_after_archive,
      archived_retter = archive_with_burger,
      expected_revision = revision_restored,
      .fail_at = "after_promote_1"
    ),
    "Testfejl"
  )
  stopifnot(
    identical(recipe_store_revision(root), before_rollback_revision),
    identical(read_store_table(file.path(root, "retter.txt"))$key, active$key),
    nrow(read_store_table(file.path(root, "retter_arkiv.txt"))) == 0L,
    file.exists(file.path(root, "recipe-store-lock.sqlite")),
    !dir.exists(file.path(root, ".recipe-store-lock"))
  )

  revision_archived <- recipe_store_commit(
    data_dir = root,
    active_retter = active_after_archive,
    archived_retter = archive_with_burger,
    expected_revision = revision_restored
  )

  links_without_burger <- links[links$ret != "Burger", , drop = FALSE]
  before_purge_revision <- recipe_store_revision(root)
  before_purge_recipe_hash <- file_hash(burger_file)

  expect_error(
    recipe_store_commit(
      data_dir = root,
      archived_retter = archive_empty,
      links = links_without_burger,
      delete_recipe_keys = "burger_opskr",
      expected_revision = revision_archived,
      .fail_at = "after_promote_1"
    ),
    "Testfejl"
  )
  stopifnot(
    identical(recipe_store_revision(root), before_purge_revision),
    identical(file_hash(burger_file), before_purge_recipe_hash),
    identical(read_store_table(file.path(root, "retter_arkiv.txt"))$key, "burger_opskr"),
    sum(read_store_table(links_file)$ret == "Burger") == 2L
  )

  stale_links <- rbind(
    links,
    data.frame(
      ret = "Ny ekstern ændring",
      link = "https://example.com/new",
      stringsAsFactors = FALSE
    )
  )
  expect_error(
    recipe_store_commit(
      data_dir = root,
      links = stale_links,
      expected_revision = revision_restored
    ),
    "ændret i en anden session"
  )
  stopifnot(identical(recipe_store_revision(root), revision_archived))

  expect_error(
    recipe_store_commit(
      data_dir = root,
      archived_retter = archive_empty,
      links = links_without_burger,
      delete_recipe_keys = "burger_opskr",
      expected_revision = revision_archived,
      .crash_at = "after_commit_marker"
    ),
    "Simuleret processtop"
  )
  purged_snapshot <- recipe_store_read(root)
  revision_purged <- purged_snapshot$revision
  stopifnot(
    !file.exists(burger_file),
    nrow(read_store_table(file.path(root, "retter_arkiv.txt"))) == 0L,
    !any(read_store_table(links_file)$ret == "Burger"),
    identical(
      read_store_table(file.path(root, "retter.txt"))$key,
      active_after_archive$key
    ),
    identical(file_hash(orphan_recipe_file), orphan_recipe_hash)
  )

  ghost_archive <- data.frame(
    retter = "Manglende opskrift",
    key = "manglende_opskr",
    type = "vegetar",
    stringsAsFactors = FALSE
  )
  ghost_links <- rbind(
    links_without_burger,
    data.frame(
      ret = "Manglende opskrift",
      link = "https://example.com/missing",
      stringsAsFactors = FALSE
    )
  )
  revision_ghost <- recipe_store_commit(
    data_dir = root,
    archived_retter = ghost_archive,
    links = ghost_links,
    expected_revision = revision_purged
  )
  revision_ghost_purged <- recipe_store_commit(
    data_dir = root,
    archived_retter = archive_empty,
    links = links_without_burger,
    delete_recipe_keys = "manglende_opskr",
    expected_revision = revision_ghost
  )
  stopifnot(
    is.character(revision_ghost_purged),
    nrow(read_store_table(file.path(root, "retter_arkiv.txt"))) == 0L,
    !any(read_store_table(links_file)$ret == "Manglende opskrift")
  )

  bad_links <- links_without_burger
  bad_links$link[[1]] <- "https://example.com/a;b"
  revision_before_validation <- recipe_store_revision(root)
  expect_error(
    recipe_store_commit(
      data_dir = root,
      links = bad_links,
      expected_revision = revision_ghost_purged
    ),
    "semikolon"
  )
  expect_error(
    recipe_store_commit(
      data_dir = root,
      recipes = stats::setNames(list(empty_recipe), "../udenfor"),
      expected_revision = revision_ghost_purged
    ),
    "ugyldig"
  )
  expect_error(
    recipe_store_commit(
      data_dir = root,
      recipes = stats::setNames(list(empty_recipe), ".skjult"),
      expected_revision = revision_ghost_purged
    ),
    "ugyldig"
  )
  expect_error(
    recipe_store_commit(
      data_dir = root,
      recipes = stats::setNames(list(kyllingespyd), "KYLLINGESPYD_OPSK"),
      expected_revision = revision_ghost_purged
    ),
    "kolliderer"
  )
  case_collision_recipes <- list(empty_recipe, empty_recipe)
  names(case_collision_recipes) <- c("NyOpskrift", "nyopskrift")
  expect_error(
    recipe_store_commit(
      data_dir = root,
      recipes = case_collision_recipes,
      expected_revision = revision_ghost_purged
    ),
    "unikke"
  )
  bad_recipe_columns <- empty_recipe
  names(bad_recipe_columns)[2] <- "antal"
  expect_error(
    recipe_store_commit(
      data_dir = root,
      recipes = stats::setNames(
        list(bad_recipe_columns),
        "schema_test_opskr"
      ),
      expected_revision = revision_ghost_purged
    ),
    "maengde"
  )
  bad_recipe_name <- empty_recipe
  names(bad_recipe_name)[1] <- ""
  expect_error(
    recipe_store_commit(
      data_dir = root,
      recipes = stats::setNames(
        list(bad_recipe_name),
        "schema_test_opskr"
      ),
      expected_revision = revision_ghost_purged
    ),
    "mangler rettens navn"
  )
  bad_recipe_amount <- empty_recipe
  bad_recipe_amount$maengde <- character()
  expect_error(
    recipe_store_commit(
      data_dir = root,
      recipes = stats::setNames(
        list(bad_recipe_amount),
        "schema_test_opskr"
      ),
      expected_revision = revision_ghost_purged
    ),
    "numeriske"
  )
  stopifnot(identical(recipe_store_revision(root), revision_before_validation))

  hidden_artifacts <- list.files(
    root,
    pattern = "^\\.(recipe-store|retter|links)",
    all.files = TRUE,
    recursive = TRUE
  )
  hidden_artifacts <- setdiff(
    hidden_artifacts,
    "recipe-store-lock.sqlite"
  )
  stopifnot(length(hidden_artifacts) == 0L)
}

run_recipe_store_tests()

# Alle store-funktioner skal fortsat have en umiddelbart foregående
# Roxygen-blok, så transaktions- og recovery-reglerne er lette at forstå.
recipe_store_lines <- readLines(
  file.path("R", "recipe_store.R"),
  encoding = "UTF-8"
)
recipe_store_function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  recipe_store_lines
)
recipe_store_has_roxygen <- vapply(
  recipe_store_function_lines,
  function(line_number) {
    line_number > 1L &&
      grepl("^#'", recipe_store_lines[[line_number - 1L]])
  },
  logical(1)
)
stopifnot(
  length(recipe_store_function_lines) > 0L,
  all(recipe_store_has_roxygen)
)

message("Alle recipe-store transaktions- og rollback-tests bestod.")
