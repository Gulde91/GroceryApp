suppressPackageStartupMessages({
  source(file.path("R", "recipe_schema.R"), encoding = "UTF-8")
  source(file.path("R", "recipe_catalog.R"), encoding = "UTF-8")
})

expect_recipe_catalog_error <- function(expression, pattern) {
  error <- tryCatch(
    {
      force(expression)
      NULL
    },
    error = function(condition) condition
  )

  stopifnot(
    inherits(error, "error"),
    grepl(pattern, conditionMessage(error), ignore.case = TRUE)
  )
}

recipe_catalog_test_recipe <- function(name, ingredients) {
  recipe <- data.frame(
    ingredient = ingredients,
    maengde = seq_along(ingredients),
    enhed = rep("stk", length(ingredients)),
    kat_1 = rep("grønt", length(ingredients)),
    kat_2 = rep("", length(ingredients)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(recipe)[1] <- name
  recipe
}

recipe_catalog_test_fixture <- function() {
  list(
    active_retter = data.frame(
      retter = c("Tortellini", "Burger", "Bøf"),
      key = c("tortellini_opskr", "burger_opskr", "bof_opskr"),
      type = c("vegetar", "kød", "kød"),
      stringsAsFactors = FALSE
    ),
    archived_retter = data.frame(
      retter = c("Suppe", "Manglende opskrift"),
      key = c("suppe_opskr", "manglende_opskr"),
      type = c("vegetar", "vegetar"),
      stringsAsFactors = FALSE
    ),
    links = data.frame(
      ret = c("Burger", "Suppe", "Burger", "Tortellini"),
      link = c(
        "https://example.com/burger-1",
        "https://example.com/suppe",
        "https://example.com/burger-2",
        "https://example.com/tortellini"
      ),
      stringsAsFactors = FALSE
    ),
    recipes = list(
      tortellini_opskr = recipe_catalog_test_recipe(
        "Tortellini",
        c("tortellini", "tomat")
      ),
      burger_opskr = recipe_catalog_test_recipe(
        "Burger",
        c("bøf", "bolle")
      ),
      bof_opskr = recipe_catalog_test_recipe("Bøf", "oksekød"),
      suppe_opskr = recipe_catalog_test_recipe("Suppe", "tomat")
    ),
    revision = "fixture-revision"
  )
}

run_recipe_catalog_tests <- function() {
  catalog <- recipe_catalog_test_fixture()
  original_bytes <- serialize(catalog, NULL, version = 2)

  created <- recipe_catalog_create(
    catalog,
    recipe_name = "Abemad",
    recipe_type = "vegetar",
    link = "example.com/abemad"
  )
  stopifnot(
    identical(names(created), c("catalog", "event", "delete_recipe_keys")),
    identical(created$event$reason, "created"),
    identical(created$event$key, "abemad_opskr"),
    identical(created$event$recipe_name, "Abemad"),
    identical(created$delete_recipe_keys, character()),
    identical(created$catalog$active_retter$retter[[1]], "Abemad"),
    identical(
      names(created$catalog$recipes$abemad_opskr),
      c("Abemad", "maengde", "enhed", "kat_1", "kat_2")
    ),
    nrow(created$catalog$recipes$abemad_opskr) == 0L,
    any(
      created$catalog$links$ret == "Abemad" &
        created$catalog$links$link == "https://example.com/abemad"
    )
  )

  collision <- recipe_catalog_create(
    catalog,
    recipe_name = "Bof",
    recipe_type = "kød"
  )
  stopifnot(
    identical(collision$event$key, "bof_opskr_2"),
    "bof_opskr_2" %in% names(collision$catalog$recipes)
  )
  expect_recipe_catalog_error(
    recipe_catalog_create(catalog, " burger ", "kød"),
    "findes allerede"
  )

  updated <- recipe_catalog_update_ingredient(
    catalog,
    key = "burger_opskr",
    row = 1L,
    amount = 3,
    unit = " kg ",
    category_1 = " kød ",
    category_2 = "fersk"
  )
  updated_recipe <- updated$catalog$recipes$burger_opskr
  stopifnot(
    identical(updated$event$reason, "ingredient_updated"),
    identical(updated$event$key, "burger_opskr"),
    identical(updated$event$row, 1L),
    identical(updated$event$recipe_name, "Burger"),
    identical(updated_recipe$maengde[[1]], 3),
    identical(updated_recipe$enhed[[1]], "kg"),
    identical(updated_recipe$kat_1[[1]], "kød"),
    identical(updated_recipe$kat_2[[1]], "fersk"),
    identical(updated_recipe[[1]][[1]], "bøf")
  )
  expect_recipe_catalog_error(
    recipe_catalog_update_ingredient(
      catalog,
      "burger_opskr",
      1L,
      0,
      "stk",
      "kød",
      ""
    ),
    "større end 0"
  )

  added <- recipe_catalog_add_ingredient(
    catalog,
    key = "burger_opskr",
    name = " løg ",
    amount = 2,
    unit = "stk",
    category_1 = "grønt",
    category_2 = ""
  )
  added_recipe <- added$catalog$recipes$burger_opskr
  stopifnot(
    nrow(added_recipe) == 3L,
    identical(added_recipe[[1]][[3]], "løg"),
    identical(names(added_recipe)[1], "Burger"),
    identical(added$event$reason, "ingredient_added"),
    identical(added$event$ingredient_name, "løg"),
    identical(added$event$line, "2 stk løg"),
    identical(added$delete_recipe_keys, character())
  )

  deleted_ingredient <- recipe_catalog_delete_ingredient(
    added$catalog,
    "burger_opskr",
    3L
  )
  stopifnot(
    nrow(deleted_ingredient$catalog$recipes$burger_opskr) == 2L,
    identical(deleted_ingredient$event$reason, "ingredient_deleted"),
    identical(deleted_ingredient$event$ingredient_name, "løg"),
    identical(deleted_ingredient$event$line, "2 stk løg"),
    identical(deleted_ingredient$delete_recipe_keys, character())
  )

  archived <- recipe_catalog_archive(catalog, "burger_opskr")
  stopifnot(
    !"burger_opskr" %in% archived$catalog$active_retter$key,
    "burger_opskr" %in% archived$catalog$archived_retter$key,
    "burger_opskr" %in% names(archived$catalog$recipes),
    any(archived$catalog$links$ret == "Burger"),
    identical(archived$event$reason, "archived"),
    identical(archived$event$recipe_name, "Burger"),
    identical(
      archived$catalog$active_retter$retter,
      sort(archived$catalog$active_retter$retter)
    ),
    identical(
      archived$catalog$archived_retter$retter,
      sort(archived$catalog$archived_retter$retter)
    )
  )

  restored <- recipe_catalog_restore(archived$catalog, "burger_opskr")
  stopifnot(
    "burger_opskr" %in% restored$catalog$active_retter$key,
    !"burger_opskr" %in% restored$catalog$archived_retter$key,
    identical(restored$event$reason, "restored"),
    identical(restored$event$recipe_name, "Burger"),
    identical(
      restored$catalog$active_retter$retter,
      sort(restored$catalog$active_retter$retter)
    )
  )
  expect_recipe_catalog_error(
    recipe_catalog_restore(catalog, "manglende_opskr"),
    "mangler"
  )

  permanently_deleted <- recipe_catalog_delete(
    archived$catalog,
    "burger_opskr"
  )
  stopifnot(
    !"burger_opskr" %in% permanently_deleted$catalog$archived_retter$key,
    !"burger_opskr" %in% names(permanently_deleted$catalog$recipes),
    !any(permanently_deleted$catalog$links$ret == "Burger"),
    any(permanently_deleted$catalog$links$ret == "Suppe"),
    identical(permanently_deleted$event$reason, "deleted"),
    identical(permanently_deleted$event$key, "burger_opskr"),
    identical(permanently_deleted$event$recipe_name, "Burger"),
    identical(permanently_deleted$delete_recipe_keys, "burger_opskr")
  )

  ghost_deleted <- recipe_catalog_delete(catalog, "manglende_opskr")
  stopifnot(
    !"manglende_opskr" %in% ghost_deleted$catalog$archived_retter$key,
    identical(ghost_deleted$delete_recipe_keys, "manglende_opskr"),
    identical(ghost_deleted$catalog$links, catalog$links)
  )

  malformed_catalog <- catalog
  names(malformed_catalog$active_retter)[1] <- "navn"
  expect_recipe_catalog_error(
    recipe_catalog_archive(malformed_catalog, "burger_opskr"),
    "kolonnerne"
  )
  expect_recipe_catalog_error(
    recipe_catalog_create(catalog, c("A", "B"), "vegetar"),
    "én tekstværdi"
  )
  expect_recipe_catalog_error(
    recipe_catalog_delete_ingredient(catalog, "burger_opskr", 99L),
    "Rækkenummeret"
  )

  stopifnot(identical(serialize(catalog, NULL, version = 2), original_bytes))
}

run_recipe_catalog_tests()
message("Alle rene recipe-catalog-regler bestod deres tests.")
