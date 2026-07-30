suppressPackageStartupMessages(
  source("indkobsseddel_catalog.R", encoding = "UTF-8")
)

indkobsseddel_catalog_test_recipe <- function(
  name,
  ingredients,
  amounts
) {
  result <- data.frame(
    ingredient = ingredients,
    maengde = amounts,
    enhed = rep("stk", length(ingredients)),
    kat_1 = rep("konserves", length(ingredients)),
    kat_2 = rep("", length(ingredients)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(result)[1] <- name
  result
}

burger <- indkobsseddel_catalog_test_recipe(
  "Burger",
  c("burgerbolle", "hakket oksekød"),
  c(1, 0.175)
)
salad <- indkobsseddel_catalog_test_recipe(
  "Grøn salat",
  c("salat", "agurk"),
  c(0.25, 0.25)
)
recipes <- list(burger_opskr = burger)
active_retter <- data.frame(
  retter = "Burger",
  key = "burger_opskr",
  type = "okse|gris",
  stringsAsFactors = FALSE
)
links <- data.frame(
  ret = "Burger",
  link = "https://example.com/burger",
  stringsAsFactors = FALSE
)
salater <- data.frame(
  retter = "Grøn salat",
  key = "gron_salat",
  stringsAsFactors = FALSE
)
salater_opskrifter <- list(gron_salat = salad)
tilbehor <- data.frame(
  Indkobsliste = "Pommes frites",
  maengde = 0.2,
  enhed = "kg",
  kat_1 = "frost",
  kat_2 = "",
  stringsAsFactors = FALSE
)

stopifnot(
  identical(indkobsseddel_person_count(NULL), 2),
  identical(indkobsseddel_person_count(numeric()), 2),
  identical(indkobsseddel_person_count(4), 4),
  is.na(indkobsseddel_person_count(0)),
  is.na(indkobsseddel_person_count(-1)),
  is.na(indkobsseddel_positive_number("ugyldig"))
)

fixture_before <- serialize(
  list(
    recipes,
    active_retter,
    links,
    salater,
    salater_opskrifter,
    tilbehor
  ),
  NULL,
  version = 2
)

selection_two <- indkobsseddel_prepare_recipe(
  recipes = recipes,
  active_retter = active_retter,
  links = links,
  salater = salater,
  salater_opskrifter = salater_opskrifter,
  tilbehor = tilbehor,
  selected_recipe = "Burger",
  selected_salad = "",
  persons = indkobsseddel_person_count(NULL),
  selected_accessory = ""
)
burger_meat_two <- selection_two$rows$maengde[
  selection_two$rows$Indkobsliste == "hakket oksekød"
]
stopifnot(
  nrow(selection_two$rows) == 2L,
  identical(burger_meat_two, 0.35),
  length(selection_two$sections) == 1L,
  identical(selection_two$sections[[1L]]$title, "Burger"),
  identical(selection_two$sections[[1L]]$pers, 2),
  identical(
    selection_two$sections[[1L]]$link,
    "https://example.com/burger"
  )
)

selection_four <- indkobsseddel_prepare_recipe(
  recipes = recipes,
  active_retter = active_retter,
  links = links,
  salater = salater,
  salater_opskrifter = salater_opskrifter,
  tilbehor = tilbehor,
  selected_recipe = "Burger",
  selected_salad = "",
  persons = 4,
  selected_accessory = ""
)
burger_meat_four <- selection_four$rows$maengde[
  selection_four$rows$Indkobsliste == "hakket oksekød"
]
stopifnot(
  identical(burger_meat_four, 0.7),
  identical(burger_meat_four, burger_meat_two * 2)
)

combined <- indkobsseddel_prepare_recipe(
  recipes = recipes,
  active_retter = active_retter,
  links = links,
  salater = salater,
  salater_opskrifter = salater_opskrifter,
  tilbehor = tilbehor,
  selected_recipe = "Burger",
  selected_salad = "Grøn salat",
  persons = 2,
  selected_accessory = "Pommes frites"
)
stopifnot(
  nrow(combined$rows) == 5L,
  all(
    c(
      "burgerbolle",
      "hakket oksekød",
      "salat",
      "agurk",
      "Pommes frites"
    ) %in% combined$rows$Indkobsliste
  ),
  length(combined$sections) == 1L,
  identical(
    combined$sections[[1L]]$title,
    "Burger m. Grøn salat"
  ),
  nrow(combined$sections[[1L]]$df) == 4L
)

accessory_only <- indkobsseddel_prepare_recipe(
  recipes = recipes,
  active_retter = active_retter,
  links = links,
  salater = salater,
  salater_opskrifter = salater_opskrifter,
  tilbehor = tilbehor,
  selected_recipe = "",
  selected_salad = "",
  persons = 2,
  selected_accessory = "Pommes frites"
)
stopifnot(
  nrow(accessory_only$rows) == 1L,
  identical(accessory_only$rows$maengde[[1L]], 0.4),
  identical(
    accessory_only$sections[[1L]]$title,
    "Tilbehør: Pommes frites"
  )
)

invalid_persons <- indkobsseddel_prepare_recipe(
  recipes = recipes,
  active_retter = active_retter,
  links = links,
  salater = salater,
  salater_opskrifter = salater_opskrifter,
  tilbehor = tilbehor,
  selected_recipe = "Burger",
  selected_salad = "",
  persons = 0,
  selected_accessory = ""
)
stopifnot(
  nrow(invalid_persons$rows) == 0L,
  length(invalid_persons$sections) == 0L
)

duplicate_recipes <- c(
  recipes,
  list(burger_2_opskr = burger)
)
duplicate_index <- rbind(
  active_retter,
  transform(active_retter, key = "burger_2_opskr")
)
ambiguous <- indkobsseddel_scaled_recipe(
  duplicate_recipes,
  duplicate_index,
  "Burger",
  2,
  "Opskriften"
)
stopifnot(nrow(ambiguous) == 0L)

varer <- data.frame(
  Indkobsliste = c("Mælk", "Agurk"),
  maengde = c(1, 1),
  enhed = c("liter", "stk"),
  kat_1 = c("mejeri", "frugt og grønt"),
  kat_2 = c("mælk", ""),
  stringsAsFactors = FALSE
)
milk <- indkobsseddel_find_item(varer, "  MÆLK ")
stopifnot(
  nrow(milk) == 1L,
  identical(milk$Indkobsliste[[1L]], "Mælk"),
  identical(indkobsseddel_item_names(varer), c("Agurk", "Mælk"))
)
duplicate_varer <- rbind(
  varer,
  transform(varer[1L, , drop = FALSE], Indkobsliste = " MÆLK ")
)
stopifnot(nrow(indkobsseddel_find_item(
  duplicate_varer,
  "mælk"
)) == 0L)

fixture_after <- serialize(
  list(
    recipes,
    active_retter,
    links,
    salater,
    salater_opskrifter,
    tilbehor
  ),
  NULL,
  version = 2
)
stopifnot(identical(fixture_after, fixture_before))

message(paste(
  "Indkøbssedlens rene katalogregler skalerer opskrifter ved både to og",
  "fire personer, kombinerer valg og finder varer uden at ændre inputdata."
))
