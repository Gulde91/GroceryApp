suppressPackageStartupMessages(library(dplyr))
source(file.path("R", "cart_state.R"), encoding = "UTF-8")

cart_rows <- function(name, amount = 1, unit = "stk", category_1 = "konserves", category_2 = "") {
  data.frame(
    Indkobsliste = name,
    maengde = amount,
    enhed = unit,
    kat_1 = category_1,
    kat_2 = category_2,
    stringsAsFactors = FALSE
  )
}

# En tom state er altid typet og giver en tom visning.
state <- new_cart_state()
stopifnot(nrow(state$rows) == 0, nrow(cart_view(state)) == 0)

# Samme strukturerede vare summeres til én linje.
empty_state <- state
state <- cart_add_rows(state, cart_rows("æbler", 1, "kg", "frugt og grønt"))
stopifnot(nrow(empty_state$rows) == 0)
state <- cart_add_rows(state, cart_rows("æbler", 2, "kg", "frugt og grønt"))
view <- cart_view(state)
apple <- view[view$Indkobsliste == "æbler", , drop = FALSE]
stopifnot(nrow(apple) == 1, apple$maengde[[1]] == 3)

# Danske tegn sorteres ens uanset operativsystemets locale.
sort_state <- new_cart_state()
for (name in c("ålekvabbe", "æbler", "Østers", "zucchini", "ost")) {
  sort_state <- cart_add_rows(sort_state, cart_rows(name))
}
stopifnot(identical(
  cart_view(sort_state)$Indkobsliste,
  c("ost", "zucchini", "Østers", "ålekvabbe", "æbler")
))

# Også kategori og underkategori bruger en eksplicit, locale-uafhængig orden.
category_sort_state <- new_cart_state()
for (category in c("å-kategori", "æ-kategori", "Ø-kategori", "z-kategori")) {
  category_sort_state <- cart_add_rows(
    category_sort_state,
    cart_rows(paste0("vare-", category), category_1 = category)
  )
}
stopifnot(identical(
  cart_view(category_sort_state)$kat_1,
  c("z-kategori", "Ø-kategori", "å-kategori", "æ-kategori")
))

subcategory_sort_state <- new_cart_state()
for (subcategory in c("å-under", "æ-under", "Ø-under", "z-under")) {
  row <- cart_rows(paste0("vare-", subcategory))
  row$kat_2 <- subcategory
  subcategory_sort_state <- cart_add_rows(subcategory_sort_state, row)
}
stopifnot(identical(
  cart_view(subcategory_sort_state)$kat_2,
  c("z-under", "Ø-under", "å-under", "æ-under")
))

# En manglende mængde forbliver ukendt og vises ikke som et misvisende nul.
unknown_amount_state <- cart_add_rows(
  new_cart_state(),
  cart_rows("citronsaft (tilbehør)", NA_real_, "", "konserves")
)
unknown_amount_view <- cart_view(unknown_amount_state)
stopifnot(is.na(unknown_amount_view$maengde[[1]]))
stopifnot(unknown_amount_view$display[[1]] == "citronsaft (tilbehør)")

# En fri tekstredigering bliver stående, selv om en ny vare ændrer visningen.
apple_id <- apple$line_id[[1]]
state <- cart_edit_line(state, apple_id, "Husk 3 kg økologiske æbler")
state <- cart_add_rows(state, cart_rows("mælk", 1, "liter", "mejeri"))
view <- cart_view(state)
stopifnot(view$display[view$line_id == apple_id] == "Husk 3 kg økologiske æbler")

# En redigeret fri tekst er låst. Samme vare senere bliver derfor en ny,
# automatisk linje i stedet for at ændre brugerens tekst i det skjulte.
state <- cart_add_rows(state, cart_rows("æbler", 1, "kg", "frugt og grønt"))
view <- cart_view(state)
apple_rows <- view[view$Indkobsliste == "æbler", , drop = FALSE]
stopifnot(nrow(apple_rows) == 2)
stopifnot(any(apple_rows$display == "Husk 3 kg økologiske æbler"))
stopifnot(any(!apple_rows$locked & apple_rows$maengde == 1))

# En slettet linje bliver ikke genskabt, når en anden vare tilføjes.
milk_id <- view$line_id[view$Indkobsliste == "mælk"][[1]]
state <- cart_delete_line(state, milk_id)
state <- cart_add_rows(state, cart_rows("brød", 1, "stk", "konserves"))
view <- cart_view(state)
stopifnot(!"mælk" %in% view$Indkobsliste)

# Bevidst gentilføjelse efter sletning opretter en ny linje.
state <- cart_add_rows(state, cart_rows("mælk", 2, "liter", "mejeri"))
view <- cart_view(state)
stopifnot(sum(view$Indkobsliste == "mælk") == 1)
stopifnot(view$line_id[view$Indkobsliste == "mælk"] != milk_id)

# Ukendte/stale klik-id'er er sikre no-ops.
rows_before_stale_click <- state$rows
state <- cart_delete_line(state, "cart_999999")
state <- cart_edit_line(state, NULL, "ignoreres")
stopifnot(identical(state$rows, rows_before_stale_click))

# Tom fri tekst afvises, så der ikke opstår en usynlig, låst række.
first_line_id <- cart_view(state)$line_id[[1]]
rows_before_blank_edit <- state$rows
state <- cart_edit_line(state, first_line_id, "   ")
stopifnot(identical(state$rows, rows_before_blank_edit))

# Ufuldstændige inputrækker afvises tydeligt.
missing_column_error <- tryCatch(
  {
    cart_add_rows(state, data.frame(Indkobsliste = "ufuldstændig"))
    FALSE
  },
  error = function(e) TRUE
)
stopifnot(missing_column_error)

# Recipe-noter og ingredienser tilføjes i samme state-opdatering. Noterne er
# kopitekst til madlavning, ikke en konkurrerende struktureret vare-state.
recipe_state <- new_cart_state()
recipe_section <- list(
  title = "Testret",
  pers = 2,
  df = bind_rows(
    cart_rows("tomater", 2, "stk", "frugt og grønt"),
    cart_rows("løg", 1, "stk", "frugt og grønt")
  ),
  link = "https://example.com"
)
recipe_state <- cart_add_recipe(
  recipe_state,
  recipe_section$df,
  recipe_sections = list(recipe_section)
)
payload <- cart_copy_payload(recipe_state)
stopifnot(payload$n_visible == 2)
stopifnot(any(grepl("Testret", payload$hidden, fixed = TRUE)))
stopifnot(any(payload$hidden == "Link: https://example.com"))
stopifnot(!"df" %in% names(recipe_state$recipe_notes[[1]]))

# Redigering/sletning ændrer vare-state, mens de oprindelige opskriftsnoter
# bevares, så man stadig kan se madlavningsmængderne i kopiteksten.
recipe_view <- cart_view(recipe_state)
tomato_id <- recipe_view$line_id[recipe_view$Indkobsliste == "tomater"][[1]]
onion_id <- recipe_view$line_id[recipe_view$Indkobsliste == "løg"][[1]]
recipe_state <- cart_edit_line(recipe_state, tomato_id, "Tomater har vi allerede")
payload <- cart_copy_payload(recipe_state)
stopifnot(any(payload$visible == "Tomater har vi allerede"))
stopifnot(any(payload$hidden == "2 stk tomater"))

recipe_state <- cart_delete_line(recipe_state, tomato_id)
payload <- cart_copy_payload(recipe_state)
stopifnot(payload$n_visible == 1)
stopifnot(any(payload$hidden == "2 stk tomater"))

# Når hele indkøbssedlen er tom, er både copy- og save-payload tomme.
recipe_state <- cart_delete_line(recipe_state, onion_id)
payload <- cart_copy_payload(recipe_state)
stopifnot(payload$n_visible == 0, length(payload$hidden) == 0)

message("Alle cart-state tests bestod.")
