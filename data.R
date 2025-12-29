
## RETTER ----
retter <- readr::read_delim(
  "./data/retter.txt", col_types = c("c", "c", "c"),
  delim = ";", escape_double = FALSE, trim_ws = TRUE
) %>% arrange()


# alle retter ----
sti <- "./data/opskrifter/"
filer <- list.files(sti)

opskrifter <- lapply(filer, function(x) {
  readr::read_delim(
    paste0(sti, x), col_types = c("c", "d", "c", "c", "c"),
    delim = ";", escape_double = FALSE, trim_ws = TRUE, 
  )
})

names(opskrifter) <- gsub("\\.txt", "", filer)


# TILBEHØR ----
tilbehor <- tibble::tribble(
  ~"Indkobsliste", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "kartofler", 0.25, "kg", "frugt og gr\u00F8nt", "",
  "rodfrugter", 150, "gram", "frugt og gr\u00F8nt", "",
  "sweet potato fries", 0.25, "kg", "frost", "",
  "guler\u00F8dder", 100, "gram", "frugt og gr\u00F8nt", "",
  "curly fries", 0.25, "kg", "frost", "",
  "r\u00F6sti", 0.25, "kg", "frost", "",
  "hvidl\u00F8gsflute", 0.33, "stk", "frost", "",
  "gitterfritter", 100, "gram", "frost", ""
)


# SALATER ----
salater <- tibble::tribble(
  ~retter, ~key, ~type,
  "", "", "",
  "Revet guler\u00F8dder", "revet_gulerodder_opskr", "vegetar",
  "Broccoli salat", "broccoli_salat_opskr", "vegetar",
  "Spidsk\u00E5lsalat med agurk og edamameb\u00F8nner", "spidskaal_agurk_opskr", "vegetar",
  "Hytteostsalat", "hytteost_salat_opskr", "vegetar",
  "Broccoli", "broccoli_opskr", "",
  "B\u00F8nnesalat", "boenne_salat_opskr", "vegetar"
  ) %>% arrange(retter)

# revet gulerødder ----
revet_gulerodder_opskr <- tibble::tribble(
  ~"Revet guler\u00F8dder", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "guler\u00F8dder", 75, "gram", "frugt og gr\u00F8nt", "",
  "rosiner", 10, "gram", "konserves", "",
  "citronsaft (tilbeh\u00F8r)", NA, "", "konserves", ""
)

# broccolisalat ----
broccoli_salat_opskr <- tibble::tribble(
  ~"Broccolisalat", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "broccoli", 0.25, "stk", "frugt og gr\u00F8nt", "",
  "granat\u00E6bler", 0.125, "stk", "frugt og gr\u00F8nt", "",
  "solsikkekerner", 5, "gram", "konserves", "",
  "t\u00F8rrede traneb\u00E6r", 10, "gram", "konserves", "",
  "creme fraiche", 0.25, "dl", "mejeri", "m\u00E6lk",
  "mayonnaise", 10, "gram", "konserves", "",
  "sukker", 5, "gram", "konserves", ""
)
# boennesalat ----
boenne_salat_opskr <- tibble::tribble(
  ~"B\u00F8nnesalat", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "gr\u00F8nne b\u00F8nner (frost)", 75, "gram", "frost", "",
  "solt\u00F8rrede tomater i tern", 12.5, "gram", "konserves", "",
  "fetaost", 25, "gram", "mejeri", "",
  "br\u00F8dcroutoner", 10, "gram", "konserves", ""
)
# spidskål agurk salat ----
spidskaal_agurk_opskr <- tibble::tribble(
  ~"Spidsk\u00E5lsalat med agurk og edamameb\u00F8nner", 
  ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "spidsk\u00E5l", 0.15, "stk", "frugt og gr\u00F8nt", "",
  "agurk", 0.25, "stk", "frugt og gr\u00F8nt", "",
  "edamameb\u00F8nner (frost)", 25, "gram", "frost", ""
)

# hytteost salat ----
hytteost_salat_opskr <- tibble::tribble(
  ~"Hytteostsalat", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "spidsk\u00E5l", 1/8, "stk", "frugt og gr\u00F8nt", "",
  "blomk\u00E5l", 0.13, "stk", "frugt og gr\u00F8nt", "",
  "edamameb\u00F8nner (frost)", 25, "gram", "frost", "",
  "\u00E6bler", 0.25, "stk", "frugt og gr\u00F8nt", "",
  "hytteost", 50, "gram", "mejeri", "ost"
)

# broccoli ----
broccoli_opskr <- tibble::tribble(
  ~"Broccoli", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "broccoli", 0.25, "stk", "frugt og gr\u00F8nt", "",
)

# alle salater ----
salater_opskrifter <- list(
  revet_gulerodder_opskr = revet_gulerodder_opskr,
  broccoli_salat_opskr = broccoli_salat_opskr,
  spidskaal_agurk_opskr = spidskaal_agurk_opskr,
  hytteost_salat_opskr = hytteost_salat_opskr,
  broccoli_opskr = broccoli_opskr,
  boenne_salat_opskr = boenne_salat_opskr
)

# opskrifter ----
opskrift_df <- c(opskrifter, salater_opskrifter) |> 
  lapply(function(x) {names(x)[1] <- "Indkobsliste"; return(x)}) |> 
  dplyr::bind_rows() |> 
  dplyr::arrange(Indkobsliste) |> 
  dplyr::mutate(maengde = 1) |> 
  dplyr::distinct()

# links ----
links <- readr::read_delim(
  "./data/links.txt", col_types = c("c", "c"),
  delim = ";", escape_double = FALSE, trim_ws = TRUE 
)

# kategorier ----
kategori_1 <- map_df(opskrifter, ~select(.x, kat_1))$kat_1 |> unique() |> sort()
kategori_2 <- map_df(opskrifter, ~select(.x, kat_2))$kat_2 |> unique() |> sort()
