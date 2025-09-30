
# RETTER ----
retter <- tibble::tribble(
  ~retter, ~key, ~type,
  "M\u00F8rbradgryde", "morbrad_opskr", "gris", 
  "Carbonara", "carbonara_opskr", "gris",
  "Spaghetti k\u00F8dsovs", "spaghetti_kodsovs_opskr", "okse",
  "Fiskefilet", "fiskefilet_opskr", "fisk",
  "Pizza surdej", "pizza_surdej_opskr", "vegetar|gris|okse",
  "Pizza", "pizza_opskr", "vegetar|gris|okse",
  "Burger", "burger_opskr", "okse|gris",
  "Frikadeller", "frikadeller_opskr", "gris",
  "Dahl", "dahl_opskr", "vegetar",
  "Madpandekager", "madpandekager_opskr", "kylling", 
  "Ravioli", "ravioli_opskr", "vegetar",
  #"Bacon kyllinge t\u00E6rte", "bacon_kylling_taerte_opskr", "gris",
  "Kyllingebowl", "kyllingebowl_opskr", "kylling",
  "Lasagne", "lasagne_opskr", "kylling",
  "Lasagne okse", "lasagne_okse_opskr", "okse",
  "Kalkunschnitzel", "kalkunschnitzel_opskr", "kalkun",
  "Risengr\u00F8d", "risengrod_opskr", "vegetar",
  "Madpangekager p\u00E5 panden", "madpangekager_paa_panden_opskr", "kylling",
  "Chili con carne", "chili_con_carne_opskr", "okse",
  "Br\u00E6ndende k\u00E6rlighed", "braendende_kaerlighed_opskr", "gris",
  "Kartoffel-porre suppe", "kartoffel_porre_suppe_opskr", "gris",
  "Kylling kiev", "kylling_kiev_opskr", "kylling",
  "Kyllingespyd", "kyllingespyd_opsk", "kylling",
  "Pitabr\u00F8d", "pitabrod_opskr", "vegetar|kylling|gris",
  "Pulled chicken", "pulledchicken_opskr", "kylling",
  #"Pulled pork", "pulledpork_opskr", "gris",
  "Gullasch", "gullasch_opskr", "okse",
  "Chicken Tikka Masala", "chickentikka_opskr", "kylling",
  "Fyldte peberfrugter", "fyldtepeberfrugter_opskr", "okse",
  "Gnocchi med kylling og chorizo", "gnocchi_med_kylling_og_chorizo_opskr", "kylling",
  "grillet medister", "grillet_medister_opsk", "gris"
  ) %>% arrange(retter)

retter$count <- 1


# gnocchi med kylling og chorizo ----
gnocchi_med_kylling_og_chorizo_opskr <- tibble::tribble(
  ~"Gnocchi med kylling og chorizo", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "chorizo", 50, "gram", "konserves", "",
  "l\u00F8g", 0.25, "stk", "frugt og gr\u00F8nt", "",
  "kylling", 0.1, "kg", "k\u00F8d", "",
  "hvidl\u00F8gsfed", 0.75, "stk", "frugt og gr\u00F8nt", "",
  "solt\u00F8rrede tomater", 0.75, "spsk", "konserves", "tomat",
  "basilikum", NA, "", "frugt og gr\u00F8nt", "",
  "fennikelfr\u00F8", 0.25, "tsk", "konserves", "",
  "squash", 75, "gram", "frugt og gr\u00F8nt", "",
  "bladselleri", NA, "", "frugt og gr\u00F8nt", "",
  "gr\u00F8ntsagsbouillon", 0.25, "stk", "konserves", "krydderi",
  "hakkede tomater", 0.25, "d\u00E5se(r)", "konserves", "tomat",
  "gnocchi", 125, "gram", "konserves", "",
  "frisk mozzarella", 0.25, "stk", "mejeri", "ost",
  "parmesan", 12.5, "gram", "mejeri", "ost",
  "olivenolie", NA, "spsk", "konserves", "",
  "salt", NA, "tsk", "konserves", "krydderi",
  "peber", NA, "tsk", "konserves", "krydderi"
)

# carbonara ----
grillet_medister_opsk <- tibble::tribble(
  ~"Grillet medister", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "medisterp\u00F8lse", 100, "gram", "k\u00F8d", "",
  "kartofler", 0.25, "kg", "frugt og gr\u00F8nt", "",
  "spidsk\u00E5l", 0.25, "stk", "frugt og gr\u00F8nt", "",
  "\u00E6rter", 37, "gram", "frugt og gr\u00F8nt", "",
  "persille", NA, "", "frugt og gr\u00F8nt", "",
  "creme fraiche", 0.25, "dl", "mejeri", "m\u00E6lk",
  "mayonnaise", 20, "gram", "konserves", "dressing",
  "sennep", 20, "gram", "konserves", "dressing",
  "sukker", 5, "gram", "konserves", "",
  "purl\u00F8g", NA, "", "frugt og gr\u00F8nt", "",
  "salt", NA, "tsk", "konserves", "krydderi",
  "peber", NA, "tsk", "konserves", "krydderi"
  
)

# mørbradgryde ----
morbrad_opskr <- tibble::tribble(
  ~"M\u00F8rbradgryde", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "svinem\u00F8rbrad", 0.25, "stk", "k\u00F8d", "",
  "cocktailp\u00F8lser", 50, "gram", "k\u00F8d", "p\u00E5l\u00E6g",
  "bacon i skiver", 40, "gram", "k\u00F8d", "p\u00E5l\u00E6g",
  "l\u00F8g", 0.5, "stk", "frugt og gr\u00F8nt", "",
  "tomatpure", 50, "gram", "konserves", "tomat",
  "gr\u00F8ntsagsbouillon", 0.5, "stk", "konserves", "krydderi",
  "oksebouillon", 0.25, "stk", "konserves", "krydderi",
  "madlavningsfl\u00F8de", 0.1, "liter", "mejeri", "m\u00E6lk",
  "ribsgele (tilsmagning)", NA, "", "konserves", "",
  "paprika", NA, "tsk", "konserves", "krydderi",
  "engelsk sovs (tilsmagning)", NA, "", "konserves", ""
)

# carbonara ----
carbonara_opskr <- tibble::tribble(
  ~"Carbonara", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "spaghetti", 75, "gram", "konserves", "",
  "parmesan", 25, "gram", "mejeri", "ost",
  "\u00E6ggeblommer", 1, "stk", "mejeri", "",
  "piskefl\u00F8de", 0.5, "dl", "mejeri", "m\u00E6lk",
  "l\u00F8g", 0.5, "stk", "frugt og gr\u00F8nt", "",
  "bacon i tern", 100, "gram", "k\u00F8d", "p\u00E5l\u00E6g"
)

# spaghetti kødsovs ----
spaghetti_kodsovs_opskr <- tibble::tribble(
  ~"Spaghetti k\u00F8dsovs", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "spaghetti", 80, "gram", "konserves", "",
  "l\u00F8g", 0.75, "stk", "frugt og gr\u00F8nt", "",
  "hvidl\u00F8gsfed", 0.5, "stk", "frugt og gr\u00F8nt", "",
  "hakket oksek\u00F8d", 0.15, "kg", "k\u00F8d", "",
  "hakkede tomater", 0.25, "d\u00E5se(r)", "konserves", "tomat",
  "guler\u00F8dder", 20, "gram", "frugt og gr\u00F8nt", "",
  "oregano", NA, "tsk", "konserves", "krydderi",
  "salt", NA, "tsk", "konserves", "krydderi",
  "peber", NA, "tsk", "konserves", "krydderi",
  "parmasam", 25, "gram", "mejeri", "ost"
)


# fiskefilet ----
fiskefilet_opskr <- tibble::tribble(
  ~"Fiskefilet", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "fiskefilet", 1, "stk", "frost", "",
  "remoulade (tilbeh\u00F8r)", NA, "", "konserves", "dressing",
  "citronsaft (tilbeh\u00F8r)", NA, "", "konserves", ""
)

# pizza surdej ----
pizza_surdej_opskr <- tibble::tribble(
  ~"Pizza surdej", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "tipo 00 mel", 125, "gram", "konserves", "mel",
  "durummel", 25, "gram", "konserves", "mel",
  "l\u00F8g", 0.25, "stk", "frugt og gr\u00F8nt", "",
  "hvidl\u00F8gsfed", 1, "stk", "frugt og gr\u00F8nt", "",
  "hvidvinseddike", 1, "spsk", "konserves", "",
  "tomatpure", 100, "gram", "konserves", "tomat",
  "hakkede tomater", 0.5, "d\u00E5se(r)", "konserves", "tomat",
  "engelsk sovs (tilsmagning)", NA, "", "konserves", "",
  "frisk mozzarella", 1, "stk", "mejeri", "ost"
)

# pizza ----
pizza_opskr <- tibble::tribble(
  ~"Pizza", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "tomatpure", 50, "gram", "konserves", "tomat",
  "pizza sauce", 1, "stk", "konserves", "tomat",
  "frisk mozzarella", 1, "stk", "mejeri", "ost",
  "pizzabund", 1, "stk", "k\u00F8l", ""
)


# burger ----
burger_opskr <- tibble::tribble(
  ~"Burger", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "hakket oksek\u00F8d", 0.175, "kg", "k\u00F8d", "",
  "bacon i skiver", 50, "gram", "k\u00F8d", "p\u00E5l\u00E6g",
  "brioche bolle", 1, "stk", "konserves", "br\u00F8d",
  "agurk", 0.2, "stk", "frugt og gr\u00F8nt", "",
  "iceberg", 0.1, "stk", "frugt og gr\u00F8nt", "",
  "toastost", 2, "skiver", "mejeri", "ost",
  "ketchup", NA, "", "konserves", "",
  "remoulade (tilbeh\u00F8r)", NA, "", "konserves", "dressing",
  "ristet l\u00F8g (tilbeh\u00F8r)", NA, "", "konserves", "",
  "BBQ dressing (tilsmagning)", NA, "", "konserves", "dressing",
  "eddike (tilsmagning)", NA, "", "konserves", ""
)

# frikadeller ----
frikadeller_opskr <- tibble::tribble(
  ~"Frikadeller", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "hakket svinek\u00F8d", 166.6, "gram", "k\u00F8d", "",
  "l\u00F8g", 0.33, "stk", "frugt og gr\u00F8nt", "",
  "hvedemel", 10, "gram", "konserves", "mel",
  "havregryn", 10, "gram", "konserves", "mel",
  "salt", NA, "tsk", "konserves", "krydderi",
  "\u00E6g", 0.33, "stk", "mejeri", "",
  "skummetm\u00E6lk", 0.066, "liter", "mejeri", "m\u00E6lk"
)

# dahl ----
dahl_opskr <- tibble::tribble(
  ~"Dahl", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "hvidl\u00F8gsfed", 1.25, "stk", "frugt og gr\u00F8nt", "",
  "ingef\u00E6r", 30, "gram", "frugt og gr\u00F8nt", "",
  "karry (tilsmagning)", NA, "", "konserves", "krydderi",
  "spidskommen (tilsmagning)", NA, "", "konserves", "krydderi",
  "koriander (tilsmagning)", NA, "", "konserves", "krydderi",
  "kardemomme (tilsmagning)", NA, "", "konserves", "krydderi",
  "chiliflager", NA, "tsk", "konserves", "krydderi",
  "gr\u00F8ntsagsbouillon", 1, "stk", "konserves", "krydderi",
  "r\u00F8de linser", 50, "gram", "konserves", "",
  "hakkede tomater", 0.5, "d\u00E5se(r)",  "konserves", "tomat",
  "yoghurt naturel", 0.5, "dl", "mejeri", "m\u00E6lk",
  "agurk", 0.13, "stk", "frugt og gr\u00F8nt", "",
  "naanbr\u00F8d", 1, "stk", "konserves", "br\u00F8d",
  "peanuts", 0.5, "pose(r)", "konserves", "chips"
)

# madpandekager ----
madpandekager_opskr <- tibble::tribble(
  ~"Madpandekager", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "kylling", 0.125, "kg", "k\u00F8d", "",
  "crispy chicken tex mex", 0.5, "stk",  "konserves", "mexico",
  "madpandekager", 1, "stk",  "konserves", "mexico",
  "tacoskal", 1, "stk",  "konserves", "mexico",
  "r\u00F8d peber", 0.5, "stk", "frugt og gr\u00F8nt", "",
  "agurk", 0.25, "stk", "frugt og gr\u00F8nt", "",
  "frosne majs", 50, "gram", "frost", "",
  "iceberg", 0.2, "stk", "frugt og gr\u00F8nt", "",
  "creme fraiche dressing", 50, "gram", "konserves", "dressing"
)

# ravioli ----
ravioli_opskr <- tibble::tribble(
  ~"Ravioli", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "ravioli", 200, "gram", "k\u00F8l", "",
  "pesto", 40, "gram", "konserves", ""
)
# pulled chicken ----
pulledchicken_opskr <- tibble::tribble(
  ~"Pulled chicken", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "pulled chicken", 0.5, "stk", "k\u00F8d", "p\u00E5l\u00E6g",
  "coleslaw", 0.33, "stk", "frugt og gr\u00F8nt", "",
  "brioche bolle", 1, "stk", "konserves", "br\u00F8d",
  "mayonnaise", 20, "gram", "konserves", "dressing"
)
# pulled pork ----
pulledpork_opskr <- tibble::tribble(
  ~"Pulled pork", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "pulled pork", 0.5, "stk", "k\u00F8d", "p\u00E5l\u00E6g",
  "coleslaw", 0.33, "stk", "frugt og gr\u00F8nt", "",
  "brioche bolle", 1, "stk", "konserves", "br\u00F8d",
  "mayonnaise", 20, "gram", "konserves", "dressing"
)
# gullasch ----
gullasch_opskr <- tibble::tribble(
  ~"Gullasch", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "l\u00F8g", 0.5, "stk", "frugt og gr\u00F8nt", "",
  "hvidl\u00F8gsfed", 0.25, "stk", "frugt og gr\u00F8nt", "",
  "kartofler", 0.25, "kg", "frugt og gr\u00F8nt", "",
  "guler\u00F8dder", 25, "gram", "frugt og gr\u00F8nt", "",
  "oksebov", 125, "gram", "k\u00F8l", "",
  "paprika", NA, "tsk", "konserves", "krydderi",
  "spidskommen (tilsmagning)", NA, "", "konserves", "krydderi",
  "gr\u00F8ntsagsbouillon", NA, "", "konserves", "krydderi",
  "tomatpure", 25, "gram", "konserves", "tomat",
  "skummetm\u00E6lk", 0.38, "liter", "mejeri", "m\u00E6lk",
  "sm\u00F8r", 25, "gram", "mejeri", ""
)
# chicken tikka masala ----
chickentikka_opskr <- tibble::tribble(
  ~"Chicken Tikka Masala", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "gr\u00E6sk yoghurt", 50, "gram", "mejeri", "m\u00E6lk",
  "hvidl\u00F8gsfed", 1.5, "stk", "frugt og gr\u00F8nt", "",
  "ingef\u00E6r", 40, "gram", "frugt og gr\u00F8nt", "",
  "garam masala (tilsmagning)", NA, "", "konserves", "krydderi",
  "gurkemeje (tilsmagning)", NA, "", "konserves", "krydderi",
  "chiliflager", NA, "tsk", "konserves", "krydderi",
  "st\u00F8dt nelike (tilsmagning)", NA, "", "konserves", "krydderi",
  "limesaft (tilsmagning)", NA, "", "konserves", "",
  "kylling", 0.15, "kg", "k\u00F8d", "",
  "l\u00F8g", 0.5, "stk", "frugt og gr\u00F8nt", "",
  "tomatpure", 20, "gram", "konserves", "tomat",
  "piskefl\u00F8de", 0.5, "dl", "mejeri", "m\u00E6lk",
  "basmatiris", 50, "gram", "konserves", ""
)
# fyldte peberfrugter ----
fyldtepeberfrugter_opskr <- tibble::tribble(
  ~"Fyldte peberfrugter", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "r\u00F8d peber", 1, "stk", "frugt og gr\u00F8nt", "",
  "l\u00F8g", 0.3, "stk", "frugt og gr\u00F8nt", "",
  "paprika", NA, "tsk", "konserves", "krydderi",
  "chiliflager", NA, "tsk", "konserves", "krydderi",
  "tomatpure", 18, "gram", "konserves", "tomat",
  "hakket oksek\u00F8d", 0.09, "kg", "k\u00F8d", "",
  "revet cheddar", 15, "gram", "mejeri", "ost",
  "basmatiris", 35, "gram", "konserves", ""
)
# bacon kyllinge tærte ----
bacon_kylling_taerte_opskr <- tibble::tribble(
  ~"Bacon kyllinge t\u00E6rte", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "f\u00E6rdig t\u00E6rtedej", 0.25, "rulle(r)", "k\u00F8l", "",
  "bacon i tern", 50, "gram", "k\u00F8d", "p\u00E5l\u00E6g",
  "for\u00E5rsl\u00F8g", 0.25, "bundt", "frugt og gr\u00F8nt", "",
  "frosne \u00E6rter", 50, "gram", "frost", "",
  "karry (tilsmagning)", NA, "", "konserves", "krydderi",
  "chiliflager", NA, "tsk", "konserves", "krydderi",
  "\u00E6g", 1.25, "stk", "mejeri", "",
  "creme fraiche", 0.63, "dl", "mejeri", "m\u00E6lk",
  "revet mozzarella", 18.75, "gram", "mejeri", "ost"
)

# kyllingebowl ----
kyllingebowl_opskr <- tibble::tribble(
  ~"Kyllingebowl", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "r\u00F8d peber", 1, "stk", "frugt og gr\u00F8nt", "",
  "r\u00F8dl\u00F8g", 0.5, "stk", "frugt og gr\u00F8nt", "",
  "frosne majs", 50, "gram", "frost", "",
  "hvidl\u00F8gsfed", 1, "stk", "frugt og gr\u00F8nt", "",
  "basmatiris", 50, "gram", "konserves", "",
  "kylling", 0.125, "kg", "k\u00F8d", "",
  "lime", 0.25, "stk", "frugt og gr\u00F8nt", "",
  "creme fraiche", 1.2, "dl", "mejeri", "m\u00E6lk",
  "fajita tex mex", 10, "gram", "konserves", "mexico"
)

# lasagne ----
lasagne_opskr <- tibble::tribble(
  ~"Lasagne", ~"maengde", ~"enhed",  ~"kat_1", ~"kat_2",
  "kylling", 0.15, "kg", "k\u00F8d", "",
  "hakkede tomater", 0.25, "d\u00E5se(r)", "konserves", "tomat",
  "l\u00F8g", 0.25, "stk", "frugt og gr\u00F8nt", "",
  "piskefl\u00F8de", 0.63, "dl", "mejeri", "m\u00E6lk",
  "guler\u00F8dder", 50, "gram", "frugt og gr\u00F8nt", "",
  "r\u00F8d peber", 0.5, "stk", "frugt og gr\u00F8nt", "",
  "ketchup", NA, "", "konserves", "",
  "lasagneplader", 2.25, "stk", "konserves", "",
  "revet mozzarella", 50, "gram", "mejeri", "ost",
  "paprika", NA, "tsk", "konserves", "krydderi",
  "oregano", NA, "tsk", "konserves", "krydderi"
)

# lasagne med oksekød ----
lasagne_okse_opskr <- tibble::tribble(
  ~"Lasagne okse", ~"maengde", ~"enhed",  ~"kat_1", ~"kat_2",
  "hakket oksek\u00F8d", 0.1, "kg", "k\u00F8d", "",
  "l\u00F8g", 0.5, "stk", "frugt og gr\u00F8nt", "",
  "hvidl\u00F8gsfed", 1, "stk", "frugt og gr\u00F8nt", "",
  "oregano", 0.5, "tsk", "konserves", "krydderi",
  "guler\u00F8dder", 50, "gram", "frugt og gr\u00F8nt", "",
  "tomatpure", 25, "gram", "konserves", "tomat",
  "hakkede tomater", 0.5, "d\u00E5se(r)", "konserves", "tomat",
  "gr\u00F8ntsagsbouillon", 0.25, "stk", "konserves", "krydderi",
  "olivenolie", 0.5, "spsk", "konserves", "",
  "sm\u00F8r", 10, "gram", "mejeri", "",
  "hvedemel", 10, "gram", "konserves", "mel",
  "skummetm\u00E6lk", 0.075, "liter", "mejeri", "m\u00E6lk",
  "muskatn\u00F8d", 0.1, "tsk", "konserves", "krydderi",
  "frisk mozzarella", 0.5, "stk", "mejeri", "ost",
  "lasagneplader", 2.25, "stk", "konserves", ""
)


# kalkunschnitzel ----
kalkunschnitzel_opskr <- tibble::tribble(
  ~"Kalkunschnitzel", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "kalkun", 150, "gram", "k\u00F8d", ""
)

# risengrød ----
risengrod_opskr <- tibble::tribble(
  ~"Risengr\u00F8d", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "gr\u00F8dris", 90, "gram", "konserves", "",
  "skummetm\u00E6lk", 0.33, "liter", "mejeri", "m\u00E6lk",
  "sukker", 25, "gram", "konserves", "",
  "kanel (tilsmagning)", NA, "", "konserves", "krydderi",
  "hvidt\u00F8l", 0.25, "liter", "konserves", ""
)

# madpandekager på panden ----
madpangekager_paa_panden_opskr <- tibble::tribble(
  ~"Madpangekager p\u00E5 panden", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "l\u00F8g", 2, "stk", "frugt og gr\u00F8nt", "",
  "kylling", 0.15, "kg", "k\u00F8d", "",
  "r\u00F8d peber", 0.75, "stk", "frugt og gr\u00F8nt", "",
  "revet mozzarella", 50, "gram", "mejeri", "ost",
  "madpandekager", 2, "stk", "konserves", "mexico",
  "salsa", 0.5, "d\u00E5se(r)", "konserves", "mexico"
)

# chili con carne -----
chili_con_carne_opskr <- tibble::tribble(
  ~"Chili con carne", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "spidskommen (tilsmagning)", NA, "", "konserves", "krydderi",
  "kanel (tilsmagning)", NA, "", "konserves", "krydderi",
  "chiliflager", NA, "tsk", "konserves", "krydderi",
  "hvidl\u00F8gsfed", 0.5, "stk", "frugt og gr\u00F8nt", "",
  "l\u00F8g", 0.5, "stk", "frugt og gr\u00F8nt", "",
  "hakket oksek\u00F8d", 0.0875, "kg", "k\u00F8d", "",
  "hakkede tomater", 0.25, "d\u00E5se(r)", "konserves", "tomat",
  "solt\u00F8rrede tomater", 15, "gram", "konserves", "tomat",
  "oksebouillon", 0.25, "stk", "konserves", "krydderi",
  "kidneyb\u00F8nner", 0.2, "d\u00E5se(r)", "konserves", "", 
  "m\u00F8rk chokolade", 10, "gram", "slik", "",
  "olivenolie", NA, "spsk", "konserves", "",
  "nachos", 31.25, "gram", "chips", "",
  "basmatiris", 0.50, "gram", "konserves", "",
  "creme fraiche", 0.5, "dl", "mejeri", "m\u00E6lk"
)

# kyllingespyd ----
kyllingespyd_opsk <- tibble::tribble(
  ~"Kyllingespyd", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "kylling", 0.15, "kg", "k\u00F8d", "",
  "r\u00F8dl\u00F8g", 0.25, "stk", "frugt og gr\u00F8nt", "",
  "r\u00F8d peber", 0.5, "stk", "frugt og gr\u00F8nt", "",
  "olivenolie", 1, "spsk", "konserves", "",
  "tomatpure", 50, "gram", "konserves", "tomat",
  "hvidvinseddike", 0.75, "spsk", "konserves", "",
  "brun farin", 0.5, "spsk", "konserves", "",
  "paprika", 2, "tsk", "konserves", "krydderi",
  "hvidl\u00F8gspulver", 2, "tsk", "konserves", "krydderi",
  "oregano", 2, "tsk", "konserves", "krydderi",
  "chiliflager", 0.1, "tsk", "konserves", "krydderi",
  "salt", 0.25, "tsk", "konserves", "krydderi",
  "peber", 0.25, "tsk", "konserves", "krydderi"
)


# brændende kærlighed ----
braendende_kaerlighed_opskr <- tibble::tribble(
  ~"Br\u00E6ndende k\u00E6rlighed", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "kartofler", 0.38, "kg", "frugt og gr\u00F8nt", "",
  "sm\u00F8r", 37.5, "gram", "mejeri", "", 
  "skummetm\u00E6lk", 0.025, "liter", "mejeri", "m\u00E6lk",
  "bacon i skiver", 125, "gram", "k\u00F8d", "p\u00E5l\u00E6g",
  "l\u00F8g", 1, "stk", "frugt og gr\u00F8nt", "",
  "r\u00F8dbeder (tilbeh\u00F8r)", NA, "", "konserves", "",
  "persille", 20, "gram", "frugt og gr\u00F8nt", "",
)

# kartoffel porre suppe ----
kartoffel_porre_suppe_opskr <- tibble::tribble(
  ~"Kartoffel-porre suppe", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "sm\u00F8r", 2.5, "gram", "mejeri", "",
  "l\u00F8g", 0.25, "stk", "frugt og gr\u00F8nt", "",
  "hvidl\u00F8gsfed", 0.5, "stk",  "frugt og gr\u00F8nt", "",
  "kartofler", 0.125, "kg", "frugt og gr\u00F8nt", "",
  "porre", 0.5, "stk", "frugt og gr\u00F8nt", "",
  "h\u00F8nsefondbouillon", 2, "stk", "konserves", "krydderi",
  "piskefl\u00F8de", 0.25, "dl", "mejeri", "m\u00E6lk",
  "bacon i skiver", 37.5, "gram", "k\u00F8d", "p\u00E5l\u00E6g",
  "br\u00F8d (tilbeh\u00F8r)", NA, "", "konserves", "br\u00F8d",
)

# kylling kiev ----
kylling_kiev_opskr <- tibble::tribble(
  ~"Kylling kiev", ~"maengde",  ~"enhed", ~"kat_1",  ~"kat_2",
  "kyllingebrystfilet", 1, "stk", "k\u00F8d", "",
  "spinat", 30, "gram",  "frugt og gr\u00F8nt", "",
  "citron",  0.5, "stk", "frugt og gr\u00F8nt", "",
  "persille", 20, "gram", "frugt og gr\u00F8nt", "",
  "r\u00F8dl\u00F8g", 0.5, "stk", "frugt og gr\u00F8nt", "",
  "hvidl\u00F8gsfed", 0.5, "stk",  "frugt og gr\u00F8nt", "",
  "panko", 15, "gram", "konserves", "",
  "aioli", 25, "gram", "konserves", "dressing",
  "\u00E6g", 0.5, "stk", "mejeri", "",
  "sm\u00F8r", 30, "gram", "mejeri", ""
)



# pitabrød ----
pitabrod_opskr <- tibble::tribble(
  ~"Pitabr\u00F8d", ~"maengde", ~"enhed", ~"kat_1", ~"kat_2",
  "Pitabr\u00F8d", 2, "stk",  "frost", "",
  "kylling", 0.08, "kg", "k\u00F8d", "",
  "pepperoni", 60, "gram", "k\u00F8d", "p\u00E5l\u00E6g",
  "bacon i skiver", 60, "gram", "k\u00F8d", "p\u00E5l\u00E6g",
  "cocktailp\u00F8lser", 40, "gram", "k\u00F8d", "p\u00E5l\u00E6g",
  "r\u00F8d peber", 0.4, "stk", "frugt og gr\u00F8nt", "",
  "agurk", 0.2, "stk", "frugt og gr\u00F8nt", "",
  "frosne majs", 40, "gram", "frost", "",
  "iceberg", 0.1, "stk", "frugt og gr\u00F8nt", "",
  "creme fraiche dressing", 50, "gram", "konserves", "dressing"
)


# alle retter ----
opskrifter <- list(morbrad_opskr = morbrad_opskr,
                   carbonara_opskr = carbonara_opskr,
                   spaghetti_kodsovs_opskr = spaghetti_kodsovs_opskr,
                   fiskefilet_opskr = fiskefilet_opskr,
                   pizza_surdej_opskr = pizza_surdej_opskr,
                   pizza_opskr = pizza_opskr,
                   burger_opskr = burger_opskr,
                   frikadeller_opskr = frikadeller_opskr,
                   dahl_opskr = dahl_opskr,
                   madpandekager_opskr = madpandekager_opskr,
                   ravioli_opskr = ravioli_opskr,
                   bacon_kylling_taerte_opskr = bacon_kylling_taerte_opskr,
                   kyllingebowl_opskr = kyllingebowl_opskr,
                   lasagne_opskr = lasagne_opskr,
                   lasagne_okse_opskr = lasagne_okse_opskr,
                   kalkunschnitzel_opskr = kalkunschnitzel_opskr,
                   kyllingespyd_opsk = kyllingespyd_opsk,
                   risengrod_opskr = risengrod_opskr,
                   madpangekager_paa_panden_opskr = madpangekager_paa_panden_opskr,
                   chili_con_carne_opskr = chili_con_carne_opskr,
                   braendende_kaerlighed_opskr = braendende_kaerlighed_opskr,
                   kartoffel_porre_suppe_opskr = kartoffel_porre_suppe_opskr,
                   kylling_kiev_opskr = kylling_kiev_opskr,
                   pitabrod_opskr = pitabrod_opskr,
                   pulledchicken_opskr = pulledchicken_opskr,
                   pulledpork_opskr = pulledpork_opskr,
                   gullasch_opskr = gullasch_opskr,
                   chickentikka_opskr = chickentikka_opskr,
                   fyldtepeberfrugter_opskr = fyldtepeberfrugter_opskr,
                   gnocchi_med_kylling_og_chorizo_opskr = gnocchi_med_kylling_og_chorizo_opskr,
                   grillet_medister_opsk = grillet_medister_opsk
                   )


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

# basis varer ----
varer_custom <- read.csv("./data/basis_varer.txt", fileEncoding = "UTF-8")

varer <- c(opskrifter, salater_opskrifter) |> 
  lapply(function(x) {names(x)[1] <- "Indkobsliste"; return(x)}) |> 
  dplyr::bind_rows() |> 
  dplyr::bind_rows(varer_custom) |> 
  dplyr::arrange(Indkobsliste) |> 
  dplyr::mutate(maengde = 1) |> 
  dplyr::distinct()

# links ----
links <- tibble::tribble(
  ~ret, ~link,
  "M\u00F8rbradgryde", "https://www.femina.dk/mad/aftensmad/klassisk-moerbradgryde",
  "Carbonara", "https://www.valdemarsro.dk/carbonara_opskrift/",
  "Spaghetti k\u00F8dsovs", "https://www.dk-kogebogen.dk/opskrifter/15127/spaghetti-med-koedsovs?personer=1",  
  "Pizza surdej", "https://rigeligtsmor.dk/opskrift-pizzadej-med-surdej/",
  "Kyllingebowl", "https://www.hellofresh.dk/recipes/fajita-kyllingebowl-5fa3dff00e1d695ec60e5721",
  "Dahl", "https://www.valdemarsro.dk/dhal-med-raita/",
  "Lasagne", "https://www.dk-kogebogen.dk/opskrifter/19934/kyllingelasagne-med-spinat",
  "Lasagne okse", "https://www.valdemarsro.dk/lasagne/",
  "Chili con carne", "https://www.valdemarsro.dk/chili-con-carne/",
  "Br\u00E6ndende k\u00E6rlighed", "https://www.valdemarsro.dk/braendende-kaerlighed/",
  "Kartoffel-porre suppe", "https://madensverden.dk/kartoffel-porre-suppe/",
  "Kylling kiev", "https://www.hellofresh.dk/recipes/chicken-kiev-5efed5bde9cd8816990b92d6",
  "Kyllingespyd", "https://martinys.dk/grill-kyllingespyd-med-barbeque-marinade/",
  "Frikadeller", "https://www.dropbox.com/scl/fi/kq7ejsqpcoo83r9q1pgoh/Frikadeller.txt?rlkey=44yccp720l6k7r8qkcbtpbwf9&dl=0"
)



# kategorier ----
kategori_1 <- map_df(opskrifter, ~select(.x, kat_1))$kat_1 |> unique() |> sort()
kategori_2 <- map_df(opskrifter, ~select(.x, kat_2))$kat_2 |> append(" ") |> unique() |> sort()

