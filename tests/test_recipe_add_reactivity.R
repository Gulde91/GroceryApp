suppressPackageStartupMessages(source("app.R", encoding = "UTF-8"))

shiny::testServer(server, {
  session$setInputs(
    ret = "",
    pers = 2,
    salat = "",
    tilbehor = "",
    basis_varer = "agurk",
    basis_varer_manuel = "agurk",
    menu_type = "Alle",
    date_from = Sys.Date() - 30,
    date_to = Sys.Date(),
    top_n = 5
  )

  # Klik-eventet og det nye ret-valg leveres i samme input-batch med klik først.
  # Tilføjelsen må derfor ikke afhænge af, om en separat preview-observer når
  # at opdatere en cache før add-handleren.
  session$setInputs(add_opskrift = 1, ret = "Burger")

  state <- rv_cart()
  payload <- cart_copy_payload(state)

  stopifnot(nrow(state$rows) == 11)
  stopifnot(payload$n_visible == 11)
  stopifnot(length(state$recipe_notes) == 1)
  stopifnot(state$recipe_notes[[1]]$title == "Burger")
  stopifnot(state$recipe_notes[[1]]$pers == 2)

  # Preview og opskriftsnote skal bruge den samme sikre standardværdi.
  session$setInputs(pers = NULL)
  session$setInputs(add_opskrift = 2)
  stopifnot(length(rv_cart()$recipe_notes) == 2)
  stopifnot(rv_cart()$recipe_notes[[2]]$pers == 2)
})

message("Burger ved standardværdien 2 personer blev tilføjet uden preview-race.")
