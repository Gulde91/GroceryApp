suppressPackageStartupMessages(source("app.R", encoding = "UTF-8"))

indkobsseddel_ns <- shiny::NS("indkobsseddel")

set_indkobsseddel_inputs <- function(session, ...) {
  values <- list(...)
  names(values) <- indkobsseddel_ns(names(values))
  do.call(session$setInputs, values)
}

shiny::testServer(server, {
  set_indkobsseddel_inputs(
    session,
    recipe_name = "",
    recipe_persons = 2,
    salad_name = "",
    accessory_name = ""
  )

  # Klik-eventet og det nye ret-valg leveres i samme input-batch med klik først.
  # Tilføjelsen må derfor ikke afhænge af, om en separat preview-observer når
  # at opdatere en cache før add-handleren.
  set_indkobsseddel_inputs(
    session,
    add_recipe = 1,
    recipe_name = "Burger"
  )

  state <- indkobsseddel_api$cart_current()
  payload <- cart_copy_payload(state)

  stopifnot(nrow(state$rows) == 11)
  stopifnot(payload$n_visible == 11)
  stopifnot(length(state$recipe_notes) == 1)
  stopifnot(state$recipe_notes[[1]]$title == "Burger")
  stopifnot(state$recipe_notes[[1]]$pers == 2)

  # Preview og opskriftsnote skal bruge den samme sikre standardværdi.
  set_indkobsseddel_inputs(session, recipe_persons = NULL)
  set_indkobsseddel_inputs(session, add_recipe = 2)
  state_after_fallback <- indkobsseddel_api$cart_current()
  stopifnot(length(state_after_fallback$recipe_notes) == 2)
  stopifnot(state_after_fallback$recipe_notes[[2]]$pers == 2)
})

message("Burger ved standardværdien 2 personer blev tilføjet uden preview-race.")
