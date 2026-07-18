app_expressions <- parse("app.R", encoding = "UTF-8")

is_assignment <- function(node) {
  is.call(node) &&
    is.symbol(node[[1]]) &&
    as.character(node[[1]]) %in% c("<-", "=")
}

server_assignments <- Filter(
  function(node) {
    is_assignment(node) &&
      is.symbol(node[[2]]) &&
      identical(as.character(node[[2]]), "server")
  },
  as.list(app_expressions)
)

stopifnot(length(server_assignments) == 1)
server_function <- server_assignments[[1]][[3]]
stopifnot(is.call(server_function), identical(as.character(server_function[[1]]), "function"))

call_name <- function(node) {
  if (!is.call(node)) return("")

  head <- node[[1]]
  if (is.symbol(head)) return(as.character(head))

  if (
    is.call(head) &&
      is.symbol(head[[1]]) &&
      as.character(head[[1]]) %in% c("::", ":::")
  ) {
    return(as.character(head[[3]]))
  }

  ""
}

output_assignment_name <- function(node) {
  if (!is_assignment(node)) return(NULL)

  lhs <- node[[2]]
  if (
    !is.call(lhs) ||
      !is.symbol(lhs[[1]]) ||
      !identical(as.character(lhs[[1]]), "$") ||
      !is.symbol(lhs[[2]]) ||
      !identical(as.character(lhs[[2]]), "output")
  ) {
    return(NULL)
  }

  as.character(lhs[[3]])
}

reactive_registration_calls <- c(
  "observe",
  "observeEvent",
  "reactive",
  "eventReactive",
  "reactivePoll",
  "reactiveFileReader",
  "renderUI",
  "renderDT",
  "renderText",
  "renderTable",
  "renderPlot",
  "renderWordcloud2"
)

all_output_assignments <- character()
nested_output_assignments <- character()

walk_server_ast <- function(node, inside_reactive_registration = FALSE) {
  if (!is.call(node)) return(invisible(NULL))

  output_name <- output_assignment_name(node)
  if (!is.null(output_name)) {
    all_output_assignments <<- c(all_output_assignments, output_name)
    if (inside_reactive_registration) {
      nested_output_assignments <<- c(nested_output_assignments, output_name)
    }
  }

  child_is_nested <- inside_reactive_registration ||
    call_name(node) %in% reactive_registration_calls

  if (length(node) > 1) {
    for (i in seq.int(2, length(node))) {
      walk_server_ast(node[[i]], child_is_nested)
    }
  }

  invisible(NULL)
}

walk_server_ast(server_function[[3]])

expected_top_level_outputs <- c(
  "opskrift_edit_context",
  "opskrift_add_context",
  "opskrift_delete_context",
  "ret_delete_context",
  "ret_permanent_delete_context",
  "opskrift_tbl_valgt"
)

stopifnot(length(nested_output_assignments) == 0)
stopifnot(all(expected_top_level_outputs %in% all_output_assignments))

message("Alle Shiny-outputs registreres uden for observers, reactives og render-funktioner.")
