r_file <- function(filename) {
  file.path("R", filename)
}

server_source_files <- c(
  app = "app.R",
  opskrifter = r_file("recipe_module.R"),
  varer = r_file("varer_module.R"),
  indkobsseddel = r_file("indkobsseddel_module.R"),
  inspiration = r_file("inspiration_module.R")
)

server_function_names <- c(
  app = "server",
  opskrifter = "mod_opskrifter_server",
  varer = "mod_varer_server",
  indkobsseddel = "mod_indkobsseddel_server",
  inspiration = "mod_inspiration_server"
)

is_assignment <- function(node) {
  is.call(node) &&
    is.symbol(node[[1]]) &&
    as.character(node[[1]]) %in% c("<-", "=")
}

find_server_function <- function(source_file, function_name) {
  expressions <- parse(source_file, encoding = "UTF-8")
  assignments <- Filter(
    function(node) {
      is_assignment(node) &&
        is.symbol(node[[2]]) &&
        identical(as.character(node[[2]]), function_name)
    },
    as.list(expressions)
  )

  stopifnot(length(assignments) == 1)
  server_function <- assignments[[1]][[3]]
  stopifnot(
    is.call(server_function),
    identical(as.character(server_function[[1]]), "function")
  )
  server_function
}

server_functions <- Map(
  find_server_function,
  unname(server_source_files),
  unname(server_function_names)
)

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

collect_call_nodes <- function(node) {
  if (!is.call(node)) return(list())

  children <- unlist(
    lapply(as.list(node), collect_call_nodes),
    recursive = FALSE
  )
  c(list(node), children)
}

input_reference_name <- function(node) {
  if (
    !is.call(node) ||
      !call_name(node) %in% c("$", "[[") ||
      length(node) < 3L ||
      !is.symbol(node[[2]]) ||
      !identical(as.character(node[[2]]), "input")
  ) {
    return("")
  }

  member <- node[[3]]
  if (!is.symbol(member) && !is.character(member)) return("")
  as.character(member)[[1]]
}

lhs_contains_catalog_field <- function(node) {
  if (!is.call(node)) return(FALSE)

  catalog_fields <- c(
    "catalog",
    "recipes",
    "active_retter",
    "archived_retter",
    "links",
    "maengde",
    "enhed",
    "kat_1",
    "kat_2"
  )
  own_match <- identical(call_name(node), "$") &&
    length(node) >= 3L &&
    as.character(node[[3]]) %in% catalog_fields

  own_match || any(vapply(
    as.list(node),
    lhs_contains_catalog_field,
    logical(1)
  ))
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

for (server_function in server_functions) {
  walk_server_ast(server_function[[3]])
}

count_function_definitions <- function(node) {
  if (!is.call(node)) return(0L)

  own_count <- as.integer(identical(call_name(node), "function"))
  if (length(node) == 1) return(own_count)

  own_count + sum(vapply(
    as.list(node)[-1],
    count_function_definitions,
    integer(1)
  ))
}

nested_module_functions <- vapply(
  server_functions[-1L],
  function(module_server_function) {
    count_function_definitions(module_server_function[[3]])
  },
  integer(1)
)

module_lines <- readLines(r_file("recipe_module.R"), encoding = "UTF-8")
module_expressions <- parse(r_file("recipe_module.R"), encoding = "UTF-8")
recipe_view_lines <- readLines(
  r_file("recipe_view.R"),
  encoding = "UTF-8"
)
recipe_view_expressions <- parse(
  r_file("recipe_view.R"),
  encoding = "UTF-8"
)
schema_lines <- readLines(r_file("recipe_schema.R"), encoding = "UTF-8")
schema_expressions <- parse(r_file("recipe_schema.R"), encoding = "UTF-8")
recipe_store_lines <- readLines(
  r_file("recipe_store.R"),
  encoding = "UTF-8"
)
catalog_lines <- readLines(r_file("recipe_catalog.R"), encoding = "UTF-8")
catalog_expressions <- parse(
  r_file("recipe_catalog.R"),
  encoding = "UTF-8"
)
catalog_state_lines <- readLines(
  r_file("recipe_catalog_state.R"),
  encoding = "UTF-8"
)
catalog_state_expressions <- parse(
  r_file("recipe_catalog_state.R"),
  encoding = "UTF-8"
)
basis_state_lines <- readLines(
  r_file("basis_varer_state.R"),
  encoding = "UTF-8"
)
basis_state_expressions <- parse(
  r_file("basis_varer_state.R"),
  encoding = "UTF-8"
)
history_state_lines <- readLines(
  r_file("shopping_history_state.R"),
  encoding = "UTF-8"
)
history_state_expressions <- parse(
  r_file("shopping_history_state.R"),
  encoding = "UTF-8"
)
top_level_function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  module_lines
)
all_module_function_definitions <- sum(vapply(
  as.list(module_expressions),
  count_function_definitions,
  integer(1)
))
module_function_assignments <- Filter(
  function(node) {
    is_assignment(node) &&
      is.symbol(node[[2]]) &&
      is.call(node[[3]]) &&
      identical(call_name(node[[3]]), "function")
  },
  as.list(module_expressions)
)
module_function_names <- vapply(
  module_function_assignments,
  function(node) as.character(node[[2]]),
  character(1)
)

recipe_view_top_level_function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  recipe_view_lines
)
all_recipe_view_function_definitions <- sum(vapply(
  as.list(recipe_view_expressions),
  count_function_definitions,
  integer(1)
))
recipe_view_function_assignments <- Filter(
  function(node) {
    is_assignment(node) &&
      is.symbol(node[[2]]) &&
      is.call(node[[3]]) &&
      identical(call_name(node[[3]]), "function")
  },
  as.list(recipe_view_expressions)
)
recipe_view_function_names <- vapply(
  recipe_view_function_assignments,
  function(node) as.character(node[[2]]),
  character(1)
)
recipe_view_has_roxygen_documentation <- vapply(
  recipe_view_top_level_function_lines,
  function(line_number) {
    line_number > 1L &&
      grepl("^#'", recipe_view_lines[[line_number - 1L]])
  },
  logical(1)
)
recipe_view_call_nodes <- unlist(
  lapply(as.list(recipe_view_expressions), collect_call_nodes),
  recursive = FALSE
)
recipe_view_call_names <- vapply(
  recipe_view_call_nodes,
  call_name,
  character(1)
)
recipe_view_uses_runtime_object <- vapply(
  recipe_view_call_nodes,
  function(node) {
    call_name(node) %in% c("$", "[[") &&
      length(node) >= 3L &&
      is.symbol(node[[2]]) &&
      as.character(node[[2]]) %in%
        c("input", "output", "session", "catalog_read")
  },
  logical(1)
)

schema_top_level_function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  schema_lines
)
schema_function_assignments <- Filter(
  function(node) {
    is_assignment(node) &&
      is.symbol(node[[2]]) &&
      is.call(node[[3]]) &&
      identical(call_name(node[[3]]), "function")
  },
  as.list(schema_expressions)
)
schema_function_names <- vapply(
  schema_function_assignments,
  function(node) as.character(node[[2]]),
  character(1)
)
all_schema_function_definitions <- sum(vapply(
  as.list(schema_expressions),
  count_function_definitions,
  integer(1)
))
schema_has_roxygen_documentation <- vapply(
  schema_top_level_function_lines,
  function(line_number) {
    line_number > 1L &&
      grepl("^#'", schema_lines[[line_number - 1L]])
  },
  logical(1)
)
schema_call_nodes <- unlist(
  lapply(as.list(schema_expressions), collect_call_nodes),
  recursive = FALSE
)
schema_call_names <- vapply(
  schema_call_nodes,
  call_name,
  character(1)
)

catalog_top_level_function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  catalog_lines
)
catalog_function_assignments <- Filter(
  function(node) {
    is_assignment(node) &&
      is.symbol(node[[2]]) &&
      is.call(node[[3]]) &&
      identical(call_name(node[[3]]), "function")
  },
  as.list(catalog_expressions)
)
catalog_function_names <- vapply(
  catalog_function_assignments,
  function(node) as.character(node[[2]]),
  character(1)
)
all_catalog_function_definitions <- sum(vapply(
  as.list(catalog_expressions),
  count_function_definitions,
  integer(1)
))
catalog_has_roxygen_documentation <- vapply(
  catalog_top_level_function_lines,
  function(line_number) {
    line_number > 1L &&
      grepl("^#'", catalog_lines[[line_number - 1L]])
  },
  logical(1)
)
catalog_call_nodes <- unlist(
  lapply(as.list(catalog_expressions), collect_call_nodes),
  recursive = FALSE
)
catalog_call_names <- vapply(
  catalog_call_nodes,
  call_name,
  character(1)
)
catalog_import_calls <- Filter(
  function(node) {
    call_name(node) %in% c("library", "require", "requireNamespace")
  },
  catalog_call_nodes
)
catalog_import_packages <- vapply(
  catalog_import_calls,
  function(node) {
    if (length(node) < 2L) return("")
    package <- node[[2]]
    if (!is.symbol(package) && !is.character(package)) return("")
    as.character(package)[[1]]
  },
  character(1)
)
catalog_uses_runtime_object <- vapply(
  catalog_call_nodes,
  function(node) {
    call_name(node) %in% c("$", "[[") &&
      length(node) >= 3L &&
      is.symbol(node[[2]]) &&
      as.character(node[[2]]) %in%
        c("input", "output", "session", "catalog_read")
  },
  logical(1)
)

catalog_state_top_level_function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  catalog_state_lines
)
catalog_state_function_assignments <- Filter(
  function(node) {
    is_assignment(node) &&
      is.symbol(node[[2]]) &&
      is.call(node[[3]]) &&
      identical(call_name(node[[3]]), "function")
  },
  as.list(catalog_state_expressions)
)
catalog_state_function_names <- vapply(
  catalog_state_function_assignments,
  function(node) as.character(node[[2]]),
  character(1)
)
catalog_state_has_roxygen_documentation <- vapply(
  catalog_state_top_level_function_lines,
  function(line_number) {
    line_number > 1L &&
      grepl("^#'", catalog_state_lines[[line_number - 1L]])
  },
  logical(1)
)
catalog_state_library_lines <- trimws(grep(
  "^library\\(",
  catalog_state_lines,
  value = TRUE
))
loaded_catalog_state_libraries <- sub(
  "^library\\(([^)]+)\\).*$",
  "\\1",
  catalog_state_library_lines
)

basis_state_top_level_function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  basis_state_lines
)
basis_state_function_assignments <- Filter(
  function(node) {
    is_assignment(node) &&
      is.symbol(node[[2]]) &&
      is.call(node[[3]]) &&
      identical(call_name(node[[3]]), "function")
  },
  as.list(basis_state_expressions)
)
basis_state_function_names <- vapply(
  basis_state_function_assignments,
  function(node) as.character(node[[2]]),
  character(1)
)
basis_state_has_roxygen_documentation <- vapply(
  basis_state_top_level_function_lines,
  function(line_number) {
    line_number > 1L &&
      grepl("^#'", basis_state_lines[[line_number - 1L]])
  },
  logical(1)
)
basis_state_library_lines <- trimws(grep(
  "^library\\(",
  basis_state_lines,
  value = TRUE
))
loaded_basis_state_libraries <- sub(
  "^library\\(([^)]+)\\).*$",
  "\\1",
  basis_state_library_lines
)

history_state_top_level_function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  history_state_lines
)
history_state_function_assignments <- Filter(
  function(node) {
    is_assignment(node) &&
      is.symbol(node[[2]]) &&
      is.call(node[[3]]) &&
      identical(call_name(node[[3]]), "function")
  },
  as.list(history_state_expressions)
)
history_state_function_names <- vapply(
  history_state_function_assignments,
  function(node) as.character(node[[2]]),
  character(1)
)
history_state_has_roxygen_documentation <- vapply(
  history_state_top_level_function_lines,
  function(line_number) {
    line_number > 1L &&
      grepl("^#'", history_state_lines[[line_number - 1L]])
  },
  logical(1)
)
history_state_library_lines <- trimws(grep(
  "^library\\(",
  history_state_lines,
  value = TRUE
))
loaded_history_state_libraries <- sub(
  "^library\\(([^)]+)\\).*$",
  "\\1",
  history_state_library_lines
)

recipe_mutation_handlers <- c(
  save_ny_ret = "recipe_catalog_create",
  save_opskrift_row = "recipe_catalog_update_ingredient",
  save_opskrift_new_row = "recipe_catalog_add_ingredient",
  confirm_delete_opskrift_row = "recipe_catalog_delete_ingredient",
  confirm_delete_ret = "recipe_catalog_archive",
  restore_ret = "recipe_catalog_restore",
  confirm_delete_archived_ret = "recipe_catalog_delete"
)
recipe_mutation_api <- unname(recipe_mutation_handlers)
recipe_server <- find_server_function(
  r_file("recipe_module.R"),
  "mod_opskrifter_server"
)
recipe_server_call_nodes <- collect_call_nodes(recipe_server[[3]])
recipe_server_call_names <- vapply(
  recipe_server_call_nodes,
  call_name,
  character(1)
)
recipe_mutation_handler_bodies <- list()

for (input_name in names(recipe_mutation_handlers)) {
  observers <- Filter(
    function(node) {
      identical(call_name(node), "observeEvent") &&
        length(node) >= 3L &&
        identical(input_reference_name(node[[2]]), input_name)
    },
    recipe_server_call_nodes
  )
  stopifnot(length(observers) == 1L)
  recipe_mutation_handler_bodies[[input_name]] <- observers[[1]][[3]]
}

has_roxygen_documentation <- vapply(
  top_level_function_lines,
  function(line_number) {
    line_number > 1L &&
      grepl("^#'", module_lines[[line_number - 1L]])
  },
  logical(1)
)

required_module_libraries <- c(
  "DT",
  "shiny",
  "shinyjs"
)
module_library_lines <- trimws(grep(
  "^library\\(",
  module_lines,
  value = TRUE
))
loaded_module_libraries <- sub(
  "^library\\(([^)]+)\\).*$",
  "\\1",
  module_library_lines
)

required_recipe_view_libraries <- c(
  "stats",
  "htmltools",
  "DT",
  "shiny",
  "shinyMobile",
  "dplyr"
)
recipe_view_library_lines <- trimws(grep(
  "^library\\(",
  recipe_view_lines,
  value = TRUE
))
loaded_recipe_view_libraries <- sub(
  "^library\\(([^)]+)\\).*$",
  "\\1",
  recipe_view_library_lines
)
required_recipe_view_functions <- c(
  "mod_opskrifter_ui",
  "mod_opskrifter_dialogs_ui",
  "recipe_active_rows",
  "recipe_choices",
  "recipe_format_line",
  "recipe_normalize_link",
  "recipe_edit_button",
  "recipe_delete_button",
  "recipe_archive_row_ui",
  "recipe_ingredient_table_rows",
  "recipe_ingredient_table_widget",
  "recipe_overview_ui",
  "recipe_selected_ui"
)

varer_module_lines <- readLines(
  r_file("varer_module.R"),
  encoding = "UTF-8"
)
varer_module_expressions <- parse(
  r_file("varer_module.R"),
  encoding = "UTF-8"
)
varer_top_level_function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  varer_module_lines
)
all_varer_module_function_definitions <- sum(vapply(
  as.list(varer_module_expressions),
  count_function_definitions,
  integer(1)
))

varer_has_roxygen_documentation <- vapply(
  varer_top_level_function_lines,
  function(line_number) {
    line_number > 1L &&
      grepl("^#'", varer_module_lines[[line_number - 1L]])
  },
  logical(1)
)

required_varer_module_libraries <- c(
  "shiny",
  "shinyMobile",
  "shinyjs",
  "dplyr",
  "DT",
  "htmltools",
  "stats"
)
varer_module_library_lines <- trimws(grep(
  "^library\\(",
  varer_module_lines,
  value = TRUE
))
loaded_varer_module_libraries <- sub(
  "^library\\(([^)]+)\\).*$",
  "\\1",
  varer_module_library_lines
)

indkobsseddel_module_lines <- readLines(
  r_file("indkobsseddel_module.R"),
  encoding = "UTF-8"
)
indkobsseddel_module_expressions <- parse(
  r_file("indkobsseddel_module.R"),
  encoding = "UTF-8"
)
indkobsseddel_catalog_lines <- readLines(
  r_file("indkobsseddel_catalog.R"),
  encoding = "UTF-8"
)
indkobsseddel_catalog_expressions <- parse(
  r_file("indkobsseddel_catalog.R"),
  encoding = "UTF-8"
)
indkobsseddel_view_lines <- readLines(
  r_file("indkobsseddel_view.R"),
  encoding = "UTF-8"
)
indkobsseddel_view_expressions <- parse(
  r_file("indkobsseddel_view.R"),
  encoding = "UTF-8"
)
indkobsseddel_top_level_function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  indkobsseddel_module_lines
)
all_indkobsseddel_module_function_definitions <- sum(vapply(
  as.list(indkobsseddel_module_expressions),
  count_function_definitions,
  integer(1)
))

indkobsseddel_has_roxygen_documentation <- vapply(
  indkobsseddel_top_level_function_lines,
  function(line_number) {
    line_number > 1L &&
      grepl(
        "^#'",
        indkobsseddel_module_lines[[line_number - 1L]]
      )
  },
  logical(1)
)

indkobsseddel_catalog_top_level_function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  indkobsseddel_catalog_lines
)
all_indkobsseddel_catalog_function_definitions <- sum(vapply(
  as.list(indkobsseddel_catalog_expressions),
  count_function_definitions,
  integer(1)
))
indkobsseddel_catalog_has_roxygen_documentation <- vapply(
  indkobsseddel_catalog_top_level_function_lines,
  function(line_number) {
    line_number > 1L &&
      grepl(
        "^#'",
        indkobsseddel_catalog_lines[[line_number - 1L]]
      )
  },
  logical(1)
)
indkobsseddel_catalog_call_nodes <- unlist(
  lapply(
    as.list(indkobsseddel_catalog_expressions),
    collect_call_nodes
  ),
  recursive = FALSE
)
indkobsseddel_catalog_call_names <- vapply(
  indkobsseddel_catalog_call_nodes,
  call_name,
  character(1)
)

indkobsseddel_view_top_level_function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  indkobsseddel_view_lines
)
all_indkobsseddel_view_function_definitions <- sum(vapply(
  as.list(indkobsseddel_view_expressions),
  count_function_definitions,
  integer(1)
))
indkobsseddel_view_has_roxygen_documentation <- vapply(
  indkobsseddel_view_top_level_function_lines,
  function(line_number) {
    line_number > 1L &&
      grepl(
        "^#'",
        indkobsseddel_view_lines[[line_number - 1L]]
      )
  },
  logical(1)
)
indkobsseddel_view_call_nodes <- unlist(
  lapply(
    as.list(indkobsseddel_view_expressions),
    collect_call_nodes
  ),
  recursive = FALSE
)
indkobsseddel_view_call_names <- vapply(
  indkobsseddel_view_call_nodes,
  call_name,
  character(1)
)

required_indkobsseddel_module_libraries <- c(
  "htmltools",
  "DT",
  "shiny",
  "shinyMobile",
  "shinyjs"
)
indkobsseddel_module_library_lines <- trimws(grep(
  "^library\\(",
  indkobsseddel_module_lines,
  value = TRUE
))
loaded_indkobsseddel_module_libraries <- sub(
  "^library\\(([^)]+)\\).*$",
  "\\1",
  indkobsseddel_module_library_lines
)
indkobsseddel_catalog_library_lines <- trimws(grep(
  "^library\\(",
  indkobsseddel_catalog_lines,
  value = TRUE
))
loaded_indkobsseddel_catalog_libraries <- sub(
  "^library\\(([^)]+)\\).*$",
  "\\1",
  indkobsseddel_catalog_library_lines
)
indkobsseddel_view_library_lines <- trimws(grep(
  "^library\\(",
  indkobsseddel_view_lines,
  value = TRUE
))
loaded_indkobsseddel_view_libraries <- sub(
  "^library\\(([^)]+)\\).*$",
  "\\1",
  indkobsseddel_view_library_lines
)

inspiration_module_lines <- readLines(
  r_file("inspiration_module.R"),
  encoding = "UTF-8"
)
funktioner_lines <- readLines(
  r_file("funktioner.R"),
  encoding = "UTF-8"
)
inspiration_module_expressions <- parse(
  r_file("inspiration_module.R"),
  encoding = "UTF-8"
)
inspiration_top_level_function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  inspiration_module_lines
)
all_inspiration_module_function_definitions <- sum(vapply(
  as.list(inspiration_module_expressions),
  count_function_definitions,
  integer(1)
))
inspiration_has_roxygen_documentation <- vapply(
  inspiration_top_level_function_lines,
  function(line_number) {
    line_number > 1L &&
      grepl("^#'", inspiration_module_lines[[line_number - 1L]])
  },
  logical(1)
)
required_inspiration_module_libraries <- c(
  "shiny",
  "shinyMobile",
  "dplyr",
  "ggplot2",
  "forcats",
  "wordcloud2"
)
inspiration_module_library_lines <- trimws(grep(
  "^library\\(",
  inspiration_module_lines,
  value = TRUE
))
loaded_inspiration_module_libraries <- sub(
  "^library\\(([^)]+)\\).*$",
  "\\1",
  inspiration_module_library_lines
)

app_lines <- readLines("app.R", encoding = "UTF-8")
app_expressions <- parse("app.R", encoding = "UTF-8")
app_call_nodes <- unlist(
  lapply(as.list(app_expressions), collect_call_nodes),
  recursive = FALSE
)
app_call_names <- vapply(app_call_nodes, call_name, character(1))
app_source_calls <- Filter(
  function(node) identical(call_name(node), "source"),
  app_call_nodes
)
app_literal_source_targets <- vapply(
  app_source_calls,
  function(node) {
    if (
      length(node) >= 2L &&
        is.character(node[[2]])
    ) {
      return(as.character(node[[2]]))
    }
    ""
  },
  character(1)
)
app_lists_r_directory <- any(vapply(
  app_call_nodes,
  function(node) {
    identical(call_name(node), "list.files") &&
      length(node) >= 2L &&
      is.character(node[[2]]) &&
      identical(as.character(node[[2]]), "R")
  },
  logical(1)
))
app_lapply_calls <- Filter(
  function(node) identical(call_name(node), "lapply"),
  app_call_nodes
)
app_passes_source_to_loader <- any(vapply(
  app_lapply_calls,
  function(node) {
    any(vapply(
      as.list(node)[-1L],
      function(argument) {
        is.symbol(argument) &&
          identical(as.character(argument), "source")
      },
      logical(1)
    ))
  },
  logical(1)
))

expected_r_script_names <- c(
  "basis_varer_state.R",
  "basis_varer_store.R",
  "cart_state.R",
  "data.R",
  "funktioner.R",
  "indkobsseddel_catalog.R",
  "indkobsseddel_module.R",
  "indkobsseddel_view.R",
  "inspiration_module.R",
  "recipe_catalog.R",
  "recipe_catalog_state.R",
  "recipe_module.R",
  "recipe_schema.R",
  "recipe_store.R",
  "recipe_view.R",
  "shopping_history_state.R",
  "shopping_history_store.R",
  "store_lock.R",
  "varer_module.R"
)
actual_r_script_names <- sort(list.files(
  "R",
  pattern = "\\.R$",
  full.names = FALSE
))
root_r_script_names <- sort(list.files(
  ".",
  pattern = "\\.R$",
  full.names = FALSE,
  recursive = FALSE
))
r_script_paths <- r_file(actual_r_script_names)
r_script_first_content <- vapply(
  r_script_paths,
  function(path) {
    lines <- readLines(path, encoding = "UTF-8")
    content <- lines[nzchar(trimws(lines))]
    if (length(content) == 0L) return("")
    trimws(content[[1L]])
  },
  character(1)
)

reference_data_lines <- readLines(
  r_file("data.R"),
  encoding = "UTF-8"
)
history_consumer_lines <- list(
  funktioner = funktioner_lines,
  indkobsseddel = indkobsseddel_module_lines,
  inspiration = inspiration_module_lines
)
runtime_files <- c(
  "app.R",
  r_script_paths
)
runtime_lines <- unlist(
  lapply(runtime_files, readLines, encoding = "UTF-8"),
  use.names = FALSE
)
legacy_history_function_patterns <- c(
  "mest_brugte_varer",
  "find_varer",
  "medtag_kun_varer",
  "indkobsseddel_save_history"
)
direct_history_io_pattern <- paste0(
  "(^|[^[:alnum:]_.])",
  "(list\\.files|load|save)[[:space:]]*\\("
)
legacy_reference_data_patterns <- c(
  "recipe_store_read[[:space:]]*\\(",
  "^[[:space:]]*recipe_store_data[[:space:]]*<-",
  "^[[:space:]]*retter[[:space:]]*<-",
  "^[[:space:]]*retter_arkiv[[:space:]]*<-",
  "^[[:space:]]*opskrifter[[:space:]]*<-",
  "^[[:space:]]*links[[:space:]]*<-",
  "^[[:space:]]*opskrift_df[[:space:]]*<-",
  "^[[:space:]]*kategori_1[[:space:]]*<-",
  "^[[:space:]]*kategori_2[[:space:]]*<-"
)
old_indkobsseddel_root_patterns <- c(
  "reactiveVal\\(new_cart_state\\(",
  "output\\$opskrift[[:space:]]*<-",
  "output\\$indkobsseddel[[:space:]]*<-",
  "output\\$tidl_kob[[:space:]]*<-",
  "input\\$add_opskrift",
  "input\\$add_varer",
  "input\\$add_varer_manuel",
  "input\\$deletePressed",
  "input\\$gem_indkobsseddel",
  "rv_cart"
)
old_inspiration_root_patterns <- c(
  "output\\$wordcloud_retter[[:space:]]*<-",
  "output\\$opskrifter_statistik_plot[[:space:]]*<-",
  "input\\$menu_type",
  "input\\$date_from",
  "input\\$date_to",
  "input\\$top_n",
  "opskrifter_statistik[[:space:]]*<-[[:space:]]*reactive"
)
old_recipe_state_root_patterns <- c(
  "rv_recipeCatalog",
  "rv_recipeCatalogSignals",
  "initial_recipe_store",
  "publish_recipe_catalog",
  "commit_recipe_store_change",
  "recipe_store_(read|revision|commit)[[:space:]]*\\("
)
old_basis_state_root_patterns <- c(
  "initial_basis_varer_store",
  "rv_basisVarerStore",
  "rv_varer_custom",
  "publish_basis_varer_store",
  "commit_basis_varer_change",
  "basis_varer_store_(read|revision|commit)[[:space:]]*\\("
)
old_history_state_root_patterns <- c(
  "initial_history_store",
  "rv_historyStore",
  "publish_shopping_history",
  "commit_shopping_history",
  "shopping_history_store_(read|revision|save)[[:space:]]*\\(",
  "reactiveVal[[:space:]]*\\([^)]*history"
)

expected_top_level_outputs <- c(
  "opskrift_edit_context",
  "opskrift_add_context",
  "opskrift_delete_context",
  "ret_delete_context",
  "ret_permanent_delete_context",
  "opskrift_tbl_valgt",
  "opskrifter_ui",
  "valgt_opskrift_ui",
  "varer_tbl",
  "recipe_preview",
  "cart_table",
  "history_suggestions",
  "wordcloud_retter",
  "opskrifter_statistik_plot"
)

forbidden_catalog_shiny_calls <- c(
  "moduleServer",
  "NS",
  "reactive",
  "reactiveVal",
  "reactiveValues",
  "reactiveValuesToList",
  "observe",
  "observeEvent",
  "eventReactive",
  "reactivePoll",
  "reactiveFileReader",
  "bindEvent",
  "isolate",
  "req",
  "validate",
  "need",
  "showNotification",
  "showModal",
  "removeModal",
  "renderUI",
  "renderText",
  "renderTable",
  "renderPlot",
  "renderDataTable",
  "renderDT",
  "insertUI",
  "removeUI",
  "updateSelectInput",
  "updateSelectizeInput",
  "updateTextInput",
  "updateNumericInput",
  "invalidateLater"
)
forbidden_catalog_persistence_calls <- c(
  "commit_catalog",
  "readRDS",
  "saveRDS",
  "readLines",
  "writeLines",
  "save",
  "load",
  "list.files",
  "dir.create",
  "file.create",
  "file.copy",
  "file.rename",
  "unlink"
)

stopifnot(length(nested_output_assignments) == 0)
stopifnot(all(expected_top_level_outputs %in% all_output_assignments))
stopifnot(all(nested_module_functions == 0L))
stopifnot(
  dir.exists("R"),
  identical(expected_r_script_names, actual_r_script_names),
  identical(root_r_script_names, "app.R"),
  all(file.exists(r_script_paths)),
  all(startsWith(r_script_first_content, "#")),
  length(app_literal_source_targets) == 0L,
  "lapply" %in% app_call_names,
  app_lists_r_directory,
  app_passes_source_to_loader,
  any(grepl(
    'exists\\("mod_varer_server",[[:space:]]*mode[[:space:]]*=[[:space:]]*"function"\\)',
    app_lines
  ))
)
stopifnot(
  all(startsWith(schema_function_names, "recipe_schema_")),
  length(schema_top_level_function_lines) > 0L,
  length(schema_top_level_function_lines) ==
    length(schema_function_assignments),
  all_schema_function_definitions ==
    length(schema_function_assignments),
  all(schema_has_roxygen_documentation),
  !any(schema_call_names %in% c("::", ":::")),
  !any(schema_call_names %in% forbidden_catalog_shiny_calls),
  !any(schema_call_names %in% forbidden_catalog_persistence_calls),
  !any(schema_call_names %in% c("<<-", "assign")),
  any(grepl(
    "recipe_schema_validate_catalog_tables",
    catalog_lines,
    fixed = TRUE
  )),
  any(grepl(
    "recipe_schema_validate_catalog_tables",
    recipe_store_lines,
    fixed = TRUE
  )),
  any(grepl(
    "recipe_schema_validate_recipes",
    catalog_lines,
    fixed = TRUE
  )),
  any(grepl(
    "recipe_schema_validate_recipes",
    recipe_store_lines,
    fixed = TRUE
  )),
  !any(grepl(
    paste0(
      "^recipe_catalog_validate_(table|keys|recipe)",
      "[[:space:]]*<-[[:space:]]*function"
    ),
    catalog_lines
  )),
  !any(grepl(
    paste0(
      "^\\.recipe_store_validate_(table|recipe|catalog_tables)",
      "[[:space:]]*<-[[:space:]]*function"
    ),
    recipe_store_lines
  ))
)
stopifnot(
  all(recipe_mutation_api %in% catalog_function_names),
  all(startsWith(catalog_function_names, "recipe_catalog_")),
  length(catalog_top_level_function_lines) > 0L,
  length(catalog_top_level_function_lines) ==
    length(catalog_function_assignments),
  all_catalog_function_definitions ==
    length(catalog_function_assignments),
  all(catalog_has_roxygen_documentation),
  !any(catalog_call_names %in% c("::", ":::")),
  !any(tolower(catalog_import_packages) %in% c("shiny", "shinymobile")),
  !any(catalog_call_names %in% forbidden_catalog_shiny_calls),
  !any(catalog_call_names %in% forbidden_catalog_persistence_calls),
  !any(startsWith(catalog_call_names, "recipe_store_")),
  !any(catalog_call_names %in% c("<<-", "assign")),
  !any(catalog_uses_runtime_object)
)
stopifnot(
  "create_recipe_catalog_state" %in% catalog_state_function_names,
  all(
    startsWith(
      catalog_state_function_names,
      "recipe_catalog_state_"
    ) |
      catalog_state_function_names == "create_recipe_catalog_state"
  ),
  length(catalog_state_top_level_function_lines) > 0L,
  length(catalog_state_top_level_function_lines) ==
    length(catalog_state_function_assignments),
  all(catalog_state_has_roxygen_documentation),
  !any(grepl("::", catalog_state_lines, fixed = TRUE)),
  "shiny" %in% loaded_catalog_state_libraries,
  sum(grepl(
    "create_recipe_catalog_state[[:space:]]*\\(",
    app_lines
  )) == 1L,
  !any(vapply(
    old_recipe_state_root_patterns,
    function(pattern) any(grepl(pattern, app_lines)),
    logical(1)
  ))
)
stopifnot(
  "create_basis_varer_state" %in% basis_state_function_names,
  all(
    startsWith(
      basis_state_function_names,
      "basis_varer_state_"
    ) |
      basis_state_function_names == "create_basis_varer_state"
  ),
  length(basis_state_top_level_function_lines) > 0L,
  length(basis_state_top_level_function_lines) ==
    length(basis_state_function_assignments),
  all(basis_state_has_roxygen_documentation),
  !any(grepl("::", basis_state_lines, fixed = TRUE)),
  "shiny" %in% loaded_basis_state_libraries,
  sum(grepl(
    "create_basis_varer_state[[:space:]]*\\(",
    app_lines
  )) == 1L,
  !any(vapply(
    old_basis_state_root_patterns,
    function(pattern) any(grepl(pattern, app_lines)),
    logical(1)
  ))
)
stopifnot(
  "create_shopping_history_state" %in% history_state_function_names,
  all(
    startsWith(
      history_state_function_names,
      "shopping_history_state_"
    ) |
      history_state_function_names == "create_shopping_history_state"
  ),
  length(history_state_top_level_function_lines) > 0L,
  length(history_state_top_level_function_lines) ==
    length(history_state_function_assignments),
  all(history_state_has_roxygen_documentation),
  !any(grepl("::", history_state_lines, fixed = TRUE)),
  "shiny" %in% loaded_history_state_libraries,
  sum(grepl(
    "create_shopping_history_state[[:space:]]*\\(",
    app_lines
  )) == 1L,
  any(grepl(
    "history_state\\$read\\$entries",
    app_lines
  )),
  any(grepl(
    "save_cart[[:space:]]*=[[:space:]]*history_state\\$commit",
    app_lines
  )),
  all(vapply(
    c(
      "shopping_history_store_read",
      "shopping_history_store_revision",
      "shopping_history_store_save"
    ),
    function(function_name) {
      any(grepl(function_name, history_state_lines, fixed = TRUE))
    },
    logical(1)
  )),
  !any(vapply(
    old_history_state_root_patterns,
    function(pattern) any(grepl(pattern, app_lines)),
    logical(1)
  ))
)
stopifnot(vapply(
  recipe_mutation_api,
  function(function_name) {
    sum(recipe_server_call_names == function_name) == 1L
  },
  logical(1)
))

for (input_name in names(recipe_mutation_handlers)) {
  expected_function <- recipe_mutation_handlers[[input_name]]
  handler_calls <- collect_call_nodes(
    recipe_mutation_handler_bodies[[input_name]]
  )
  handler_call_names <- vapply(
    handler_calls,
    call_name,
    character(1)
  )

  stopifnot(
    sum(handler_call_names == expected_function) == 1L,
    sum(handler_call_names %in% recipe_mutation_api) == 1L,
    sum(handler_call_names == "commit_catalog") == 1L
  )

  mutation_call <- Filter(
    function(node) identical(call_name(node), expected_function),
    handler_calls
  )[[1]]
  stopifnot(
    length(mutation_call) >= 2L,
    identical(mutation_call[[2]], quote(catalog_read$snapshot()))
  )

  assignments <- Filter(is_assignment, handler_calls)
  stopifnot(!any(vapply(
    assignments,
    function(node) {
      lhs <- node[[2]]
      (
        is.call(lhs) &&
          call_name(lhs) %in% c("[", "[[")
      ) ||
        lhs_contains_catalog_field(lhs)
    },
    logical(1)
  )))
}
stopifnot(
  !any(vapply(
    legacy_reference_data_patterns,
    function(pattern) any(grepl(pattern, reference_data_lines)),
    logical(1)
  )),
  !any(grepl("kategori_[12]", app_lines))
)
stopifnot(
  file.exists(r_file("indkobsseddel_module.R")),
  any(grepl("mod_indkobsseddel_ui", app_lines, fixed = TRUE)),
  any(grepl("mod_indkobsseddel_dialogs_ui", app_lines, fixed = TRUE)),
  any(grepl("mod_indkobsseddel_server", app_lines, fixed = TRUE)),
  !any(vapply(
    old_indkobsseddel_root_patterns,
    function(pattern) any(grepl(pattern, app_lines)),
    logical(1)
  ))
)
stopifnot(
  file.exists(r_file("inspiration_module.R")),
  any(grepl("mod_inspiration_ui", app_lines, fixed = TRUE)),
  any(grepl("mod_inspiration_filters_ui", app_lines, fixed = TRUE)),
  any(grepl("mod_inspiration_server", app_lines, fixed = TRUE)),
  !any(vapply(
    old_inspiration_root_patterns,
    function(pattern) any(grepl(pattern, app_lines)),
    logical(1)
  ))
)
stopifnot(
  file.exists(r_file("shopping_history_store.R")),
  file.exists(r_file("shopping_history_state.R")),
  !any(vapply(
    legacy_history_function_patterns,
    function(pattern) any(grepl(pattern, runtime_lines)),
    logical(1)
  )),
  !any(vapply(
    history_consumer_lines,
    function(lines) any(grepl(direct_history_io_pattern, lines)),
    logical(1)
  ))
)
stopifnot(length(top_level_function_lines) > 0L)
stopifnot(all_module_function_definitions == length(top_level_function_lines))
stopifnot(all(has_roxygen_documentation))
stopifnot(!any(grepl("::", module_lines, fixed = TRUE)))
stopifnot(identical(required_module_libraries, loaded_module_libraries))
stopifnot(
  match("DT", loaded_module_libraries) <
    match("shiny", loaded_module_libraries),
  match("shiny", loaded_module_libraries) <
    match("shinyjs", loaded_module_libraries)
)
stopifnot(
  length(recipe_view_top_level_function_lines) > 0L,
  all_recipe_view_function_definitions ==
    length(recipe_view_top_level_function_lines),
  all(recipe_view_has_roxygen_documentation),
  !any(grepl("::", recipe_view_lines, fixed = TRUE)),
  identical(
    required_recipe_view_libraries,
    loaded_recipe_view_libraries
  ),
  all(required_recipe_view_functions %in% recipe_view_function_names),
  !any(required_recipe_view_functions %in% module_function_names),
  !any(recipe_view_call_names %in% reactive_registration_calls),
  !any(recipe_view_uses_runtime_object)
)
stopifnot(
  match("stats", loaded_recipe_view_libraries) <
    match("dplyr", loaded_recipe_view_libraries),
  match("htmltools", loaded_recipe_view_libraries) <
    match("shiny", loaded_recipe_view_libraries),
  match("DT", loaded_recipe_view_libraries) <
    match("shiny", loaded_recipe_view_libraries)
)
stopifnot(length(varer_top_level_function_lines) > 0L)
stopifnot(
  all_varer_module_function_definitions ==
    length(varer_top_level_function_lines)
)
stopifnot(all(varer_has_roxygen_documentation))
stopifnot(!any(grepl("::", varer_module_lines, fixed = TRUE)))
stopifnot(
  all(
    required_varer_module_libraries %in%
      loaded_varer_module_libraries
  )
)
stopifnot(
  match("stats", loaded_varer_module_libraries) <
    match("dplyr", loaded_varer_module_libraries),
  match("htmltools", loaded_varer_module_libraries) <
    match("shiny", loaded_varer_module_libraries),
  match("DT", loaded_varer_module_libraries) <
    match("shiny", loaded_varer_module_libraries),
  match("shiny", loaded_varer_module_libraries) <
    match("shinyjs", loaded_varer_module_libraries)
)
stopifnot(
  length(indkobsseddel_catalog_top_level_function_lines) > 0L,
  all_indkobsseddel_catalog_function_definitions ==
    length(indkobsseddel_catalog_top_level_function_lines),
  all(indkobsseddel_catalog_has_roxygen_documentation),
  !any(grepl("::", indkobsseddel_catalog_lines, fixed = TRUE)),
  identical(loaded_indkobsseddel_catalog_libraries, "dplyr"),
  !any(
    indkobsseddel_catalog_call_names %in%
      forbidden_catalog_shiny_calls
  ),
  !any(
    indkobsseddel_catalog_call_names %in%
      forbidden_catalog_persistence_calls
  ),
  any(grepl(
    "^indkobsseddel_prepare_recipe[[:space:]]*<-",
    indkobsseddel_catalog_lines
  )),
  any(grepl(
    "^indkobsseddel_find_item[[:space:]]*<-",
    indkobsseddel_catalog_lines
  ))
)
stopifnot(
  length(indkobsseddel_view_top_level_function_lines) > 0L,
  all_indkobsseddel_view_function_definitions ==
    length(indkobsseddel_view_top_level_function_lines),
  all(indkobsseddel_view_has_roxygen_documentation),
  !any(grepl("::", indkobsseddel_view_lines, fixed = TRUE)),
  identical(
    loaded_indkobsseddel_view_libraries,
    c("htmltools", "DT", "shiny")
  ),
  !any(
    indkobsseddel_view_call_names %in%
      forbidden_catalog_shiny_calls
  ),
  !any(
    indkobsseddel_view_call_names %in%
      forbidden_catalog_persistence_calls
  ),
  any(grepl(
    "^indkobsseddel_cart_widget[[:space:]]*<-",
    indkobsseddel_view_lines
  )),
  any(grepl(
    "^indkobsseddel_recipe_preview_widget[[:space:]]*<-",
    indkobsseddel_view_lines
  ))
)
stopifnot(length(indkobsseddel_top_level_function_lines) > 0L)
stopifnot(
  all_indkobsseddel_module_function_definitions ==
    length(indkobsseddel_top_level_function_lines)
)
stopifnot(all(indkobsseddel_has_roxygen_documentation))
stopifnot(
  !any(
    grepl(
      "::",
      indkobsseddel_module_lines,
      fixed = TRUE
    )
  )
)
stopifnot(
  all(
    required_indkobsseddel_module_libraries %in%
      loaded_indkobsseddel_module_libraries
  ),
  !"dplyr" %in% loaded_indkobsseddel_module_libraries,
  !any(grepl(
    paste0(
      "^indkobsseddel_(prepare_recipe|find_item|cart_widget|",
      "recipe_preview_widget)[[:space:]]*<-"
    ),
    indkobsseddel_module_lines
  ))
)
stopifnot(
  match("htmltools", loaded_indkobsseddel_module_libraries) <
    match("shiny", loaded_indkobsseddel_module_libraries),
  match("DT", loaded_indkobsseddel_module_libraries) <
    match("shiny", loaded_indkobsseddel_module_libraries),
  match("shiny", loaded_indkobsseddel_module_libraries) <
    match("shinyjs", loaded_indkobsseddel_module_libraries)
)
stopifnot(length(inspiration_top_level_function_lines) > 0L)
stopifnot(
  all_inspiration_module_function_definitions ==
    length(inspiration_top_level_function_lines)
)
stopifnot(all(inspiration_has_roxygen_documentation))
stopifnot(!any(grepl("::", inspiration_module_lines, fixed = TRUE)))
stopifnot(
  all(
    required_inspiration_module_libraries %in%
      loaded_inspiration_module_libraries
  )
)

message(paste(
  "Opskrifts-, vare-, indkøbsseddel- og inspirationsmodulerne har",
  "dokumenterede",
  "topniveau-funktioner,",
  "ingen nested funktioner, ingen ::-kald og ingen nested outputs.",
  "Opskriftskatalogets syv ændringer ligger i dokumenterede, rene",
  "katalogfunktioner uden Shiny- eller persistensafhængigheder.",
  "Katalog og fillager deler de dokumenterede skemaregler i",
  "recipe_schema.R, mens filreglerne bliver i recipe_store.R.",
  "Opskriftsfanens rene UI- og tabelbyggere ligger i recipe_view.R,",
  "mens outputregistreringer og reaktiv koordinering bliver i recipe_module.R.",
  "Indkøbssedlens rene valgregler og view-buildere ligger i hver sin fil,",
  "mens den reaktive koordinering bliver i indkobsseddel_module.R.",
  "Alle produktionsscripts bortset fra app.R ligger dokumenteret i R-mappen,",
  "som Shiny indlæser automatisk, og app.R har kun en dynamisk test-fallback.",
  "Katalogets kanoniske state, polling og commit-koordinering ligger",
  "i recipe_catalog_state.R og ikke i app.R. Basisvarernes tilsvarende",
  "state ligger i basis_varer_state.R, og indkøbshistorikkens state ligger",
  "i shopping_history_state.R."
))
