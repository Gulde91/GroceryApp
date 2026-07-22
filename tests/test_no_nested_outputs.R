server_source_files <- c(
  app = "app.R",
  opskrifter = "recipe_module.R",
  varer = "varer_module.R",
  indkobsseddel = "indkobsseddel_module.R",
  inspiration = "inspiration_module.R"
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

module_lines <- readLines("recipe_module.R", encoding = "UTF-8")
module_expressions <- parse("recipe_module.R", encoding = "UTF-8")
top_level_function_lines <- grep(
  "^[[:alnum:]_.]+[[:space:]]*<-[[:space:]]*function\\(",
  module_lines
)
all_module_function_definitions <- sum(vapply(
  as.list(module_expressions),
  count_function_definitions,
  integer(1)
))

has_roxygen_documentation <- vapply(
  top_level_function_lines,
  function(line_number) {
    line_number > 1L &&
      grepl("^#'", module_lines[[line_number - 1L]])
  },
  logical(1)
)

required_module_libraries <- c(
  "shiny",
  "shinyMobile",
  "shinyjs",
  "dplyr",
  "purrr",
  "DT",
  "htmltools",
  "stats"
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

varer_module_lines <- readLines("varer_module.R", encoding = "UTF-8")
varer_module_expressions <- parse(
  "varer_module.R",
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
  "indkobsseddel_module.R",
  encoding = "UTF-8"
)
indkobsseddel_module_expressions <- parse(
  "indkobsseddel_module.R",
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

required_indkobsseddel_module_libraries <- c(
  "htmltools",
  "DT",
  "shiny",
  "shinyMobile",
  "dplyr",
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

inspiration_module_lines <- readLines(
  "inspiration_module.R",
  encoding = "UTF-8"
)
funktioner_lines <- readLines("funktioner.R", encoding = "UTF-8")
inspiration_module_expressions <- parse(
  "inspiration_module.R",
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
reference_data_lines <- readLines("data.R", encoding = "UTF-8")
history_consumer_lines <- list(
  funktioner = funktioner_lines,
  indkobsseddel = indkobsseddel_module_lines,
  inspiration = inspiration_module_lines
)
runtime_files <- c(
  "app.R",
  "data.R",
  "funktioner.R",
  "cart_state.R",
  "recipe_store.R",
  "basis_varer_store.R",
  "shopping_history_store.R",
  "recipe_module.R",
  "varer_module.R",
  "indkobsseddel_module.R",
  "inspiration_module.R"
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

stopifnot(length(nested_output_assignments) == 0)
stopifnot(all(expected_top_level_outputs %in% all_output_assignments))
stopifnot(all(nested_module_functions == 0L))
stopifnot(
  !any(vapply(
    legacy_reference_data_patterns,
    function(pattern) any(grepl(pattern, reference_data_lines)),
    logical(1)
  )),
  !any(grepl("kategori_[12]", app_lines))
)
stopifnot(
  any(grepl(
    'source\\("\\./indkobsseddel_module\\.R"\\)',
    app_lines
  )),
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
  any(grepl(
    'source\\("\\./inspiration_module\\.R"\\)',
    app_lines
  )),
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
  any(grepl(
    'source\\("\\./shopping_history_store\\.R"\\)',
    app_lines
  )),
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
stopifnot(all(required_module_libraries %in% loaded_module_libraries))
stopifnot(
  match("stats", loaded_module_libraries) <
    match("dplyr", loaded_module_libraries),
  match("htmltools", loaded_module_libraries) <
    match("shiny", loaded_module_libraries),
  match("DT", loaded_module_libraries) <
    match("shiny", loaded_module_libraries),
  match("shiny", loaded_module_libraries) <
    match("shinyjs", loaded_module_libraries)
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
  )
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
  "ingen nested funktioner, ingen ::-kald og ingen nested outputs."
))
