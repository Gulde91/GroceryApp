suppressPackageStartupMessages({
  source(file.path("R", "funktioner.R"), encoding = "UTF-8")
  source(file.path("R", "indkobsseddel_view.R"), encoding = "UTF-8")
})

expect_indkobsseddel_view_error <- function(expression, pattern) {
  error <- tryCatch(
    {
      force(expression)
      NULL
    },
    error = identity
  )

  stopifnot(
    inherits(error, "error"),
    grepl(pattern, conditionMessage(error), ignore.case = TRUE)
  )
}

empty_payload <- list(
  visible = character(),
  hidden = character(),
  line_ids = character(),
  n_visible = 0L,
  category_break_after = integer()
)
empty_widget <- indkobsseddel_cart_widget(
  empty_payload,
  shiny::NS("kurv")
)
stopifnot(
  nrow(empty_widget$x$data) == 0L,
  identical(
    names(empty_widget$x$data),
    c("Indkøbsliste", "edit", "delete")
  ),
  identical(empty_widget$x$options$pageLength, 1L)
)

payload <- list(
  visible = c("1 liter Mælk", "2 stk Agurk"),
  hidden = c("", "Burger (2 pers.)"),
  line_ids = c("cart_1", "cart_2"),
  n_visible = 2L,
  category_break_after = 1L
)
widget <- indkobsseddel_cart_widget(
  payload,
  shiny::NS("kurv")
)
stopifnot(
  nrow(widget$x$data) == 4L,
  identical(
    widget$x$data$Indkøbsliste,
    c(payload$visible, payload$hidden)
  ),
  identical(widget$x$options$pageLength, 2L),
  grepl(
    "[0].forEach",
    as.character(
      widget$x$options$buttons[[1L]]$exportOptions$customizeData
    ),
    fixed = TRUE
  ),
  grepl(
    "+ '\\n'",
    as.character(
      widget$x$options$buttons[[1L]]$exportOptions$customizeData
    ),
    fixed = TRUE
  ),
  grepl(
    'id="kurv-edit_cart_1"',
    widget$x$data$edit[[1L]],
    fixed = TRUE
  ),
  grepl(
    "kurv-edit_pressed",
    widget$x$data$edit[[1L]],
    fixed = TRUE
  ),
  grepl(
    'id="kurv-delete_cart_2"',
    widget$x$data$delete[[2L]],
    fixed = TRUE
  ),
  grepl(
    "kurv-delete_pressed",
    widget$x$data$delete[[2L]],
    fixed = TRUE
  ),
  identical(widget$x$data$edit[[3L]], ""),
  identical(widget$x$data$delete[[4L]], "")
)

expect_indkobsseddel_view_error(
  indkobsseddel_cart_widget(
    list(
      visible = "Mælk",
      hidden = character(),
      line_ids = character(),
      n_visible = 1L,
      category_break_after = integer()
    ),
    shiny::NS("kurv")
  ),
  "inkonsistent"
)
expect_indkobsseddel_view_error(
  indkobsseddel_cart_widget(
    list(
      visible = character(),
      hidden = character(),
      line_ids = character(),
      n_visible = -1L,
      category_break_after = integer()
    ),
    shiny::NS("kurv")
  ),
  "ugyldigt rækkeantal"
)
expect_indkobsseddel_view_error(
  indkobsseddel_cart_widget(
    list(
      visible = c("Mælk", "Smør"),
      hidden = character(),
      line_ids = c("cart_1", "cart_2"),
      n_visible = 2L,
      category_break_after = 2L
    ),
    shiny::NS("kurv")
  ),
  "kategoriskift"
)

preview_rows <- data.frame(
  Indkobsliste = c("Mælk", "Agurk"),
  maengde = c(1, 2),
  enhed = c("liter", "stk"),
  kat_1 = c("mejeri", "frugt og grønt"),
  kat_2 = c("mælk", ""),
  stringsAsFactors = FALSE
)
preview <- indkobsseddel_recipe_preview_widget(preview_rows)
stopifnot(
  identical(
    names(preview$x$data),
    c("Indkobsliste", "maengde", "enhed")
  ),
  nrow(preview$x$data) == 2L,
  identical(preview$x$options$pageLength, 2L)
)

message(paste(
  "Indkøbssedlens view-buildere laver tomme og udfyldte widgets,",
  "namespacer knapper og afviser inkonsistente payloads."
))
