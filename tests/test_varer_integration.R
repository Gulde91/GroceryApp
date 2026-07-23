suppressPackageStartupMessages(source("app.R", encoding = "UTF-8"))

varer_ns <- shiny::NS("varer")
indkobsseddel_ns <- shiny::NS("indkobsseddel")

set_varer_inputs <- function(session, ...) {
  values <- list(...)
  names(values) <- varer_ns(names(values))
  do.call(session$setInputs, values)
}

set_indkobsseddel_inputs <- function(session, ...) {
  values <- list(...)
  names(values) <- indkobsseddel_ns(names(values))
  do.call(session$setInputs, values)
}

initialize_varer_test_inputs <- function(session) {
  set_indkobsseddel_inputs(
    session,
    catalog_item = "agurk",
    catalog_amount = 1,
    catalog_unit = "stk"
  )
}

integration_basis_row <- function(
  navn,
  enhed = "stk",
  kat_1 = "konserves",
  kat_2 = ""
) {
  data.frame(
    Indkobsliste = navn,
    maengde = 1,
    enhed = enhed,
    kat_1 = kat_1,
    kat_2 = kat_2,
    stringsAsFactors = FALSE
  )
}

run_varer_integration_tests <- function() {
  original_store_read <- basis_varer_store_read
  original_store_revision <- basis_varer_store_revision
  original_store_commit <- basis_varer_store_commit
  on.exit(
    {
      assign(
        "basis_varer_store_read",
        original_store_read,
        envir = .GlobalEnv
      )
      assign(
        "basis_varer_store_revision",
        original_store_revision,
        envir = .GlobalEnv
      )
      assign(
        "basis_varer_store_commit",
        original_store_commit,
        envir = .GlobalEnv
      )
    },
    add = TRUE
  )

  store_snapshot <- original_store_read("./data")
  store_read_calls <- 0L
  store_revision_calls <- 0L
  store_commit_calls <- list()
  store_commit_number <- 0L
  fail_next_commit <- FALSE

  store_read_stub <- function(data_dir = "./data") {
    store_read_calls <<- store_read_calls + 1L
    store_snapshot
  }
  store_revision_stub <- function(data_dir = "./data") {
    store_revision_calls <<- store_revision_calls + 1L
    store_snapshot$revision
  }
  store_commit_stub <- function(
    varer,
    expected_revision,
    data_dir = "./data",
    ...
  ) {
    store_commit_calls[[length(store_commit_calls) + 1L]] <<- list(
      varer = varer,
      expected_revision = expected_revision,
      data_dir = data_dir
    )

    if (fail_next_commit) {
      fail_next_commit <<- FALSE
      stop("Fremprovokeret store-fejl.", call. = FALSE)
    }
    if (!identical(expected_revision, store_snapshot$revision)) {
      .basis_varer_store_stop_conflict()
    }

    store_commit_number <<- store_commit_number + 1L
    store_snapshot <<- list(
      varer = .basis_varer_store_normalize(varer),
      revision = paste0(
        "stub-basis-revision-",
        store_commit_number
      )
    )
    store_snapshot
  }

  assign(
    "basis_varer_store_read",
    store_read_stub,
    envir = .GlobalEnv
  )
  assign(
    "basis_varer_store_revision",
    store_revision_stub,
    envir = .GlobalEnv
  )
  assign(
    "basis_varer_store_commit",
    store_commit_stub,
    envir = .GlobalEnv
  )

  shiny::testServer(server, {
    initialize_varer_test_inputs(session)

    # State-laget følger persist-first: en store-fejl publicerer ikke kandidaten.
    initial_local <- basis_state$read$snapshot()
    root_candidate <- rbind(
      initial_local$varer,
      integration_basis_row("Persistens-testvare")
    )

    fail_next_commit <<- TRUE
    stopifnot(
      identical(
        basis_state$commit(root_candidate),
        FALSE
      ),
      identical(basis_state$read$snapshot(), initial_local),
      identical(store_snapshot, initial_local)
    )

    stopifnot(
      identical(
        basis_state$commit(root_candidate),
        TRUE
      ),
      "Persistens-testvare" %in%
        basis_state$read$varer()$Indkobsliste,
      identical(basis_state$read$snapshot(), store_snapshot)
    )
    successful_root_call <- store_commit_calls[[length(
      store_commit_calls
    )]]
    stopifnot(
      identical(
        successful_root_call$expected_revision,
        initial_local$revision
      ),
      identical(successful_root_call$data_dir, "./data")
    )

    # En vare fra fanen Varer publiceres i root og kan straks ses i det
    # samlede varekatalog.
    new_name <- "Modul-integration-testvare"
    set_varer_inputs(
      session,
      ny_vare_navn = new_name,
      ny_vare_enhed = "stk",
      ny_vare_kat1 = "konserves",
      ny_vare_kat2 = ""
    )
    set_varer_inputs(session, save_ny_vare = 1L)

    stopifnot(
      new_name %in% basis_state$read$varer()$Indkobsliste,
      new_name %in% rv_varer()$Indkobsliste,
      identical(basis_state$read$snapshot(), store_snapshot)
    )

    # Simulér at en anden session har gemt "Te", efter denne session læste
    # sin revision. Første forsøg med "Kaffe" afvises og genindlæser Te.
    external_snapshot <- list(
      varer = .basis_varer_store_normalize(rbind(
        store_snapshot$varer,
        integration_basis_row("Te")
      )),
      revision = "external-basis-revision"
    )
    store_snapshot <<- external_snapshot

    set_varer_inputs(
      session,
      ny_vare_navn = "Kaffe",
      ny_vare_enhed = "stk",
      ny_vare_kat1 = "konserves",
      ny_vare_kat2 = ""
    )
    reads_before_conflict <- store_read_calls
    set_varer_inputs(session, save_ny_vare = 2L)

    stopifnot(
      store_read_calls == reads_before_conflict + 1L,
      "Te" %in% basis_state$read$varer()$Indkobsliste,
      !"Kaffe" %in% basis_state$read$varer()$Indkobsliste,
      identical(basis_state$read$snapshot(), external_snapshot)
    )

    # Dialogen og inputtet er bevaret. Ved nyt klik bygges kandidaten fra det
    # opdaterede snapshot, så både den eksterne og lokale vare gemmes.
    set_varer_inputs(session, save_ny_vare = 3L)
    stopifnot(
      all(
        c("Te", "Kaffe") %in%
          basis_state$read$varer()$Indkobsliste
      ),
      identical(basis_state$read$snapshot(), store_snapshot)
    )
    retry_call <- store_commit_calls[[length(store_commit_calls)]]
    stopifnot(
      identical(
        retry_call$expected_revision,
        external_snapshot$revision
      ),
      all(
        c("Te", "Kaffe") %in%
          retry_call$varer$Indkobsliste
      )
    )

    # Polling henter kun et nyt snapshot, når revisionen faktisk ændres.
    reads_before_unchanged_poll <- store_read_calls
    session$elapse(2001)
    session$flushReact()
    stopifnot(
      store_read_calls == reads_before_unchanged_poll
    )

    polled_snapshot <- list(
      varer = .basis_varer_store_normalize(rbind(
        store_snapshot$varer,
        integration_basis_row("Polling-testvare")
      )),
      revision = "polled-basis-revision"
    )
    store_snapshot <<- polled_snapshot
    reads_before_changed_poll <- store_read_calls
    session$elapse(2001)
    session$flushReact()
    stopifnot(
      store_read_calls == reads_before_changed_poll + 1L,
      identical(basis_state$read$snapshot(), polled_snapshot),
      "Polling-testvare" %in% rv_varer()$Indkobsliste,
      store_revision_calls > 0L
    )

    # Det namespacede indkøbsseddelmodul læser fortsat det fælles katalog.
    set_indkobsseddel_inputs(
      session,
      catalog_item = new_name,
      catalog_amount = 2,
      catalog_unit = "stk"
    )
    set_indkobsseddel_inputs(session, add_catalog_item = 1L)

    cart_rows <- indkobsseddel_api$visible_rows()
    added_row <- cart_rows[
      cart_rows$Indkobsliste == new_name,
      ,
      drop = FALSE
    ]
    stopifnot(
      nrow(added_row) == 1L,
      identical(added_row$maengde[[1]], 2),
      identical(added_row$enhed[[1]], "stk")
    )

    # Dubletter fra opskriftsdata bliver fortsat til én custom-prioriteret
    # katalogrække.
    tomato_rows <- rv_varer()[
      tolower(trimws(rv_varer()$Indkobsliste)) == "tomatsuppe",
      ,
      drop = FALSE
    ]
    stopifnot(
      nrow(tomato_rows) == 1L,
      identical(tomato_rows$enhed[[1]], "dåse(r)")
    )

    set_indkobsseddel_inputs(
      session,
      catalog_item = "tomatsuppe",
      catalog_amount = 1,
      catalog_unit = tomato_rows$enhed[[1]]
    )
    set_indkobsseddel_inputs(session, add_catalog_item = 2L)

    tomato_cart_rows <- indkobsseddel_api$visible_rows()
    tomato_cart_rows <- tomato_cart_rows[
      tolower(trimws(tomato_cart_rows$Indkobsliste)) ==
        "tomatsuppe",
      ,
      drop = FALSE
    ]
    stopifnot(
      nrow(tomato_cart_rows) == 1L,
      identical(tomato_cart_rows$maengde[[1]], 1)
    )

    cart_before_invalid_add <- indkobsseddel_api$cart_current()
    set_indkobsseddel_inputs(
      session,
      catalog_item = "",
      catalog_amount = 1,
      catalog_unit = "stk"
    )
    set_indkobsseddel_inputs(session, add_catalog_item = 3L)
    stopifnot(
      identical(
        indkobsseddel_api$cart_current(),
        cart_before_invalid_add
      )
    )

    set_indkobsseddel_inputs(
      session,
      catalog_item = new_name,
      catalog_amount = 0,
      catalog_unit = "stk"
    )
    set_indkobsseddel_inputs(session, add_catalog_item = 4L)
    stopifnot(
      identical(
        indkobsseddel_api$cart_current(),
        cart_before_invalid_add
      )
    )
  })
}

run_varer_integration_tests()

app_lines <- readLines("app.R", encoding = "UTF-8")
varer_module_lines <- readLines(
  "varer_module.R",
  encoding = "UTF-8"
)
indkobsseddel_module_lines <- readLines(
  "indkobsseddel_module.R",
  encoding = "UTF-8"
)
stopifnot(
  !any(grepl(
    "(read|write)\\.csv\\(",
    c(
      app_lines,
      varer_module_lines,
      indkobsseddel_module_lines
    )
  )),
  any(grepl(
    'source\\("\\./basis_varer_store\\.R"\\)',
    app_lines
  )),
  any(grepl(
    'source\\("\\./basis_varer_state\\.R"\\)',
    app_lines
  )),
  sum(grepl(
    "create_basis_varer_state[[:space:]]*\\(",
    app_lines
  )) == 1L,
  !any(grepl(
    "basis_varer_store_(read|revision|commit)[[:space:]]*\\(",
    app_lines
  )),
  !any(grepl(
    paste(
      "initial_basis_varer_store|rv_basisVarerStore|rv_varer_custom|",
      "publish_basis_varer_store|commit_basis_varer_change",
      sep = ""
    ),
    app_lines
  )),
  any(grepl(
    'source\\("\\./indkobsseddel_module\\.R"\\)',
    app_lines
  )),
  any(grepl(
    "mod_indkobsseddel_server",
    app_lines
  )),
  any(grepl(
    'ns\\("catalog_item"\\)',
    indkobsseddel_module_lines
  )),
  any(grepl(
    'observeEvent\\(input\\$add_catalog_item',
    indkobsseddel_module_lines
  )),
  !any(grepl(
    'observeEvent\\(input\\$add_catalog_item',
    app_lines
  ))
)

message(
  paste(
    "Varemodulet gemmer gennem basisvare-store, håndterer konflikt/retry",
    "og indkøbsseddelmodulets Liste-knap bruger fortsat det fælles katalog."
  )
)
