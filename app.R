# ShinyMobile-skal for GroceryApp
library(shiny)
library(shinyMobile)

ui <- f7Page(
  title = "GroceryApp",
  options = list(theme = "auto"),
  f7TabLayout(
    navbar = f7Navbar(title = "GroceryApp"),
    f7Tabs(
      animated = FALSE,
      swipeable = TRUE,
      f7Tab(
        tabName = "Indkøbsseddel",
        icon = f7Icon("cart"),
        active = TRUE,
        f7BlockTitle(title = "Indhold kommer snart"),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          "Her kan du senere bygge funktionaliteten til indkøbssedlen."
        )
      ),
      f7Tab(
        tabName = "Varer",
        icon = f7Icon("cube"),
        f7BlockTitle(title = "Indhold kommer snart"),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          "Her kan du senere arbejde med dine varer."
        )
      ),
      f7Tab(
        tabName = "Opskrifter",
        icon = f7Icon("book"),
        f7BlockTitle(title = "Indhold kommer snart"),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          "Her kan du senere tilføje og se opskrifter."
        )
      ),
      f7Tab(
        tabName = "Inspiration",
        icon = f7Icon("sparkles"),
        f7BlockTitle(title = "Indhold kommer snart"),
        f7Block(
          inset = TRUE,
          strong = TRUE,
          "Her kan du senere samle idéer og inspiration."
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Logik tilføjes senere
}

shinyApp(ui = ui, server = server)