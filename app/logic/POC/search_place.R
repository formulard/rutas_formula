library(shiny)
library(googleway)
library(reactable)
library(shinyjs)

box::use(
  app/logic/places[get_place_location]
)

centros_comerciales <- readRDS("app/data/centros_comerciales.rds")

"Plaza Colina Centro Villa Mella"

map_key <- Sys.getenv("API_KEY")

ui <- fluidPage(
  useShinyjs(),
  div(
    style = "display: flex; gap: 5px; align-items: center;",
    textInput(
      "query",
      "Buscar destino",
      placeholder = "Nombre del lugar y otros detalles de su ubicación",
      width = 400
    ),
    actionButton("search", "Buscar", class = "btn-primary")
  ),
  reactableOutput("table"),
  google_mapOutput("map"),
  disabled(actionButton("agregar", "Agregar", class = "btn-primary"))
)

server <- function(input, output, session) {
  places <- reactiveVal(centros_comerciales)

  output$map <- renderGoogle_map({
    googleway::google_map(
      key = map_key,
      location = c(18.45, -69.95),
      zoom = 10, map_type_control = FALSE
    ) |>
      googleway::add_traffic()
  })

  new_places <- eventReactive(input$search, {
    req(input$query)
    get_place_location(input$query)
  })

  selected <- reactive(getReactableState("table", "selected"))

  selected_place <- reactive({
    req(new_places())

    if (nrow(new_places()) == 1) {
      enable(id = "agregar")
      return(new_places())
    }
    req(selected())
    enable("agregar")
    new_places()[selected(), ]
  })

  output$table <- renderReactable({
    req(nrow(new_places()) > 1)

    new_places() |>
      reactable(selection = "single", onClick = "select")
  })

  observe(req(selected_place()))

  observeEvent(input$agregar, {
    req(selected_place())
    old_places <- places()
    new_places_list <- dplyr::bind_rows(
      old_places,
      selected_place()
    )
    places(new_places_list)
  })

  observeEvent(new_places(), {
    google_map_update("map") |>
      clear_markers() |>
      add_markers(data = new_places())
  }, ignoreNULL = TRUE)
}

shinyApp(ui, server)
