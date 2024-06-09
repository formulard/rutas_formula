box::use(
  shiny,
  leaflet,
  reactable,
  waiter,
  dplyr,
)

box::use(
  app/logic/places[get_place_location]
)

map_key <- config::get(file = "google_api_config.yml", value = "map_key")

ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$div(
      style = "display: flex; gap: 5px; align-items: center;",
      shiny$textInput(
        ns("query"),
        label = NULL,
        placeholder = "Término de búsqueda, lo más específico posible",
        width = "500px"
      ),
      shiny$actionButton(
        ns("search"),
        "Buscar",
        icon = shiny$icon("search"),
        style = "margin-bottom: 15px;"
      )
    ),
    leaflet$leafletOutput(ns("map")),
    reactable$reactableOutput(ns("table"))
  )
}

server <- function(id, nueva_ubicacion) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    spinner <- waiter$Waiter$new(
      id = ns("map"),
      html = waiter$spin_3(),
      color = waiter$transparent(0.6)
    )

    output$map <- leaflet$renderLeaflet({
      leaflet$leaflet() |>
        leaflet$addTiles() |>
        leaflet$addProviderTiles(leaflet$providers$CartoDB.Positron) |>
        leaflet$setView(lng = -69.95, lat = 18.45, zoom = 12)
    })

    new_places <- shiny$eventReactive(input$search, {
      shiny$req(input$query)
      spinner$show()

      get_place_location(input$query, key = map_key)
    })

    shiny$observeEvent(new_places(), {
      on.exit(spinner$hide())

      lat <- new_places()[["lat"]][1]
      lng <- new_places()[["lng"]][1]

      leaflet::leafletProxy(ns("map")) |>
        leaflet::clearMarkers() |>
        leaflet$setView(lng = lng, lat = lat, zoom = 15) |>
        leaflet::addMarkers(data = new_places(), label = ~name)

    }, ignoreNULL = TRUE)

    output$table <- reactable$renderReactable({
      shiny$req(nrow(new_places()) > 1)

      new_places() |>
        dplyr$select(name, lat, lng) |>
        reactable$reactable(
          selection = "single",
          onClick = "select",
          columns = list(
            name = reactable$colDef(name = "Nombre"),
            lat = reactable$colDef(format = reactable$colFormat(digits = 2)),
            lng = reactable$colDef(format = reactable$colFormat(digits = 2))
          )
        )
    })
    
    selected <- shiny$reactive({
      reactable$getReactableState("table", "selected")
    })

    selected_place <- shiny$reactive({
      shiny$req(new_places())

      if (nrow(new_places()) == 1) {
        return(new_places())
      }
      
     shiny$req(selected())

     new_places()[selected(), ]
    })

    shiny$observe({
      shiny$req(selected_place())
      nueva_ubicacion(selected_place())
    })
  })
}
