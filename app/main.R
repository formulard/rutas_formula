box::use(
  shiny,
  bslib,
  shinyWidgets[pickerInput],
  googleway,
  bsicons[bs_icon]
)

map_key <- Sys.getenv("API_KEY")

box::use(
  app/logic/app_components[header],
  app/logic/places,
  app/logic/places_helpers
)

centros_comerciales <- readRDS("app/data/centros_comerciales.rds") |>
  mutate(location = map2(lat, lng, ~c(.x, .y))) |>
  as_tibble()

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  bslib$page_sidebar(
    theme = bslib$bs_theme(
      version = "5",
      navbar_bg = "#FFF",
      base_font = bslib$font_google("Poppins"),
      font_scale = 0.9
    ),
    title = header("static/logo.png", "| Coordinador de rutas", icon_width = "200px"),
    sidebar = bslib$sidebar(
      shiny$selectInput(
        ns("origen"),
        labe = "Origen",
        choices = centros_comerciales$name,
        selected = "Downtown Center"
      ),
      shiny$selectInput(
        ns("destino"),
        labe = "Destino",
        choices = centros_comerciales$name,
        selected = "Coral Mall"
      ),
      pickerInput(
        ns("paradas"),
        "Paradas intermedias",
        choices = NULL,
        multiple = TRUE,
        options = list(`selected-text-format` = "count > 1")
      ),
      shiny$selectInput(
        ns("tipo_vehiculo"),
        "Tipo de vehículo",
        choices = c(
          "Camioneta",
          "Cambión pequeño",
          "Caminón de 3 ejes",
          "Caminón de 5 ejes"
        ) 
      ),
      shiny$actionButton(ns("compute"), "Estimar ruta", class = "primary")
    ),
    bslib$layout_columns(
      bslib$value_box(
        title = "Tarifa estimada",
        value = shiny$textOutput("tarifa"),
        showcase = bs_icon("cash-stack")
      ),
      bslib$value_box(
        title = "Distancia",
        value = shiny$textOutput("distancia"),
        showcase = bs_icon("Speedometer2")
      ),
      bslib$value_box(
        title = "Tiempo de conducción",
        value = shiny$textOutput("tiempo"),
        showcase = bs_icon("stopwatch")
      ),
      bslib$value_box(
        title = "Paradas",
        value = shiny$textOutput("paradas"),
        showcase = bs_icon("sign-stop")
      ),
      bslib$value_box(
        title = "Combustible",
        value = shiny$textOutput("combustible"), showcase = bs_icon("fuel-pump")),
      fill = FALSE,
      height = "100px"
    ),
    bslib$card(
      full_screen = TRUE,
      googleway::google_mapOutput(ns("map"), height = "100%")
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    paradas_intermedias <- shiny$reactiveVal()
    shiny$observeEvent(c(input$origen, input$destino), {
      filtered <- centros_comerciales |>
        dplyr::filter(!name %in% c(input$origen, input$destino))
      
      shinyWidgets::updatePickerInput(
        session,
        "paradas",
        choices = filtered$name,
      )
    })
    
    output$map <- googleway::renderGoogle_map({
      googleway::google_map(
        key = map_key,
        location = c(18.45, -69.95),
        zoom = 10, map_type_control = FALSE
      )
    })
    routes <- shiny$eventReactive(input$compute, {
      index_destinos <- which(centros_comerciales %in% c(input$origen, input$paradas, input$destino))
      selected <- centros_comerciales[destinos, index_destinos]
      
      selected 
    )
    
    })
  })
}
