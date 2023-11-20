box::use(
  shiny,
  bslib,
  shinyWidgets[pickerInput],
  googleway,
  bsicons[bs_icon],
  dplyr[mutate, as_tibble, slice, lag, select],
  purrr[map2, map2_chr],
  tidyr[unnest],
  googleway[
    google_directions,
    direction_polyline
  ],
  shinyjs[useShinyjs, hidden, showElement],
  waiter[Waiter, useWaiter, spin_3, transparent]
)

#map_key <- Sys.getenv("API_KEY")
map_key <- "AIzaSyBfC5S5hAYVWKbjn-ySyxjBBOXwYk6_lFc"

box::use(
  app/logic/app_components[header],
  app/logic/places,
  app/logic/places_helpers[location_vector_to_df],
  app/logic/indicadores[calcular_indicadores],
  app/view/agregar_ubicacion
)

centros_comerciales <- readRDS("app/data/centros_comerciales.rds") |>
  mutate(location = map2(lat, lng, ~c(.x, .y))) |>
  as_tibble()

vehiculos <- readRDS("app/data/vehiculos.rds")

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
    useShinyjs(),
    useWaiter(),
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
      shiny$actionButton(
        ns("agregar"),
        "Agregar destino",
        icon = shiny$icon("location-dot"),
        class = "modal-opener-btn"
      ),
      shiny$selectInput(
        ns("vehiculo"),
        "Tipo de vehículo",
        choices = vehiculos$vehiculo
      ),
      shiny$actionButton(ns("compute"), "Estimar ruta", class = "btn-primary")
    ),
    bslib$layout_columns(
      id = "indicadores",
      bslib$value_box(
        title = "Tarifa estimada",
        value = shiny$textOutput(ns("tarifa")),
        showcase = bs_icon("cash-stack")
      ),
      bslib$value_box(
        title = "Distancia",
        value = shiny$textOutput(ns("distancia")),
        showcase = bs_icon("Speedometer2")
      ),
      bslib$value_box(
        title = "Tiempo de conducción",
        value = shiny$textOutput(ns("tiempo")),
        showcase = bs_icon("stopwatch")
      ),
      bslib$value_box(
        title = "Paradas",
        value = shiny$textOutput(ns("paradas")),
        showcase = bs_icon("sign-stop")
      ),
      bslib$value_box(
        title = "Combustible",
        value = shiny$textOutput(ns("combustible")),
        showcase = bs_icon("fuel-pump")
      ),
      fill = FALSE,
      height = "100px"
    ) |>
      hidden(),
    bslib$card(
      full_screen = TRUE,
      googleway::google_mapOutput(ns("map"), height = "100%")
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    centros_comerciales <- shiny$reactiveVal(centros_comerciales)
    nueva_ubicacion <- shiny$reactiveVal()

    waiter <- Waiter$new(html = spin_3(), color = waiter::transparent(0.7))

    shiny$observeEvent(c(input$origen, input$destino), {
      filtered <- centros_comerciales() |>
        dplyr::filter(!name %in% c(input$origen, input$destino))

      selected <- input$selected

      shinyWidgets::updatePickerInput(
        session,
        "paradas",
        choices = filtered$name,
        selected = selected
      )
    })

    shiny$observeEvent(centros_comerciales(), {
      filtered <- centros_comerciales() |>
        dplyr::filter(!name %in% c(input$origen, input$destino))

      origen <- input$origen
      destino <- input$destino
      paradas <- input$paradas

      shiny$updateSelectInput(
        inputId = "origen",
        choices = centros_comerciales()$name,
        selected = origen
      )

      shiny$updateSelectInput(
        inputId = "destino",
        choices = centros_comerciales()$name,
        selected = destino
      )

      shinyWidgets::updatePickerInput(
        session,
        "paradas",
        choices = filtered$name,
        selected = paradas
      )
    }, ignoreNULL = TRUE)

    output$map <- googleway::renderGoogle_map({
      googleway::google_map(
        key = map_key,
        location = c(18.45, -69.95),
        zoom = 10, map_type_control = FALSE
      ) |>
        googleway::add_traffic()
    })

    selected_places <- shiny$eventReactive(input$compute, {
      index_destinos <- which(
        centros_comerciales()$name %in% c(input$origen, input$paradas, input$destino)
        )
      centros_comerciales()[index_destinos, ]
    })

    routes <- shiny$eventReactive(selected_places(), {
      waiter$show()
      selected_places() |>
        dplyr::bind_rows(dplyr::slice(selected_places(), 1)) |>
        mutate(
          destination = lag(location, default = NA)
        ) |>
        slice(-1) |>
        mutate(
          polyline = map2_chr(
            location,
            destination,
            ~google_directions(.x, .y, key = map_key) |> direction_polyline()
          ),
          distance_duration = map2(
            location,
            destination,
            \(origen, destino) {
              origen <- location_vector_to_df(origen)
              destino <- location_vector_to_df(destino)

              places$fetch_distance_and_duration(origen, destino, key = map_key)
            }
          )
        ) |>
        unnest(distance_duration)
    }, ignoreNULL = TRUE)

    parametros <- shiny$reactive({
      shiny$req(routes(), selected_places())
      calcular_indicadores(routes(), input$vehiculo, input$paradas)
    })

    shiny$observeEvent(routes(), {
      showElement(id = "indicadores", asis = TRUE)
      on.exit(waiter$hide())

      googleway::google_map_update(ns("map")) |>
        googleway::clear_polylines() |>
        googleway::clear_markers() |>
        googleway::add_markers(data = selected_places()) |>
        googleway::add_polylines(
          data = dplyr::select(routes(), polyline),
          polyline = "polyline",
          stroke_weight = 5
        )
    }, ignoreNULL = TRUE)

    output$distancia <- shiny$renderText({
      shiny$req(parametros())
      scales::comma(parametros()$distancia / 1000, accuracy = 0.1)
    })

    output$tiempo <- shiny$renderText({
      shiny$req(parametros())
      as.character(parametros()$tiempo)
    })

    output$paradas <- shiny$renderText({
      shiny$req(parametros())
      length(input$paradas)
    })

    output$combustible <- shiny$renderText({
      shiny$req(parametros())
      parametros()$galones |>
        round(1)
    })

    output$tarifa <- shiny$renderText({
      shiny$req(parametros())
      parametros()$tarifa |>
        scales::comma(prefix = "RD$ ")
    })

    shiny$observeEvent(input$agregar, {
      shiny$showModal(
        shiny$modalDialog(
          agregar_ubicacion$ui(ns("modal")),
          size = "xl",
          easyClose = TRUE,
          footer = shiny$actionButton(ns("add_confirmation"), "Agregar")
        )
      )

      shinyjs::runjs("google.maps.event.trigger(map, 'resize')")
      agregar_ubicacion$server("modal", nueva_ubicacion)
    })

    shiny$observeEvent(input$add_confirmation, {
      shiny$req(nueva_ubicacion())

      old_places <- centros_comerciales()

      new_places_list <- dplyr::bind_rows(
        old_places,
        mutate(nueva_ubicacion(), location = list(c(lat, lng)))
      )
      centros_comerciales(new_places_list)
      shiny$removeModal()
    })
  })
}
