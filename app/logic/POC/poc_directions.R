library(googleway)
library(dplyr)
library(purrr)

box::use(
  app/logic/places,
  app/logic/indicadores[calcular_indicadores]
)

map_key <- Sys.getenv("API_KEY")

centros_comerciales <- readRDS("app/data/centros_comerciales.rds") |>
  mutate(location = map2(lat, lng, ~c(.x, .y))) |>
  as_tibble()

vehiculos <- readRDS("app/data/vehiculos.rds")

location_vector_to_df <- function(location) {
  location |>
    as.list() |>
    purrr::set_names(c("lat", "lng")) |>
    as.data.frame()
}

rutas <- centros_comerciales |>
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

        places$fetch_distance_and_duration(origen, destino)
      }
    )
  ) |>
  tidyr::unnest(distance_duration)

calcular_indicadores(rutas, "Camioneta", paradas = c("Megacentro"))

google_map(key = map_key) %>%
  add_polylines(
    data = select(rutas, polyline),
    polyline = "polyline",
    stroke_weight = 5
  ) |>
  add_markers(data = centros_comerciales)
