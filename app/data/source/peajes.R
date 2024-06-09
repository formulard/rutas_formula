locations <- centros_comerciales |>
  dplyr::mutate(
    location = purrr::map2(lat, lng, ~c(.x, .y))
  ) 
response <- googleway::google_directions(
  locations[["location"]][[1]],
  locations[["location"]][[8]],
  key = Sys.getenv("API_KEY"),
  alternatives = TRUE
)


googleway::direction_polyline(response) 

googleway::direction_routes(response)

response$routes$waypoint_order

resp <- googleway::google_places("peaje", key = Sys.getenv("API_KEY"))

second <- googleway::google_places("peaje", key = Sys.getenv("API_KEY"), page_token = resp$next_page_token)

dplyr::bind_rows(resp$results, second$results) |>
  dplyr::as_tibble() |>
  dplyr::select(formatted_address, geometry, name, place_id, reference) |>
  View(
  )
