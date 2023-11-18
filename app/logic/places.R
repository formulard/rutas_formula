#' Get place location
#'
#' Use google location API to get a place coodinates using a query. Queries
#' should be as complete as possible to get accurate results.
#'
#' @param query a string to search
#' @param key your google api key. Can be saved in you .Renviron with the name
#' API_KEY
#'
#' @return a data frame
#' @export
get_place_location <- function(query, key = Sys.getenv("API_KEY")) {
  place <- googleway::google_find_place(query, key = key)
  
  details <- place$candidates |>
    dplyr::select(place_id, name)
  
  location <- place$candidates$geometry$location
  
  dplyr::bind_cols(details, location)
}

#' Get time and distance between two locations
#'
#' Use google distance matrix API to fecth the time and distance between two
#' places
#'
#' @param origin data frame with `lat` and `lng` coordinates
#' @param destination data frame with `lat` and `lng` coordinates
#' @param mode string with the travel mode. "driving" by default
#' @param ... other parameters for google_distance from googleway package
#'
#' @return a data frame
#' @export
fetch_distance_and_duration <- function(
    origin,
    destination,
    ...,
    mode = "driving",
    key = Sys.getenv("API_KEY")
) {
  response <- googleway::google_distance(
    origins = dplyr::select(origin, lat, lng),
    destinations = dplyr::select(destination, lat, lng),
    mode = mode,
    key = key,
    ...
  )
  
  distance <- response$rows$elements[[1]]$distance |>
    purrr::set_names(c("distance_km", "distance_metters"))
  
  time <- response$rows$elements[[1]]$duration |>
    dplyr::transmute(
      period = lubridate::seconds_to_period(value),
      time_secons = value
    )
  
  dplyr::bind_cols(distance, time)
}