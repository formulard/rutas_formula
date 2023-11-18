#' Convert lat long vectors to data frame
#'
#' Some of the googleway functions takes location as vector and others as data
#' frame. this function helps converting from vector to list
#'
#' @param location a vector with `lat` and `lon`, in that order
#'
#' @return a data frame
#' @export
location_vector_to_df <- function(location) {
  location |>
    as.list() |>
    purrr::set_names(c("lat", "lng")) |>
    as.data.frame()
}
