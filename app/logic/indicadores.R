vehiculos <- readRDS("app/data/vehiculos.rds")

#' @export
calcular_indicadores <- function(rutas, tipo_vehiculo, paradas) {
  distancia <- rutas |>
    dplyr::pull(distance_metters) |>
    sum()
  
  tiempo <- rutas |>
    dplyr::pull(time_secons) |>
    sum() |>
    lubridate::seconds_to_period()
  
  distancia <- rutas |>
    dplyr::pull(distance_metters) |>
    sum()
  
  paradas <- length(paradas)
  
  by_vehiculo <- vehiculos |>
    dplyr::filter(vehiculo == tipo_vehiculo) |>
    dplyr::mutate(
      galones = distancia / 1000  / rendimiento,
      tarifa = (tarifa_pkm * distancia / 1000) + paradas * 500
    )
  
  list(
    distancia = distancia,
    tiempo = tiempo,
    paradas = paradas,
    tarifa = by_vehiculo$tarifa,
    galones = by_vehiculo$galones
  )
}
