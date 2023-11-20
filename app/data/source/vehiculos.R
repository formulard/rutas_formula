vehiculos <- tibble::tribble(
            ~vehiculo, ~rendimiento, ~tarifa_pkm,
          "Camioneta",           20,        2000,
    "Cambión pequeño",           16,        2600,
  "Caminón de 3 ejes",           12,        3000,
  "Caminón de 5 ejes",            9,        3350
)

saveRDS(vehiculos, "app/data/vehiculos.rds")
