vehiculos <- tibble::tribble(
            ~vehiculo, ~rendimiento, ~tarifa_pkm,
          "Camioneta",           20,        300,
    "Cambión pequeño",           16,        450,
  "Caminón de 3 ejes",           12,        600,
  "Caminón de 5 ejes",            9,        670
)

saveRDS(vehiculos, "app/data/vehiculos.rds")
