tibble::tribble(
  ~place_id,              ~name,       ~lat,        ~lng,
  "ChIJoUdErx1ipY4RZOhn3D3Ykv0",   "Down Town Center", 18.4509765, -69.9527804,
  "ChIJ7-Ebog5ipY4RvDafuTLCFSU",   "Bella Vista Mall", 18.4571034, -69.9374813,
  "ChIJ3yQ4KtljpY4RZPn22BSJJUs",      "Plaza Central", 18.4635493, -69.9347652,
  "ChIJtSqa4-SJr44RykDt6mQH35g",          "Blue Mall", 18.4726717, -69.9409414,
  "ChIJgdpIUeyJr44RkGowJkvdldc",         "Agora Mall", 18.4837096, -69.9394001,
  "ChIJIY62EcqJr44RTIWW-SQdydc",             "Sambil", 18.4822353, -69.9119431,
  "ChIJ5eDc1LmHr44RI3YlcZjqShs",         "Coral Mall", 18.4866968, -69.8323177,
  "ChIJ-S_I2SuGr44RNC6atgHJwds",         "Megacentro", 18.5073011, -69.8565263
) |>
  saveRDS("app/data/centros_comerciales.rds")
