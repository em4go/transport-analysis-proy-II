library(dplyr)
library(arrow)
library(tidyr)

data <- read.csv2('./parkings/parkings.csv', header = TRUE, sep = ';')

parkings <- data %>%
  select(-c("Adreça...Direccion", "Portal", "Tipus...Tipo", "Places.lliures...Plazas.Libres", "Data.Última.Modificació...Fecha.Ultima.Modificación", "objectid", "ocupacion"))

parkings <- separate(parkings, geo_point_2d, into = c("Latitude", "Longitude"), sep = ",")

new_names <- c("Name", "Id", "Total parking spaces", "Geo_shape", "Latitude", "Longitude")
colnames(parkings) <- new_names

write.csv(parkings, "./parkings/dataParkings.csv")
write_parquet(parkings, "./parkings/dataParkings.parquet")

