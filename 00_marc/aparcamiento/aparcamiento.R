library(dplyr)
library(arrow)

data <- read.csv('./parkings/aparcamientoDistrito.csv', header = TRUE, sep = ',')

df<- head(data, 19)

colnames(df)

aparcamiento <-df %>% select(-c("X_id", "GRUP", "DISTRICTE", "BARRI", "LLIURES", "ORA", "GUALS", "PÀRQUINGS", "SOLARS", "ALTRES", "HABITANTS.GRUP", "HABITANTS.HABITANTS.20.70"))

colnames(aparcamiento)

new_names <- c(
  "Residents",
  "Residents.20.70",
  "Total",
  "Tourism",
  "Places.Residents",
  "Places.Residents.20.70",
  "Places.Tourism",
  "Residents.Tourism",
  "Residents.20.70.Tourism"
)

colnames(aparcamiento) <- new_names

write.csv(parkings, "./parkings/dataPlazasAparcamiento.csv")
write_parquet(parkings, "./parkings/dataPlazasAparcamiento.parquet")
