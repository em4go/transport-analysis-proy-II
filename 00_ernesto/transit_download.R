df <- read.csv("./data/gtfs_data/sources.csv")
library(dplyr)

df_spain <- df %>% filter(location.country_code == "ES")

df_filtered <- df %>% filter(
  location.subdivision_name == "Valenciana, Comunidad" | 
  location.subdivision_name == "Valenciana, Comunitat" |
  location.subdivision_name == "Madrid" |
  location.subdivision_name == "Barcelona")

df_filtered
