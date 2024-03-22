library(arrow)

plazas_mob <- read_parquet('./parkings/dataTratada/plazas_mobred_dist.parquet')
plazas_mob

distritos <- read.csv2('./GitHub/transport-analysis-proy-II/00_hervas/Distritos_pob.csv', sep = ';')
distritos

info_plazas <- read.csv('./parkings/dataTratada/dataPlazasAparcamiento.csv')
info_plazas 

# Seleccionamos los distritos que nos interesan
filtrados <- distritos[2:20,]

#Eliminamos el ". " de la columna X (disrito)
filtrados$X <- sapply(strsplit(filtrados$X, "\\. "), function(x) x[2])
filtrados$X

# Ordenamos los distritos
filtrados<- filtrados[order(filtrados$X), ]
filtrados$X

info_plazas <- info_plazas[order(info_plazas$Disrtict), ]
info_plazas$Disrtict

filtrados$plazas_mob <- plazas_mob$n_plazas

filtrados$total_plazas <- info_plazas$Total
filtrados$turismos <- info_plazas$Tourism

filtrados

colnames(filtrados)[colnames(filtrados) == "X"] <- "district"

write.csv(filtrados, "./parkings/dataTratada/info_dist.csv")
write_parquet(filtrados, "./parkings/dataTratada/info_dist.parquet")
