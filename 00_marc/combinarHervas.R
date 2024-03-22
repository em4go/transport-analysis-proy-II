library(arrow)
library(stringi)

plazas_mob <- read_parquet('./parkings/dataTratada/plazas_mobred_dist.parquet')
plazas_mob

distritos <- read.csv2('./GitHub/transport-analysis-proy-II/00_hervas/Distritos_pob.csv', sep = ';')
distritos

info_plazas <- read.csv('./parkings/dataTratada/dataPlazasAparcamiento.csv')
info_plazas 

parkings <- read.csv('./GitHub/transport-analysis-proy-II/00_marc/dataTratada/dataParkings_por_dist.csv')
parkings

# Seleccionamos los distritos que nos interesan
filtrados <- distritos[2:20,]

#Eliminamos el ". " de la columna X (disrito)
filtrados$X <- sapply(strsplit(filtrados$X, "\\. "), function(x) x[2])
filtrados$X

filtrados<- filtrados[order(filtrados$X), ]
filtrados$X

dist <- toupper(stri_trans_general(filtrados$X, "Latin-ASCII"))
dist

info_plazas <- info_plazas[order(info_plazas$Disrtict), ]
info_plazas$Disrtict

filtrados$plazas_mob <- plazas_mob$n_plazas
filtrados$total_plazas <- info_plazas$Total
filtrados$turismos <- info_plazas$Tourism

filtrados$distritos <- dist

parkings <- parkings[,2:3]
parkings

info <- merge(filtrados, parkings, by.x = "distritos", by.y = "districto", all.x = TRUE)
info

info <- info[,setdiff(colnames(info), c("X"))]
info

write.csv(info, "./GitHub/transport-analysis-proy-II/00_marc/dataTratada/info_dist.csv")
write_parquet(info, "./GitHub/transport-analysis-proy-II/00_marc/dataTratada/info_dist.parquet")
