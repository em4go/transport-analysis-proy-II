

plazas_mob <- read.csv('./parkings/dataTratada/plazas_mobred_geo.csv', header = TRUE, sep = ',')
plazas_mob

plazas_ora <- read.csv('./parkings/dataPre/aparcaments-ora-aparcamientos-ora.csv', header = TRUE, sep = ';')

plazas_ora$Color
unique(plazas_ora$Color)
table(plazas_ora$Color)

