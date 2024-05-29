# imagen base de R con la versión correcta de Shiny
FROM rocker/shiny:4.3.2

# librerías necesarias para sf en linux
RUN apt-get -y update && apt-get install -y  libudunits2-dev libgdal-dev libgeos-dev libproj-dev

# librerías R
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('leaflet')"
RUN R -e "install.packages('shinythemes')"
RUN R -e "install.packages('sf')"
RUN R -e "install.packages('arrow')"


# directorio de trabajo
WORKDIR /app_shiny
# copia de la apliación
COPY ./app_shiny /app_shiny

# exponer el puerto 3838
EXPOSE 3838

# punto de entrada
CMD R -e "shiny::runApp('./app1.R', port = 3838)"
