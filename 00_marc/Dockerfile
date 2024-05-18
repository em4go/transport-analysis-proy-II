# Usa la imagen base de R con Shiny, compatible con Apple Silicon
FROM rocker/shiny:4.1.2

#Definimos el directorio de trabajo
WORKDIR /app_shiny

#RUN R install.packages("shiny")

COPY ./app_shiny /app_shiny

# Exponer el puerto 3838 para la aplicación Shiny
EXPOSE 3838

#RUN R -e "install.packages('shiny')"

# Copia la aplicación Shiny en el contenedor
#COPY app.R /srv/shiny-server/


# Iniciar el servidor Shiny
CMD ["./app_shiny/runApp.sh"]

#Para 