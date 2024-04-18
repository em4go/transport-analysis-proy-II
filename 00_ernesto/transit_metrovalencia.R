
.lib<- c("dplyr", "leaflet", "ggplot2", "tidytransit", "sf")

.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst])
lapply(.lib, require, character.only=TRUE)

gtfs <- read_gtfs("data/gtfs_data/transit_metrovalencia.zip")

names(gtfs)

head(gtfs$.$dates_services)

head(gtfs$stops)

head(gtfs$routes)


head(gtfs$stop_times)

gtfs$calendar

gtfs$calendar$service_id <- as.factor(gtfs$calendar$service_id)

levels(gtfs$calendar$service_id)

head(gtfs$trips)

gtfs$trips$service_id <- as.factor(gtfs$trips$service_id)

levels(gtfs$trips$service_id)

gtfs$calendar_dates

# get the most frequent service_id

gtfs$trips %>%
  group_by(service_id) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

# get the frequenct for routes

gtfs$trips %>%
  group_by(route_id) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

gtfs$routes %>%
  filter(route_id == "V10-190-197")

# merge the routes with the trips

yes <- gtfs$trips %>%
  inner_join(gtfs$routes, by = "route_id")

yes %>%
  filter(route_color == "CE142B")

yes %>%
  group_by(route_color) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency)) %>%
  print(n = 1000)

yes %>%
  filter(trip_id == "2965738") %>%
  print(n = 1000)

# get the stops for each route

gtfs$stop_times %>%
  print(n = 100)

gtfs$stops %>%
  filter(stop_id == "89")

# get the freq for benimaclet station

benimaclet_id <- 12

stop_times_at_stop <- gtfs$stop_times %>%
  filter(stop_id == benimaclet_id)

trips_for_stop <- gtfs$trips %>%
  filter(trip_id %in% stop_times_at_stop$trip_id)

full_schedule <- stop_times_at_stop %>%
  inner_join(trips_for_stop, by = "trip_id")

full_schedule$arrival_time <- strptime(full_schedule$arrival_time, format = "%H:%M:%S")

full_schedule$hour_of_day <- format(full_schedule$arrival_time, "%H")

frequency_by_hour <- full_schedule %>%
  group_by(hour_of_day) %>%
  summarise(frequency = n())

frequency_by_hour

ggplot(frequency_by_hour, aes(x = hour_of_day, y = frequency)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Frecuencia de Metros por Hora",
       x = "Hora del Día",
       y = "Frecuencia")
