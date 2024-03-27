library(osmdata)

# Get all the tags available in the OSM database from differente categories
food <- c("bar", "biergarten", "cafe", "fast_food", "food_court", "ice_cream", "pub", "restaurant")
food <- data.frame(Key = "amenity", Value = food) # Create a data.frame for food with the columns Key and Value

sports <- c("fitness_centre", "fitness_station", "sports_centre", "swimming_pool", "pitch", "stadium")
sports <- data.frame(Key = "leisure", Value = sports) # Create a data.frame for sports with the columns Key and Value
sports2 <-  available_tags(feature="sport")

sports <- rbind(sports, sports2) # Combine sports and sports2

monuments <- available_tags(feature="historic")

shops <- c("department_store", "general", "kiosk", "mall", "supermarket", "wholesale")
shops <- data.frame(Key = "shop", Value = shops) # Create a data.frame for shops with the columns Key and Value

vlc_bb <- getbb("Valencia, Spain")


# Example of getting 
bar <- vlc_bb %>%
  opq() %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf() -> food_vlc
bar

names(bar$osm_points)
print(bar$osm_points$geometry$'11666267669')

# Should get data for all of the categories mentioned before
# Should storage for each category the ubication for each point

