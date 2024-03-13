library(osmextract)

# Define the place
place <- "Spain"

# Download the data from Geofabrik
oe_download(place, provider = "geofabrik")

# Read the boundaries
boundaries <- oe_read(place, layer = "admin")

# Filter the boundaries for Valencia
valencia_boundaries <- boundaries[boundaries$name == "Valencia", ]

# Print the result
print(valencia_boundaries)




library(osmextract)

# Define the path to the file
file_path <- "00_ferran/valencia-latest.osm.pbf"

# Read the boundaries
boundaries <- oe_read(file_path, layer = "multipolygons")


admin_bound <- boundaries[boundaries$boundary == 'administrative', ]

valencia_boundaries <- admin_bound[admin_bound$name == "Ciutat Vella", ]
