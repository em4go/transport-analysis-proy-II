import osmnx as ox

cities = ["Valencia", "Madrid", "Barcelona"]
cities = ["Madrid", "Barcelona"]
network_types = ["bike", "drive", "walk"]

for city in cities:
    for network_type in network_types:
        place = f"{city}, Spain"
        G = ox.graph_from_place(place, network_type=network_type)
        ox.save_graphml(
            G, f"./data/networks_data/{city.lower()}_{network_type}.graphml"
        )
