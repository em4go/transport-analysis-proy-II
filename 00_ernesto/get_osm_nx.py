import osmnx as ox

# get the street network for Valencia's bounding box

# define a bounding box in Valencia
north, south, east, west = 39.5, 39.45, -0.35, -0.4
bbox = (north, south, east, west)

place = "Valencia, Spain"

# create a network from that bounding box
G = ox.graph_from_place(place, network_type='walk')

G

len(G.nodes())

ox.save_graphml(G, 'valencia_walk.graphml')
