import osmnx as ox

place = "Valencia, Spain"

# create a network from that bounding box
G = ox.graph_from_place(place, network_type="drive")

print(len(G.nodes), len(G.edges))

ox.save_graphml(G, "valencia_drive.graphml")
