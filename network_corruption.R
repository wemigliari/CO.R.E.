library(igraph)
library(readxl)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(extrafont)
library(RColorBrewer)
library(ggrepel)
library(reshape2)
library(xlsx)
library(dplyr)

corruption <- read.xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/notes_file_procurement_transparency.xlsx",
                                      sheetName ="WP-Compendium")

sources <- corruption %>%
  distinct(source) %>%
  rename(label = source)

destinations <- corruption %>%
  distinct(destination) %>%
  rename(label = destination)

nodes <- full_join(sources, destinations, by = "label")
nodes

library(tibble)

nodes <- nodes %>% rowid_to_column("id")
nodes


per_route <- corruption %>%  
  group_by(source, destination) %>%
  summarise(weight = n()) %>% 
  ungroup()
per_route


edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)
edges

library(network)

routes_network <- network(edges, vertex.attr = nodes, 
                          matrix.type = "edgelist", ignore.eval = FALSE)
class(routes_network)
routes_network


plot(routes_network, vertex.cex = 3)
plot(routes_network, vertex.cex = 3, mode = "circle")


detach(package:network)
rm(routes_network)
library(igraph)


routes_igraph <- graph_from_data_frame(d = edges, 
                   vertices = nodes, 
                   directed = TRUE)

V(routes_igraph)$label.cex[1:20 %% 2 == 0] = 0.5

plot(routes_igraph, edge.arrow.size = 0.2)
plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)


library(tidygraph)
library(ggraph)


routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph() 

ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(
    aes(width = weight),
    arrow = grid::arrow(type = "closed", length = unit(0.03, "inches")),
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(0.1, 1)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Corruption") +
  theme_graph()

ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Corruption") +
  theme_graph()


library(networkD3)
nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)


my_color <- 'd3.scaleOrdinal() .domain(["a", "b"]) .range(["#69b3a2", "steelblue"])'

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, 
              Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", 
              fontSize = 13, unit = "Corruption")










