library(igraph)
library(readxl)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(extrafont)
library(RColorBrewer)
library(ggrepel)
library(xlsx)
library(dplyr)


countries <- read.xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/notes_file_procurement_transparency.xlsx",
                       sheetName ="Network 2")

library(reshape2)
test <- melt(countries, variable.name = "values", value.name = "weight")


corruption <- read.xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/notes_file_procurement_transparency.xlsx",
                        sheetName ="WP-Compendium")
corruption$destination2 <- NULL
corruption$weight <- NULL

corruption_country <- merge(corruption, test, by="destination")


#### Austria

austria <- filter(corruption_country, values == "Austria")

origin <- paste0(austria$source)
destination <- paste0(austria$destination, " ",austria$weight)
data <- data.frame(origin, destination)

# Transform input data in a adjacency matrix
adjacencyData <- with(data, table(origin, destination))

# Charge the circlize library
library(circlize)

# Make the circular plot

set.seed(12345)
grid.col <- setNames(rainbow(length(unlist(dimnames(data)))), union(rownames(data), colnames(data)))

par(mar = c(2, 2, 2, 2), cex=0.7)
circos.par(track.margin=c(0,0))
circos.clear()
circos.par(start.degree = 90)
chordDiagram(adjacencyData, transparency = 0.3, annotationTrack = "grid", 
             annotationTrackHeight = mm_h(c(3, 2)),
             preAllocateTracks = 1, grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .089, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 1))
  #circos.axis(h = "top", labels.cex = 0.07, major.tick.percentage = 0.07, sector.index = sector.name, track.index = 1)
}, bg.border = NA)

#### Belgium

belgium <- filter(corruption_country, values == "Belgium")

origin <- paste0(belgium$source)
destination <- paste0(belgium$destination, " ",belgium$weight)
data <- data.frame(origin, destination)

# Transform input data in a adjacency matrix
adjacencyData <- with(data, table(origin, destination))

# Charge the circlize library
library(circlize)

# Make the circular plot

set.seed(12345)
grid.col <- setNames(rainbow(length(unlist(dimnames(data)))), union(rownames(data), colnames(data)))

par(mar = c(2, 2, 2, 2), cex=0.7)
circos.par(track.margin=c(0,0))
circos.clear()
circos.par(start.degree = 90)
chordDiagram(adjacencyData, transparency = 0.3, annotationTrack = "grid", 
             annotationTrackHeight = mm_h(c(3, 2)),
             preAllocateTracks = 1, grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .089, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 1))
  #circos.axis(h = "top", labels.cex = 0.07, major.tick.percentage = 0.07, sector.index = sector.name, track.index = 1)
}, bg.border = NA)


#### Bulgaria


bulgaria <- filter(corruption_country, values == "Bulgaria")

origin <- paste0(bulgaria$source)
destination <- paste0(bulgaria$destination, " ",bulgaria$weight)
data <- data.frame(origin, destination)

# Transform input data in a adjacency matrix
adjacencyData <- with(data, table(origin, destination))

# Charge the circlize library
library(circlize)

# Make the circular plot

set.seed(12345)
grid.col <- setNames(rainbow(length(unlist(dimnames(data)))), union(rownames(data), colnames(data)))

par(mar = c(2, 2, 2, 2), cex=0.7)
circos.par(track.margin=c(0,0))
circos.clear()
circos.par(start.degree = 90)
chordDiagram(adjacencyData, transparency = 0.3, annotationTrack = "grid", 
             annotationTrackHeight = mm_h(c(3, 2)),
             preAllocateTracks = 1, grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .089, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 1))
  #circos.axis(h = "top", labels.cex = 0.07, major.tick.percentage = 0.07, sector.index = sector.name, track.index = 1)
}, bg.border = NA)



#### Croatia


croatia <- filter(corruption_country, values == "Croatia")

origin <- paste0(croatia$source)
destination <- paste0(croatia$destination, " ",croatia$weight)
data <- data.frame(origin, destination)

# Transform input data in a adjacency matrix
adjacencyData <- with(data, table(origin, destination))

# Charge the circlize library
library(circlize)

# Make the circular plot

set.seed(12345)
grid.col <- setNames(rainbow(length(unlist(dimnames(data)))), union(rownames(data), colnames(data)))

par(mar = c(2, 2, 2, 2), cex=0.7)
circos.par(track.margin=c(0,0))
circos.clear()
circos.par(start.degree = 90)
chordDiagram(adjacencyData, transparency = 0.3, annotationTrack = "grid", 
             annotationTrackHeight = mm_h(c(3, 2)),
             preAllocateTracks = 1, grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .089, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 1))
  #circos.axis(h = "top", labels.cex = 0.07, major.tick.percentage = 0.07, sector.index = sector.name, track.index = 1)
}, bg.border = NA)



#### Cyprus


cyprus <- filter(corruption_country, values == "Cyprus")

origin <- paste0(cyprus$source)
destination <- paste0(cyprus$destination, " ",cyprus$weight)
data <- data.frame(origin, destination)

# Transform input data in a adjacency matrix
adjacencyData <- with(data, table(origin, destination))

# Charge the circlize library
library(circlize)

# Make the circular plot

set.seed(12345)
grid.col <- setNames(rainbow(length(unlist(dimnames(data)))), union(rownames(data), colnames(data)))

par(mar = c(2, 2, 2, 2), cex=0.7)
circos.par(track.margin=c(0,0))
circos.clear()
circos.par(start.degree = 90)
chordDiagram(adjacencyData, transparency = 0.3, annotationTrack = "grid", 
             annotationTrackHeight = mm_h(c(3, 2)),
             preAllocateTracks = 1, grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .089, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 1))
  #circos.axis(h = "top", labels.cex = 0.07, major.tick.percentage = 0.07, sector.index = sector.name, track.index = 1)
}, bg.border = NA)


#### the Czech Republic


czech <- filter(corruption_country, values == "Czech.Rep.")

origin <- paste0(czech$source)
destination <- paste0(czech$destination, " ",czech$weight)
data <- data.frame(origin, destination)

# Transform input data in a adjacency matrix
adjacencyData <- with(data, table(origin, destination))

# Charge the circlize library
library(circlize)

# Make the circular plot

set.seed(12345)
grid.col <- setNames(rainbow(length(unlist(dimnames(data)))), union(rownames(data), colnames(data)))

par(mar = c(2, 2, 2, 2), cex=0.7)
circos.par(track.margin=c(0,0))
circos.clear()
circos.par(start.degree = 90)
chordDiagram(adjacencyData, transparency = 0.3, annotationTrack = "grid", 
             annotationTrackHeight = mm_h(c(3, 2)),
             preAllocateTracks = 1, grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .089, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 1))
  #circos.axis(h = "top", labels.cex = 0.07, major.tick.percentage = 0.07, sector.index = sector.name, track.index = 1)
}, bg.border = NA)



#### Denmark


denmark <- filter(corruption_country, values == "Denmark")

origin <- paste0(denmark$source)
destination <- paste0(denmark$destination, " ",denmark$weight)
data <- data.frame(origin, destination)

# Transform input data in a adjacency matrix
adjacencyData <- with(data, table(origin, destination))

# Charge the circlize library
library(circlize)

# Make the circular plot

set.seed(12345)
grid.col <- setNames(rainbow(length(unlist(dimnames(data)))), union(rownames(data), colnames(data)))

par(mar = c(2, 2, 2, 2), cex=0.7)
circos.par(track.margin=c(0,0))
circos.clear()
circos.par(start.degree = 90)
chordDiagram(adjacencyData, transparency = 0.3, annotationTrack = "grid", 
             annotationTrackHeight = mm_h(c(3, 2)),
             preAllocateTracks = 1, grid.col = grid.col)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .089, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 1))
  #circos.axis(h = "top", labels.cex = 0.07, major.tick.percentage = 0.07, sector.index = sector.name, track.index = 1)
}, bg.border = NA)





########

sources <- corruption_country %>%
  distinct(source) %>%
  rename(label = source)

destinations <- corruption_country %>%
  distinct(destination) %>%
  rename(label = destination)

nodes <- full_join(sources, destinations, by = "label")
nodes

library(tibble)

nodes <- nodes %>% rowid_to_column("id")
nodes


per_route <- corruption_country %>%  
  group_by(source, destination) %>%
  summarise(weight = weight) %>% 
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

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)


routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

ggraph(routes_tidy) + 
  geom_edge_link(aes(width = weight), alpha = 0.3, color="#708090") + 
  geom_node_point() + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  geom_node_point(color="#2F4F4F") +
  theme_graph()

set.seed(12345)

ggraph(routes_tidy, layout='graphopt') + 
  geom_node_point(color="#2F4F4F") +
  geom_edge_link(aes(width = weight), alpha = 0.3, color="#708090") + 
  scale_edge_width(range = c(0.2, 1)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Values") +
  labs(x ="", y = "Points",
       subtitle = "Network of the Austrian e-procurement website",
       caption = "Source: CO.R.E. Elaborado por W. Migliari, 2022.") +
  theme_graph()


library(networkD3)
nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)


my_color <- 'd3.scaleOrdinal() .domain(["a", "b"]) .range(["#69b3a2", "steelblue"])'

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, 
              Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", 
              fontSize = 13, unit = "Corruption")

library(tidygraph)
library(ggraph)

