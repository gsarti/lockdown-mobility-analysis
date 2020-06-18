# Required packages
#install.packages('ggmap', 'igraph', 'dplyr', 'stringr')
library(ggmap)
library(igraph)
library(dplyr)
library(stringr)
library(statnet)

standardize <- function(x){(x-min(x))/(max(x)-min(x))}

create_graph_from_data <- function(dataframe, metric="n", loops=T) {
  # Filter on Italy and only metric "n" (raw mov counts)
  # and fill NAs with 0
  dataframe_ita <- dataframe %>% 
    filter(country == "IT", metric_name == metric) %>% 
    mutate(metric_value = ifelse(is.na(metric_value), 0, metric_value))
  # Build the vertex list for both start and end locations
  start_locs <- dataframe_ita %>% 
    select(name = start_polygon_names, id = start_polygon_id,
           x = start_x, y = start_y, province = start_name_stack) %>% 
    distinct()
  end_locs <- dataframe_ita %>% 
    select(name = end_polygon_names, id = end_polygon_id,
           x = end_x, y = end_y, province = end_name_stack) %>% 
    distinct()
  locations <- bind_rows(start_locs, end_locs) %>% distinct()
  # Create the new "region" field
  regions = str_match_all(locations$province, '^(.*) //.*')
  locations$region <- as.factor(sapply(regions, function(x) x[2]))
  # Build the movements edgelist in the required format
  movements <- dataframe_ita %>% 
    select(start_polygon_names, end_polygon_names, utc_date, time, length_km, 
           metric_name, metric_value, level, tile_size, country)
  # Create graph, set color and layout
  g <- graph_from_data_frame(movements, directed=T, vertices=locations)
  V(g)$color <- as.numeric(locations$region)
  g$layout <- cbind(V(g)$x, V(g)$y)
  if(loops == F){g <- simplify(g)}
  return(g)
}

# Inspired by: 
# https://medium.com/@infinex/visualisation-of-airport-connectivities-in-r-using-ggmap-ggplot-igraph-d26af3267ca1
plot_graph_on_map <- function(g, map) {
  p <- ggmap(map)
  plot_vector <- as.data.frame(cbind(V(g)$x, V(g)$y))
  edgelist <- get.edgelist(g)
  edgelist[,1]<-as.numeric(match(edgelist[,1],V(g)$name))
  edgelist[,2]<-as.numeric(match(edgelist[,2],V(g)$name))
  E(g)$color = rgb(0,0,0,standardize(E(g)$length_km)/10)
  edges <- data.frame(plot_vector[edgelist[,1],],
                      plot_vector[edgelist[,2],], E(g)$color)
  colnames(edges) <- c("X1", "Y1", "X2", "Y2", "Color")            
  p + geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2), 
                   data=edges, size = 0.5, colour=edges$Color) + 
    geom_point(aes(V1, V2), data=plot_vector) + 
    xlab('Longitude') + ylab('Latitude')
}