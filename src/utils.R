# Required packages
#install.packages('ggmap', 'igraph', 'dplyr', 'stringr')
library(ggmap)
library(igraph)
library(dplyr)
library(stringr)
library(statnet)

standardize <- function(x){(x-min(x))/(max(x)-min(x))}
#mean(as.numeric(E(g)$metric_value)))/
plot_size <- function(g, scale=10){lapply(E(g)$weight, function(x){ max(log(as.numeric(x))/scale, 0)})}

create_graph_from_data <- function(dataframe, metric="n", loops=T, zeros = T) {
  # Filter on Italy and only metric "n" (raw mov counts)
  # and fill NAs with 0
  dataframe_ita <- dataframe %>% 
    filter(country == "IT", metric_name == metric) %>% 
    mutate(metric_value = ifelse(is.na(metric_value), 0, metric_value)) %>% 
    arrange(start_polygon_names, end_polygon_names, start_name_stack, end_name_stack)
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
    select(start_polygon_names, end_polygon_names, head_province = start_name_stack, 
           tail_province = end_name_stack, utc_date, time, length_km, 
           metric_name, metric_value, level, tile_size, country)
  head_regions <- str_match_all(movements$head_province, '^(.*) //.*')
  movements$head_region <- as.factor(sapply(head_regions, function(x) x[2]))
  tail_regions <- str_match_all(movements$tail_province, '^(.*) //.*')
  movements$tail_region <- as.factor(sapply(tail_regions, function(x) x[2]))
  # We filter out small movements that were set to 0 for privacy reasons
  if(zeros == F){
    movements <- subset(movements, movements["metric_value"] > 0)
  }
  # Average length_km and metric values for different times
  movements <- movements %>% 
    group_by(start_polygon_names, end_polygon_names, head_region, tail_region,
             utc_date, metric_name, level, tile_size, country) %>% 
    summarise(length_km = mean(length_km), metric_value = mean(metric_value))
  # Create graph, set color
  g <- graph_from_data_frame(movements, directed=T, vertices=locations)
  V(g)$color <- as.numeric(locations$region)
  g$layout <- cbind(V(g)$x, V(g)$y)
  if(zeros == F){
  E(g)$weight <- movements[1:nrow(movements),]$metric_value
  }
  if(loops == F){
    g <- simplify(g, remove.multiple = F)
  }
  V(g)$in_strength <- strength(g, mode="in")
  V(g)$out_strength <- strength(g, mode="out")
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
  E(g)$color = rgb(1,0,0,standardize(E(g)$length_km))
  edges <- data.frame(plot_vector[edgelist[,1],],
                      plot_vector[edgelist[,2],], E(g)$color)
  colnames(edges) <- c("X1", "Y1", "X2", "Y2", "Color")            
  p + geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2), 
                   data=edges, size = 0.5, colour=edges$Color) + 
    geom_point(aes(V1, V2), data=plot_vector) + 
    xlab('Longitude') + ylab('Latitude')
}

coreness_layout <- function(g) {
  coreness <- graph.coreness(g);
  xy <- array(NA, dim=c(length(coreness), 2));
  shells <- sort(unique(coreness));
  for(shell in shells) {
    v <- 1 - ((shell-1) / max(shells));
    nodes_in_shell <- sum(coreness==shell);
    angles <- seq(0,360,(360/nodes_in_shell));
    angles <- angles[-length(angles)]; # remove last element
    xy[coreness==shell, 1] <- sin(angles) * v;
    xy[coreness==shell, 2] <- cos(angles) * v;
  }
  return(xy);
}
