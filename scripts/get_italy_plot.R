install.packages('ggmap', 'igraph', 'dplyr')
library(ggmap)
library(igraph)
library(dplyr)
library(stringr)

mobility_pre <- read.csv('data/2020-02-25.csv',sep=";")

# Filter on Italy and only metric "n"
mobility_pre %>% count(country)
mobility_pre_ita <- mobility_pre %>% 
  filter(country == "IT", metric_name == "n")
mobility_pre_ita %>% count(country)

start_locs <- mobility_pre_ita %>% 
  select(name = start_polygon_names,
    id = start_polygon_id,
    x = start_x,
    y = start_y,
    province = start_name_stack) %>% 
  distinct()
end_locs <- mobility_pre_ita %>% 
  select(name = end_polygon_names,
    id = end_polygon_id,
    x = end_x,
    y = end_y,
    province = end_name_stack) %>% 
  distinct()
locations <- bind_rows(start_locs, end_locs) %>% distinct()
locations$name <- locations$name
locations$province <- locations$province
regions = str_match_all(locations$province, '^(.*) //.*')
locations$region <- as.factor(sapply(regions, function(x) x[2]))

movements <- mobility_pre_ita %>% 
  select(start_polygon_names, end_polygon_names, utc_date, time, length_km, 
         metric_name, metric_value, level, tile_size, country)
movements$start_polygon_names <- movements$start_polygon_names
movements$end_polygon_names <- movements$end_polygon_names
g <- graph_from_data_frame(movements, directed=T, vertices=locations)
V(g)$color <- as.numeric(locations$region)
plot(g, vertex.size=5, arrow.size=1)

#TODO: Use ggmap as background for the igraph plot
# My Google API credentials
register_google(key="AIzaSyAI883o1IDmjT8CWoSeT1EPN36Bk2sUxHs")
map <- get_map(
  c(6.7499552751, 36.619987291, 18.4802470232, 47.1153931748), 
  zoom=8, color='bw',
  maptype='roadmap', 
  source="google"
)
ggmap(map)
