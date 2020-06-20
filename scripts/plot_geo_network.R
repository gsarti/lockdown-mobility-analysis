source('src/utils.R')

# My Google API credentials
register_google(key="AIzaSyAI883o1IDmjT8CWoSeT1EPN36Bk2sUxHs")
italy_coordinates <- c(6.7499552751, 36.619987291, 
                       18.4802470232, 47.1153931748)

# Get Italy's map
map <- get_map(italy_coordinates, zoom=8, color='bw', 
               maptype='roadmap', source="google")
mobility_pre <- read.csv('data/2020-02-25.csv',sep=";")
graph_pre <- create_graph_from_data(mobility_pre, metric="n", loops=F, zeros = F)
plot_graph_on_map(graph_pre, map)
