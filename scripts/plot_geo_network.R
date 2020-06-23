source('src/utils.R')
source('src/plot.R')

# My Google API credentials
register_google(key="AIzaSyAI883o1IDmjT8CWoSeT1EPN36Bk2sUxHs")
italy_coordinates <- c(6.7499552751, 36.619987291, 
                       18.4802470232, 47.1153931748)

# Get Italy's map
map <- get_map(italy_coordinates, zoom=8, color='bw', 
               maptype='terrain', source="google")
mobility_pre <- read.csv('data/2020-02-25.csv',sep=";")
mobility_mid <- read.csv('data/2020-03-10.csv',sep=";")
mobility_post <- read.csv('data/2020-05-05.csv',sep=";")
graph_pre_inter <- create_graph_from_data(mobility_pre, metric="n", loops=F, zeros = F)
graph_mid_inter <- create_graph_from_data(mobility_mid, metric="n", loops=F, zeros = F)
graph_post_inter <- create_graph_from_data(mobility_post, metric="n", loops=F, zeros = F)
par(mfrow=c(1,3))
plot_graph_on_map(graph_pre_inter, map)
ggsave("img/pre.png", units="mm", width=97, height=116, dpi=300)
plot_graph_on_map(graph_mid_inter, map)
ggsave("img/mid.png", units="mm", width=97, height=116, dpi=300)
plot_graph_on_map(graph_post_inter, map)
ggsave("img/post.png", units="mm", width=97, height=116, dpi=300)
