source('src/utils.R')

### MOBILITY WITHOUT INTRA-PROVINCES MOVEMENTS

par(mfrow=c(1,1))

mobility_pre <- read.csv('data/2020-02-25.csv',sep=";")
mobility_mid <- read.csv('data/2020-03-10.csv',sep=";")
mobility_post <- read.csv('data/2020-05-05.csv',sep=";")

graph_pre <- create_graph_from_data(mobility_pre, metric="n", loops=F, zeros=F)
graph_mid <- create_graph_from_data(mobility_mid, metric="n", loops=F, zeros=F)
graph_post <- create_graph_from_data(mobility_post, metric="n", loops=F, zeros=F)

print(paste("2020-02-25 components:", tail(unique(igraph::components(graph_pre)$membership),1)))
print(paste("2020-03-10 components:", tail(unique(igraph::components(graph_mid)$membership),1)))
print(paste("2020-05-05 components:", tail(unique(igraph::components(graph_post)$membership),1)))

V(graph_pre)$core <- coreness(graph_pre, "all") # assign as node attribute the k-core to which nodes belong
layout <- coreness_layout(graph_pre)
col <- network::as.color(V(graph_pre)$core, opacity = 1)
igraph::plot.igraph(graph_pre, layout=layout, vertex.size=6, vertex.label.cex=0.8, vertex.color=col, edge.arrow.size=0.35, main='Coreness')
