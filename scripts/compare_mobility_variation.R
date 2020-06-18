source('src/utils.R')

mobility_pre <- read.csv('data/2020-02-25.csv',sep=";")
mobility_mid <- read.csv('data/2020-03-10.csv',sep=";")
mobility_post <- read.csv('data/2020-05-05.csv',sep=";")

# Mobility doesn't change much intra-provincially...
graph_pre <- create_graph_from_data(mobility_pre, metric="n")
graph_mid <- create_graph_from_data(mobility_mid, metric="n")
graph_post <- create_graph_from_data(mobility_post, metric="n")

par(mfrow=c(1,3))
plot_size <- function(graph){(as.numeric(E(graph)$metric_value)/mean(as.numeric(E(graph)$metric_value)))/10000}

plot(graph_pre, vertex.size=5, edge.width=plot_size(graph_pre),edge.lty=c("dotted"), edge.arrow.size=0.02)
plot(graph_mid, vertex.size=5, edge.width=plot_size(graph_mid),edge.lty=c("dotted"),  edge.arrow.size=0.02)
plot(graph_post, vertex.size=5, edge.width=plot_size(graph_post),edge.lty=c("dotted"),  edge.arrow.size=0.02)

# ... but is significantly lower inter-provincially
graph_pre <- create_graph_from_data(mobility_pre, metric="n", loops=F)
graph_mid <- create_graph_from_data(mobility_mid, metric="n", loops=F)
graph_post <- create_graph_from_data(mobility_post, metric="n", loops=F)
par(mfrow=c(1,3))

plot(graph_pre, vertex.size=5, edge.width=plot_size(graph_pre),edge.lty=c("dotted"),  edge.arrow.size=0.02)
plot(graph_mid, vertex.size=5, edge.width=plot_size(graph_mid),edge.lty=c("dotted"),  edge.arrow.size=0.02)
plot(graph_post, vertex.size=5, edge.width=plot_size(graph_post),edge.lty=c("dotted"),  edge.arrow.size=0.02)
