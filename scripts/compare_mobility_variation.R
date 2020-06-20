source('src/utils.R')

mobility_pre <- read.csv('data/2020-02-25.csv',sep=";")
mobility_mid <- read.csv('data/2020-03-10.csv',sep=";")
mobility_post <- read.csv('data/2020-05-05.csv',sep=";")

# Mobility doesn't change much intra-provincially...
graph_pre_all <- create_graph_from_data(mobility_pre, metric="n", zeros = F)
graph_mid_all <- create_graph_from_data(mobility_mid, metric="n", zeros = F)
graph_post_all <- create_graph_from_data(mobility_post, metric="n", zeros = F)

print(paste("2020-02-25 movements (all):", sum(E(graph_pre_all)$weight)))
print(paste("2020-03-10 movements (all):", sum(E(graph_mid_all)$weight)))
print(paste("2020-05-05 movements (all):", sum(E(graph_post_all)$weight)))

# Inter-province connections are present but invisible since their value is much lower than intra-province ones
par(mfrow=c(1,3))
igraph::plot.igraph(graph_pre_all, vertex.size=5, edge.width=plot_size(graph_pre_all, 7), edge.lty=c("dotted"), edge.arrow.size=0.02)
igraph::plot.igraph(graph_mid_all, vertex.size=5, edge.width=plot_size(graph_mid_all, 7), edge.lty=c("dotted"),  edge.arrow.size=0.02)
igraph::plot.igraph(graph_post_all, vertex.size=5, edge.width=plot_size(graph_post_all, 7), edge.lty=c("dotted"),  edge.arrow.size=0.02)

# ... but is significantly lower inter-provincially
graph_pre_inter <- create_graph_from_data(mobility_pre, metric="n", loops=F, zeros = F)
graph_mid_inter <- create_graph_from_data(mobility_mid, metric="n", loops=F, zeros = F)
graph_post_inter <- create_graph_from_data(mobility_post, metric="n", loops=F, zeros = F)

print(paste("2020-02-25 movements (inter-province):", sum(E(graph_pre_inter)$weight)))
print(paste("2020-03-10 movements (inter-province):", sum(E(graph_mid_inter)$weight)))
print(paste("2020-05-05 movements (inter-province):", sum(E(graph_post_inter)$weight)))

par(mfrow=c(1,3))
igraph::plot.igraph(graph_pre_inter, vertex.size=5, edge.width=plot_size(graph_pre_inter,3), edge.lty=c("dotted"),  edge.arrow.size=0.02)
igraph::plot.igraph(graph_mid_inter, vertex.size=5, edge.width=plot_size(graph_mid_inter,3), edge.lty=c("dotted"),  edge.arrow.size=0.02)
igraph::plot.igraph(graph_post_inter, vertex.size=5, edge.width=plot_size(graph_post_inter,3), edge.lty=c("dotted"),  edge.arrow.size=0.02)

regions <- c("Friuli-Venezia Giulia", "Trentino-South Tyrol", "Veneto")
regions <- c("Lombardy")
graph_pre_local <- simplify(igraph::induced.subgraph(graph_pre_inter, vids=which(V(graph_pre_inter)$region %in% regions)))
graph_mid_local <- simplify(igraph::induced.subgraph(graph_mid_inter, vids=which(V(graph_mid_inter)$region %in% regions)))
graph_post_local <- simplify(igraph::induced.subgraph(graph_post_inter, vids=which(V(graph_post_inter)$region %in% regions)))
graph_pre_local$layout <- cbind(V(graph_pre_local)$x, V(graph_pre_local)$y)
graph_mid_local$layout <- cbind(V(graph_mid_local)$x, V(graph_mid_local)$y)
graph_post_local$layout <- cbind(V(graph_post_local)$x, V(graph_post_local)$y)
par(mfrow=c(1,3))
plot(graph_pre_local, vertex.size=10, edge.width=plot_size(graph_pre_local,3),edge.lty=c("dotted"),  edge.arrow.size=0.1)
plot(graph_mid_local, vertex.size=10, edge.width=plot_size(graph_mid_local,3),edge.lty=c("dotted"),  edge.arrow.size=0.1)
plot(graph_post_local, vertex.size=10, edge.width=plot_size(graph_post_local,3),edge.lty=c("dotted"),  edge.arrow.size=0.1)
