source('src/utils.R')

### MOBILITY WITHOUT INTRA-PROVINCES MOVEMENTS

mobility_pre <- read.csv('data/2020-02-25.csv',sep=";")
mobility_mid <- read.csv('data/2020-03-10.csv',sep=";")
mobility_post <- read.csv('data/2020-05-05.csv',sep=";")

graph_pre_all <- create_graph_from_data(mobility_pre, metric="n", zeros=F)
graph_mid_all <- create_graph_from_data(mobility_mid, metric="n", zeros=F)
graph_post_all <- create_graph_from_data(mobility_post, metric="n", zeros=F)
graph_pre <- create_graph_from_data(mobility_pre, metric="n", loops=F, zeros=F)
graph_mid <- create_graph_from_data(mobility_mid, metric="n", loops=F, zeros=F)
graph_post <- create_graph_from_data(mobility_post, metric="n", loops=F, zeros=F)

par(mfrow=c(1,1))
hist(V(graph_pre)$in_strength, xlab = "Total Inbound Strength", ylab="# of Provinces", main = "Mobility pre lockdown", breaks=15)
hist(V(graph_mid)$in_strength, xlab = "Total Inbound Strength", ylab="# of Provinces", main = "Mobility mid lockdown", breaks=15)
hist(V(graph_post)$in_strength, xlab = "Total Inbound Strength",  ylab="# of Provinces", main = "Mobility post lockdown", breaks=15)
hist(V(graph_pre_all)$in_strength, xlab = "Total Inbound Strength", ylab="# of Provinces", main = "Mobility pre lockdown", breaks=15)
hist(V(graph_mid_all)$in_strength, xlab = "Total Inbound Strength", ylab="# of Provinces", main = "Mobility mid lockdown", breaks=15)
hist(V(graph_post_all)$in_strength, xlab = "Total Inbound Strength",  ylab="# of Provinces", main = "Mobility post lockdown", breaks=15)

# Coreness by total degree
par(mfrow=c(1,1))
V(graph_pre)$core <- coreness(graph_pre, "all")
V(graph_mid)$core <- coreness(graph_mid, "all")
V(graph_post)$core <- coreness(graph_post, "all")
layout <- coreness_layout(graph_pre, graph.coreness)
igraph::plot.igraph(graph_pre, layout=layout, vertex.size=6, vertex.label.cex=0.8, edge.arrow.size=0.05)
layout <- coreness_layout(graph_mid, graph.coreness)
igraph::plot.igraph(graph_mid, layout=layout, vertex.size=6, vertex.label.cex=0.8, edge.arrow.size=0.05)
layout <- coreness_layout(graph_post, graph.coreness)
igraph::plot.igraph(graph_post, layout=layout, vertex.size=6, vertex.label.cex=0.8, edge.arrow.size=0.05)

# Coreness by inbound edge strength
par(mfrow=c(1,1))
all_bin_size <- 10000
intra_bin_size <- 500
# Comparing only inter-province movements, we see that Lombardy provinces have a high inbound strength
V(graph_pre)$wcore <- weighted_coreness(graph_pre, bin_size=intra_bin_size)
V(graph_mid)$wcore <- weighted_coreness(graph_mid, bin_size=intra_bin_size)
V(graph_post)$wcore <- weighted_coreness(graph_post, bin_size=intra_bin_size)
layout <- coreness_layout(graph_pre, weighted_coreness, bin_size=intra_bin_size)
igraph::plot.igraph(graph_pre, layout=layout, vertex.size=6, vertex.label.cex=0.8, edge.arrow.size=0.05)
layout <- coreness_layout(graph_mid, weighted_coreness, bin_size=intra_bin_size)
igraph::plot.igraph(graph_mid, layout=layout, vertex.size=6, vertex.label.cex=0.8, edge.arrow.size=0.05)
layout <- coreness_layout(graph_post, weighted_coreness, bin_size=intra_bin_size)
igraph::plot.igraph(graph_post, layout=layout, vertex.size=6, vertex.label.cex=0.8, edge.arrow.size=0.05)
# When including intra-province movments, we can clearly see the correlation by province population
V(graph_pre_all)$wcore <- weighted_coreness(graph_pre_all, bin_size=all_bin_size)
V(graph_mid_all)$wcore <- weighted_coreness(graph_mid_all, bin_size=all_bin_size)
V(graph_post_all)$wcore <- weighted_coreness(graph_post_all, bin_size=all_bin_size)
layout <- coreness_layout(graph_pre_all, weighted_coreness, bin_size=all_bin_size)
igraph::plot.igraph(graph_pre_all, layout=layout, vertex.size=6, vertex.label.cex=0.8, edge.arrow.size=0.05)
layout <- coreness_layout(graph_mid_all, weighted_coreness, bin_size=all_bin_size)
igraph::plot.igraph(graph_mid_all, layout=layout, vertex.size=6, vertex.label.cex=0.8, edge.arrow.size=0.05)
layout <- coreness_layout(graph_post_all, weighted_coreness, bin_size=all_bin_size)
igraph::plot.igraph(graph_post_all, layout=layout, vertex.size=6, vertex.label.cex=0.8, edge.arrow.size=0.05)

print(paste("2020-02-25 components:", tail(unique(igraph::components(graph_pre)$membership),1)))
print(paste("2020-03-10 components:", tail(unique(igraph::components(graph_mid)$membership),1)))
print(paste("2020-05-05 components:", tail(unique(igraph::components(graph_post)$membership),1)))
