source('src/utils.R')
source('src/plot.R')

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

plot_attr_hist(graph_pre_all, graph_mid_all, graph_post_all, 'in_strength', "Total Inbound Strength")
plot_attr_hist(graph_pre, graph_mid, graph_post, 'in_strength', "Total Inbound Strength")

# Coreness by total degree
plot_coreness(graph_pre_all, graph_mid_all, graph_post_all, coreness, coreness_param="all", 
              vertex.size=6, vertex.label.cex=0.8, edge.arrow.size=0.05)
plot_coreness(graph_pre, graph_mid, graph_post, coreness, coreness_param="all", 
              vertex.size=6, vertex.label.cex=0.8, edge.arrow.size=0.05)

# Coreness by inbound edge strength
all_bin_size <- 10000
intra_bin_size <- 500
# Comparing only inter-province movements, we see that Lombardy provinces have a high inbound strength
plot_coreness(graph_pre, graph_mid, graph_post, weighted_coreness, coreness_param=intra_bin_size,
              attr_name="wcore", vertex.size=6, vertex.label.cex=0.8, edge.arrow.size=0.05)
# When including intra-province movments, we can clearly see the correlation by province population
plot_coreness(graph_pre_all, graph_mid_all, graph_post_all, weighted_coreness, coreness_param=all_bin_size, 
              attr_name="wcore", vertex.size=6, vertex.label.cex=0.8, edge.arrow.size=0.05)

# We will use weighted coreness going forward since it's most representative of the actual mobility
# Analyze weighted coreness scores distribution across provinces
plot_attr_hist(graph_pre_all, graph_mid_all, graph_post_all, 'wcore', "Weighted Coreness")
plot_attr_hist(graph_pre, graph_mid, graph_post, 'wcore', "Weighted Coreness")

# The network is basically a single giant component


# We filter out nodes with weighted coreness rank = 2, a.k.a. with less than 1000 raw inter-province movements (for inter-province graph)
# and with less than 20'000 total movements (for the all graph).
n_filter <- 2
wcore_pre <- induced.subgraph(graph_pre,vids=which(V(graph_pre)$wcore > n_filter))
wcore_mid <- induced.subgraph(graph_mid,vids=which(V(graph_mid)$wcore > n_filter))
wcore_post <- induced.subgraph(graph_post,vids=which(V(graph_post)$wcore > n_filter))
wcore_pre$layout <- cbind(V(wcore_pre)$x, V(wcore_pre)$y)
wcore_mid$layout <- cbind(V(wcore_mid)$x, V(wcore_mid)$y)
wcore_post$layout <- cbind(V(wcore_post)$x, V(wcore_post)$y)

print(paste("2020-02-25 components (after filtering):", tail(unique(igraph::components(wcore_pre)$membership),1)))
print(paste("2020-03-10 components (after filtering):", tail(unique(igraph::components(wcore_mid)$membership),1)))
print(paste("2020-05-05 components (after filtering):", tail(unique(igraph::components(wcore_post)$membership),1)))
par(mfrow=c(1,1))
plot(wcore_pre, vertex.size=6, edge.arrow.size=0.05, vertex.color=igraph::components(wcore_pre)$membership)
plot(wcore_mid, vertex.size=6, edge.arrow.size=0.05, vertex.color=igraph::components(wcore_mid)$membership)
plot(wcore_post, vertex.size=6, edge.arrow.size=0.05, vertex.color=igraph::components(wcore_post)$membership)

wcore_pre_all <- induced.subgraph(graph_pre,vids=which(V(graph_pre)$wcore > n_filter))
wcore_mid_all <- induced.subgraph(graph_mid,vids=which(V(graph_mid)$wcore > n_filter))
wcore_post_all <- induced.subgraph(graph_post,vids=which(V(graph_post)$wcore > n_filter))
wcore_pre_all$layout <- cbind(V(wcore_pre_all)$x, V(wcore_pre_all)$y)
wcore_mid_all$layout <- cbind(V(wcore_mid_all)$x, V(wcore_mid_all)$y)
wcore_post_all$layout <- cbind(V(wcore_post_all)$x, V(wcore_post_all)$y)

print(paste("2020-02-25 components (after filtering):", tail(unique(igraph::components(wcore_pre)$membership),1)))
print(paste("2020-03-10 components (after filtering):", tail(unique(igraph::components(wcore_mid)$membership),1)))
print(paste("2020-05-05 components (after filtering):", tail(unique(igraph::components(wcore_post)$membership),1)))
par(mfrow=c(1,1))
plot(wcore_pre, vertex.size=6, edge.arrow.size=0.05, vertex.color=igraph::components(wcore_pre)$membership)
plot(wcore_mid, vertex.size=6, edge.arrow.size=0.05, vertex.color=igraph::components(wcore_mid)$membership)
plot(wcore_post, vertex.size=6, edge.arrow.size=0.05, vertex.color=igraph::components(wcore_post)$membership)
