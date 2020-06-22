source('src/utils.R')
source('src/plot.R')

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
# From now on, we limit ourselves to analyzing only the network without intra-province movements (no loops)

# We filter out nodes with weighted coreness rank = 2, a.k.a. with less than 1000 raw inter-province movements 
# (for inter-province graph)
n_filter <- 2
wcore_pre <- get_subgraph(graph_pre, ids=which(V(graph_pre)$wcore > n_filter), type="vertex")
wcore_mid <- get_subgraph(graph_mid, ids=which(V(graph_mid)$wcore > n_filter), type="vertex")
wcore_post <- get_subgraph(graph_post, ids=which(V(graph_post)$wcore > n_filter), type="vertex")

# We can clearly see the three main clusters: Milan macro region, Rome-Naples macroregion and Tuscany
plot_components(wcore_pre, wcore_mid, wcore_post, e_scale=2)

# Results are very similar when highlighting existing cliques (especially in the mid case)
plot_cliques(graph_pre, graph_mid, graph_post)

# The graph isn't connected
print_connectivity(graph_pre, graph_mid, graph_post)


igraph::is.connected(graph_pre,mode = "weak")
igraph::is.connected(graph_mid,mode = "weak")
igraph::is.connected(graph_post,mode = "weak")



