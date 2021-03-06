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

plot_attr_hist(graph_pre_all, graph_mid_all, graph_post_all, 'in_strength', "Total Inbound Strength", mfrow=c(1,3))
plot_attr_hist(graph_pre, graph_mid, graph_post, 'in_strength', "Total Inbound Strength", mfrow=c(1,3))

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

# From now on, we limit ourselves to analyzing only the network without intra-province movements (no loops)

# The network is basically a single giant component, with some exception
group_pre <- igraph::groups(igraph::components(graph_pre))$`1`
group_mid <- igraph::groups(igraph::components(graph_mid))$`1`
group_post <- igraph::groups(igraph::components(graph_post))$`1`

sub_group_pre <- subgraph(graph_pre,group_pre)
sub_group_mid <- subgraph(graph_mid,group_mid)
sub_group_post <- subgraph(graph_post,group_post)

# # The pre-lockdown and the post-lockdown giant components are connected so we can calculate the closeness
# igraph::is.connected(sub_group_pre,mode = "strong")
# igraph::is.connected(sub_group_mid,mode = "strong")
# igraph::is.connected(sub_group_post,mode = "strong")
# 
# # Obtain weights that reflects distance, so the inverse of the "strength" or "popularity" of nodes
# E(sub_group_pre)$inverted_weight <- (1/E(sub_group_pre)$weight*10000) 
# E(sub_group_post)$inverted_weight <- (1/E(sub_group_post)$weight*10000)
# 
# # Closeness on giant components, it isn't infomrative since all nodes have basically the same values
# # for this measure. The difference between the first and the last graph is only the order of magnitude,
# # that decreases
# clos_sub_pre<- igraph::closeness(sub_group_pre,weights = E(sub_group_pre)$inverted_weight )
# clos_sub_post <- igraph::closeness(sub_group_post, weights =  E(sub_group_post)$inverted_weight)
# par(mfrow=c(1,2))
# plot_single_weighted_graph(sub_group_pre, v_attr = clos_sub_pre* 10000)
# plot_single_weighted_graph(sub_group_post, v_attr = clos_sub_post* 10000 )

# Checking the "smallworld" property on giant components
# A network can be said "smallworld" if its smallworldness is higher 
# than one (a stricter rule is smallworldness>=3; Humphries & Gurney, 2008).

require(qgraph)
qgraph::smallworldness(sub_group_pre, B = 1000, up = 0.995, lo = 0.005)
qgraph::smallworldness(sub_group_mid, B = 1000, up = 0.995, lo = 0.005)
qgraph::smallworldness(sub_group_post, B = 1000, up = 0.995, lo = 0.005)

# We filter out nodes with weighted coreness rank = 2, a.k.a. with less than 1000 raw inter-province movements 
# (for inter-province graph)
n_filter <- 2
wcore_pre <- get_subgraph(graph_pre, ids=which(V(graph_pre)$wcore > n_filter), type="vertex")
wcore_mid <- get_subgraph(graph_mid, ids=which(V(graph_mid)$wcore > n_filter), type="vertex")
wcore_post <- get_subgraph(graph_post, ids=which(V(graph_post)$wcore > n_filter), type="vertex")

# We can clearly see the three main clusters: Milan macro region, Rome-Naples macroregion and Tuscany
plot_components(wcore_pre, wcore_mid, wcore_post, e_scale=2, mfrow=c(1,3))

# Results are very similar when highlighting existing cliques (especially in the mid case)
plot_cliques(graph_pre, graph_mid, graph_post)

# The graph isn't connected since there are few provinces that are left disconnected after removing
# 0-valued edges weighted with raw movements. We will discard those for our community detection analysis.
print_connectivity(graph_pre, graph_mid, graph_post)

gc_pre <- decompose(graph_pre, mode="weak")[[1]]
gc_mid <- decompose(graph_mid, mode="weak")[[1]]
gc_post <- decompose(graph_post, mode="weak")[[1]]
gc_pre$layout <- cbind(V(gc_pre)$x, V(gc_pre)$y)
gc_mid$layout <- cbind(V(gc_mid)$x, V(gc_mid)$y)
gc_post$layout <- cbind(V(gc_post)$x, V(gc_post)$y)

# Girvan-Newman Algorithm
# Since edges are interpreted as distances instead of connection strengths, we follow an approach proposed in
# literature and use the inverse of the weight to reflect "distance" as lower is better.
plot_clustering(gc_pre, gc_mid, gc_post, cluster_edge_betweenness, "Girvan-Newman", invert_w=T, mfrow=c(1,3))

# Label Propagation Algorithm
# Here larger edges correspond to stronger connections, hence we can use normal weights
plot_clustering(gc_pre, gc_mid, gc_post, cluster_label_prop, "Label Propagation", mfrow=c(1,3))

# Louvain Algorithm cannot be used for directed graphs since it uses multi-level modularity optimization
# We use the monolevel modularity optimization instead (This works only if igraph was compiled with GLPK!)
plot_clustering(gc_pre, gc_mid, gc_post, cluster_optimal, "Optimal Modularity", mfrow=c(1,3))

# Spinglass Clustering
# Creates some weird connections across the map, not that good
plot_clustering(gc_pre, gc_mid, gc_post, cluster_spinglass, "Spinglass", mfrow=c(1,3))

# Walktrap Clustering
# Works as well as Label Propagation for finding regional groups
plot_clustering(gc_pre, gc_mid, gc_post, cluster_walktrap, "Walktrap", mfrow=c(1,3))
