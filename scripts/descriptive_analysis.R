source('src/utils.R')
source('src/plot.R')

mobility_pre <- read.csv('data/2020-02-25.csv',sep=";")
mobility_mid <- read.csv('data/2020-03-10.csv',sep=";")
mobility_post <- read.csv('data/2020-05-05.csv',sep=";")

### MOBILITY WITHOUT INTRA-PROVINCES MOVEMENTS

graph_pre <- create_graph_from_data(mobility_pre, metric="n", loops=F, zeros = F)
graph_mid <- create_graph_from_data(mobility_mid, metric="n", loops=F, zeros = F)
graph_post <- create_graph_from_data(mobility_post, metric="n", loops=F, zeros = F)

 # ### MOBILITY WITH INTRA-PROVINCES MOVEMENTS
 # 
 # graph_pre <- create_graph_from_data(mobility_pre, metric="n", zeros = F)
 # graph_mid <- create_graph_from_data(mobility_mid, metric="n", zeros = F)
 # graph_post <- create_graph_from_data(mobility_post, metric="n", zeros = F)

# Degree centrality for the 3 graphs

V(graph_pre)$deg <- igraph::degree(graph_pre) 
V(graph_pre)$ideg <- igraph::degree(graph_pre, mode="in")   
V(graph_pre)$odeg <- igraph::degree(graph_pre, mode="out")

V(graph_mid)$deg <- igraph::degree(graph_mid) 
V(graph_mid)$ideg <- igraph::degree(graph_mid, mode="in") 
V(graph_mid)$odeg <- igraph::degree(graph_mid, mode="out")

V(graph_post)$deg <- igraph::degree(graph_post) 
V(graph_post)$ideg <- igraph::degree(graph_post, mode="in") 
V(graph_post)$odeg <- igraph::degree(graph_post, mode="out")

# Degree/In/Out distributions
plot_attr_hist(graph_pre, graph_mid, graph_post, attr="deg", xlab ="Degree distribution", type="vertex", mfrow=c(1,3))
plot_attr_hist(graph_pre, graph_mid, graph_post, attr="ideg", xlab ="In-Degree distribution", type="vertex", mfrow=c(1,3))
plot_attr_hist(graph_pre, graph_mid, graph_post, attr="odeg", xlab ="Out-Degree distribution", type="vertex", mfrow=c(1,3))

# Densities for the 3 graphs
denisty_pre <- igraph::edge_density(graph_pre, loops = FALSE)
density_mid <- igraph::edge_density(graph_mid, loops = FALSE)
denisty_post <- igraph::edge_density(graph_post, loops = FALSE)

cat(sprintf("Network density pre-lockdown: %f,\nNetwork density mid-lockdown: %f,\nNetwork density post-lockdown: %f\n", 
            denisty_pre,density_mid,denisty_post))


# Centrality indexes

# Obtain weights that reflects distance, so the inverse of the "strength" or "popularity" of nodes
E(graph_pre)$inverted_weight <- (1/E(graph_pre)$weight*10000) 
E(graph_mid)$inverted_weight <- (1/E(graph_mid)$weight*10000)
E(graph_post)$inverted_weight <- (1/E(graph_post)$weight*10000)

# Betweenness
btn_pre <- igraph::betweenness(graph_pre,weights = E(graph_pre)$inverted_weight)
btn_mid <- igraph::betweenness(graph_mid, weights = E(graph_mid)$inverted_weight)
btn_post <- igraph::betweenness(graph_post, weights =  E(graph_post)$inverted_weight)

# Closeness is not well defined for disconnected graphs
# cls_pre <- igraph::closeness(graph_pre,weights = E(graph_pre)$inverted_weight )
# cls_mid <- igraph::closeness(graph_mid, weights = E(graph_mid)$inverted_weight  )
# cls_post <- igraph::closeness(graph_post, weights =  E(graph_post)$inverted_weight)

# Eigenvector centrality
eig_pre <- igraph::eigen_centrality(graph_pre,weights = E(graph_pre)$inverted_weight)
eig_mid <- igraph::eigen_centrality(graph_mid, weights = E(graph_mid)$inverted_weight)
eig_post <- igraph::eigen_centrality(graph_post, weights =  E(graph_post)$inverted_weight)


# Graphical representation where the size of each vertex is proportional to betweenness
v_attrs <- list(btn_pre, btn_mid, btn_post)
plot_weighted_graph(graph_pre, graph_mid, graph_post,  v_attr=v_attrs, mfrow = c(1,3), e_scale = 2, v_scale = 0.8)

# Graphical representation of the graphs where the size of each vertex is proportional
# to eigenvector centrality
v_attrs <- list(eig_pre$vector*100, eig_mid$vector*100, eig_post$vector*100)
plot_weighted_graph(graph_pre, graph_mid, graph_post, v_attr=v_attrs, mfrow = c(1,3), e_scale = 2, v_scale = 0.4)

# Assortativity
assort_pre <- assortativity_degree(graph_pre,directed = TRUE)
assort_mid <- assortativity_degree(graph_mid,directed = TRUE)
assort_post <- assortativity_degree(graph_post,directed = TRUE)
cat(sprintf("Network assortativity by degree pre-lockdown: %f,\nNetwork assortativity by degree mid-lockdown: %f,\nNetwork assortativity by degree post-lockdown: %f\n", 
            assort_pre,assort_mid,assort_post))

# Weight distribution
plot_attr_hist(graph_pre, graph_mid, graph_post, attr = "weight", type = "edge", mfrow = c(1,3), xlab = "Edge weights")

# Set a threshold of weights to inspect transitivity to all the selected nodes
w_threshold <- 500
sub_pre_weight_up<- subgraph.edges(graph_pre, E(graph_pre)[E(graph_pre)$weight>w_threshold],delete.vertices = TRUE )
sub_mid_weight_up<- subgraph.edges(graph_mid, E(graph_mid)[E(graph_mid)$weight>w_threshold],delete.vertices = TRUE )
sub_post_weight_up<- subgraph.edges(graph_post, E(graph_post)[E(graph_post)$weight>w_threshold],delete.vertices = TRUE )

# Obtain transitivity for the subgraph of nodes which have larger weight than the threshold
tran_sub_pre_weight_up <- transitivity(sub_pre_weight_up,weights = NULL)
tran_sub_mid_weight_up <- transitivity(sub_mid_weight_up, weights = NULL)
tran_sub_post_weight_up <- transitivity(sub_post_weight_up, weights = NULL)

cat(sprintf("Network transitivity by weight over threshold pre-lockdown: %f,\nNetwork transitivity by weight over threshold mid-lockdown: %f,\nNetwork transitivity by weight over threshold post-lockdown: %f\n", 
            tran_sub_pre_weight_up,tran_sub_mid_weight_up,tran_sub_post_weight_up))

# Obtain transitivity for the subgraph of nodes which have smaller weight than the threshold
sub_pre_weight_lo<- subgraph.edges(graph_pre, E(graph_pre)[E(graph_pre)$weight<=w_threshold],delete.vertices = TRUE )
sub_mid_weight_lo<- subgraph.edges(graph_mid, E(graph_mid)[E(graph_mid)$weight<=w_threshold],delete.vertices = TRUE )
sub_post_weight_lo<- subgraph.edges(graph_post, E(graph_post)[E(graph_post)$weight<=w_threshold],delete.vertices = TRUE )

tran_sub_pre_weight_lo <- transitivity(sub_pre_weight_lo,weights = NULL)
tran_sub_mid_weight_lo <- transitivity(sub_mid_weight_lo, weights = NULL)
tran_sub_post_weight_lo <- transitivity(sub_post_weight_lo, weights = NULL)

cat(sprintf("Network transitivity by weight under threshold pre-lockdown: %f,\nNetwork transitivity by weight under threshold mid-lockdown: %f,\nNetwork transitivity by weight under threshold post-lockdown: %f\n", 
            tran_sub_pre_weight_lo,tran_sub_mid_weight_lo,tran_sub_post_weight_lo))


# Obtain coreness and k cores
coreness_pre <- graph.coreness(graph_pre) 
coreness_mid <- graph.coreness(graph_mid)
coreness_post <- graph.coreness(graph_post) 
selected_coreness <- min(max(coreness_pre),max(coreness_mid), max(coreness_post))

kcore_pre <- induced.subgraph(graph_pre,vids=which(coreness_pre == selected_coreness))
kcore_mid <- induced.subgraph(graph_mid,vids=which(coreness_mid == selected_coreness))
kcore_post <- induced.subgraph(graph_post,vids=which(coreness_post == selected_coreness))

# Compute transitiity in the k cores subgraphs
tran_sub_pre_kcore <- transitivity(kcore_pre,weights = NULL)
tran_sub_mid_kcore <- transitivity(kcore_mid, weights = NULL)
tran_sub_post_kcore <- transitivity(kcore_post, weights = NULL)

cat(sprintf("Network transitivity by degree pre-lockdown: %f,\nNetwork transitivity by degree mid-lockdown: %f,\nNetwork transitivity by degree post-lockdown: %f\n", 
            tran_sub_pre_kcore,tran_sub_mid_kcore,tran_sub_post_kcore))


# Check if the general networks are strongly or weakly connected
igraph::is.connected(graph_pre, mode = "strong")
igraph::is.connected(graph_mid, mode = "strong")
igraph::is.connected(graph_post, mode = "strong")

igraph::is.connected(graph_pre,mode = "weak")
igraph::is.connected(graph_mid,mode = "weak")
igraph::is.connected(graph_post,mode = "weak")


# Check if the subgraphs of the netowrks based on weights are strongly or weakly connected
igraph::is.connected(sub_pre_weight_up, mode = "strong")
igraph::is.connected(sub_mid_weight_up, mode = "strong")
igraph::is.connected(sub_post_weight_up, mode = "strong")

igraph::is.connected(sub_pre_weight_up,mode = "weak")
igraph::is.connected(sub_mid_weight_up,mode = "weak")
igraph::is.connected(sub_post_weight_up,mode = "weak")

# Connected components in subgraphs where weights are above the threshold
igraph::components(sub_pre_weight_up, mode = "weak")
igraph::components(sub_mid_weight_up, mode = "weak")
igraph::components(sub_post_weight_up, mode = "weak")



# Largest cliques for the netowrks
cliques_pre<- largest.cliques(graph_pre)
cliques_mid <- largest_cliques(graph_mid)
cliques_post <- largest_cliques(graph_post)

# Plot of these cliques
vcol1 <- rep("grey80", vcount(graph_pre))
vcol1[unlist(cliques_pre)] <- "gold"
vcol2 <- rep("grey80", vcount(graph_mid))
vcol2[unlist(cliques_mid)] <- "gold"
vcol3 <- rep("grey80", vcount(graph_post))
vcol3[unlist(cliques_post)] <- "gold"
vcols <- list(vcol1, vcol2, vcol3)
plot_clique_graph(graph_pre, graph_mid, graph_post, vcols, mfrow = c(1,3), e_scale = 2)


# ### MOBILITY WITH INTRA-PROVINCES MOVEMENTS 

# graph_pre <- create_graph_from_data(mobility_pre, metric="n", zeros = F)
# graph_mid <- create_graph_from_data(mobility_mid, metric="n", zeros = F)
# graph_post <- create_graph_from_data(mobility_post, metric="n", zeros = F)
# 
# # degree centrality
# deg_pre <- igraph::degree(graph_pre) 
# ideg_pre <- igraph::degree(graph_pre, mode="in") 
# odeg_pre <- igraph::degree(graph_pre, mode="out")
# 
# deg_mid <- igraph::degree(graph_mid) # Default: total degree
# ideg_mid <- igraph::degree(graph_mid, mode="in") 
# odeg_mid <- igraph::degree(graph_mid, mode="out")
# 
# deg_post <- igraph::degree(graph_post) # Default: total degree
# ideg_post <- igraph::degree(graph_post, mode="in") 
# odeg_post <- igraph::degree(graph_post, mode="out")
# 
# par(mfrow=c(1,1))
# plot(ideg_pre, odeg_pre, type="n", xlab="inDegree (popularity)", ylab="outDegree (activity)") # Plot ideg by odeg
# abline(0, 1, lty=3)
# text(jitter(ideg_pre), jitter(ideg_pre), igraph::vertex_attr(graph_pre, "name"), cex=0.75, col=2)
# 
# par(mfrow=c(1,3))
# hist(ideg_pre, xlab="degree", main="Mobility pre lockdown in-degree distribution", prob=TRUE)
# hist(ideg_mid, xlab="degree", main="Mobility mid lockdown in-degree distribution", prob=TRUE)
# hist(ideg_post, xlab="degree", main="Mobility post lockdown in-degree distribution", prob=TRUE)
# 
# hist(odeg_pre, xlab="degree", main="Mobility pre lockdown out-degree distribution", prob=TRUE)
# hist(odeg_mid, xlab="degree", main="Mobility mid lockdown out-degree distribution", prob=TRUE)
# hist(odeg_post, xlab="degree", main="Mobility post lockdown out-degree distribution", prob=TRUE)
# 
# denisty_pre <- igraph::edge_density(graph_pre)
# density_mid <- igraph::edge_density(graph_mid)
# denisty_post <- igraph::edge_density(graph_post)
# 
# cat(sprintf("Nertwork density pre-lockdown: %f,\nNertwork density mid-lockdown: %f,\nNertwork density post-lockdown: %f\n", 
#             denisty_pre,density_mid,denisty_post))
# 
# btn_pre <- igraph::betweenness(graph_pre)
# btn_mid <- igraph::betweenness(graph_mid)
# btn_post <- igraph::betweenness(graph_post)
# 
# cls_pre <- igraph::closeness(graph_pre)
# cls_mid <- igraph::closeness(graph_mid)
# cls_post <- igraph::closeness(graph_post)
# 
# eig_pre <- igraph::eigen_centrality(graph_pre)
# eig_mid <- igraph::eigen_centrality(graph_mid)
# eig_post <- igraph::eigen_centrality(graph_post)
# 
# par(mfrow=c(1,3))
#igraph::plot.igraph(graph_pre, vertex.size=cls_pre*1500,edge.arrow.size=0.02,
#                    edge.width=plot_size(graph_pre, E(graph_pre)$weight, 5))
#igraph::plot.igraph(graph_mid, vertex.size=cls_mid*1500,edge.arrow.size=0.02,
#                    edge.width=plot_size(graph_mid, E(graph_pre)$weight, 5))
#igraph::plot.igraph(graph_post, vertex.size=cls_post*1500,edge.arrow.size=0.02,
#                    edge.width=plot_size(graph_post, E(graph_pre)$weight, 5))

