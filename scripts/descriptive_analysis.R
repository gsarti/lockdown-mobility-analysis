source('src/utils.R')

mobility_pre <- read.csv('data/2020-02-25.csv',sep=";")
mobility_mid <- read.csv('data/2020-03-10.csv',sep=";")
mobility_post <- read.csv('data/2020-05-05.csv',sep=";")

### MOBILITY WITHOUT INTRA-PROVINCES MOVEMENTS

graph_pre <- create_graph_from_data(mobility_pre, metric="n", loops=F, zeros = F)
graph_mid <- create_graph_from_data(mobility_mid, metric="n", loops=F, zeros = F)
graph_post <- create_graph_from_data(mobility_post, metric="n", loops=F, zeros = F)

# Degree centrality for the 3 graphs

deg_pre <- igraph::degree(graph_pre) 
ideg_pre <- igraph::degree(graph_pre, mode="in")   
odeg_pre <- igraph::degree(graph_pre, mode="out")

deg_mid <- igraph::degree(graph_mid) 
ideg_mid <- igraph::degree(graph_mid, mode="in") 
odeg_mid <- igraph::degree(graph_mid, mode="out")

deg_post <- igraph::degree(graph_post) 
ideg_post <- igraph::degree(graph_post, mode="in") 
odeg_post <- igraph::degree(graph_post, mode="out")

# Degree distribution
par(mfrow=c(1,3))
hist(ideg_pre, xlab="degree", main="Mobility pre lockdown", prob=TRUE)
hist(ideg_mid, xlab="degree", main="Mobility mid lockdown", prob=TRUE)
hist(ideg_post, xlab="degree", main="Mobility post lockdown", prob=TRUE)

# In-degree distribution
par(mfrow=c(1,3))
hist(ideg_pre, xlab="in-degree", main="Mobility pre lockdown", prob=TRUE)
hist(ideg_mid, xlab="in-degree", main="Mobility mid lockdown", prob=TRUE)
hist(ideg_post, xlab="in-degree", main="Mobility post lockdown", prob=TRUE)

# Out-degree distribution
hist(odeg_pre, xlab="out-degree", main="Mobility pre lockdown", prob=TRUE)
hist(odeg_mid, xlab="outdegree", main="Mobility mid lockdown", prob=TRUE)
hist(odeg_post, xlab="out-degree", main="Mobility post lockdown", prob=TRUE)

# Densities for the 3 graphs
denisty_pre <- igraph::edge_density(graph_pre, loops = FALSE)
density_mid <- igraph::edge_density(graph_mid, loops = FALSE)
denisty_post <- igraph::edge_density(graph_post, loops = FALSE)

cat(sprintf("Network density pre-lockdown: %f,\nNetwork density mid-lockdown: %f,\nNetwork density post-lockdown: %f\n", 
            denisty_pre,density_mid,denisty_post))


# # Centrality indexes
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
# graphical representation of the graphs where the size of each vertex is proportional 
# to closeness
#par(mfrow=c(1,3))
#igraph::plot.igraph(graph_pre, vertex.size=cls_pre*100000,edge.arrow.size=0.02,edge.lty=c("dotted"),
#                    edge.width=plot_size(graph_pre, E(graph_pre)$weight, 5))
#igraph::plot.igraph(graph_mid, vertex.size=cls_mid*100000,edge.arrow.size=0.02,edge.lty=c("dotted"),
#                    edge.width=plot_size(graph_mid, E(graph_mid)$weight, 5))
#igraph::plot.igraph(graph_post, vertex.size=cls_post*100000,edge.arrow.size=0.02,edge.lty=c("dotted"),
#                    edge.width=plot_size(graph_post, E(graph_post)$weight, 5))

# graphical representation of the graphs where the size of each vertex is proportional 
# to betweenness
#par(mfrow=c(1,3))
#igraph::plot.igraph(graph_pre, vertex.size=btn_pre/400,edge.arrow.size=0.02,edge.lty=c("dotted"),
#                    edge.width=plot_size(graph_pre, E(graph_pre)$weight, 5))
#igraph::plot.igraph(graph_mid, vertex.size=btn_mid/400,edge.arrow.size=0.02,edge.lty=c("dotted"),
#                    edge.width=plot_size(graph_mid, E(graph_mid)$weight, 5))
#igraph::plot.igraph(graph_post, vertex.size=btn_post/400,edge.arrow.size=0.02,edge.lty=c("dotted"),
#                    edge.width=plot_size(graph_post, E(graph_post)$weight, 5))

assort_pre <- assortativity_degree(graph_pre,directed = TRUE)
assort_mid <- assortativity_degree(graph_mid,directed = TRUE)
assort_post <- assortativity_degree(graph_post,directed = TRUE)
cat(sprintf("Network assortativity by degree pre-lockdown: %f,\nNetwork assortativity by degree mid-lockdown: %f,\nNetwork assortativity by degree post-lockdown: %f\n", 
            assort_pre,assort_mid,assort_post))

hist(E(graph_pre)$weight, xlab = "Edge weights", main = "Mobility pre lockdown")
hist(E(graph_mid)$weight, xlab = "Edge weights", main = "Mobility mid lockdown")
hist(E(graph_post)$weight, xlab = "Edge weights", main = "Mobility post lockdown")

w_threshold <- 500
sub_pre_weight_up<- subgraph.edges(graph_pre, E(graph_pre)[E(graph_pre)$weight>w_threshold],delete.vertices = TRUE )
sub_mid_weight_up<- subgraph.edges(graph_mid, E(graph_mid)[E(graph_mid)$weight>w_threshold],delete.vertices = TRUE )
sub_post_weight_up<- subgraph.edges(graph_post, E(graph_post)[E(graph_post)$weight>w_threshold],delete.vertices = TRUE )

tran_sub_pre_weight_up <- transitivity(sub_pre_weight_up,weights = NULL)
tran_sub_mid_weight_up <- transitivity(sub_mid_weight_up, weights = NULL)
tran_sub_post_weight_up <- transitivity(sub_post_weight_up, weights = NULL)

cat(sprintf("Network transitivity by weight over threshold pre-lockdown: %f,\nNetwork transitivity by weight over threshold mid-lockdown: %f,\nNetwork transitivity by weight over threshold post-lockdown: %f\n", 
            tran_sub_pre_weight_up,tran_sub_mid_weight_up,tran_sub_post_weight_up))

sub_pre_weight_lo<- subgraph.edges(graph_pre, E(graph_pre)[E(graph_pre)$weight<w_threshold],delete.vertices = TRUE )
sub_mid_weight_lo<- subgraph.edges(graph_mid, E(graph_mid)[E(graph_mid)$weight<w_threshold],delete.vertices = TRUE )
sub_post_weight_lo<- subgraph.edges(graph_post, E(graph_post)[E(graph_post)$weight<w_threshold],delete.vertices = TRUE )

tran_sub_pre_weight_lo <- transitivity(sub_pre_weight_lo,weights = NULL)
tran_sub_mid_weight_lo <- transitivity(sub_mid_weight_lo, weights = NULL)
tran_sub_post_weight_lo <- transitivity(sub_post_weight_lo, weights = NULL)

cat(sprintf("Network transitivity by weight under threshold pre-lockdown: %f,\nNetwork transitivity by weight under threshold mid-lockdown: %f,\nNetwork transitivity by weight under threshold post-lockdown: %f\n", 
            tran_sub_pre_weight_lo,tran_sub_mid_weight_lo,tran_sub_post_weight_lo))


coreness_pre <- graph.coreness(graph_pre) 
coreness_mid <- graph.coreness(graph_mid)
coreness_post <- graph.coreness(graph_post) 

selected_coreness <- min(max(coreness_pre),max(coreness_mid), max(coreness_post))

kcore_pre <- induced.subgraph(graph_pre,vids=which(coreness_pre == selected_coreness))
kcore_mid <- induced.subgraph(graph_mid,vids=which(coreness_mid == selected_coreness))
kcore_post <- induced.subgraph(graph_post,vids=which(coreness_post == selected_coreness))

tran_sub_pre_kcore <- transitivity(kcore_pre,weights = NULL)
tran_sub_mid_kcore <- transitivity(kcore_mid, weights = NULL)
tran_sub_post_kcore <- transitivity(kcore_post, weights = NULL)

cat(sprintf("Network transitivity by degree pre-lockdown: %f,\nNetwork transitivity by degree mid-lockdown: %f,\nNetwork transitivity by degree post-lockdown: %f\n", 
            tran_sub_pre_kcore,tran_sub_mid_kcore,tran_sub_post_kcore))

igraph::components(sub_pre_weight_up, mode = "weak")
igraph::components(sub_mid_weight_up, mode = "weak")
igraph::components(sub_post_weight_up, mode = "weak")



# # ### MOBILITY WITH INTRA-PROVINCES MOVEMENTS 
# 
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


