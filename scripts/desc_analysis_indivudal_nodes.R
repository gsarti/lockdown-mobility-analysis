source('src/utils.R')

mobility_pre <- read.csv('data/2020-02-25.csv',sep=";")
mobility_mid <- read.csv('data/2020-03-10.csv',sep=";")
mobility_post <- read.csv('data/2020-05-05.csv',sep=";")

### MOBILITY WITHOUT INTRA-PROVINCES MOVEMENTS

graph_pre <- create_graph_from_data(mobility_pre, metric="n", loops=F)
graph_mid <- create_graph_from_data(mobility_mid, metric="n", loops=F)
graph_post <- create_graph_from_data(mobility_post, metric="n", loops=F)

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

par(mfrow=c(1,1))
plot(ideg_pre, odeg_pre, type="n", xlab="inDegree (popularity)", ylab="outDegree (activity)") # Plot ideg by odeg
abline(0, 1, lty=3)
text(jitter(ideg_pre), jitter(ideg_pre), igraph::vertex_attr(graph_pre, "name"), cex=0.75, col=2)


# In-degree distribution
par(mfrow=c(1,3))
hist(ideg_pre, xlab="degree", main="Mobility pre lockdown in-degree distribution", prob=TRUE)
hist(ideg_mid, xlab="degree", main="Mobility mid lockdown in-degree distribution", prob=TRUE)
hist(ideg_post, xlab="degree", main="Mobility post lockdown in-degree distribution", prob=TRUE)

# Out-degree distribution
hist(odeg_pre, xlab="degree", main="Mobility pre lockdown out-degree distribution", prob=TRUE)
hist(odeg_mid, xlab="degree", main="Mobility mid lockdown out-degree distribution", prob=TRUE)
hist(odeg_post, xlab="degree", main="Mobility post lockdown out-degree distribution", prob=TRUE)

# Densities for the 3 graphs
denisty_pre <- igraph::edge_density(graph_pre, loops = FALSE)
density_mid <- igraph::edge_density(graph_mid, loops = FALSE)
denisty_post <- igraph::edge_density(graph_post, loops = FALSE)

cat(sprintf("Nertwork density pre-lockdown: %f,\nNertwork density mid-lockdown: %f,\nNertwork density post-lockdown: %f\n", 
            denisty_pre,density_mid,denisty_post))


# Centrality indexes
btn_pre <- igraph::betweenness(graph_pre)
btn_mid <- igraph::betweenness(graph_mid)
btn_post <- igraph::betweenness(graph_post)

cls_pre <- igraph::closeness(graph_pre)
cls_mid <- igraph::closeness(graph_mid)
cls_post <- igraph::closeness(graph_post)

eig_pre <- igraph::eigen_centrality(graph_pre)
eig_mid <- igraph::eigen_centrality(graph_mid)
eig_post <- igraph::eigen_centrality(graph_post)

# graphical representation of the grpahs where the size of each vertex is proportional 
# to closeness
par(mfrow=c(1,3))
plot_size <- function(graph){E(graph)$metric_value/15000}
igraph::plot.igraph(graph_pre, vertex.size=cls_pre*1500,edge.arrow.size=0.02,
                    edge.width=plot_size(graph_pre))
igraph::plot.igraph(graph_mid, vertex.size=cls_mid*1500,edge.arrow.size=0.02,
                    edge.width=plot_size(graph_mid))
igraph::plot.igraph(graph_post, vertex.size=cls_post*1500,edge.arrow.size=0.02,
                    edge.width=plot_size(graph_post))




# ### MOBILITY WITH INTRA-PROVINCES MOVEMENTS 
# 
# graph_pre <- create_graph_from_data(mobility_pre, metric="n")
# graph_mid <- create_graph_from_data(mobility_mid, metric="n")
# graph_post <- create_graph_from_data(mobility_post, metric="n")
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
# plot_size <- function(graph){E(graph)$metric_value/15000}
# igraph::plot.igraph(graph_pre, vertex.size=cls_pre*1500,edge.arrow.size=0.02,
#                     edge.width=plot_size(graph_pre))
# igraph::plot.igraph(graph_mid, vertex.size=cls_mid*1500,edge.arrow.size=0.02,
#                     edge.width=plot_size(graph_mid))
# igraph::plot.igraph(graph_post, vertex.size=cls_post*1500,edge.arrow.size=0.02,
#                     edge.width=plot_size(graph_post))


