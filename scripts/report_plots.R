source('src/utils.R')
source("src/plot.R")

mobility_pre <- read.csv('data/2020-02-25.csv',sep=";")
mobility_mid <- read.csv('data/2020-03-10.csv',sep=";")
mobility_post <- read.csv('data/2020-05-05.csv',sep=";")

graph_pre <- create_graph_from_data(mobility_pre, metric="n", loops=F, zeros = F)
graph_mid <- create_graph_from_data(mobility_mid, metric="n", loops=F, zeros = F)
graph_post <- create_graph_from_data(mobility_post, metric="n", loops=F, zeros = F)

graph_pre_all <- create_graph_from_data(mobility_pre, metric="n", zeros=F)
graph_mid_all <- create_graph_from_data(mobility_mid, metric="n", zeros=F)
graph_post_all <- create_graph_from_data(mobility_post, metric="n", zeros=F)

# Focus on regions of middle Italy to capture the increasing and decreasing of betweenness
par(mfrow=c(1,3), mai=c(0,0,0,0), omi=c(0,0,0,0), mar=c(0,0,0,0), yaxs="i")
regions <- c("Lazio", "Umbria", "Abruzzo")
type <- "vertex"
E(graph_pre)$inverted_weight <- (1/E(graph_pre)$weight*10000) 
E(graph_mid)$inverted_weight <- (1/E(graph_mid)$weight*10000)
E(graph_post)$inverted_weight <- (1/E(graph_post)$weight*10000)
V(graph_pre)$betweenness <- igraph::betweenness(graph_pre,weights = E(graph_pre)$inverted_weight)
V(graph_mid)$betweenness  <- igraph::betweenness(graph_mid, weights = E(graph_mid)$inverted_weight)
V(graph_post)$betweenness  <- igraph::betweenness(graph_post, weights =  E(graph_post)$inverted_weight)
get_ids <- function(g) {which(V(g)$region %in% regions)}
sub_pre <- get_subgraph(graph_pre, ids=get_ids(graph_pre), type=type)
sub_mid <- get_subgraph(graph_mid, ids=get_ids(graph_mid), type=type)
sub_post <- get_subgraph(graph_post, ids=get_ids(graph_post), type=type)
plot_single_weighted_graph(sub_pre, edge.lty=c("dotted"), v_attr="betweenness", 
                           vertex.label.dist=1, edge.arrow.size=0.05, e_scale=2, vertex.label.cex=1.5)
plot_single_weighted_graph(sub_mid, edge.lty=c("dotted"), v_attr="betweenness", 
                           vertex.label.dist=1, edge.arrow.size=0.05, e_scale=2, vertex.label.cex=1.5)
plot_single_weighted_graph(sub_post, edge.lty=c("dotted"), v_attr="betweenness", 
                           vertex.label.dist=1, edge.arrow.size=0.05, e_scale=2, vertex.label.cex=1.5)

# Graphical representation of the graphs where the size of each vertex is proportional
# to eigenvector centrality
par(mfrow=c(1,3), mai=c(0,0,0,0), omi=c(0,0,0,0), mar=c(0,0,0,0), yaxs="i")
eig_pre <- igraph::eigen_centrality(graph_pre,weights = E(graph_pre)$inverted_weight)
eig_mid <- igraph::eigen_centrality(graph_mid, weights = E(graph_mid)$inverted_weight)
eig_post <- igraph::eigen_centrality(graph_post, weights =  E(graph_post)$inverted_weight)
v_attrs <- list(eig_pre$vector*100, eig_mid$vector*100, eig_post$vector*100)
plot_single_weighted_graph(graph_pre, edge.lty=c("dotted"), v_attr=v_attrs[[1]], vertex.label=NA,
                           vertex.label.dist=1, edge.arrow.size=0.02, e_scale=2, v_scale = 0.4)
plot_single_weighted_graph(graph_mid, edge.lty=c("dotted"), v_attr=v_attrs[[2]], vertex.label=NA,
                           vertex.label.dist=1, edge.arrow.size=0.02, e_scale=2, v_scale = 0.4)
plot_single_weighted_graph(graph_post, edge.lty=c("dotted"), v_attr=v_attrs[[3]], vertex.label=NA,
                           vertex.label.dist=1, edge.arrow.size=0.02, e_scale=2, v_scale = 0.4)
plot_weighted_graph(graph_pre, graph_mid, graph_post, v_attr=v_attrs, mfrow = c(1,3), e_scale = 2, v_scale = 0.4)


# Coreness, plot at 1500x750
par(mfrow=c(1,2), mai=c(0,0,0,0), omi=c(0,0,0,0), mar=c(0,2,0,4), yaxs="i")
pre_core <- set_vertex_attr(graph_pre, "core", value=coreness(graph_pre, "all"))
layout <- coreness_layout(graph_pre, coreness, "all")
igraph::plot.igraph(graph_pre, layout=layout, vertex.size=6, vertex.label.cex=1, edge.arrow.size=0.01)
pre_core_all <- set_vertex_attr(graph_pre_all, "core", value=coreness(graph_pre_all, "all"))
layout <- coreness_layout(graph_pre_all, coreness, "all")
igraph::plot.igraph(graph_pre_all, layout=layout, vertex.size=6, vertex.label.cex=1, edge.arrow.size=0.01)


# Weighted coreness, plot at 1500x750
all_bin_size <- 10000
intra_bin_size <- 500
par(mfrow=c(1,2), mai=c(0,0,0,0), omi=c(0,0,0,0), mar=c(0,2,0,4), yaxs="i")
pre_core <- set_vertex_attr(graph_pre, "wcore", value=weighted_coreness(graph_pre, intra_bin_size))
layout <- coreness_layout(graph_pre, weighted_coreness, intra_bin_size)
igraph::plot.igraph(graph_pre, layout=layout, vertex.size=6, vertex.label.cex=1, edge.arrow.size=0.01)
pre_core_all <- set_vertex_attr(graph_pre_all, "wcore", value=weighted_coreness(graph_pre_all, all_bin_size))
layout <- coreness_layout(graph_pre_all, weighted_coreness, all_bin_size)
igraph::plot.igraph(graph_pre_all, layout=layout, vertex.size=6, vertex.label.cex=1, edge.arrow.size=0.01)


# Components & Cliques, 1500x750
all_bin_size <- 10000
intra_bin_size <- 500
plot_coreness(graph_pre, graph_mid, graph_post, weighted_coreness, coreness_param=intra_bin_size,
              attr_name="wcore", vertex.size=6, vertex.label.cex=0.8, edge.arrow.size=0.05)
plot_coreness(graph_pre_all, graph_mid_all, graph_post_all, weighted_coreness, coreness_param=all_bin_size, 
              attr_name="wcore", vertex.size=6, vertex.label.cex=0.8, edge.arrow.size=0.05)

n_filter <- 2
wcore_pre <- get_subgraph(graph_pre, ids=which(V(graph_pre)$wcore > n_filter), type="vertex")
wcore_mid <- get_subgraph(graph_mid, ids=which(V(graph_mid)$wcore > n_filter), type="vertex")
wcore_post <- get_subgraph(graph_post, ids=which(V(graph_post)$wcore > n_filter), type="vertex")
par(mfrow=c(2,3), mai=c(0,0,0,0), omi=c(0,0,0,0), mar=c(0,2,1.1,4), yaxs="i")
pre_member <- igraph::components(wcore_pre)$membership
mid_member <- igraph::components(wcore_mid)$membership
post_member <- igraph::components(wcore_post)$membership
vcols <- list(pre_member, mid_member, post_member)
vlab_pre <- ifelse(igraph::degree(wcore_pre) > 11, V(wcore_pre)$name, NA)
vlab_pre[c("Varese", "Monza e Brianza", "Brescia")] <- NA
vlab_pre["Catania"] <- "Catania"
vlab_mid <- ifelse(igraph::degree(wcore_mid) > 5, V(wcore_mid)$name, NA)
vlab_mid[c("Como", "Monza e Brianza", "Brescia", "Prato", "Pistoia", "Vicenza", "Lecco")] <- NA
vlab_post <- ifelse(igraph::degree(wcore_post) > 5, V(wcore_post)$name, NA)
vlab_post[c("Como", "Monza e Brianza", "Brescia", "Prato", "Pistoia", "Vicenza", "Lecco")] <- NA
vlab_post["Pisa"] <- "Pisa"
plot_single_weighted_graph(wcore_pre, edge.lty=c("dotted"), edge.arrow.size=0.001, vertex.size=6, vertex.label.cex=1.5,
                           vertex.color=vcols[[1]], vertex.label.dist=2, e_scale=2, vertex.label = vlab_pre)
plot_single_weighted_graph(wcore_mid, edge.lty=c("dotted"), edge.arrow.size=0.001, vertex.size=6, vertex.label.cex=1.5,
                           vertex.color=vcols[[2]], vertex.label.dist=2, e_scale=2, vertex.label = vlab_mid)
plot_single_weighted_graph(wcore_post, edge.lty=c("dotted"), edge.arrow.size=0.001, vertex.size=6, vertex.label.cex=1.5, 
                           vertex.color=vcols[[3]], vertex.label.dist=2, e_scale=2, vertex.label = vlab_post)
cliques_pre<- largest.cliques(graph_pre)
cliques_mid <- largest.cliques(graph_mid)
cliques_post <- largest.cliques(graph_post)
V(graph_pre)$clique <- get_clique_ids(graph_pre, cliques_pre)
V(graph_mid)$clique <- get_clique_ids(graph_mid, cliques_mid)
V(graph_post)$clique <- get_clique_ids(graph_post, cliques_post)
c_pre <- get_subgraph(graph_pre, ids=unlist(cliques_pre), type="vertex")
c_mid <- get_subgraph(graph_mid, ids=unlist(cliques_mid), type="vertex")
c_post <- get_subgraph(graph_post, ids=unlist(cliques_post), type="vertex")
#vcol1 <- rep("grey80", vcount(graph_pre))
#vcol1[unlist(cliques_pre)] <- "gold"
#vcol2 <- rep("grey80", vcount(graph_mid))
#vcol2[unlist(cliques_mid)] <- "gold"
#vcol3 <- rep("grey80", vcount(graph_post))
#vcol3[unlist(cliques_post)] <- "gold"
vcols2 <- list(V(c_pre)$clique, V(c_mid)$clique, V(c_post)$clique)
vlab_pre <- ifelse(igraph::degree(c_pre) > 7, V(c_pre)$name, NA)
vlab_pre[c("Varese", "Monza e Brianza", "Lecco", "Massa-Carrara")] <- NA
vlab_mid <- ifelse(igraph::degree(c_mid) > 7, V(c_mid)$name, NA)
vlab_mid[c("Como", "Monza e Brianza", "Lucca", "Prato", "Pistoia", "Latina", "Lecco")] <- NA
vlab_post <- ifelse(igraph::degree(c_post) > 6, V(c_post)$name, NA)
vlab_post[c("Lecco", "Prato", "Monza e Brianza")] <- NA
plot_single_weighted_graph(c_pre, edge.lty=c("dotted"), edge.arrow.size=0.001, vertex.size=6, e_scale=2,
                           vertex.color=vcols2[[1]], vertex.label.cex=1.5, vertex.label.dist=2,
                           vertex.label = vlab_pre)
plot_single_weighted_graph(c_mid, edge.lty=c("dotted"), edge.arrow.size=0.001, vertex.size=6, e_scale=2 ,
                           vertex.color=vcols2[[2]], vertex.label.cex=1.5, vertex.label.dist=2,
                           vertex.label = vlab_mid)
plot_single_weighted_graph(c_post, edge.lty=c("dotted"), edge.arrow.size=0.001, vertex.size=6, e_scale=2,
                           vertex.color=vcols2[[3]], vertex.label.cex=1.5, vertex.label.dist=2,
                           vertex.label = vlab_post)


# Newman-Girvan and Label Propagation Clustering
par(mfrow=c(2,3), mai=c(0,0,0,0), omi=c(0,0,0,0), mar=c(1,2,1,4), yaxs="i")
gc_pre <- decompose(graph_pre, mode="weak")[[1]]
gc_mid <- decompose(graph_mid, mode="weak")[[1]]
gc_post <- decompose(graph_post, mode="weak")[[1]]
gc_pre$layout <- cbind(V(gc_pre)$x, V(gc_pre)$y)
gc_mid$layout <- cbind(V(gc_mid)$x, V(gc_mid)$y)
gc_post$layout <- cbind(V(gc_post)$x, V(gc_post)$y)
plot_clustering <- function(pre, mid, post, cluster_func, name, gini_attr="region", invert_w=F, mfrow=c(1,1), ...) {
  if (invert_w == T) {
    E(pre)$weight <- (1/E(pre)$weight*10000) 
    E(mid)$weight <- (1/E(mid)$weight*10000)
    E(post)$weight <- (1/E(post)$weight*10000)
  }
  comm_pre <- cluster_func(pre, ...)
  comm_mid <- cluster_func(mid, ...)
  comm_post <- cluster_func(post, ...)
  print(paste("==", name, "Community Detection =="))
  print_community_detection_metrics(pre, mid, post, comm_pre, comm_mid, comm_post, gini_attr)
  colors <- rainbow(max(max(unique(comm_pre$membership)), max(unique(comm_mid$membership)), max(unique(comm_post$membership))))
  plot(comm_pre, pre, vertex.size = 6, vertex.color=colors[membership(comm_pre)], edge.width = 0.75,
       edge.arrow.size=0.001, vertex.label=NA)
  plot(comm_mid, mid, vertex.size = 6, vertex.color=colors[membership(comm_mid)], edge.width = 0.75,
       edge.arrow.size=0.001, vertex.label=NA)
  plot(comm_post, post, vertex.size = 6, vertex.color=colors[membership(comm_post)], edge.width = 0.75,
       edge.arrow.size=0.001, vertex.label=NA)
  print(paste("Created plots of dim (",paste(mfrow,collapse=","),")"))
}
plot_clustering(gc_pre, gc_mid, gc_post, cluster_edge_betweenness, "Girvan-Newman", invert_w=T)
plot_clustering(gc_pre, gc_mid, gc_post, cluster_label_prop, "Label Propagation")

