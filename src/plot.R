source('src/utils.R')

plot_attr_hist <- function(pre, mid, post, attr, xlab, type="vertex", breaks=15, mfrow=c(1,1)) {
  par(mfrow=mfrow)
  pre_attr <- get_attr(pre, attr, type)
  mid_attr <- get_attr(mid, attr, type)
  post_attr <- get_attr(post, attr, type)
  hist(pre_attr, xlab = xlab, ylab="# of Provinces", main = "Mobility pre lockdown", breaks=breaks)
  hist(mid_attr, xlab = xlab, ylab="# of Provinces", main = "Mobility mid lockdown", breaks=breaks)
  hist(post_attr, xlab = xlab, ylab="# of Provinces", main = "Mobility post lockdown", breaks=breaks)
  print(paste("Created plots of dim (",paste(mfrow,collapse=","),")"))
}

plot_coreness <- function(pre, mid, post, coreness_func, coreness_param, attr_name="core", mfrow=c(1,1), ...) {
  par(mfrow=mfrow)
  pre_core <- set_vertex_attr(pre, attr_name, value=coreness_func(pre, coreness_param))
  mid_core <- set_vertex_attr(mid, attr_name, value=coreness_func(mid, coreness_param))
  post_core <- set_vertex_attr(post, attr_name, value=coreness_func(post, coreness_param))
  # Set attribute by reference
  eval.parent(substitute(pre <- pre_core))
  eval.parent(substitute(mid <- mid_core))
  eval.parent(substitute(post <- post_core))
  # Plot with the coreness layout
  layout <- coreness_layout(pre, coreness_func, coreness_param)
  igraph::plot.igraph(pre, layout=layout, main="Coreness pre", ...)
  layout <- coreness_layout(mid, coreness_func, coreness_param)
  igraph::plot.igraph(mid, layout=layout, main="Coreness mid", ...)
  layout <- coreness_layout(post, coreness_func, coreness_param)
  igraph::plot.igraph(post, layout=layout, main="Coreness post", ...)
  print(paste("Created plots of dim (",paste(mfrow,collapse=","),")"))
}

plot_single_weighted_graph <- function(g, v_attr="in_strength", e_attr="weight", 
                                       v_scale=1, v_min=3, e_scale=1, e_min=0, ...) {
  if(is.character(v_attr)) {
    v_attr <- vertex_attr(g, v_attr)
  }
  if(is.character(e_attr)) {
    e_attr <- edge_attr(g, e_attr)
  }
  igraph::plot.igraph(g, vertex.size=plot_size(g, v_attr, v_scale, v_min), 
                      edge.width=plot_size(g, e_attr, e_scale, e_min), ...)
}

# v_attr/e_attr are either names of vertex/edge properties, or vectors of three variables each
plot_weighted_graph <- function(pre, mid, post, v_attr="in_strength", e_attr="weight", mfrow=c(1,1), ...) {
  par(mfrow=mfrow)
  if(is.character(v_attr)) {
    v_attr <- list(v_attr, v_attr, v_attr)
  }
  if(is.character(e_attr)) {
    e_attr <- list(e_attr, e_attr, e_attr)
  }
  plot_single_weighted_graph(pre, v_attr[[1]], e_attr[[1]], main="Mobility pre", edge.lty=c("dotted"), edge.arrow.size=0.02, ...)
  plot_single_weighted_graph(mid, v_attr[[2]], e_attr[[2]], main="Mobility mid", edge.lty=c("dotted"),  edge.arrow.size=0.02, ...)
  plot_single_weighted_graph(post, v_attr[[3]], e_attr[[3]], main="Mobility post", edge.lty=c("dotted"),  edge.arrow.size=0.02, ...)
  print(paste("Created plots of dim (",paste(mfrow,collapse=","),")"))
}

plot_community_graph <- function(pre, mid, post, vcols, mfrow=c(1,1), ...) {
  par(mfrow=mfrow)
  plot_single_weighted_graph(pre, main="Mobility pre", edge.lty=c("dotted"), edge.arrow.size=0.02, vertex.color=vcols[[1]], ...)
  plot_single_weighted_graph(mid, main="Mobility mid", edge.lty=c("dotted"), edge.arrow.size=0.02, vertex.color=vcols[[2]], ...)
  plot_single_weighted_graph(post, main="Mobility post", edge.lty=c("dotted"), edge.arrow.size=0.02, vertex.color=vcols[[3]], ...)
  print(paste("Created plots of dim (",paste(mfrow,collapse=","),")"))
}

plot_regions_subgraph <- function(pre, mid, post, regions, egonet=F, ego_mode="outbound", mfrow=c(1,1), ...) {
  type <- ifelse(egonet == F, "vertex", "edge")
  get_mode_attr <- ifelse(ego_mode == "outbound", function(g){E(g)$head_region}, function(g){E(g)$tail_region})
  # Filter either by region of belonging, or by region from where edges start or end (ego in/outbound)
  get_ids <- ifelse(egonet == F,  function(g) {which(V(g)$region %in% regions)},
                function(g) {which(get_mode_attr(g) %in% regions)})
  sub_pre <- get_subgraph(pre, ids=get_ids(pre), type=type)
  sub_mid <- get_subgraph(mid, ids=get_ids(mid), type=type)
  sub_post <- get_subgraph(post, ids=get_ids(post), type=type)
  plot_weighted_graph(sub_pre, sub_mid, sub_post, v_scale=0.5, v_min=3, e_scale=3, mfrow=mfrow, ...)
}

plot_components <- function(pre, mid, post, mfrow=c(1,1), ...) {
  par(mfrow=mfrow)
  pre_member <- igraph::components(pre)$membership
  mid_member <- igraph::components(mid)$membership
  post_member <- igraph::components(post)$membership
  print(paste("Components: Pre", tail(unique(pre_member),1), 
              " -> Mid", tail(unique(mid_member),1), 
              " -> Post", tail(unique(post_member),1)))
  vcols <- list(pre_member, mid_member, post_member)
  plot_community_graph(pre, mid, post, vcols, mfrow, vertex.size=6, edge.arrow.size=0.05, ...)
}

plot_cliques <- function(pre, mid, post, mfrow=c(1,1), ...) {
  par(mfrow=mfrow)
  cliques_pre<- largest.cliques(pre)
  cliques_mid <- largest.cliques(mid)
  cliques_post <- largest.cliques(post)
  print(paste("Cliques: Pre", length(cliques_pre), 
              " -> Mid", length(cliques_mid), 
              " -> Post", length(cliques_post)))
  vcol1 <- rep("grey80", vcount(graph_pre))
  vcol1[unlist(cliques_pre)] <- "gold"
  vcol2 <- rep("grey80", vcount(graph_mid))
  vcol2[unlist(cliques_mid)] <- "gold"
  vcol3 <- rep("grey80", vcount(graph_post))
  vcol3[unlist(cliques_post)] <- "gold"
  vcols <- list(vcol1, vcol2, vcol3)
  plot_community_graph(pre, mid, post, vcols, mfrow, edge.arrow.size=0.05, ...)
}

print_connectivity <- function(pre, mid, post) {
  return(cat("Is the graph strongly connected? Pre", igraph::is.connected(graph_pre, mode = "strong"),
      "-> Mid", igraph::is.connected(graph_mid, mode = "strong"),
      "-> Post", igraph::is.connected(graph_post, mode = "strong"),
      "\nIs the graph weakly connected? Pre", igraph::is.connected(graph_pre, mode = "weak"),
      "-> Mid", igraph::is.connected(graph_mid, mode = "weak"),
      "-> Post", igraph::is.connected(graph_post, mode = "weak")))
}

print_community_detection_metrics <- function(pre, mid, post, comm_pre, comm_mid, comm_post, gini_attr="region") {
  V(pre)$member <- membership(comm_pre)
  V(mid)$member <- membership(comm_mid)
  V(post)$member <- membership(comm_post)
  print(paste("# Communities : Pre", max(unique(comm_pre$membership)), " -> Mid", 
              max(unique(comm_mid$membership)), " -> Post", max(unique(comm_post$membership))))
  print(paste("Modularity: Pre", modularity(pre, membership(comm_pre)), " -> Mid", 
              modularity(mid, membership(comm_mid)), " -> Post", modularity(post, membership(comm_post))))
  pre_icd <- sapply(unique(membership(comm_pre)), function(x){round(intra_clust_density(pre, comm_pre, x),3)})
  mid_icd <- sapply(unique(membership(comm_mid)), function(x){round(intra_clust_density(mid, comm_mid, x),3)})
  post_icd <- sapply(unique(membership(comm_post)), function(x){round(intra_clust_density(post, comm_post, x),3)})
  print("Pre Intra-cluster Density:")
  print(pre_icd)
  print("Mid Intra-cluster Density:")
  print(mid_icd)
  print("Post Intra-cluster Density:")
  print(post_icd)
  pre_gini <- sapply(unique(membership(comm_pre)), function(x){round(gini_index(vertex_attr(pre, gini_attr)[V(pre)$member==x]),3)})
  mid_gini <- sapply(unique(membership(comm_mid)), function(x){round(gini_index(vertex_attr(mid, gini_attr)[V(mid)$member==x]),3)})
  post_gini <- sapply(unique(membership(comm_post)), function(x){round(gini_index(vertex_attr(post, gini_attr)[V(post)$member==x]),3)})
  print("Pre Gini Index:")
  print(pre_gini)
  print("Mid Gini Index:")
  print(mid_gini)
  print("Post Gini Index:")
  print(post_gini)
}

# Girvan-Newman Algorithm
# Since edges are interpreted as distances instead of connection strengths, we follow an approach proposed in
# literature and use the inverse of the weight to reflect "distance" as lower is better.
plot_girvan_newman <- function(pre, mid, post, gini_attr="region", mfrow=c(1,1)) {
  par(mfrow=mfrow)
  E(pre)$inverted_weight <- (1/E(pre)$weight*10000) 
  E(mid)$inverted_weight <- (1/E(mid)$weight*10000)
  E(post)$inverted_weight <- (1/E(post)$weight*10000)
  gn_comm_pre <- cluster_edge_betweenness(pre, weights=E(pre)$inverted_weight)
  gn_comm_mid <- cluster_edge_betweenness(mid, weights=E(mid)$inverted_weight)
  gn_comm_post <- cluster_edge_betweenness(post, weights=E(post)$inverted_weight)
  print("== Girvan-Newman Community Detection ==")
  print_community_detection_metrics(pre, mid, post, gn_comm_pre, gn_comm_mid, gn_comm_post, gini_attr)
  colors <- rainbow(max(max(unique(gn_comm_pre$membership)), max(unique(gn_comm_mid$membership)), max(unique(gn_comm_post$membership))))
  plot(gn_comm_pre, pre, vertex.size = 6, vertex.color=colors[membership(gn_comm_pre)], edge.width = 1,
       edge.arrow.size=0.05, main="Girvan-Newman pre")
  plot(gn_comm_mid, mid, vertex.size = 6, vertex.color=colors[membership(gn_comm_mid)], edge.width = 1,
       edge.arrow.size=0.05, main="Girvan-Newman mid")
  plot(gn_comm_post, post, vertex.size = 6, vertex.color=colors[membership(gn_comm_post)], edge.width = 1,
       edge.arrow.size=0.05, main="Girvan-Newman post")
  print(paste("Created plots of dim (",paste(mfrow,collapse=","),")"))
}

# Label Propagation Algorithm
# Here larger edges correspond to stronger connections, hence we can use weights
plot_label_propagation <- function(pre, mid, post, gini_attr="region", mfrow=c(1,1)) {
  par(mfrow=mfrow)
  lp_comm_pre <- cluster_label_prop(pre, weights=E(pre)$weight)
  lp_comm_mid <- cluster_label_prop(mid, weights=E(mid)$weight)
  lp_comm_post <- cluster_label_prop(post, weights=E(post)$weight)
  print("== Label Propagation Community Detection ==")
  print_community_detection_metrics(pre, mid, post, lp_comm_pre, lp_comm_mid, lp_comm_post, gini_attr)
  colors <- rainbow(max(max(unique(lp_comm_pre$membership)), max(unique(gn_comm_mid$membership)), max(unique(gn_comm_post$membership))))
  plot(lp_comm_pre, pre, vertex.size = 6, vertex.color=colors[membership(lp_comm_pre)], edge.width = 1,
       edge.arrow.size=0.05, main="Label Propagation pre")
  plot(lp_comm_mid, mid, vertex.size = 6, vertex.color=colors[membership(lp_comm_mid)], edge.width = 1,
       edge.arrow.size=0.05, main="Label Propagation mid")
  plot(lp_comm_post, post, vertex.size = 6, vertex.color=colors[membership(lp_comm_post)], edge.width = 1,
       edge.arrow.size=0.05, main="Label Propagation post")
  print(paste("Created plots of dim (",paste(mfrow,collapse=","),")"))
}

# Girvan-Newman Algorithm
# Since edges are interpreted as distances instead of connection strengths, we follow an approach proposed in
# literature and use the inverse of the weight to reflect "distance" as lower is better.
plot_girvan_newman <- function(pre, mid, post, gini_attr="region", mfrow=c(1,1)) {
  par(mfrow=mfrow)
  E(pre)$inverted_weight <- (1/E(pre)$weight*10000) 
  E(mid)$inverted_weight <- (1/E(mid)$weight*10000)
  E(post)$inverted_weight <- (1/E(post)$weight*10000)
  gn_comm_pre <- cluster_edge_betweenness(pre, weights=E(pre)$inverted_weight)
  gn_comm_mid <- cluster_edge_betweenness(mid, weights=E(mid)$inverted_weight)
  gn_comm_post <- cluster_edge_betweenness(post, weights=E(post)$inverted_weight)
  print("== Girvan-Newman Community Detection ==")
  print_community_detection_metrics(pre, mid, post, gn_comm_pre, gn_comm_mid, gn_comm_post, gini_attr)
  colors <- rainbow(max(max(unique(gn_comm_pre$membership)), max(unique(gn_comm_mid$membership)), max(unique(gn_comm_post$membership))))
  plot(gn_comm_pre, pre, vertex.size = 6, vertex.color=colors[membership(gn_comm_pre)], edge.width = 1,
       edge.arrow.size=0.05, main="Girvan-Newman pre")
  plot(gn_comm_mid, mid, vertex.size = 6, vertex.color=colors[membership(gn_comm_mid)], edge.width = 1,
       edge.arrow.size=0.05, main="Girvan-Newman mid")
  plot(gn_comm_post, post, vertex.size = 6, vertex.color=colors[membership(gn_comm_post)], edge.width = 1,
       edge.arrow.size=0.05, main="Girvan-Newman post")
  print(paste("Created plots of dim (",paste(mfrow,collapse=","),")"))
}