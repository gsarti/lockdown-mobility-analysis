source('src/utils.R')

plot_attr_hist <- function(pre, mid, post, attr, xlab, type="vertex", breaks=15, mfrow=c(1,1)) {
  par(mfrow=mfrow)
  if(type == "vertex") {
    pre_attr <- vertex_attr(pre, attr)
    mid_attr <- vertex_attr(mid, attr)
    post_attr <- vertex_attr(post, attr)
  }
  else if (type == "edge") {
    pre_attr <- edge_attr(pre, attr)
    mid_attr <- edge_attr(mid, attr)
    post_attr <- edge_attr(post, attr)
  }
  else {print("Error, type should be either vertex or edge")}
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

plot_weighted_graph <- function(pre, mid, post, v_scale = 1, v_min = 3, e_scale = 1, e_min = 0, mfrow=c(1,1), ...) {
  par(mfrow=mfrow)
  igraph::plot.igraph(pre, vertex.size=plot_size(pre, V(pre)$in_strength, v_scale, v_min), 
                      edge.width=plot_size(pre, E(pre)$weight, e_scale, e_min), 
                      main="Mobility pre", edge.lty=c("dotted"), edge.arrow.size=0.02, ...)
  igraph::plot.igraph(mid, vertex.size=plot_size(mid, V(mid)$in_strength, v_scale, v_min), 
                      edge.width=plot_size(mid, E(mid)$weight, e_scale, e_min), 
                      main="Mobility mid", edge.lty=c("dotted"),  edge.arrow.size=0.02, ...)
  igraph::plot.igraph(post, vertex.size=plot_size(post, V(post)$in_strength, v_scale, v_min), 
                      edge.width=plot_size(post, E(post)$weight, e_scale, e_min), 
                      main="Mobility post", edge.lty=c("dotted"),  edge.arrow.size=0.02, ...)
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
  print(paste("Pre components:", tail(unique(pre_member),1)))
  print(paste("Mid components:", tail(unique(mid_member),1)))
  print(paste("Post components:", tail(unique(post_member),1)))
  plot(pre, vertex.color=pre_member, ...)
  plot(mid, vertex.color=mid_member, ...)
  plot(post, vertex.color=post_member, ...)
  print(paste("Created plots of dim (",paste(mfrow,collapse=","),")"))
}
