source('src/utils.R')
source('src/plot.R')
library(intergraph)

mobility_pre <- read.csv('data/2020-02-25.csv',sep=";")
mobility_mid <- read.csv('data/2020-03-10.csv',sep=";")
mobility_post <- read.csv('data/2020-05-05.csv',sep=";")

graph_pre <- create_graph_from_data(mobility_pre, metric="n", loops=F, zeros=F)
graph_mid <- create_graph_from_data(mobility_mid, metric="n", loops=F, zeros=F)
graph_post <- create_graph_from_data(mobility_post, metric="n", loops=F, zeros=F)

provinces <- read.csv("data/provinces_data.csv")
covid_pre <- read.csv("data/covid_25_02.csv")
covid_mid <- read.csv("data/covid_10_03.csv")
covid_post <- read.csv("data/covid_05_05.csv")

# Assign to vertex some metric used in net models

V(graph_pre)$deg <- igraph::degree(graph_pre) 
V(graph_pre)$ideg <- igraph::degree(graph_pre, mode="in")   
V(graph_pre)$odeg <- igraph::degree(graph_pre, mode="out")

V(graph_mid)$deg <- igraph::degree(graph_mid) 
V(graph_mid)$ideg <- igraph::degree(graph_mid, mode="in") 
V(graph_mid)$odeg <- igraph::degree(graph_mid, mode="out")

V(graph_post)$deg <- igraph::degree(graph_post) 
V(graph_post)$ideg <- igraph::degree(graph_post, mode="in") 
V(graph_post)$odeg <- igraph::degree(graph_post, mode="out")
E(graph_pre)$inverted_weight <- (1/E(graph_pre)$weight*10000) 
E(graph_mid)$inverted_weight <- (1/E(graph_mid)$weight*10000)
E(graph_post)$inverted_weight <- (1/E(graph_post)$weight*10000)

V(graph_pre)$betweenness <- igraph::betweenness(graph_pre,weights = E(graph_pre)$inverted_weight)
V(graph_mid)$betweenness  <- igraph::betweenness(graph_mid, weights = E(graph_mid)$inverted_weight)
V(graph_post)$betweenness  <- igraph::betweenness(graph_post, weights =  E(graph_post)$inverted_weight)

V(graph_pre)$eig_vect_centr <- igraph::eigen_centrality(graph_pre,weights = E(graph_pre)$inverted_weight)$vector
V(graph_mid)$eig_vect_centr <- igraph::eigen_centrality(graph_mid, weights = E(graph_mid)$inverted_weight)$vector
V(graph_post)$eig_vect_centr <- igraph::eigen_centrality(graph_post, weights =  E(graph_post)$inverted_weight)$vector

# Data are double-checked to follow alphabetical order in both graphs and dataframes
# so that there are no mismatchs in this procedure.
V(graph_pre)$covid_cases <- covid_pre$totale_casi_pre
V(graph_pre)$covid_deaths <- covid_pre$deceduti_pre
V(graph_pre)$population <- provinces$Popolazione
V(graph_pre)$surface <- provinces$Superficie
V(graph_pre)$pop_density <- provinces$Densità
V(graph_pre)$municipalities <- provinces$Comuni

V(graph_mid)$covid_cases <- covid_mid$totale_casi_mid
V(graph_mid)$covid_deaths <- covid_mid$deceduti_mid
V(graph_mid)$population <- provinces$Popolazione
V(graph_mid)$surface <- provinces$Superficie
V(graph_mid)$pop_density <- provinces$Densità
V(graph_mid)$municipalities <- provinces$Comuni

V(graph_post)$covid_cases <- covid_post$totale_casi_post
V(graph_post)$covid_deaths <- covid_post$deceduti_post
V(graph_post)$population <- provinces$Popolazione
V(graph_post)$surface <- provinces$Superficie
V(graph_post)$pop_density <- provinces$Densità
V(graph_post)$municipalities <- provinces$Comuni

# Select the giant component
gc_pre <- decompose(graph_pre, mode="weak")[[1]]
gc_mid <- decompose(graph_mid, mode="weak")[[1]]
gc_post <- decompose(graph_post, mode="weak")[[1]]
gc_pre$layout <- cbind(V(gc_pre)$x, V(gc_pre)$y)
gc_mid$layout <- cbind(V(gc_mid)$x, V(gc_mid)$y)
gc_post$layout <- cbind(V(gc_post)$x, V(gc_post)$y)

# Modelling
gc_pre_net <- intergraph::asNetwork(gc_pre)
gc_mid_net <- intergraph::asNetwork(gc_mid)
gc_post_net <- intergraph::asNetwork(gc_post)


# The obtained models are the reslults after many trials and errors. The general idea that have been
# followed is: We start from a model with full covariates, removing at each try the less significant 
# variable, selecting also the model according to the AIC criteria.

# Model for the graph_pre. 
gc_pre_model <- ergm(gc_pre_net ~ edges + asymmetric +  
                       nodematch('region', diff=F) +
                       nodecov('out_strength') +
                       nodecov('ideg'))
summary(gc_pre_model)
pre_model_gof <- gof(gc_pre_model)
plot(pre_model_gof)


# Model for the graph_mid. 
gc_mid_model <- ergm(gc_mid_net ~ edges + mutual  + 
                       nodematch('region', diff=F) +
                       nodecov('odeg'))
summary(gc_mid_model)
mid_model_gof <- gof(gc_mid_model)
plot(mid_model_gof)


# Model for the graph_post.  
gc_post_model <- ergm(gc_post_net ~ edges + mutual+ nodecov('covid_deaths') + 
                        nodematch('region', diff=F) +
                        nodecov('eig_vect_centr') +
                        nodecov('ideg'))
summary(gc_post_model)
post_model_gof <- gof(gc_post_model)
plot(post_model_gof)



# degree
# idegree
# idegree1.5
# odegree
# odegree1.5 
# transitive 
# mutual
# 
# in_strength
# out_strength
# covid_cases
# covid_deaths
# population
# surface
# pop_density
# municipalities
# region
# 
# weight
# length_km
# 
# edges
# absdiff()
