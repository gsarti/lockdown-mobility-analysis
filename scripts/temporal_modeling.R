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

# We also compute some measures that will be used for fitting STERGMs
E(graph_pre)$inverted_weight <- (1/E(graph_pre)$weight*10000) 
E(graph_mid)$inverted_weight <- (1/E(graph_mid)$weight*10000)
E(graph_post)$inverted_weight <- (1/E(graph_post)$weight*10000)

V(graph_pre)$betweenness <- igraph::betweenness(graph_pre,weights = E(graph_pre)$inverted_weight)
V(graph_mid)$betweenness  <- igraph::betweenness(graph_mid, weights = E(graph_mid)$inverted_weight)
V(graph_post)$betweenness  <- igraph::betweenness(graph_post, weights =  E(graph_post)$inverted_weight)

V(graph_pre)$eig_vect_centr <- igraph::eigen_centrality(graph_pre,weights = E(graph_pre)$inverted_weight)$vector
V(graph_mid)$eig_vect_centr <- igraph::eigen_centrality(graph_mid, weights = E(graph_mid)$inverted_weight)$vector
V(graph_post)$eig_vect_centr <- igraph::eigen_centrality(graph_post, weights =  E(graph_post)$inverted_weight)$vector

# This is done for using regions as plotting colors later
V(graph_pre)$region <- factor(V(graph_pre)$region)
V(graph_mid)$region<- factor(V(graph_mid)$region)
V(graph_post)$region<- factor(V(graph_post)$region)

# This is done because edgecov requires the attribute to be assigned directly to the 
# graph instead of edges.
graph_pre$length_km <- as_adjacency_matrix(graph_pre, attr="length_km", sparse=F)
graph_pre$weight <- as_adjacency_matrix(graph_pre, attr="weight", sparse=F)
graph_mid$length_km <- as_adjacency_matrix(graph_mid, attr="length_km", sparse=F)
graph_mid$weight <- as_adjacency_matrix(graph_mid, attr="weight", sparse=F)
graph_post$length_km <- as_adjacency_matrix(graph_post, attr="length_km", sparse=F)
graph_post$weight <- as_adjacency_matrix(graph_post, attr="weight", sparse=F)


# Modelling
pre_net <- intergraph::asNetwork(graph_pre)
mid_net <- intergraph::asNetwork(graph_mid)
post_net <- intergraph::asNetwork(graph_post)

# Requires ImageMagick library: sudo apt-get install libmagick++-dev
library(ndtv)
library(networkDynamic)
library(htmlwidgets)
library(latticeExtra)

par(mfrow=c(1,1))
# Build and visualize the dynamic temporal graph
netlist <- list(pre_net, mid_net, post_net)
mobility <- networkDynamic(network.list = netlist, create.TEAs = TRUE)
tSnaStats(mobility, "degree")
tErgmStats(mobility, "~ edges+triangle+mutual")
plot.network(network.extract(mobility, at=0), 
             coord=get.network.attribute.active(mobility, "layout", at=0))
render.d3movie(mobility, plot.par=list(displaylabels=T),
               render.par=list(
                 initial.coords=get.network.attribute.active(mobility, "layout", at=0)))
proximity.timeline(mobility,default.dist = 6,
                   mode = 'gvNeato',labels.at = 2.6, vertex.cex = 1, vertex.cex.labels=1)

# Fit the temporal graph
# This was the final configuration after many trials and errors
require(tergm)    # lib for temporal ergm simulations
stergm.fit <- stergm(netlist,
                       formation= ~ edges + gwesp(0.25, fixed=T) +
                          nodecov('eig_vect_centr') +
                          nodematch('region', diff=F) +
                         mutual,
                       dissolution = ~ edges + nodecov('covid_deaths') +  
                         nodecov('pop_density') +
                         edgecov('length_km') +
                         edgecov('weight') + mutual,
                       targets="formation",  
                       estimate = "CMLE",
                       control = control.stergm(SA.plot.progress = TRUE),
                       times = c(1:3))
# Check that everything is allright with MCMC, and significance of our features
summary(stergm.fit)
mcmc.diagnostics(stergm.fit)

# Simulate network evolution through time
# We simulate for 10 steps, starting from the first actual configuration used to
# fit the STERGM model (i.e. the 25/02 mobility situation in Italy)
stergm.sim.1 <- simulate(stergm.fit, nsim = 1, time.slices = 10, nw.start="first")
render.d3movie(stergm.sim.1, plot.par=list(
  displaylabels=T,
  edge.col=ifelse(
    get.edge.attribute.active(stergm.sim.1,'head_region',onset=0, terminus=3 ) != 
      get.edge.attribute.active(stergm.sim.1,'tail_region',onset=0, terminus=3),
    'red','black'
  ),
  vertex.col=get.vertex.attribute.active(stergm.sim.1,'region',onset=0, terminus=3 ),
  vertex.cex=0.5,
  edge.cex=3,
  label.cex=0.5))

saveGIF(render.animation(stergm.sim.1, plot.par=list(
  displaylabels=T,
  edge.col=ifelse(
    get.edge.attribute.active(stergm.sim.1,'head_region',onset=0, terminus=3 ) != 
      get.edge.attribute.active(stergm.sim.1,'tail_region',onset=0, terminus=3),
    'red','black'
  ),
  vertex.col=get.vertex.attribute.active(stergm.sim.1,'region',onset=0, terminus=3 ),
  vertex.cex=0.5,
  edge.cex=3,
  label.cex=0.5)))

# Check goodness of fit
stergm.fit.gof <- gof(stergm.fit)
stergm.fit.gof
par(mfrow=c(2,5))
plot(stergm.fit.gof, main="")
