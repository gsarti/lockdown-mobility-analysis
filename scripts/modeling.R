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

# First attempt with all vertex features and only edges
gc_pre_model <- ergm(gc_pre_net ~ edges+ nodecov('covid_cases') + nodecov('covid_deaths') + 
                       nodecov('population') + nodecov('surface') + nodecov('pop_density') + 
                       nodecov('municipalities'))
summary(gc_pre_model)

gc_mid_model <- ergm(gc_mid_net ~ edges+ nodecov('covid_cases') + nodecov('covid_deaths') + 
                       nodecov('population') + nodecov('surface') + nodecov('pop_density') + 
                       nodecov('municipalities'))
summary(gc_mid_model)

gc_post_model <- ergm(gc_post_net ~ edges+ nodecov('covid_cases') + nodecov('covid_deaths') + 
                       nodecov('population') + nodecov('surface') + nodecov('pop_density') + 
                       nodecov('municipalities'))
summary(gc_post_model)


# Second attempt with only significant vertex features from previous analysis and only edges
gc_pre_model <- ergm(gc_pre_net ~ edges + nodecov('covid_deaths') +  nodecov('population'))
summary(gc_pre_model)

gc_mid_model <- ergm(gc_mid_net ~ edges + nodecov('covid_deaths') + nodecov('population'))
summary(gc_mid_model)

gc_post_model <- ergm(gc_post_net ~ edges + nodecov('covid_deaths') + nodecov('population'))
summary(gc_post_model)

# Third attempt with only significant vertex features from previous analysis and some statistics of the graphs
gc_pre_model <- ergm(gc_pre_net ~ edges + idegree1.5 + odegree1.5 + transitive + mutual
                     + nodecov('covid_deaths') +  nodecov('population'))
summary(gc_pre_model)
mcmc.diagnostics(gc_pre_model)

gc_mid_model <- ergm(gc_pre_net ~ edges + idegree1.5 + odegree1.5 + transitive + mutual
                     + nodecov('covid_deaths') +  nodecov('population'))
summary(gc_mid_model)
mcmc.diagnostics(gc_mid_model)

gc_post_model <- ergm(gc_pre_net ~ edges + idegree1.5 + odegree1.5 + transitive + mutual
                      + nodecov('covid_deaths') +  nodecov('population'))
summary(gc_post_model)
mcmc.diagnostics(gc_post_model)

??'ergm-terms'

degree
idegree
idegree1.5
odegree
odegree1.5 
transitive 
mutual
