source('src/utils.R')
source('src/plot.R')

mobility_pre <- read.csv('data/2020-02-25.csv',sep=";")
mobility_mid <- read.csv('data/2020-03-10.csv',sep=";")
mobility_post <- read.csv('data/2020-05-05.csv',sep=";")

# Mobility doesn't change much intra-provincially...
graph_pre_all <- create_graph_from_data(mobility_pre, metric="n", zeros = F)
graph_mid_all <- create_graph_from_data(mobility_mid, metric="n", zeros = F)
graph_post_all <- create_graph_from_data(mobility_post, metric="n", zeros = F)

print(paste("Movements (all): Pre", round(sum(E(graph_pre_all)$weight)), " -> Mid", 
            round(sum(E(graph_mid_all)$weight)), " -> Post", round(sum(E(graph_post_all)$weight))))

# Inter-province connections are present but invisible since their value is much lower than intra-province ones
plot_weighted_graph(graph_pre_all, graph_mid_all, graph_post_all, v_scale=1.5, v_min=3, e_scale=4)

# ... but is significantly lower inter-provincially
graph_pre_inter <- create_graph_from_data(mobility_pre, metric="n", loops=F, zeros = F)
graph_mid_inter <- create_graph_from_data(mobility_mid, metric="n", loops=F, zeros = F)
graph_post_inter <- create_graph_from_data(mobility_post, metric="n", loops=F, zeros = F)

print(paste("Movements (inter-province): Pre", round(sum(E(graph_pre_inter)$weight)), " -> Mid", 
            round(sum(E(graph_mid_inter)$weight)), " -> Post", round(sum(E(graph_post_inter)$weight))))

# Now we can see inter-province movements
plot_weighted_graph(graph_pre_inter, graph_mid_inter, graph_post_inter, v_scale=1, v_min=3, e_scale=3)

# Visualize movements in one or more regions
regions <- c("Friuli-Venezia Giulia", "Trentino-South Tyrol", "Veneto")
plot_regions_subgraph(graph_pre_inter, graph_mid_inter, graph_post_inter, regions=regions, mfrow=c(1,3))

# Egocentric network of a region (outbound)
regions <- c("Lombardy")
plot_regions_subgraph(graph_pre_inter, graph_mid_inter, graph_post_inter, regions=regions, mfrow=c(1,1),
                      egonet=T, ego_mode="outbound")
# Egocentric network of a region (inbound)
plot_regions_subgraph(graph_pre_inter, graph_mid_inter, graph_post_inter, regions=regions, mfrow=c(1,1),
                      egonet=T, ego_mode="inbound")

