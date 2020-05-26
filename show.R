#density for active players
graph.density(network2)#0.06030532
#density for all-time players
edge_density(network5)
#edge weight for active players
edgeofplayer <- get.edgelist(network2)
weight<-data.frame(weight=edges_players$weight)
edges_player_weight <-cbind(edgeofplayer,weight)

#edge weight for all-time players
edgeofplayer_all <- get.edgelist(network5)
weight_all<-data.frame(weight=E(network5)$weight)
edges_player_weight_all <-cbind(edgeofplayer_all,weight_all)

# Diameter
farthest.nodes(network2, directed=F)#distance 4

# Node degrees
deg <- degree(network2, mode="all")
deg_dataframe <- as.data.frame(deg)
deg_dataframe$node <- row.names(deg_dataframe)
deg_all=degree(network5,mode="all")
deg_all_dataframe=as.data.frame(deg_all)

# Betweenness
betweenness(network2, directed=F, weights=NA)
edge_betweenness(network2, directed=F, weights=NA)
centr_betw(network2, directed=F, normalized=T)


# Plot node degree distribution
ggplot(data = top20_nodes, aes(x = reorder(node, deg), y = deg)) +
  geom_histogram(stat = "identity") +
  coord_flip()

# Centrality & centralization
# Degree (number of ties)
degree_=degree(network2, mode="all")
centr_degree(network2, mode="all", normalized=T)
degree_dataframe<-as.data.frame(degree_)
degree_dataframe$player <- row.names(degree_dataframe)
top20_degrees <- degree_dataframe %>%
  select(player,degree_) %>%
  arrange(desc(degree_)) %>%
  head(20)

# Closeness
closeness_=closeness(network2, mode="all") 
centr_clo_=centr_clo(network2, mode="all")
closeness_dataframe <- as.data.frame(centr_clo_)
closeness_dataframe$player <- players
#res:The node-level centrality scores.
top20_nodes_closeness <- closeness_dataframe %>%
  select(player, res) %>%
  arrange(desc(res)) %>%
  head(20)
top20_nodes_closeness

# Eigenvector
eigen_centrality_<-eigen_centrality(network2, directed=F, weights=NA)
eigen_centrality_dataframe<-data.frame(eigen_centrality_)
eigen_centrality_dataframe$player<-row.names(eigen_centrality_dataframe)
top20_nodes_eigen_centrality <- eigen_centrality_dataframe %>%
  select(player, vector, value) %>%
  arrange(desc(vector)) %>%
  head(20)
top20_nodes_eigen_centrality



