library(igraph) # SNA
library(dplyr) # Data manipulation & data management
library(ggplot2) # Data visualization
library(dbscan)
library(networkR)
library(tibble)
#########################################
#acrive players
data<-read.csv(file = "NBA_network.csv", header = FALSE, sep = ",")
#show the first 5 records
data[1:5,]
#extract players col
players = data[,2]
#extract team col
teams = data[,1]
#edge
edges<-data.frame(players,teams)
length(edges[,1])
glimpse(players)
#players without repetition
players<-unique(edges$players)
#revised node dataframe
player.vector<-unique(edges$players)
team.vector<-unique(edges$teams)
player.vector.type<-rep("player", times=length(player.vector))
team.vector.type<-rep("team", times=length(team.vector))
df1<-data.frame(node=as.character(player.vector),
                type=as.character(player.vector.type))
df2<-data.frame(node=as.character(team.vector),
                type=as.character(team.vector.type))
edges[]<-lapply(edges, as.character)
revised_nodes <-rbind(df1, df2)
glimpse(revised_nodes)
#if is a player, replace it with true, if is a team, replace it with false
revised_nodes$type<-ifelse(revised_nodes$type == "player", TRUE, FALSE)
head(revised_nodes,6)
glimpse(revised_nodes)
#create igraph graph from data frame
network1<-graph_from_data_frame(d=edges, vertices = revised_nodes, directed = F)
#plot the whole network
plot.igraph(network1,
            vertex.size = 1,
            vertex.label.cex = 0.5,
            main = "SNA")
#plot the network of player to player (the main network in this project)
network2 <- bipartite_projection(network1, which = "TRUE")
plot.igraph(network2,
            vertex.size = 5,
            vertex.shape="csquare",
            vertex.color="skyblue2",
            vertex.label = NA,
            edge.width=0.1,
            edge.lty="dashed",
            main = "Network of PLayer to Player")
#plot the network of team to team
network3 <- bipartite_projection(network1, which = "FALSE")
plot.igraph(network3,
            vertex.size = 3,
            vertex.label.cex = 0.5,
            main = "A Network of Team to Team")
############################################################
#NBA players of all time
data_all <- read.csv(file="NBA.csv", encoding = "", header = FALSE,sep = ",")
head(data_all,10)
player_all <- data_all[,2]
team_all <- data_all[,1]
edges_all <- data.frame(player_all,team_all)
player_all <- unique(edges_all$player_all)
#revised node dataframe
player_all.vector <- unique(edges_all$player_all)
team_all.vector <- unique(edges_all$team_all)
player_all.vector.type <- rep("player", times=length(player_all.vector))
team_all.vector.type <- rep("team", times=length(team_all.vector))
df3 <- data.frame(node=as.character(player_all.vector),
                type=as.character(player_all.vector.type))
df4 <- data.frame(node=as.character(team_all.vector),
                type=as.character(team_all.vector.type))
edges_all[] <- lapply(edges_all, as.character)
revised_nodes_all <- rbind(df3, df4)
glimpse(revised_nodes_all)
#if is a player, replace it with true, if is a team, replace it with false
revised_nodes_all$type <- ifelse(revised_nodes_all$type == "player", TRUE, FALSE)
head(revised_nodes_all,6)
glimpse(revised_nodes_all)
#create igraph graph from data frame
network4 <- graph_from_data_frame(d=edges_all, vertices = revised_nodes_all, directed = F)
#plot the whole network
plot.igraph(network4,
            vertex.size = 1,
            vertex.label.cex = 0.5,
            main = "SNA")
#plot the network of player to player (the main network in this project)
network5 <- bipartite_projection(network4, which = "TRUE")
plot.igraph(network5,
            vertex.size = 3,
            vertex.shape="circle",
            vertex.color="orange",
            vertex.label = NA,
            edge.width=0.1,
            edge.lty="dashed",
            main = "Network of PLayer to Player (All Time)")
#plot the network of team to team
network6 <- bipartite_projection(network4, which = "FALSE")
plot.igraph(network3,
            vertex.size = 3,
            vertex.label.cex = 0.5,
            main = "Bipartite Projection: a Network of Team to Team")
###################################################
#Compositional and Structural Analysis
###################################################
#see vertices in the main network
V(network2)
#see edges in the main network
edges_players <- E(network2)

#show the first 6 records
head(data,6)
data[,2]
#Get the top 10 player who played the longest seasons
head(sort(table(data[,2]), decreasing = T),10)

#density for active players
graph.density(network2)#0.06030532
#density for all-time players
edge_density(network5)

#edge weight for active players
edgeofplayer <- get.edgelist(network2)
weight <- data.frame(weight=edges_players$weight)
edges_player_weight <- cbind(edgeofplayer,weight)
#edge weight for all-time players
edgeofplayer_all <- get.edgelist(network5)
weight_all <- data.frame(weight=E(network5)$weight)
edges_player_weight_all <- cbind(edgeofplayer_all,weight_all)

# Diameter for active players
farthest.nodes(network2, directed=F)
# Diameter for all-time players
farthest.nodes(network5, directed=F)

# Node degrees for active players
deg <- degree(network2, mode="all")
deg_dataframe <- as.data.frame(deg)
deg_dataframe$node <- row.names(deg_dataframe)
top20_nodes <- deg_dataframe %>%
  select(node, deg) %>%
  arrange(desc(deg)) %>%
  head(20)
top20_nodes
# Node degrees for all-time players
deg_all <- degree(network5,mode="all")
deg_all_dataframe <- as.data.frame(deg_all)
deg_all_dataframe$node <- row.names(deg_all_dataframe)
top20_all_nodes <- deg_all_dataframe %>%
  select(node, deg_all) %>%
  arrange(desc(deg_all)) %>%
  head(20)
top20_all_nodes

# Betweenness for active players
btw <- betweenness(network2, directed=F, weights=NA)
btw <- as.data.frame(btw)
btw <- t(btw)
edge_btw <- edge_betweenness(network2, directed=F, weights=NA)
edge_btw_ <- get.edgelist(network2)
edge_btw_list <- cbind(edge_btw_, edge_btw)
cent_btw <- centr_betw(network2, directed=F, normalized=T)
# Betweenness for all-time players
btw_all <- betweenness(network5, directed=F, weights=NA)
btw_all <- as.data.frame(btw_all)
btw_all <- t(btw_all)
edge_btw_all <- edge_betweenness(network5, directed=F, weights=NA)
edge_btw_all_ <- get.edgelist(network5)
edge_btw_all_list <- cbind(edge_btw_all_, edge_btw_all)
centr_betw(network5, directed=F, normalized=T)

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
top20_degrees

# Closeness for active players
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
# Closeness for all-time players
closeness_all_=closeness(network5, mode="all") 
centr_clo_all_=centr_clo(network5, mode="all")
closeness_all_dataframe <- as.data.frame(centr_clo_all_)
closeness_all_dataframe$player <- player_all
#res:The node-level centrality scores.
top20_nodes_closeness_all <- closeness_all_dataframe %>%
  select(player, res) %>%
  arrange(desc(res)) %>%
  head(20)
top20_nodes_closeness_all

# Eigenvector for active players
eigen_centrality_<-eigen_centrality(network2, directed=F, weights=NA)
eigen_centrality_dataframe<-data.frame(eigen_centrality_)
eigen_centrality_dataframe$player<-row.names(eigen_centrality_dataframe)
top20_nodes_eigen_centrality <- eigen_centrality_dataframe %>%
  select(player, vector, value) %>%
  arrange(desc(vector)) %>%
  head(20)
top20_nodes_eigen_centrality
# Eigenvector for all-time players
eigen_centrality_all_<-eigen_centrality(network5, directed=F, weights=NA)
eigen_centrality_all_dataframe<-data.frame(eigen_centrality_all_)
eigen_centrality_all_dataframe$player<-row.names(eigen_centrality_all_dataframe)
top20_nodes_eigen_centrality_all <- eigen_centrality_all_dataframe %>%
  select(player, vector, value) %>%
  arrange(desc(vector)) %>%
  head(20)
top20_nodes_eigen_centrality_all

#######################################
# Community detection
#######################################
clv <- cluster_louvain(network2)
class(clv)
length(clv) # number of communities
membership(clv) # community membership for each node
modularity(clv) # how modular the graph partitioning is
crossing(clv, network2)  # boolean vector: TRUE for edges across communities
plot(clv, network2,vertex.label.cex=0.5, vertex.size=10, main="CLV")
clv_member<- as.data.frame(clv$membership)
clv_member$player <- players
###############all-time
clv_all <- cluster_louvain(network5)
class(clv_all)
length(clv_all) # number of communities
membership(clv_all) # community membership for each node
modularity(clv_all) # how modular the graph partitioning is
crossing(clv_all, network5)  # boolean vector: TRUE for edges across communities
plot(clv_all, network5,vertex.label=NA, vertex.size=10, main="CLV All")
clv_member_all<- as.data.frame(clv_all$membership)
clv_member_all$player <- player_all

######################################################
#Proximity Mearsure
######################################################
#Proximity Mearsure for active players
adj<-as.matrix(get.adjacency(network2))
g<-graph.adjacency(adj)
plot(g,edge.arrow.size=.1)
source("SNN_GRAPH.R") 
SNN_output<-SNN_GRAPH(adj,2)
SNN_output
SNN_output_ <- as.data.frame(SNN_output[[2]])
#Proximity Mearsure for all-time players
adj_all<-as.matrix(get.adjacency(network5))
g_all<-graph.adjacency(adj_all)
plot(g_all,edge.arrow.size=.1)
SNN_output_all<-SNN_GRAPH(adj_all,2)
SNN_output_all
SNN_output_all_ <- as.data.frame(SNN_output_all[[2]])

##################################################
#Graph Cluster Analysis
##################################################
#Maximal Clique R code
source("maximalCliqueEnumerator.R") 
library(RBGL)
maximalCliqueEnumerator_<-maximalCliqueEnumerator(network2)
#Maximal Clique R code for all-time players
maximalCliqueEnumerator_all_<-maximalCliqueEnumerator(network5)





