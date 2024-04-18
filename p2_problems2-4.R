# Load libraries ##############################################################

library(igraph)   # igraph_1.4.2 
library (readr)   # readr_2.1.4   
library(ggplot2)  # ggplot2_3.4.2 


# Create Twitter Network Graphs ###############################################

# Create an empty list to store the graphs
graph_list <- list()

for (i in 1:5) {
  # Load comma separated file with the list of edges of the network 
  edgelist <- read_csv(paste0("data/edgelist_July_", i, ".csv"), 
                       show_col_types = FALSE)
  nodelist <- read_csv(paste0("data/nodelist_July_", i, ".csv"), 
                       show_col_types = FALSE)
  
  # Create directed weighted igraph graph
  graph_list[[paste0("graph_", i)]] <- graph_from_data_frame(edgelist, 
                                         directed = TRUE, vertices = nodelist)
  # Checks
  cat(paste("  Graph of July", i), "\n")
  cat("|------------------|\n")
  cat(paste("| Vertices:", vcount(graph_list[[i]]), "|"), "\n")
  cat(paste("| Edges:", ecount(graph_list[[i]]), "   |"), "\n")
  cat("|------------------|\n")
  # edge.attributes(graph_list[[paste0("graph_", i)]])$weight
  # vertex.attributes(graph_list[[paste0("graph_", i)]])$name
  # vertex.attributes(graph_list[[paste0("graph_", i)]])$topic_of_interest
}

# Free space
rm(edgelist, nodelist, i)
invisible(gc())


# Average degree over time ####################################################

# Start of comment ************************************************************
# # Create a matrix to store the metric values for each day
# metrics <- matrix(NA, nrow = 5, ncol = 5)
# colnames(metrics) <- c("NumVertices", "NumEdges", "Diameter", "AvgInDegree",
#                        "AvgOutDegree")
# 
# # Loop through the five days and calculate the metrics for each day's graph
# # By using the strength function, the weights of the edges are considered in 
# # the calculation of the average in-degree and average out-degree, which is 
# # appropriate for a weighted directed graph.
# for (i in 1:5) {
#   graph <- graph_list[[i]]  # Get the graph object for the current day
# 
#   metrics[i, "NumVertices"] <- vcount(graph)
#   metrics[i, "NumEdges"] <- ecount(graph)
#   metrics[i, "Diameter"] <- diameter(graph)
#   metrics[i, "AvgInDegree"] <- mean(strength(graph, mode = "in", 
#                                              loops = FALSE))
#   metrics[i, "AvgOutDegree"] <- mean(strength(graph, mode = "out", 
#                                               loops = FALSE))
# }
# 
# # Convert the matrix to a data frame for easier manipulation and visualization
# metrics_df <- as.data.frame(metrics)
# 
# # Write the data.frame to a CSV file
# write.csv(metrics_df, file = "data/metrics.csv", row.names = FALSE)
# End of comment **************************************************************

# Because the diameter is a computationally heavy measurement in large graphs,
# the previously computed metrics of the graphs were exported and the code was
# commented out
metrics_df <- read_csv("data/metrics.csv", show_col_types = FALSE)
print(metrics_df)

# Define a custom theme for the plots
custom_theme <- theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

# Plot for Number of Vertices
ggplot(metrics_df, aes(x = 1:5, y = NumVertices)) +
  geom_point(color = "steelblue", size = 3) +
  geom_line(color = "steelblue", linewidth = 1.5, alpha = 0.8) +
  geom_text(aes(label = format(NumVertices, big.mark = ",")),
            color = "black", size = 3,
            vjust = ifelse(1:5 <= 3, -1.5, 2),
            hjust = ifelse(1:5 <= 3, 0, 1)) +
  labs(x = "Day of July 2009", y = "Number of Vertices") +
  ggtitle("Evolution of Number of Vertices") + 
  ylim(0, max(metrics_df$NumVertices)*1.1) + custom_theme

# Plot for Number of Edges
ggplot(metrics_df, aes(x = 1:5, y = NumEdges)) +
  geom_bar(stat = "identity", fill = "darkgreen", width = 0.5) +
  geom_text(aes(label = format(NumEdges, big.mark = ",")),
            vjust = -0.5, color = "black", size = 3) +
  labs(x = "Day of July 2009", y = "Number of Edges") +
  ggtitle("Evolution of Number of Edges") +
  custom_theme

# Plot for Diameter
ggplot(metrics_df, aes(x = 1:5, y = Diameter)) +
  geom_area(fill = "purple", alpha = 0.5, color = "black") +
  geom_point(color = "black", size = 3) +
  labs(x = "Day of July 2009", y = "Diameter") +
  ggtitle("Evolution of Diameter") +
  custom_theme +
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(limits = c(0, max(metrics_df$Diameter) * 1.1)) +
  annotate("text", x = 1:5, y = metrics_df$Diameter, label = 
             format(metrics_df$Diameter, big.mark = ","), vjust = -1, size = 3)

# Plot for Average In-degree
ggplot(metrics_df, aes(x = 1:5, y = AvgInDegree)) +
  geom_line(color = "red", linewidth = 1.5, linetype = "dashed") +
  geom_point(color = "red", size = 3, shape = 21, fill = "white") +
  labs(x = "Day of July 2009", y = "Average In-degree") +
  ggtitle("Evolution of Average In-degree") +
  custom_theme +
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(limits = c(0, max(metrics_df$AvgInDegree) * 1.5)) +
  geom_text(aes(label = format(round(AvgInDegree, 2))), vjust = 
              ifelse(1:5%%2==1, -1, 1.7), color = "black", size = 3)

# Plot for Average Out-degree
ggplot(metrics_df, aes(x = 1:5, y = AvgOutDegree)) +
  geom_line(color = "orange", linewidth = 1.5, linetype = "dashed") +
  geom_point(color = "orange", size = 3, shape = 21, fill = "white") +
  labs(x = "Day of July 2009", y = "Average Out-degree") +
  ggtitle("Evolution of Average Out-degree") +
  custom_theme +
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(limits = c(0, max(metrics_df$AvgOutDegree) * 1.5)) +
  geom_text(aes(label = format(round(AvgOutDegree, 2))), vjust = 
              ifelse(1:5%%2==1, -1, 1.7), color = "black", size = 3)


# Important nodes #############################################################

# Create empty data frames for each metric
df_indegree <- data.frame(matrix(NA, nrow = 10, ncol = 5))
df_outdegree <- data.frame(matrix(NA, nrow = 10, ncol = 5))
df_pagerank <- data.frame(matrix(NA, nrow = 10, ncol = 5))

# Set the column names for each data frame
colnames(df_indegree) <- paste0("Day_", 1:5)
colnames(df_outdegree) <- paste0("Day_", 1:5)
colnames(df_pagerank) <- paste0("Day_", 1:5)

# Loop through the five days
for (i in 1:5) {
  # Get the graph for the current day
  graph <- graph_list[[i]]
  
  # Calculate in-degree, out-degree, and PageRank for each vertex
  indegree <- strength(graph, mode = "in", loops = FALSE)
  outdegree <- strength(graph, mode = "out", loops = FALSE)
  pagerank <- page.rank(graph)$vector

  # Sort the metrics and select the top-10 users
  indegree <- sort(indegree, decreasing = TRUE)[1:10]
  outdegree <- sort(outdegree, decreasing = TRUE)[1:10]
  pagerank <- sort(pagerank, decreasing = TRUE)[1:10]
  
  # Update the data frames with the top-10 users and their scores
  df_indegree[, i] <- names(indegree)
  df_outdegree[, i] <- names(outdegree)
  df_pagerank[, i] <- names(pagerank)
}

# Remove not needed variables
rm(graph, indegree, outdegree, pagerank, i)

# Print the data frames
{
cat("Top-10 Twitter Users based on In-Degree for different days of July 2009:")
cat("\n")
print(df_indegree)
cat("\n")

cat("Top-10 Twitter Users based on Out-Degree for different days of July 2009:")
cat("\n")
print(df_outdegree)
cat("\n")

cat("Top-10 Twitter Users based on PageRank for different days of July 2009:")
cat("\n")
print(df_pagerank)
cat("\n")
}


# Communities #################################################################

# Convert the graphs to undirected versions
graph_list_undirected <- lapply(graph_list, as.undirected)

# Apply Fast Greedy Clustering (58 min)

# t1 <- Sys.time()
# fgc <- lapply(graph_list_undirected, cluster_fast_greedy)
# t2 <- Sys.time()
# saveRDS(fgc, "data/fgc.rds")
fgc <- readRDS("data/fgc.rds")
mean(unlist(lapply(fgc, modularity))) # 0.8753156

# Apply Infomap Clustering (26 mins for nb.trials=1, 6.9 hours nb.trials=2)

# t3 <- Sys.time()
# imc2 <- lapply(graph_list_undirected, cluster_infomap, nb.trials = 1)
# t4 <- Sys.time()
# saveRDS(imc, "data/imc.rds")
# t5 <- Sys.time()
# imc2 <- lapply(graph_list_undirected, cluster_infomap, nb.trials = 2)
# t6 <- Sys.time()
# saveRDS(imc2, "data/imc2.rds")
imc <- readRDS("data/imc.rds")
imc2 <- readRDS("data/imc2.rds")
mean(unlist(lapply(imc, modularity)))  # 0.774634
mean(unlist(lapply(imc2, modularity))) # 0.7746889
# With nb.trials = 2 it took approximately 6.5 hours more to run with minor
# improvement in the clustering quality, so it is of no gain to continue to
# run the algorithm for greater number of attempts (nb.trials >= 3) 

# Apply Louvain Clustering (15 sec)

# t7 <- Sys.time()
# lc <- lapply(graph_list_undirected, cluster_louvain)
# t8 <- Sys.time()
# saveRDS(lc, "data/lc.rds")
lc <- readRDS("data/lc.rds")
mean(unlist(lapply(lc, modularity))) # 0.880615

# Louvain Clustering approximately the same as Fast Greedy and slightly better
# in terms of quality of clustering and a lot faster too
rm(fgc, imc, imc2)
invisible(gc())

# Create a list of users appearing in all graphs
common_users <- Reduce(intersect, list(V(graph_list_undirected$graph_1)$name, 
                                       V(graph_list_undirected$graph_2)$name, 
                                       V(graph_list_undirected$graph_3)$name, 
                                       V(graph_list_undirected$graph_4)$name, 
                                       V(graph_list_undirected$graph_5)$name))
{
# Randomly select one user from the common_users list
# Un-comment the following line to select a random user from the pool of common
# random_user <- sample(common_users, 1)
# and comment the following line
random_user <- "fabioricotta" # selected a stable user for the report
cat("The topic of interest for the user '", random_user, "' is:\n", sep = "")

# Iterate over the graph list and print the topic of interest for each day
for (i in 1:5) {
  topic_of_interest <- V(graph_list_undirected[[i]])[
    random_user]$topic_of_interest
  cat("Day", i, ":", topic_of_interest, "\n")
}
cat("\n")
cat("-----------------------------------------------------------------------\n")
cat("\n")

# Retrieve community membership of the selected user for each day
community_membership_list <- lapply(lc, membership)
user_communities <- lapply(community_membership_list, function(cm) 
  cm[random_user])

# Extract vertices belonging to the community
community_vertices <- lapply(1:5, function(i) 
  V(graph_list_undirected[[i]])[community_membership_list[[i]] == 
                                  user_communities[[i]]])

# Iterate over each day (graph) to compare community membership
for (i in 1:4) {
  # Retrieve the community membership of the user for the current and next day
  user_community_current <- community_membership_list[[i]][random_user]
  user_community_next <- community_membership_list[[i+1]][random_user]
  
  # Get the communities for the current and next day
  communities_current <- communities(lc[[paste0("graph_", i)]])
  communities_next <- communities(lc[[paste0("graph_", i + 1)]])
  
  # Retrieve the community for the user in the current and next day
  user_community_current <- communities_current[[user_community_current]]
  user_community_next <- communities_next[[user_community_next]]

  top_5_pageRank_current <- head(sort(page.rank(graph_list_undirected[[
    i]])$vector[user_community_current], decreasing = TRUE), 10)
  top_5_pageRank_next <- head(sort(page.rank(graph_list_undirected[[
    i+1]])$vector[user_community_next], decreasing = TRUE), 10)
  intersection <- intersect(names(top_5_pageRank_current), 
                            names(top_5_pageRank_next))
  
  
  # Compare the two communities
  if (length(intersection) > 0) {
    cat("The user belongs to the same community in day", i, "and day", 
        i + 1, "\nwith these highly PageRank evaluated (important) users:\n| ")
    cat(intersection, "\n\n", sep = " | ")
  } else {
    cat("The user belongs to different communities in day", i, "and day", 
        i + 1, "\n\n")
  }
}
cat("-----------------------------------------------------------------------\n")

# Retrieve topic attributes for those vertices
topic_attributes <- lapply(community_vertices, function(vertices) 
  V(graph_list_undirected[[i]])$topic_of_interest[vertices])
topic_attributes <- lapply(topic_attributes, function(attrs) 
  attrs[!is.na(attrs)])

# Retrieve the top 10 most frequent topics for each day
top_10_topics <- lapply(topic_attributes, function(attrs) 
  head(sort(table(attrs), decreasing = TRUE), 10))
cat("\nThe communities that the user was partitioned have the",
    "following 10 most important topics:\n")
for (i in 1:5) {
  cat("\n")
  cat("Day:", i, "- Community:", unname(user_communities[[i]]), "\n Topics:", 
      names(top_10_topics[[i]]), "\n")
}
cat("\nThe communities that the user was partitioned share the following", 
    "topics:\n")
intersection <- Reduce(intersect, lapply(top_10_topics, names))
if (is.null(intersection)) {
  cat("None in common\n\n")
} else {
  cat(intersection, "\n\n")
}
cat("-----------------------------------------------------------------------\n")
}
rm(community_membership_list, graph_list_undirected)
invisible(gc())

# Visualization ###############################################################

for (i in 1:5) {
  # Color vertices by community membership as a factor
  communities <- lc[[paste0("graph_", i)]]
  V(graph_list[[paste0("graph_", i)]])$color <- factor(membership(communities))
  
  # Does the edge cross between communities?
  is_crossing <- crossing(graph_list[[paste0("graph_", i)]], 
                          communities = communities)
  
  # Set edge line type: solid for crossings, dotted otherwise
  E(graph_list[[paste0("graph_", i)]])$lty <- ifelse(is_crossing, "solid", 
                                                     "dotted")
  
  # Get the sizes of each community
  community_size <- sizes(communities)
  
  # Select mid-size communities
  in_mid_community <- unlist(communities[community_size > 50 & 
                                           community_size < 300])
  
  # Induce a subgraph of the current graph using mid-size communities
  subgraph <- induced.subgraph(graph_list[[paste0("graph_", i)]], 
                               in_mid_community)
  
  # Plot the mid-size communities
  plot(subgraph, vertex.label = NA, edge.arrow.width = 0.8,
       edge.arrow.size = 0.2, coords = layout_with_fr(subgraph),
       margin = 0, vertex.size = 3, main = paste("Day", i))
}
