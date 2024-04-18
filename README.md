# From raw data to temporal graph structure exploration

## General Instructions
Your answers should be as concise as possible.  
**Submission instructions:** You should submit a compressed directory, containing your answers and code.  
**Submitting answers:** Prepare a report with your answers on this homework in a single PDF file named `p2.pdf`.  
**Submitting code:** Prepare the source file(s) with your code.

## Problem
1. **Twitter mention graph**  
   Create a weighted directed graph with igraph using raw data from Twitter. Manipulate the raw data to create 5 .csv files, each representing the weighted directed mention graph for the respective day of July 2009. Identify the most important topic for each user based on their hashtags and create 5 .csv files, each containing the user and their most important topic.

2. **Average degree over time**  
   Create plots visualizing the 5-day evolution of different metrics for the graph:
   - Number of vertices
   - Number of edges
   - Diameter of the graph
   - Average in-degree
   - Average out-degree  
   Provide observations on the fluctuations of these metrics during the five days.

3. **Important nodes**  
   Write code to create and print data frames for the 5-day evolution of the top-10 Twitter users with regard to:
   - In-degree
   - Out-degree
   - PageRank  
   Provide comments on the variations of the top-10 lists for different days.

4. **Communities**  
   Perform community detection on the mention graphs using fast greedy clustering, infomap clustering, and Louvain clustering on the undirected versions of the 5 mention graphs. Write code to detect the evolution of communities for a random user that appears in all 5 graphs. Visualize the graph using a different color for each community, filtering out small or large communities for a meaningful visualization. Include comments on the performance of the clustering algorithms and observations on the communities and their topics of interest.
