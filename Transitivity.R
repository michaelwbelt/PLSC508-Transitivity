#
library(network)
library(igraph)
library(intergraph)
library(sna)
#
############Edge Types & Transitivity Scores############
#
# Holland & Leinhardt (1971) define a general transitivity graph (referred to here as a t-graph) under the following conditions. 
# In a set of values called X with values labeled x, y, z, u, v, and w, and directed binary relations, C, 
# that indicate choice (e.g. xCy means “x chooses y”):
# a directed edge goes from x to y if and only if xCy.
# From this, we can define three types of choices:
#   Mutual choices - xMy if and only if xCy and yCx
#   Asymmetric choices - xAy if and only if xCy and not yCx
#   Null choices - xNy if and only if neither xCy nor yCx
# The general structure of a t-graph, they argue, follows a number of criteria: 
# between nodes, edges should indicate an equivalence relation based on reflexivity (xMx), 
# symmetry (xMy implies that yMx), and transitivity (xMy and yMz imply that xMz). 
# These groupings are referred to as M-cliques. We can test these properties at their base level 
# for a theoretical exploration of the concepts, using a function in the igraph package known as dyad_census. 
#
# First, let’s set a seed.
#
set.seed(12345)
#
# We’ll begin with a basic random graph using the sample_gnp function. 
# Similar to the erdos.renyi.game function, sample_gnp takes a number of nodes, 
# a probability of edges between arbitrary vertices, an argument for whether or not 
# the graph is directed, and an argument for whether or not loops can exist within it. 
# For ease of analysis, we’ll start with 10 nodes, and a probability of 0.3. 
# Since transitivity requires direction, we’ll set this argument to TRUE.
#
graph1 <- sample_gnp(10, 0.3, directed = TRUE)
#
# From here, we can test for these three types of edges (mutual, asymmetric, and null) using dyad_census
#
dyad_census(graph1)
#
# It’s important to note here that null edges constitute a lack of edges between two given points 
# throughout the entirety of the graph. This does not mean that these nodes lacking connections are isolates; 
# rather, it indicates null edges between two particular nodes.
#
# Transvity is measured using the transitivity command. Here, we have a number of options. 
# Inputting the graph and setting the level of analysis to global is the same as inputting the graph and foregoing a type. 
# This gives you the degree of transitivity of the whole network, ranging from 0 (no transitivity) to 1 (full transitivity).
#
transitivity(graph1, type = c(type = "global"))
transitivity(graph1)
#
# Likewise, setting the type as undirected or globalundirected produce the exact same results.
# 
transitivity(graph1, type = c(type = "undirected"))
transitivity(graph1, type = c(type = "globalundirected"))
#
# Adding a local modifier will give you transitivities of each particular node in the graph, as will localundirected.
#
transitivity(graph1, type = c(type = "local"))
transitivity(graph1, type = c(type = "localundirected"))
#
# While barrat and weighted, identical modifiers, calculate weighted transitivities, 
# we’re now going to turn to the vids argument, which can output transitivities for specific nodes. 
# If vids is set to NULL, the function outputs all transitivities.
#
transitivity(graph2, type = c(type = "local"), vids=NULL)
#
#Let’s say, however, we want just to incorporate one node ID, such as node 11.
#
transitivity(graph2, type = c(type = "local"), vids=11)
#
# There are a number of other modifiers to this function, but for basic transitivity analysis, these will suffice.
#
############Dyad Restrictions############
#
# Some t-graphs contain only one type of edge. These are labeled as “completely connected graphs” (only M-edges), 
# “transitive tournaments” (only A-edges), and “completely disconnected graphs” (only N-edges). 
# Holland & Leinhardt (1971) list a number of instances, however, where t-graphs will have two, but not three, specific edge types. 
# For example: a graph with only M-edges and A-edges is called a “quasi-series”. 
# Likewise, graphs with only M-edges and N-edges are called “clusterable graphs”, 
# and graphs with A-edges and N-edges but no M-edges are called “partial orders”.
#
# The sna package contains a function called rguman, which allows us to select either the probability of each dyad type 
# or the number of occurrences. We shall create graphs for all six graphs with dyad restrictions, using both methods.
#
# First, we need to detach the igraph package, due to near-identical functions in both igraph and sna. We’ll bring it back in later.
#
detach(package:igraph, unload = T)
#
# We’ll begin with a completely connected graph. As these graphs only contain M-edges, 
# we will indicate this dyad type, the number of graphs we want generated (1), 
# the number of nodes (12), the associated probability (1.0), and the probability method. 
# Note that for M-edges only, we have to exclude values for the other two types.
#
cc1 <- gplot(rguman(1,12,mut=1.0, method=c("probability")))
#
# We can also generate a specific number of dyads of this type using the “exact” argument. 
# In this case, we will try 66, since each edge to another node starts at n-1 edges 
# and decreases by 1 each time. Note that for M-edges, we have to include values for all edge types here.
#
cc2 <- gplot(rguman(1,12,mut=66,asym=0,null=0,method="exact"))
#
# Next, we’ll draw a transitive tournament (A-edges only).
#
tt1 <- gplot(rguman(1,12,mut=0,asym=1.0,null=0, method=c("probability")))
#
tt2 <- gplot(rguman(1,12,mut=0,asym=66,null=0,method="exact"))
#
# Next, let’s graph a completely disconnected graph (N-edges only). 
# For no edges to appear, we have to indicate all 3 edge probabilities.
cd1 <- gplot(rguman(1,12,mut=0,asym=0,null=1.0, method=c("probability")))
#
cd2 <- gplot(rguman(1,12,mut=0,asym=0,null=66,method="exact"))
#
# Now we can move on to using two edge types. First, let’s plot a quasi-series (M and A-edges). 
# Let’s assume the probability is 0.5 for both edges.
qs1 <- gplot(rguman(1,12,mut=0.5,asym=0.5,null=0, method=c("probability")))
#
qs2 <- gplot(rguman(1,12,mut=33,asym=33,null=0,method="exact"))
#
# Next, we can model a clusterable graph (M and N-edges).
cg1 <- gplot(rguman(1,12,mut=0.5,asym=0,null=0.5, method=c("probability")))
#
cg2 <- gplot(rguman(1,12,mut=33,asym=0,null=33,method="exact"))
#
# Finally, we’ll draw a partial order (A and N-edges).
po1 <- gplot(rguman(1,12,mut=0,asym=0.5,null=0.5, method=c("probability")))
#
po2 <- gplot(rguman(1,12,mut=0,asym=33,null=33,method="exact"))
#
# If necessary, one can use dyad.census (the SNA version) to calculate the exact number of dyads in the graphs.
#
dyad.census(cc1)
#
# Or one can calculate transitivity scores (if they loaded in the igraph package again).
#
############Triad Restrictions############
#
# Regarding triads, Holland & Leinhardt (1971) list sixteen different types that are either transitive or intransitive. 
# Each code represents, respectively, how many of each edge-type exist in a triad, 
# with further modifiers added as necessary for direction. To analyze their presence in a graph, 
# we can use the triad_census function in igraph. The function pulls a graph and outputs the number of triads 
# in the graph in the order seen here (https://igraph.org/r/doc/triad_census.html). 
#
# [003, 012, 102, 021D, 021U, 021C, 111D, 111U, 030T, 030C, 201, 120D, 120U, 120C, 210, and 300]
#
#To use this package, we’ll need to reload igraph.
#
library(igraph)
#
# Next, let’s try using this function on the first graph we generated, graph1.
#
graph1 <- sample_gnp(10, 0.3, directed = TRUE)
#
# Then, we can use the triad_census function to examine all triad types.
#
# Let’s also plot another random graph using sample_gnp. 
# This time, let’s incorporate 50 nodes with a probability of .4. We’ll follow it up with triad_census.
#
graph2 <- sample_gnp(50, 0.4, directed = TRUE)
#
triad_census(graph2)
#
# It’s as simple as that. Now, let’s use a practical example to test a number of these techniques on.
#
############Segregation in religion networks - sample############
#
# The data in this study (Hu et al., 2019) comes from an extraction from Weibo, a Chinese social networking website 
# that’s similar to Twitter. The authors set out to examine how persons of particular religious identities group together 
# while distancing themselves from those who share a different religious identity. To do this, they used a keyword search 
# to look for religious terminology in user’s descriptions and nicknames, then hand-checked the data to verify user relgiosity. 
# The sample was narrowed down from over 170,000 users to 6,875, identifying as either Christian (3,151), Buddhist (2,791), Muslim (470), or Taoist (461). 
#
# The included data contains an edgelist (linking user IDs in the lefthand column and followee IDs in the righthand column), 
# and a set of religious identities for each user listed in the edgelist. 
# Since the original dataset contains so many nodes (and since generating a network on the full dataset takes at least 10 minutes on my computer), 
# I have narrowed down the dataset to the first thirty user IDs in the edgelist and their adjacent followees.
#
# First, let's detatch igraph for data loading, then load in the data. We'll start with the edgelist.
detach("package:igraph", unload = TRUE)
library(readr)
EL <- read_csv("EL.csv", na = "empty")
#
# Next, the VLD file, called Tag.
#
library(readr)
Tag <- read_csv("Tag.csv", na = "empty")
#
# We'll create a network object with the edgelist as its input.
#
net <- network(EL[,1:2], matrix.type = 'edgelist', directed = T)
#
# Now, we'll set vertex labels and attributes.
#
network.vertex.names(net)  <- Tag$X.Userid
set.vertex.attribute(net,"Religion",Tag$Religion)
#
# Finally, we'll make a plot.
#
set.seed(12345)
plot(net,displaylabels=T,label.cex=.5,edge.col=rgb(150,150,150,100,maxColorValue=255))
#
# In order to check transitivity scores and run dyad and triad censuses, we need to bring igraph back in.
#
library(igraph)
#
# Let's start by converting the network to an igraph object.
#
inet <- asIgraph(net)
#
# Now, we can check transitivity of the entire network.
#
transitivity(inet)
#
# If the transitivity degree appears low, this is likely due to the sample selection.
# We can also run a local transitivity function to examine specific transitivities.
transitivity(inet, type = c(type = "local"))
# As expected, the majority of transitivities in this network are fairly low.
# Visually, most edges look to be asymetrical. We can verify this with a dyad census.
#
dyad_census(inet)
#
# The dyad census output says this network has few M-edges, slightly more A-edges, and many N-edges. 
# A triad census could also tell us a little more about triad transitivity.
#
triad_census(inet)
#
# The output states there are 3,728,011 empty graphs (003), 81,406 graphs with a single directed edge (012),
# 7,492 out-star formations (021D), 205 in-star formations (021U), and 100 directed lines, which are intransitive by nature (021C).
# From this test, it seems that triad transitivity in this network is substantially low.
# 
# While this subset of the data likely isn't as sufficient as the full edgelist for testing the hypothesis of Hu et al. (2019),
# it still presents a way to examine how these functions operate in a much larger, real-world setting.