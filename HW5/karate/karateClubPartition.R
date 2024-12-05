# Karate example

library(igraph)

# if you do not have igraphdata package, install it
# install.packages("igraphdata")

library(igraphdata)

data(karate, package = "igraphdata")

# This is not a standard dataset stored in a simple matrix

print(karate)

plot(karate,vertex.color="pink")

# Karate club split into two factions

get.vertex.attribute(karate, "Faction")

# Other vertex attributes

get.vertex.attribute(karate)

# Obtain layout and plot graph
# Many different layouts to choose from 

karate_layout <- layout.davidson.harel(karate)

# nodes in faction 1 will be rectangles
# nodes in faction 2 will be circles

shapes = c('rectangle', 'circle')

faction_vertex_shape = get.vertex.attribute(karate, "Faction")

faction_vertex_shape[faction_vertex_shape == 1] = shapes[1]

faction_vertex_shape[faction_vertex_shape == 2] = shapes[2]


plot(karate, 
     layout=karate_layout, 
     vertex.shape=faction_vertex_shape)


# Compute degree for each node

karate_degree = degree(karate)

order_degree = order(karate_degree, decreasing = T)


# Get 5 most connected people and their degree

karate_degree[order_degree[1:5]]


plot(karate, 
     layout=karate_layout, 
     vertex.shape=faction_vertex_shape,
     vertex.size=karate_degree / sum(karate_degree) * 200)

# Plot adjacency matrix
source("plot_matrix.R")

adj<-get.adjacency(karate)

adj<-data.matrix(adj)

colnames(adj)<-get.vertex.attribute(karate)$name

rownames(adj)<-get.vertex.attribute(karate)$name


myImagePlot(adj)

title("Adjacency matrix: Karate Club")

# Can we see communities from this plot?
# Not yet

# Let's try to reshuffle the rows and columns
# Note that there are 34 people in the network


permute<-c(3,9,10,14,15,16,19,21,23,24,25,
		   26,27,28,29,30,31,32,33,34,1,
		    2,4,5,6,7,8,11,12,13,17,18,20,22)
 
# the permutation is an output from an algorithm, we do not know it before we start analyzing the data
 
adj_permuted<-adj[permute,permute]

# it turns out that we can tease out the two groups quite well with the right permutation (reshuffling)

myImagePlot(adj_permuted)

title("Permuted Adjacency matrix: Karate Club")
# Can you see the communities now?

# How did we come up with the permutation???? With a community detection algorithm 



# Large number of community detection algorithms

# edge.betweenness.community [Newman and Girvan, 2004]
# fastgreedy.community [Clauset et al., 2004] (modularity optimization method)
# label.propagation.community [Raghavan et al., 2007]
# leading.eigenvector.community [Newman, 2006]
# multilevel.community [Blondel et al., 2008] (the Louvain method)
# optimal.community [Brandes et al., 2008]
# spinglass.community [Reichardt and Bornholdt, 2006]
# walktrap.community [Pons and Latapy, 2005]
# infomap.community [Rosvall and Bergstrom, 2008]


# Let's try the edge betweenness community detection of 
# Girvan and Newman


plot(karate, 
     layout=karate_layout, 
     vertex.shape=faction_vertex_shape)


eb = edge_betweenness(karate, directed=F, weights = NULL)

as_edgelist(karate)[which.max(eb),]

ebc = edge.betweenness.community(karate)

par(mfrow=c(1,1))


# Group into two communities

ebc2 = cutat(ebc, 2)

colors = c('blue', 'red')

plot(karate,
     layout=karate_layout, 
     vertex.shape=faction_vertex_shape,
     vertex.color=colors[ebc2]
     )

# Communities based on our network data analysis

ebc2

# These labels provide the ordering we used before in the permuted adjacency matrix

order(ebc2) 

permute 

# Communities that actually happened when the club split

vertex.attributes(karate)$Faction

# Some nodes were misclassified 

t<-table(vertex.attributes(karate)$Faction,ebc2==1)

t


# only two people were misclassified, our community detection almost perfectly identified the two clusters!


plot(karate, 
     layout=karate_layout, vertex.color=ebc2,
     vertex.shape=faction_vertex_shape)

# Boxes and circles correspond to the actual communities 
# Colors are our estimated clusters

# There are two yellow boxes: these are two people who misclassified. All the other people were correctly assigned their community.


# EXTRA STUFF: hierarchical clustering on the graph 

# This is a dendrogram plot

dendPlot(ebc)

# You can read it from the bottom to the top.
# At the bottom, there are no links between people. Each person is in their own community (34 communities).
# Then we add a link (first link is between John A and Actor 33 at the bottom left corner). THese two now form a joint community. We continue to add the links, merging communities.
#Eventually, there will be one huge graph (a single community containing everyone, very top of the graph)
# The question is, when do we stop adding edges?
# ``Modularity" is a concept for characterizing how well the  clusters are separated. Zero modularity means that there is basically no community structure. 
# The four clusters on the plot are determined by modularity.


plot(ebc$modularity)

modularity(ebc) 


abline(v=which.max(ebc$modularity))

which.max(ebc$modularity)

# 29 edges in the cut graph

plot(ebc, karate,
     layout=karate_layout, 
     vertex.shape=faction_vertex_shape)



