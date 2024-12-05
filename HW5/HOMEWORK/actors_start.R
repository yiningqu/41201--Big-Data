## actors network example

library(igraph)

### GRAPH
## read in a graph in the `graphml' format: xml for graphs.
## it warns about pre-specified ids, but we want this here
## (these ids match up with the castlists in movies.txt)

actnet <- read.graph("actors.graphml",format="graphml")

### TRANSACTION
## read in the table of actor ids for movies
## this is a bit complex, because the movie names
## contain all sorts of special characters.

movies <- read.table("movies.txt", sep="\t", 
	row.names=1, as.is=TRUE, comment.char="", quote="")

## it's a 1 column matrix.  treat it like a vector

movies <- drop(as.matrix(movies))

## each element is a comma-separated set of actor ids.  
## use `strsplit' to break these out

movies <- strsplit(movies,",")

## and finally, match ids to names from actnet

casts <- lapply(movies, 
	function(m) V(actnet)$name[match(m,V(actnet)$id)])

## check it

casts['True Romance']

## format as arules transaction baskets

library(arules)

casttrans <- as(casts, "transactions")


## Set up STM information

castsize <- unlist(lapply(casts, function(m) length(m)))

## see ?rep.int: we're just repeating movie names for each cast member

acti <- factor(rep.int(names(casts),times=castsize))

## actors

actj <- factor(unlist(casts), levels=V(actnet)$name)

## format as STM (if you specify without `x', its binary 0/1)

actmat <- sparseMatrix(i=as.numeric(acti),j=as.numeric(actj),
		dimnames=list(movie=levels(acti),actor=levels(actj)))

## count the number of appearences by actor

nroles <- colSums(actmat)

names(nroles) <- colnames(actmat)


# Q1
# The actors network has an edge if the two actors were in the same movie. 
# Plot the entire actors network.


# plot(actnet, vertex.size=10, vertex.label=NA,
#      vertex.color="skyblue", edge.color="gray",
#      main="Actors Network")


# Q2
# Plot the neighborhoods for “Bacon, Kevin” at orders 1-3. 
# How does the size of the network change with order?

edge_list <- get.edgelist(actnet)
# Create a new graph object from the edge list
kblink <- graph_from_edgelist(edge_list, directed = FALSE)
kb <- graph.neighborhood(kblink, 1, V(kblink)["Bacon, Kevin"],mode="in")[[1]]
V(kb)$color <- "gold"
V(kb)["Bacon, Kevin"]$color <- "red"
plot(kb,vertex.label=NA, vertex.frame.color=0, edge.arrow.width=.75)


kb2 <- graph.neighborhood(kblink, 2, V(kblink)["Bacon, Kevin"])[[1]]
V(kb2)$color <- "green"  
V(kb2)[V(kb)$name]$color <- "gold"
V(kb2)["Bacon, Kevin"]$color <- "red"
plot(kb2,  edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA, vertex.frame.color=0, vertex.size=6)


# Neighborhoods of order 1, 2, and 3
neighborhood1 <- neighborhood(actnet, order = 1, nodes = kevin_bacon_id, mode = "in")
neighborhood2 <- neighborhood(actnet, order = 2, nodes = kevin_bacon_id, mode = "in")
neighborhood3 <- neighborhood(actnet, order = 3, nodes = kevin_bacon_id, mode = "in")

# Plotting the neighborhoods
par(mfrow=c(1,3))  # Set up the plotting area to display three plots side by side
plot(induced_subgraph(actnet, vids = unlist(neighborhood1)), main = "1st Order Neighborhood", vertex.label.cex=0.7)
plot(induced_subgraph(actnet, vids = unlist(neighborhood2)), main = "2nd Order Neighborhood", vertex.label.cex=0.7)
plot(induced_subgraph(actnet, vids = unlist(neighborhood3)), main = "3rd Order Neighborhood", vertex.label.cex=0.7)






# Q3
# Who were the most common actors? 
# Who were most connected? 
# Pick a pair of actors and describe the shortest path between them.

# Most common actors by number of roles
top_actor <- sort(nroles, decreasing = TRUE)[1]
print(top_actor)

# Most connected actors by number of connections (degree)
actor_degrees <- degree(actnet)
most_connected_actor <- sort(actor_degrees, decreasing = TRUE)[1]
print(names(most_connected_actor))

actor1 <- names(sort(actor_degrees, decreasing = TRUE))[1]
actor2 <- names(sort(actor_degrees, decreasing = TRUE))[2]
# Shortest path between two actors
path <- get.shortest.paths(actnet, from=actor1, to=actor2)
path_actors <- V(actnet)$name[path$vpath[[1]]]
print(path_actors)


# Q4
# Find pairwise actor-cast association rules with at least 0.01% support and 10% confidence. 
# Describe what you find.

# Run Apriori algorithm
rules <- apriori(casttrans, parameter = list(supp = 0.0001, conf = 0.1, maxlen = 2))

# Inspecting the rules, only print out the first 10
inspect(rules[1:10])

cat("The total number of rules generated is:", length(rules), "\n")

# Bonus
# What would be a regression based alternative to ARules? 
# Execute it for a single RHS actor.

# Prepare data for logistic regression
# Kevin Bacon will be our target (RHS actor)
response <- actmat[, "Bacon, Kevin"]  # This is our dependent variable
predictors <- actmat[, colnames(actmat) != "Bacon, Kevin"]  # Independent variables are all other actors

# Convert the sparse matrix to a regular matrix
regular_matrix <- as.matrix(predictors)

# Now convert this regular matrix to a data frame
predictor_data <- as.data.frame(regular_matrix)
response_data <- as.factor(response)  # Convert to factor for logistic regression

# Fit logistic regression model
#glm_model <- glm(response_data ~ ., data = predictor_data, family = binomial(link = "logit"))

# Output summary of the model to see coefficients and significance
#summary(glm_model)



