

library(arules)
library(arulesViz)
library(igraph)
library(tcltk)
library(rgl)
set.seed(42)



###############################
# iGraph
###############################
SampleGraph3 <- read.csv("/Users/Suki/Desktop/Wechat.csv")
summary(SampleGraph3)


# choose Directed or Undirected graph:
#g3 <- graph.data.frame(SampleGraph3, directed=TRUE)
g3 <- graph.data.frame(SampleGraph3, directed=FALSE)

is.simple(g3)
g3 <- simplify(g3)
is.simple(g3)

edge(g3)
mean(degree(g3))


V(g3)
E(g3)
plot(degree(g3))
hist(degree(g3))
plot(degree.distribution(g3), log="xy")

plot(degree.distribution(g3))

tkplot(g3, layout=layout.fruchterman.reingold, vertex.size=10, vertex.label.cex=0.9,vertex.color=7)


# Plot graph with Page.Rank color
# page.rank weights - more discrimination levels - more color scales
pr3 = page.rank(g3, directed = FALSE)$vector


pr3
plot(pr3)
hist(pr3)

comps <- pr3 * 100000
comps

min(comps)
max(comps)

comps <- log(comps) *3 -10
min(comps)
max(comps)

plot(comps)
hist(comps)

colbar <- heat.colors(max(comps+1))
V(g3)$color <- colbar[comps+1]
tkplot(g3, layout=layout.fruchterman.reingold, vertex.size=10, vertex.label.cex=0.6, vertex.frame.color=V(g3)$color)







###############################
# Extract Largest Connected sub-graph
###############################

# sub-graph use clustering / is.connected
clusters(g3)
c3 <- clusters(g3)
c3$membership
clusters(g3)$membership

# largest connected sub-graph
# 2nd largest connected sub-graph
#g3sub2 <- induced.subgraph(g3, c3$membership==order(-c3$csize)[2])
g3sub2 <- induced.subgraph(g3, c3$membership==order(-c3$csize)[1])

edge(g3sub2)
tkplot(g3sub2, layout=layout.fruchterman.reingold, vertex.size=10, vertex.label.cex=0.9, vertex.frame.color=V(g3sub2)$color)



###############################
# detect community against largest sub-graph
###############################

#walktrap.community / cluster detection

set.seed(42)
wc <- walktrap.community(g3sub2)
edge(g3sub2)
modularity(wc)
membership(wc)

# assign rainbow color to sub-graph based on community clusters
comps <- membership(wc)
comps
colbar <- rainbow(max(comps)+1)
V(g3sub2)$color <- colbar[comps+1]
mean(degree(g3sub2))
tkplot(g3sub2, layout=layout.fruchterman.reingold, vertex.size=12, vertex.frame.color=V(g3sub2)$color, vertex.label.cex=0.9)


# extract the several / largest community among all communities:
# x <- which.max(sizes(wc))
# g3sub <- induced.subgraph(g3sub2, which(membership(wc) <= x))

# select some community:
g3sub <- g3sub2
g3sub <- induced.subgraph(g3sub2, which((membership(wc) %in% c("1","3","5","7"))))

# only display selected sub-graph
V(g3sub)
edge(g3sub)
tkplot(g3sub, layout=layout.fruchterman.reingold, vertex.size=15, vertex.label.cex=0.9, vertex.frame.color=V(g3sub)$color)




