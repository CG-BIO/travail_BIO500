install.packages("igraph")
library(igraph)
##doit trouver une façon d'ajouter les données à L
g <- graph.adjacency(L)
plot(g)
##met des couleurs
col.vec <- heat.colors(S)
##ajoute les couleurs au graphique
V(g)$color = col.vec[rk]

#faire un ordre de grandeur
col.vec <- seq(10, 25, length.out = S)
# Attribuer ordre de grandeur au graphique
V(g)$size = col.vec[rk]

#faire un layout en forme de cercle
plot(g, vertex.label=NA, edge.arrow.mode = 0,
     vertex.frame.color = NA,
     layout = layout.circle(g))