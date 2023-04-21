#codes pour les figures

fct_figures <- function(analyses){
  ##### CRÉATION DE FIGURES
  
  moyenne_liens_annee <- read.csv("moyenne_liens_annee.csv")
  
  ## Analyses pour les figures
  
  # Créer une liste unique des noms d'étudiants
  students <- unique(c(nb_liens_paires$etudiant1, nb_liens_paires$etudiant2))
  
  # Créer une matrice d'adjacence binaire
  L <- matrix(0, nrow = length(students), ncol = length(students))
  
  # Remplir la matrice d'adjacence binaire avec les liens présents
  for(i in 1:nrow(nb_liens_paires)) {
    L[match(nb_liens_paires$etudiant1[i], students),
      match(nb_liens_paires$etudiant2[i], students)] <- 1
  }
  
  # Ajouter les noms d'étudiants comme en-tête de la matrice
  dimnames(L) <- list(students, students)
  
  # Convertir la matrice en objet igraph
  library(igraph)
  
  head(L)
  g <- graph_from_adjacency_matrix(L, mode = "undirected")
  
  
  
  ## FIGURE 1: CENTRALITÉ (RÉSEAU)
  
  # Calcul de la centralité
  eigen_centrality(g)$vector
  
  # Calculer le degré
  deg <- apply(L, 2, sum) + apply(L, 1, sum)
  
  # Le rang pour chaque noeud
  rk <- rank(deg)
  
  # Faire un code de couleur
  col.vec <- heat.colors(length(rk))
  
  # Attribuer aux noeuds la couleur
  V(g)$color = col.vec[rk]
  
  # Calculer la taille des noeuds en fonction de leur degré
  V(g)$size <- 10*sqrt(deg)/max(sqrt(deg))
  
  plot(g, vertex.label=NA, edge.arrow.mode = 0,
       vertex.frame.color = NA,
       layout = layout.kamada.kawai(g))
  
  
  ## FIGURE 2: MODULARITÉ
  
  # Identifier les communautés
  wtc <- walktrap.community(g)
  communities <- membership(wtc)
  communities(wtc)
  table(communities)
  
  # Trouver les communautés à une seule personne
  comm_sizes <- table(communities)
  singletons <- names(comm_sizes[comm_sizes == 1])
  
  # Supprimer les nœuds correspondants du graphe
  g_no_singletons <- delete.vertices(g, singletons)
  
  # Recalculer les communautés sur le nouveau graphe
  wtc_no_singletons <- walktrap.community(g_no_singletons)
  
  # Plot le graphe sans les singletons
  plot(wtc_no_singletons, g_no_singletons, vertex.label = NA, vertex.size = 0,
       edge.arrow.mode = 0, vertex.frame.color = NA, 
       layout = layout.fruchterman.reingold(g_no_singletons), edge.width = 0)
  
  
  # FIGURE 3: HISTOGRAMME (moyenne liens par étudiant selon l'année de début de programme)
  moyenne_liens_annee <- read.csv("moyenne_liens_annee.csv")
  
  library(ggplot2)
  
  # Convertir l'année de début en facteur
  moyenne_liens_annee$annee_debut <- factor(moyenne_liens_annee$annee_debut, 
                                            levels = c("H2019", "A2019", "H2020", "A2020", "A2021", "H2022", "A2022"))
  
  # Tracer l'histogramme
  ggplot(moyenne_liens_annee, aes(x = annee_debut, y = moyenne_liens_annee)) + 
    geom_bar(stat = "identity", fill = "steelblue") + 
    labs(x = "Année de début", y = "Nombre moyen de liens") + 
    ggtitle("Histogramme du nombre moyen de liens en fonction de l'année de début")
  
  return(fct_figures)
}
