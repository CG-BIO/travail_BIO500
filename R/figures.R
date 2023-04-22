fct_figures <- function(analyses){
  
  # Charger les packages
  library(igraph)
  
  # Charger les objets du target precedent
  nb_liens_paires <- analyses[[1]]
  moyenne_liens_annee <- analyses[[2]]
  
  
  ## CREER LA MATRICE D'ADJACENCE ET L'OBJET IGRAPH

  
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
  

  ## CREER  L'OBJET IGRAPH
  g <- graph_from_adjacency_matrix(L, mode = "undirected")
  
  
  ## ANALYSES POUR FIGURE 1: CENTRALITE
  

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
  
  
  ## ANALYSES POUR FIGURE 2: MODULARITE

  
  # Identifier les communautés
  wtc <- walktrap.community(g)
  communities <- membership(wtc)
  communities(wtc)
  table(communities)
  
  # Identifier les communautes a une seule personne
  comm_sizes <- table(communities)
  singletons <- names(comm_sizes[comm_sizes == 1])
  
  # Supprimer les nœuds des communautes a une seule personne
  g_no_singletons <- delete.vertices(g, singletons)
  
  # Recalculer les communautes sur le nouveau graphe
  wtc_no_singletons <- walktrap.community(g_no_singletons)
  
  
  ## ANALYSES POUR FIGURE 3: HISTOGRAMME
  
  
  # Convertir l'année de début en facteur
  moyenne_liens_annee$annee_debut <- factor(moyenne_liens_annee$annee_debut, 
                                            levels = c("H2019", "A2019", "H2020", "A2020", "A2021", "H2022", "A2022"))
  
  
  ## RETOURNER OBJETS IMPORTANTS DE LA FONCTION
  
  liste_figures <- list(g, wtc_no_singletons, g_no_singletons, moyenne_liens_annee)
  return(liste_figures)
}
