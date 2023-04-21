#codes pour les figures

fct_figures <- function(){
  ##### CRÉATION DE FIGURES
  
  ## Créer une matrice d'adjacence
  
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
  
  
  ## Créer la figure réseau (la meilleure)
  
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
  
  
  ## CRÉER LA FIGURE RÉSEAU SOUS FORME DE CERCLE
  
  # Faire un code de taille
  col.vec <- seq(10, 25, length.out = length(rk))
  
  # Attribuer aux noeuds la taille
  V(g)$size = col.vec[rk]
  
  # Refaire la figure
  
  plot(g, vertex.label=NA, edge.arrow.mode = 0,
       vertex.frame.color = NA,
       layout = layout.circle(g))
  
  
  ## CRÉER LA FIGURE DE MODULARITÉ
  
  # Évalue la présence communautés dans le graphe
  wtc = walktrap.community(g)
  
  # Calcul de la modularité
  modularity(wtc) # 0.427854
  
  distances(g)
  
  ebc <- cluster_edge_betweenness(g)
  plot(ebc, g, vertex.label = NA, vertex.size = 0, edge.arrow.mode = 0, 
       vertex.frame.color = NA, layout = layout.fruchterman.reingold(g), edge.color = "blue")
  
  plot(ebc, g, vertex.label = students, vertex.label.cex=0.2, vertex.size = 5, edge.arrow.mode = 0, 
       vertex.frame.color = NA, layout = layout.fruchterman.reingold(g), edge.color = "white", )
  
  
  
  
  communities(wtc)
  table(communities)
  
  
  
  
  # Identifier les communautés
  wtc <- walktrap.community(g)
  communities <- membership(wtc)
  
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
  
  
  
  # Extrait les communautés identifiées par l'algorithme de la betweenness des arêtes
  communities <- membership(ebc)
  
  # Affiche le nombre de nœuds dans chaque communauté
  table(communities)
  
  
  ## CRÉER LA FIGURE DE CENTRALITÉ
  
  # Calcul de la centralité
  eigen_centrality(g)$vector
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############################
  # Calculer les clusters en utilisant la méthode de l'edge-betweenness
  ebc <- cluster_edge_betweenness(g)
  
  # Définir les paramètres de la figure
  plot_size <- 800
  vertex_size <- 3
  vertex_label_size <- 0.8 * vertex_size
  edge_arrow_mode <- 0
  vertex_frame_color <- NA
  vertex_color <- heat.colors(length(unique(ebc$membership)))[ebc$membership]
  
  # Calculer la disposition des nœuds en utilisant la méthode de Fruchterman-Reingold
  layout <- layout_with_fr(g, dim = 2, niter = 100, area = plot_size^2)
  
  # Dessiner le graphe avec les paramètres spécifiés
  plot(g, layout = layout, vertex.label = NA, vertex.size = vertex_size, 
       vertex.label.cex = vertex_label_size, edge.arrow.mode = edge_arrow_mode,
       vertex.frame.color = vertex_frame_color, vertex.color = vertex_color)
  
  
  
  #####################
  #Tentative bubble map
  
  sql_requete <- "
SELECT e.region_administrative, AVG(nb_liens) as moyenne_liens
FROM etudiant e
LEFT JOIN (
  SELECT etudiant1, COUNT(etudiant2) as nb_liens
  FROM collab
  GROUP BY etudiant1
) c
ON e.prenom_nom = c.etudiant1
GROUP BY e.region_administrative
ORDER BY moyenne_liens DESC;"
  moyenne_liens <- dbGetQuery(con, sql_requete)
  moyenne_liens
  
  
  sql_requete <- "
SELECT e.annee_debut, AVG(nb_liens) as moyenne_liens_annee
FROM etudiant e
LEFT JOIN (
  SELECT etudiant1, COUNT(etudiant2) as nb_liens
  FROM collab
  GROUP BY etudiant1
) c
ON e.prenom_nom = c.etudiant1
GROUP BY e.annee_debut;"
  moyenne_liens_annee <- dbGetQuery(con, sql_requete)
  moyenne_liens_annee
  
  
  library(ggplot2)
  
  # Convertir l'année de début en facteur
  moyenne_liens_annee$annee_debut <- factor(moyenne_liens_annee$annee_debut, 
                                            levels = c("H2019", "A2019", "H2020", "A2020", "A2021", "H2022", "A2022"))
  
  # Tracer l'histogramme
  ggplot(moyenne_liens_annee, aes(x = annee_debut, y = moyenne_liens_annee)) + 
    geom_bar(stat = "identity", fill = "steelblue") + 
    labs(x = "Année de début", y = "Nombre moyen de liens") + 
    ggtitle("Histogramme du nombre moyen de liens en fonction de l'année de début")
  
  
  
  
  
  
  
  
  ##### ESSAYER DE FAIRE LA BUBBLE MAP
  
  # Définir les coordonnées centrales pour les régions administratives manquantes
  region_administrative <- c("<NA>", "abitibi-temiscamingue", "bas-saint-laurent", 
                             "capitale-nationale", "centre-du-quebec", "estrie", 
                             "gaspesie_iles_de_la_madeleine", "lanaudiere", 
                             "laurentides", "laval", "mauricie", "monteregie", 
                             "montreal", "outaouais", "saguenay-lac-saint-jean")
  
  latitude <- c(NA, 47.6762, 48.130000, 46.804520, 46.060960, 45.468429,
                48.831779, 46.400002, 46.833328, 45.569611, 46.535828, 45.374168,
                45.508839, 45.839221, 48.666672)
  
  longitude <- c(NA, -78.7516, -68.449722, -71.242310, -72.739030, -71.822998,
                 -64.483276, -73.500000, -74.000000, -73.692223, -72.749977, -73.505005,
                 -73.587807, -76.666664, -71.250000)
  
  coord_centrales <- data.frame(region_administrative, latitude, longitude)
  coord_centrales <- coord_centrales[complete.cases(coord_centrales), ]
  coord_centrales
  
  library(leaflet)
  library(dplyr)
  
  # Supprimer les lignes contenant des NA
  coord_centrales <- coord_centrales %>% na.omit()
  
  # Joindre les données des liens par région
  liens_region <- data.frame(region_administrative = c("abitibi-temiscamingue", "bas-saint-laurent", 
                                                       "capitale-nationale", "centre-du-quebec", "estrie", 
                                                       "gaspesie_iles_de_la_madeleine", "lanaudiere", 
                                                       "laurentides", "laval", "mauricie", "monteregie", 
                                                       "montreal", "outaouais", "saguenay-lac-saint-jean"),
                             moyenne_liens = c(66, 36, 45, 67, 45.9, 77, 1.5, 53.333333, 53, 65, 55.3125, 49.5, 37, 61))
  
  coord_centrales_liens <- left_join(coord_centrales, liens_region, by = "region_administrative")
  
  # Create a leaflet map centered on Quebec
  m <- leaflet() %>% 
    setView(lng = -71.208282, lat = 46.813878, zoom = 7)
  
  
  # Add markers for each region administrative with link data
  m %>% addTiles() %>% 
    addCircleMarkers(data = coord_centrales_liens, 
                     lng = ~longitude, lat = ~latitude,
                     radius = 5000, # ajuster le rayon du cercle selon vos besoins
                     stroke = FALSE, # retirer la bordure des cercles
                     fillOpacity = 0.7, # ajuster l'opacité du remplissage
                     popup = ~paste(region_administrative, "<br>",
                                    "Moyenne de liens:", moyenne_liens)) %>% 
    addLabelOnlyMarkers(data = coord_centrales_liens,
                        lng = ~longitude, lat = ~latitude,
                        label = ~moyenne_liens, # utiliser les valeurs de moyenne_liens pour les étiquettes
                        labelOptions = labelOptions(noHide = TRUE,
                                                    direction = "center",
                                                    textOnly = TRUE,
                                                    offset = c(0, 0)), # ajuster la position des étiquettes
                        radius = 0) # retirer les cercles pour ne garder que les étiquettes
  
  
  #### HISTOGRAMME NOMBRE DE LIENS MOYENS PAR RÉGION ADMINISTRATIVE
  # Tri des régions administratives en ordre décroissant de liens moyens
  moyenne_liens <- moyenne_liens[order(moyenne_liens$moyenne_liens, decreasing = TRUE),]
  
  # Création du graphique avec gradient de couleurs
  ggplot(data = moyenne_liens, aes(x = reorder(region_administrative, -moyenne_liens), y = moyenne_liens, fill = moyenne_liens)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(x = "Région administrative", y = "Moyenne de liens par étudiant",
         title = "Histogramme de la moyenne de liens par étudiant en fonction de la région administrative")
  
  
  return(fct_figures)
}
