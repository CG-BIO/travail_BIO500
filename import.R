

# IMPORTER LES BASES DE DONNEES PRIMAIRES
fct_import <- function(file_paths){
  
  # Extraire le nom des fichers de chaque groupe
  allFiles <- dir('./data')
  
  # Tables à fusioner
  tabNames <- c('collaboration', 'cours', 'etudiant')
  
  # Nombre de groupes
  nbGroupe <- length(grep(tabNames[1], allFiles))
  
  # Charger les donnees
  for(tab in tabNames) {
    # prendre seulement les fichers de la table specifique `tab`
    tabFiles <- allFiles[grep(tab, allFiles)]
    
    for(groupe in 1:nbGroupe) {
      # Definir le nom de l'obj dans lequel sauver les donnees de la table `tab` du groupe `groupe`
      tabName <- paste0(tab, "_", groupe)
      
      # Avant  de charger les données, il faut savoir c'est quoi le séparateur utilisé car
      # il y a eu des données separées par "," et des autres separes par ";"
      ficher <- paste0('./data/', tabFiles[groupe])
      L <- readLines(ficher, n = 1) # charger première ligne du donnée
      separateur <- ifelse(grepl(';', L), ';', ',') # S'il y a un ";", separateur est donc ";"
      
      # charger le donnée avec le bon séparateur et donner le nom `tabName`
      assign(tabName, read.csv(ficher, sep = separateur, stringsAsFactors = FALSE))
      
    }
  }
  
  # nettoyer des objets temporaires utilisé dans la boucle
  rm(list = c('allFiles', 'tab', 'tabFiles', 'tabName', 'ficher', 'groupe'))
  
  return(fct_import)
}
