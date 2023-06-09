fct_prep <- function(x){
  
  library(dplyr)
  library(tidyverse)
  library(stringr)
  library(graphics)
  
  
  ## NETTOYER LES BASES DE DONNEES PRIMAIRES
  # Extraire le nom des fichers de chaque groupe
  allFiles <- dir('./data/')
  
  # Tables à fusioner
  tabNames <- c('collab', 'cours', 'etudiant')
  
  # Colonnes à supprimer
  colsToRemove <- c("X", "X.1", "X.2", "X.3", "X.4","X.5","...9")
  
  # Nombre de groupes
  nbGroupe <- length(grep(tabNames[1], allFiles))
  
  # Charger les donnees
  for(tab in tabNames) {
    # prendre seulement les fichiers de la table specifique tab
    tabFiles <- allFiles[grep(tab, allFiles)]
    
    for(groupe in 1:nbGroupe) {
      # Definir le nom de l'obj dans lequel sauver les donnees de la table tab du groupe groupe
      tabName <- paste0(tab, "", groupe)
      
      # Avant  de charger les données, il faut savoir c'est quoi le séparateur utilisé car
      # il y a eu des données separées par "," et des autres separes par ";"
      ficher <- paste0('./data/', tabFiles[groupe])
      L <- readLines(ficher, n = 1) # charger première ligne du donnée
      separateur <- ifelse(grepl(';', L), ';', ',') # S'il y a un ";", separateur est donc ";"
      
      # charger le donnée avec le bon séparateur et donner le nom tabName
      data <- read.csv(ficher, sep = separateur, stringsAsFactors = FALSE)
      
      # Renommer les vilaines colonnes aux noms corrompus
      names(data)[names(data) == "prenom_nom."] <- "prenom_nom"
      
      # Supprimer les colonnes spécifiées
      data <- data[, !(names(data) %in% colsToRemove)]
      
      # Sauvegarder la version modifiée des données
      assign(tabName, data)
    }
  }
  # Fusionner les matrices de chaque groupe
  collab <- do.call(rbind, lapply(1:nbGroupe, function(x) {
    tabName <- paste0("collab", x)
    get(tabName)
  }))
  
  # Supprimer les doublons
  collab <- unique(collab)
  
  # Fusionner les matrices de chaque groupe
  cours <- do.call(rbind, lapply(1:nbGroupe, function(x) {
    tabName <- paste0("cours", x)
    get(tabName)
  }))
  
  # Supprimer les doublons
  cours <- unique(cours)
  
  # Fusionner les matrices de chaque groupe
  etudiant <- do.call(rbind, lapply(1:nbGroupe, function(x) {
    tabName <- paste0("etudiant", x)
    get(tabName)
  }))
  
  # Supprimer les doublons
  etudiant <- unique(etudiant)
  
  #Enlever la vilaine ligne de NA
  cours <- cours[which(complete.cases(cours)),]
  
  # Pour importer les données de façon non-reproductible
  #collab1 <- file_paths[[1]]
  #cours1 <- import_donnees[[2]]
  #  etudiant1 <- import_donnees[[3]]
  # collab10 <- import_donnees[[4]]
  #cours10 <- import_donnees[[5]]
  #etudiant10 <- import_donnees[[6]]
  #collab2 <- import_donnees[[7]]
  #cours2 <- import_donnees[[8]]
  #etudiant2 <- import_donnees[[9]]
  #collab3 <- import_donnees[[10]]
  #cours3 <- import_donnees[[11]]
  #etudiant3 <- import_donnees[[12]]
  #collab4 <- import_donnees[[13]]
  #cours4 <- import_donnees[[14]]
  #etudiant4 <- import_donnees[[15]]
  #collab5 <- import_donnees[[16]]
  #cours5 <- import_donnees[[17]]
  #etudiant5 <- import_donnees[[18]]
  #collab6 <- import_donnees[[19]]
  #cours6 <- import_donnees[[20]]
  #etudiant6 <- import_donnees[[21]]
  #collab7 <- import_donnees[[22]]
  #cours7 <- import_donnees[[23]]
  #etudiant7 <- import_donnees[[24]]
  #collab8 <- import_donnees[[25]]
  #cours8 <- import_donnees[[26]]
  #etudiant8 <- import_donnees[[27]]
  #collab9 <- import_donnees[[28]]
  #cours9 <- import_donnees[[29]]
  #etudiant9 <- import_donnees[[30]]
  
  # Pour vérifier l'ordre: tar_load(file_paths)
  
  # Supprimer les vilaines colonnes en trop
  #ecollab7 <-subset(collab7, select=-c(X, X.1, X.2, X.3, X.4))
  #cours4 <-subset(cours4, select=-X)
  #cours6 <-subset(cours6, select=-c(X, X.1, X.2, X.3, X.4, X.5))
  #etudiant2 <-subset(etudiant2, select=-X)
  #etudiant3 <-subset(etudiant3, select=-X)
  #etudiant6 <-subset(etudiant6, select=-X)
  #etudiant8 <-subset(etudiant8, select=-...9)
  
  # Remplacer toutes les cases vides par des NA
  collab[collab== ""]<-NA
  cours[cours== ""]<-NA
  etudiant[etudiant== ""]<-NA
  
  
  ## PURIFIER LA BASE DE DONNEES "COURS"
  
  
  # Respecter la loi 101 dans "cours"
  cours[cours== "FALSE"]<-'FAUX'
  cours[cours== "TRUE"]<-'VRAI'
  
  # Conserver seulement les lignes uniques de "cours"
  cours <- unique(cours)
  
  # Supprimer les lignes de "cours" contenant seulement des NA
  cours <- cours[apply(cours, 1, function(x) !all(is.na(x))),]
  
  # Ordonner alphabetiquement la colonne "sigle"  de "cours" pour mieux reperer les sournois doublons
  cours <- cours[order(cours$sigle, decreasing = FALSE, na.last = TRUE), ]
  
  # Faire ressortir les sournois doublons pour la colonne "sigle" de "cours"
  doublons_cours <- cours[duplicated(cours$sigle) | duplicated(cours$sigle, fromLast = TRUE), ]
  
  # Identifier les lignes de "doublons_cours" sans erreurs
  vrais_cours <- list(c("BCM112", "FAUX", 2),
                      c("BCM113", "FAUX", 1),
                      c("BIO109", "FAUX", 1),
                      c("BIO401", "VRAI", 3),
                      # A retirer: les cours du microprogramme (ECL215 et ECL315)!!!
                      c("ECL406", "FAUX", 1),
                      c("ECL515", "FAUX", 2),
                      c("ECL522", "VRAI", 3),
                      c("ECL527", "FAUX", 2),
                      c("ECL544", "VRAI", 3),
                      c("ECL610", "FAUX", 2),
                      c("ECL611", "FAUX", 1),
                      c("TSB303", "FAUX", 2),
                      c("ZOO304", "VRAI", 3))
  
  # Identifier les lignes de "doublons_cours" sans erreurs
  keep_rows <- apply(doublons_cours, 1, function(row) any(sapply(vrais_cours, function(vect) all(row == vect)))) # Sous-ensemble du data frame avec les lignes a conserver
  
  # Conserver seulement les lignes de "doublons_cours" sans erreurs
  doublons_uniques_cours <- doublons_cours[keep_rows, ]
  
  # Retirer "doublons_cours" de "cours"
  cours_sans_doublons <- cours %>% anti_join(doublons_cours)
  
  # Combiner "cours_sans_doublons" et "doublons_uniques_cours"
  cours <- rbind(cours_sans_doublons, doublons_uniques_cours)
  
  
  ## PURIFIER LA BASE DE DONNEES "COLLAB"
  
  
  # Conserver seulement les lignes uniques de "collab"
  collab <- unique(collab)
  
  # Supprimer les lignes de "collab" contenant seulement des NA
  collab <- collab[complete.cases(collab), ]
  
  # Conserver seulement les lignes uniques de "etudiant1"
  collab_tri1 <- unique(collab$etudiant1)
  
  # Rendre "collab_tri1" sous forme matricielle
  collab_tri1 <- as.matrix(collab_tri1)
  
  # Ordonner alphabetiquement "collab_tri1" pour mieux reperer les noms malveillants
  collab_tri1 <- collab_tri1[order(collab_tri1, decreasing = FALSE, na.last = TRUE), ]
  
  # Rendre à nouveau "collab_tri1" sous forme matricielle
  collab_tri1 <- as.matrix(collab_tri1)
  
  # Corriger manuellement les noms malveillants de "etudiant1" dans "collab"
  collab[collab == "amelie_harbeck_bastien"]<-'amelie_harbeck-bastien'
  collab[collab == "arianne_barette"]<-'ariane_barrette'
  collab[collab == "cassandra_gobin"]<-'cassandra_godin'
  collab[collab == "catherine_viel_lapointe"]<-'catherine_viel-lapointe'
  collab[collab == "edouard_nadon-baumier"]<-'edouard_nadon-beaumier'
  collab[collab == "francis_bolly"]<-'francis_boily'
  collab[collab == "francis_bourrassa"]<-'francis_bourassa'
  collab[collab == "frederick_laberge"]<-'frederic_laberge'
  collab[collab == "ihuoma_elsie_ebere"]<-'ihuoma_elsie-ebere'
  collab[collab == "jonathan_rondeau_leclaire"]<-'jonathan_rondeau-leclaire'
  collab[collab == "justine_lebelle"]<-'justine_labelle'
  collab[collab == "laurianne_plante "]<-'laurianne_plante'
  collab[collab == "laurie_anne_cournoyer"]<-'laurie-anne_cournoyer'
  collab[collab == "louis-phillippe_theriault"]<-'louis-philippe_theriault'
  collab[collab == "mael_gerin"]<-'mael_guerin'
  collab[collab == "marie_burghin"]<-'marie_bughin'
  collab[collab == "noemie_perrier-mallette"]<-'noemie_perrier-malette'
  collab[collab == "peneloppe_robert"]<-'penelope_robert'
  collab[collab == "philippe_barette"]<-'philippe_barrette'
  collab[collab == "philippe_bourrassa"]<-'philippe_bourassa'
  collab[collab == "phillippe_bourassa"]<-'philippe_bourassa'
  collab[collab == "philippe_leonard_dufour"]<-'philippe_leonard-dufour'
  collab[collab == "sabrica_leclercq"]<-'sabrina_leclercq'
  collab[collab == "sara_jade_lamontagne"]<-'sara-jade_lamontagne'
  collab[collab == "yanick_sagneau"]<-'yanick_sageau'
  collab[collab == "yannick_sageau"]<-'yanick_sageau'
  collab[collab == "marie_eve_gagne"]<-'marie-eve_gagne'
  collab[collab == "marie_christine_arseneau"]<-'marie-christine_arseneau'
  collab[collab == "savier_samson"]<-'xavier_samson'
  collab[collab == "louis_philippe_raymond"]<-'louis-philippe_raymond'
  
  #Supprimer les étudiants égoistes
  
  collab <- subset(collab, etudiant1 != etudiant2)
  
  
  # Supprimer les "<a0>" et "�" adjacents a certains noms malveillants de "collab"
  collab$etudiant1 <- str_replace_all(collab$etudiant1, "\\s", "")
  collab$etudiant2 <- str_replace_all(collab$etudiant2, "\\s", "")
  collab$etudiant1 <- str_replace_all(collab$etudiant1, "�", "")
  collab$etudiant2 <- str_replace_all(collab$etudiant2, "�", "")
  
  # Conserver seulement les lignes uniques de "etudiant2"
  collab_tri2 <- unique(collab$etudiant2)
  collab_tri2 <- as.matrix(collab_tri2)
  
  # Rendre "collab_tri2" sous forme matricielle
  collab_tri2 <- as.matrix(collab_tri2)
  
  # Ordonner alphabetiquement "collab_tri2" pour mieux reperer les noms malveillants
  collab_tri2 <- collab_tri2[order(collab_tri2, decreasing = FALSE, na.last = TRUE), ]
  
  # Rendre à nouveau "collab_tri2" sous forme matricielle
  collab_tri2 <- as.matrix(collab_tri2)
  
  # Corriger manuellement les noms malveillants de "etudiant2" (ne se retrouvant pas deja dans "etudiant1") dans "collab"
  collab[collab== "madison_mcclean"]<-'madison_mclean'
  collab[collab== "raphael_charlesbois"]<-'raphael_charlebois'
  
  
  ## PURIFIER LA BASE DE DONNEES "ETUDIANT"
  
  
  # Conserver seulement les lignes uniques de "etudiant"
  etudiant <- unique(etudiant)
  
  # Ordonner alphabetiquement "prenom_nom" pour mieux reperer les noms malveillants
  etudiant <- etudiant[order(etudiant$prenom_nom, decreasing = FALSE, na.last = TRUE), ]
  
  # Supprimer les "<a0>" et "�" adjacents a certains noms malveillants de "etudiant"
  etudiant <- data.frame(lapply(etudiant, str_replace_all, pattern = "\\s", replacement = ""))
  etudiant <- data.frame(lapply(etudiant, str_replace_all, pattern = "�", replacement = ""))
  
  # Corriger les memes erreurs que dans "collab" pour s'assurer qu'elles ne se retrouvent pas dans "etudiant"
  etudiant[etudiant == "amelie_harbeck_bastien"]<-'amelie_harbeck-bastien'
  etudiant[etudiant == "arianne_barette"]<-'ariane_barrette'
  etudiant[etudiant == "cassandra_gobin"]<-'cassandra_godin'
  etudiant[etudiant == "catherine_viel_lapointe"]<-'catherine_viel-lapointe'
  etudiant[etudiant == "edouard_nadon-baumier"]<-'edouard_nadon-beaumier'
  etudiant[etudiant == "francis_bolly"]<-'francis_boily'
  etudiant[etudiant == "francis_bourrassa"]<-'francis_bourassa'
  etudiant[etudiant == "frederick_laberge"]<-'frederic_laberge'
  etudiant[etudiant == "ihuoma_elsie_ebere"]<-'ihuoma_elsie-ebere'
  etudiant[etudiant == "jonathan_rondeau_leclaire"]<-'jonathan_rondeau-leclaire'
  etudiant[etudiant == "justine_lebelle"]<-'justine_labelle'
  etudiant[etudiant == "laurianne_plante "]<-'laurianne_plante'
  etudiant[etudiant == "laurie_anne_cournoyer"]<-'laurie-anne_cournoyer'
  etudiant[etudiant == "louis-phillippe_theriault"]<-'louis-philippe_theriault'
  etudiant[etudiant == "mael_gerin"]<-'mael_guerin'
  etudiant[etudiant == "marie_burghin"]<-'marie_bughin'
  etudiant[etudiant == "noemie_perrier-mallette"]<-'noemie_perrier-malette'
  etudiant[etudiant == "peneloppe_robert"]<-'penelope_robert'
  etudiant[etudiant == "philippe_barette"]<-'philippe_barrette'
  etudiant[etudiant == "philippe_bourrassa"]<-'philippe_bourassa'
  etudiant[etudiant == "phillippe_bourassa"]<-'philippe_bourassa'
  etudiant[etudiant == "philippe_leonard_dufour"]<-'philippe_leonard-dufour'
  etudiant[etudiant == "sabrica_leclercq"]<-'sabrina_leclercq'
  etudiant[etudiant == "sara_jade_lamontagne"]<-'sara-jade_lamontagne'
  etudiant[etudiant == "yanick_sagneau"]<-'yanick_sageau'
  etudiant[etudiant == "yannick_sageau"]<-'yanick_sageau'
  etudiant[etudiant == "marie_eve_gagne"]<-'marie-eve_gagne'
  etudiant[etudiant == "marie_christine_arseneau"]<-'marie-christine_arseneau'
  
  # Corriger les noms malveillants supplementaires dans "etudiant"
  etudiant[etudiant == "amelie_harbeckbastien"]<-'amelie_harbeck-bastien'
  etudiant[etudiant == "harbeck_bastien"]<-'harbeck-bastien'
  etudiant[etudiant == "arianne"]<-'ariane'
  etudiant[etudiant == "barette"]<-'barrette'
  etudiant[etudiant == "gobin"]<-'godin'
  etudiant[etudiant == "cassandre"]<-'cassandra'
  etudiant[etudiant == "viel_lapointe"]<-'viel-lapointe'
  etudiant[etudiant == "nadon-baumier"]<-'nadon-beaumier'
  etudiant[etudiant == "bolly"]<-'boily'
  etudiant[etudiant == "elsie_ebere"]<-'elsie-ebere'
  etudiant[etudiant == "rondeau_leclaire"]<-'rondeau-leclaire'
  etudiant[etudiant == "katherin_dubois"]<-'kathrin_dubois'
  etudiant[etudiant == "katherin"]<-'kathrin'
  etudiant[etudiant == "trempe-kay"]<-'trempe_kay'
  etudiant[etudiant == "kayla_trempe-kay"]<-'kayla_trempe_kay'
  etudiant[etudiant == "louis-phillipe"]<-'louis-philippe'
  etudiant[etudiant == "therrien"]<-'theriault'
  etudiant[etudiant == "gerin"]<-'guerin'
  etudiant[etudiant == "louis_philipe_raymond"]<-'louis-philippe_raymond'
  etudiant[etudiant == "margerite_duchesne"]<-'marguerite_duchesne'
  etudiant[etudiant == "peneloppe"]<-'penelope'
  etudiant[etudiant == "burghin"]<-'bughin'
  etudiant[etudiant == "sabrina_leclerc"]<-'sabrina_leclercq'
  etudiant[etudiant == "leclerc"]<-'leclercq'
  etudiant[etudiant == "sara_jade"]<-'sara-jade'
  etudiant[etudiant == "guilemette"]<-'guillemette'
  etudiant[etudiant == "ramond"]<-'raymond'
  etudiant[etudiant == "sagneau"]<-'sageau'
  etudiant[etudiant == "yannick"]<-'yanick'
  etudiant[etudiant == "samule_fortin"]<-'samuel_fortin'
  etudiant[etudiant == "sara-jade_lamontagne"]<-'sara_jade_lamontagne'
  etudiant[etudiant == "savier_samson"]<-'xavier_samson'
  
  # Respecter la loi 101 dans "etudiant"
  etudiant[etudiant== "FALSE"]<-'FAUX'
  etudiant[etudiant== "TRUE"]<-'VRAI'
  
  # Conserver seulement les lignes uniques de "etudiant"
  etudiant <- unique(etudiant)
  
  # Faire ressortir les sournois doublons pour la colonne "prenom_nom" de "etudiant"
  doublons_etudiant <- etudiant[duplicated(etudiant$prenom_nom) | duplicated(etudiant$prenom_nom, fromLast = TRUE), ]
  
  # Conserver seulement les lignes de "doublons_etudiant" exemptes de NA pour supprimer les lignes inutiles
  doublons_uniques_etudiant <- doublons_etudiant[complete.cases(doublons_etudiant$region_administrative), ]
  
  # Le cas Cassandra Godin: ligne a supprimer
  doublons_uniques_etudiant <- doublons_uniques_etudiant[!(doublons_uniques_etudiant$annee_debut == "E2021" & doublons_uniques_etudiant$prenom_nom == "cassandra_godin"), ]
  
  # Le cas Rosalie Gagnon: ligne a supprimer
  doublons_uniques_etudiant <- doublons_uniques_etudiant[!(doublons_uniques_etudiant$regime_coop == "VRAI" & doublons_uniques_etudiant$prenom_nom == "rosalie_gagnon"), ]
  
  # Retirer "doublons_etudiant" de "etudiant"
  etudiant_sans_doublons <- etudiant %>% anti_join(doublons_etudiant)
  
  # Combiner "doublons_uniques_etudiant" et "etudiant_sans_doublons"
  etudiant <- rbind(doublons_uniques_etudiant, etudiant_sans_doublons)
  nrow(etudiant) # 156 lignes
  
  # Supprimer la ligne contenant seulement des NA dans "etudiant"
  etudiant <- etudiant[apply(etudiant, 1, function(x) !all(is.na(x))),]
  nrow(etudiant) # 155 lignes
  
  # Corriger les blasphemes de "region_administrative" dans "etudiant
  table(etudiant$region_administrative) # avec blasphemes
  etudiant[etudiant == "monterigie"]<-'monteregie'
  etudiant[etudiant == "bas-st-laurent"]<-'bas-saint-laurent'
  table(etudiant$region_administrative) # sans blasphemes
  
  # Enregistrer les trois grandes bases de donnees ("cours", "collab", "etudiant") en .csv
  write.csv(collab, file = "collab.csv", row.names=FALSE)
  write.csv(etudiant, file = "etudiant.csv", row.names=FALSE)
  write.csv(cours, file = "cours.csv", row.names=FALSE)
  
  liste_prep_donnees <- list(collab, etudiant, cours)
  return(liste_prep_donnees)
}


