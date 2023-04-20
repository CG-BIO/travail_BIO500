

# IMPORTER LES BASES DE DONNEES PRIMAIRES
fct_import <- function(){
  
  collab1 <- read.csv(file = '1_collaboration.csv', sep=";")
  cours1 <- read.csv(file = '1_cours.csv', sep=";")
  etudiant1 <- read.csv(file = '1_etudiant.csv', sep=";")
  
  collab2 <- read.csv(file = '2_collaboration.csv', sep=";")
  cours2 <- read.csv(file = '2_cours.csv', sep=";")
  etudiant2 <- read.csv(file = '2_etudiant.csv', sep=";")
  
  collab3 <- read.csv(file = '3_collaboration.csv', sep=";")
  cours3 <- read.csv(file = '3_cours.csv', sep=";")
  etudiant3 <- read.csv(file = '3_etudiant.csv', sep=";")
  
  collab4 <- read.csv(file = '4_collaboration.csv', sep=";")
  cours4 <- read.csv(file = '4_cours.csv', sep=";")
  etudiant4 <- read.csv(file = '4_etudiant.csv', sep=";")
  
  collab5 <- read.csv(file = '5_collaboration.csv', sep=";")
  cours5 <- read.csv(file = '5_cours.csv', sep=";")
  etudiant5 <- read.csv(file = '5_etudiant.csv', sep=";")
  
  collab6 <- read.csv(file = '6_collaboration.csv', sep=";")
  cours6 <- read.csv(file = '6_cours.csv', sep=";")
  etudiant6 <- read.csv(file = '6_etudiant.csv', sep=";")
  
  collab7 <- read.csv(file = '7_collaboration.csv', sep=";")
  cours7 <- read.csv(file = '7_cours.csv', sep=";")
  etudiant7 <- read.csv(file = '7_etudiant.csv', sep=";")
  
  collab8 <- read.csv(file = '8_collaboration.csv', sep=",")
  cours8 <- read.csv(file = '8_cours.csv', sep=",")
  etudiant8 <- read.csv(file = '8_etudiant.csv', sep=",")
  
  collab9 <- read.csv(file = '9_collaboration.csv', sep=";")
  cours9 <- read.csv(file = '9_cours.csv', sep=";")
  etudiant9 <- read.csv(file = '9_etudiant.csv', sep=";")
  
  collab10 <- read.csv(file = '10_collaboration.csv', sep=";")
  cours10 <- read.csv(file = '10_cours.csv', sep=";")
  etudiant10 <- read.csv(file = '10_etudiant.csv', sep=";" )
  
  return(fct_import)
}
