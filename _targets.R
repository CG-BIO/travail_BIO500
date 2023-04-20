
# Charger les packages
library(targets)
library(tarchetypes)
library(visNetwork) #pour pouvoir utiliser la commande tar_glimpse (qui nous permet de voir notre réseau)

# Créer une fonction pour le nettoyage et l'assemblage des données
prep_donnees <- function(data_files) {
  # Fonction qui lit les fichiers de données et les joint
  data <- data.frame()
  
  for (file in data_files) {
    new_dat <- read.table(file, header = T)
    data <- rbind(data, new_dat)
  }
  
  return(data)
}

##Pourquoi on veut aller chercher notre fichier d'analyses???????
# Importer le fichier source
source("analyses.R") ##ne trouve pas le nombre de liens pcq manque des info dans analyses

source("prep_donnees.R")

# Créer les tragets du criss
tar_option_set(packages = c("rmarkdown","knitr"))

list(
  tar_target(
    name = path, # Cible
    command = "./data", # Dossier contenant les fichiers de données
    format = "file" # Format de la cible
  ),
  
  tar_target(
    name = file_paths, # Cible
    command = list.files(path, full.names = TRUE) # Liste les fichiers dans le dossier
  ),
  
  tar_target(nettoyage,prep_donnees(file_paths)),

  tar_target(
    name = resultat_modele, # Cible pour le modèle
    command = mon_modele(data) # Exécution de l'analyse
  ),
  tar_render(
    name = rapport, # Cible du rapport
    path = "rapport.Rmd" # Le path du rapport à renderiser
  )
)
