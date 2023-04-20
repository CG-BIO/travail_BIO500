
# Charger les packages
library(targets)
library(tarchetypes)

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

# Importer le fichier source
source("R/analyse.R")

# Créer les tragets du criss
tar_option_set(packages = c("rmarkdown","knitr"))
list(
  tar_target(
    name = path, # Cible
    command = "./data_collab", # Dossier contenant les fichiers de données
    format = "file" # Format de la cible
  ),
  tar_target(
    name = file_paths, # Cible
    command = list.files(path, full.names = TRUE) # Liste les fichiers dans le dossier
  ),
  tar_target(
    name = data, # Cible pour le modèle
    command = prep_donnees(file_paths) # Jointure des jeux de données
  ),
  tar_target(
    name = resultat_modele, # Cible pour le modèle
    command = mon_modele(data) # Exécution de l'analyse
  ),
  tar_render(
    name = rapport, # Cible du rapport
    path = "rapport/rapport.Rmd" # Le path du rapport à renderiser
  )
)
