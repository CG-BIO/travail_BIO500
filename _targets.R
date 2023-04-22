# Charger les packages entourant target
library(targets)
library(tarchetypes)
library(visNetwork)

# Importer les fichiers sources
source("./R/prep_donnees.R")
source("./R/analyses.R")
source("./R/figures.R")

# Charger les autres packages
tar_option_set(packages = c("rmarkdown","knitr", "pandoc"))

list(
  
  # Creer un target pour aller chercher le dossier "data" contenant les fichiers de donnees
  tar_target(
    name = path, # Cible
    command = "./data", # Dossier contenant les fichiers de données
    format = "file" # Format de la cible
  ),

  # Creer un target pour lister les fichiers presents dans le dossier "data"
  tar_target(
    name = file_paths, # Cible
    command = list.files(path, full.names = TRUE) # Liste les fichiers dans le dossier
  ),
  
  # Creer un target pour importer et nettoyer les donnees
  tar_target(
    name = nettoyage,
    command = fct_prep(file_paths)
  ),

  # Creer un target pour analyser (par SQL et R) les donnees
  tar_target(
    name = analyses,
    command = fct_analyse(nettoyage)
  ),

  tar_target(
  name = figures,
   command = fct_figures(analyses)
  ),
  
  tar_render(
    name = rapport, # Cible du rapport
    path = "./rapport/rapport.Rmd", # Le path du rapport à render
  )
)
