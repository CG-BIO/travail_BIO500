
# Charger les packages
library(targets)
library(tarchetypes)
library(visNetwork) #pour pouvoir utiliser la commande tar_glimpse (qui nous permet de voir notre réseau)

# Importer les fichiers sources

source("prep_donnees.R")
source("import_file.R")
source("sql_analyses.R")
source("r_analyses.R")

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
  
  tar_target(
    name = import_donnees,
    command = import_the_data(file_paths)
  ),
  
  tar_target(
    name = nettoyage,
    command = fct_prep(import_donnees)
  )#,
  
  #  tar_target(
  # name = sql,
  # command = fct_sql(nettoyage)
  # ),
  
  #tar_target(
  #name = analyses,
  # command = fct_r(sql) 
  #),
  
  #tar_target(
  #name = figures,
  # command = fct_figures(analyses)
  #)#,
  
  #tar_render(
  #  name = rapport, # Cible du rapport
  # path = "rapport.Rmd" # Le path du rapport à renderiser
  #)
)


