fct_analyse <- function (nettoyage) {

  ## CREER LA BASE DE DONNEES
  
  # Aller chercher le package
  library(RSQLite)
  
  # Etablir la connexion
  con <- dbConnect(SQLite(), dbname="projet_session.db")
  
  # Creer la table etudiant
  etudiant_sql <- "
CREATE TABLE etudiant (
  prenom_nom                  VARCHAR(50),
  prenom                      VARCHAR(30),
  nom                         VARCHAR(30),
  region_administrative       VARCHAR(40),
  regime_coop                 BOLEAN,
  formation_prealable         VARCHAR(20),
  annee_debut                 VARCHAR(5),
  programme                   VARCHAR(6),
  PRIMARY KEY (prenom_nom)
);"
  dbSendQuery(con,etudiant_sql)
  
  # Creer la table cours
  cours_sql <- "
CREATE TABLE cours (
  sigle                       VARCHAR(6),
  optionnel                   BOLEAN,
  credits                     INTEGER,
  PRIMARY KEY (sigle)
);"
  dbSendQuery(con,cours_sql)
  
  # Creer la table collab
  collab_sql <- "
CREATE TABLE collab (
  etudiant1   VARCHAR(50),
  etudiant2   VARCHAR(50),
  sigle       VARCHAR(6),
  session     VARCHAR(5),
  PRIMARY KEY (etudiant1, etudiant2, sigle, session)
  FOREIGN KEY (etudiant1) REFERENCES etudiant(prenom_nom),
  FOREIGN KEY (etudiant2) REFERENCES etudiant(prenom_nom),
  FOREIGN KEY (sigle)     REFERENCES cours(sigle)
);"
  dbSendQuery(con,collab_sql)
  
  # Joindre les tables ensemble dans un base de donnees nommee "projet_session.db"
  dbListTables(con)
  
  
  ## INJECTER LES DONNEES
  
  
  # Renommer les bases de donnees pour utiliser avec SQL (juste pour pas fuck up le chien)
  bd_cours <- read.csv(file = "cours.csv")
  bd_collab <- read.csv(file = "collab.csv")
  bd_etudiant <- read.csv(file = "etudiant.csv")
  
  
  # Injecter les donnees dans les bases de donnees SQL --> si problème, voir l'argument "append"
  
  dbWriteTable(con, name = "cours", value = bd_cours, overwrite = TRUE, row.names = FALSE)
  dbWriteTable(con, name = "collab", value = bd_collab, overwrite = TRUE, row.names = FALSE)
  dbWriteTable(con, name = "etudiant", value = bd_etudiant, overwrite = TRUE, row.names = FALSE)
  
  
  ## FAIRE LES REQUETES DEMANDEES
  
  
  # Nombre de liens par etudiant
  sql_requete <- "
  SELECT etudiant1, count(etudiant2) AS nb_liens
  FROM collab
  GROUP BY etudiant1
  ORDER BY nb_liens DESC;"
  nb_liens <- dbGetQuery(con, sql_requete)
  head(nb_liens)
  
  # Nombre de liens par paire d'etudiants
  sql_requete <- "
SELECT c1.etudiant1, c1.etudiant2, COUNT(*) AS nb_liens
FROM collab c1
WHERE EXISTS (
  SELECT *
  FROM collab c2
  WHERE c2.etudiant1 = c1.etudiant2 AND c2.etudiant2 = c1.etudiant1
)
GROUP BY c1.etudiant1, c1.etudiant2
ORDER BY nb_liens DESC;"
nb_liens_paires <- dbGetQuery(con, sql_requete)
head(nb_liens_paires)

write.csv(nb_liens, file = "nb_liens.csv", row.names=FALSE)
write.csv(nb_liens_paires, file = "nb_liens_paires.csv", row.names=FALSE)

# Calculer le nombre d'étudiants
nrow(nb_liens) # 163 étudiants

# Calculer le nombre de liens
sum(nb_liens_paires$nb_liens) # 3193

# Connectance du réseau
# Nombre de connexions existantes par rapport au nombre total de connexions possibles dans un réseau donné
nrow(nb_liens) * nrow(nb_liens) # 26 569 connexions possibles
connectance <- sum(nb_liens_paires$nb_liens) / (nrow(nb_liens) * nrow(nb_liens))
connectance # 0.1201777

# Nombre de liens moyens par étudiant
moy_lien <- mean(nb_liens$nb_liens) # 19.6319
var(nb_liens$nb_liens) # 580.3081


return(fct_analyse)
}

