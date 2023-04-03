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


# Enregistrer les trois grandes bases de donnees ("cours", "collab", "etudiant") en .csv
write.csv(cours, file = "cours.csv", row.names=FALSE)
write.csv(collab, file = "collab.csv", row.names=FALSE)
write.csv(etudiant, file = "etudiant.csv", row.names=FALSE)

# Renommer les bases de donnees pour utiliser avec SQL (juste pour pas fuck up le chien)
bd_cours <- read.csv(file = "cours.csv")
bd_collab <- read.csv(file = "collab.csv")
bd_etudiant <- read.csv(file = "etudiant.csv")


# Injecter les donnees dans les bases de donnees SQL --> si problème, voir l'argument "append"

dbWriteTable(con, name = "cours", value = bd_cours, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, name = "collab", value = bd_collab, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, name = "etudiant", value = bd_etudiant, overwrite = TRUE, row.names = FALSE)
