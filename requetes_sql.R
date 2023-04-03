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
