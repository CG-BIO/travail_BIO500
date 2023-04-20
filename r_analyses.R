
fct_r <-function(){
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

return(fct_r)
}
