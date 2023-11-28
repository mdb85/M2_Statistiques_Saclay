library(readxl)
library(reshape2)
library(dplyr)

# Importation des fichiers
autoeval <- read_excel("C:/Users/Mehdi/Desktop/Statistiques/DevoirStatAvance/outils autoeval.xls")
View(autoeval)

groupe <- read_excel("C:/Users/Mehdi/Desktop/Statistiques/DevoirStatAvance/outils groupe.xls")
View(groupe)

hdrs <- read_excel("C:/Users/Mehdi/Desktop/Statistiques/DevoirStatAvance/outils hdrs.xls")
View(hdrs)

# HAMD16A : perte de poids releve par lexamen clinique
# HAMD16B : perte de poids releve en pesant le sujet
# Fusionner HAMD16A et HAMD16B afin d'eliminer les NA
hdrs$HAMD16 <- ifelse(is.na(hdrs$HAMD16A),hdrs$HAMD16B,hdrs$HAMD16A)

### Supprimer HAMD16A et HAMD16B de la base hdrs
hdrs <- subset(hdrs, select = c(1:17,20,21))

# Merger les 2 bases hdrs et autoeval
hdrs_autoeval <- merge(hdrs,autoeval, by=c("NUMERO","VISIT"), all = TRUE)

### recoder tous les items de l'échelle autoeval >4 en NA car présence des valeurs aberrantes dans les réponses aux items
hdrs_autoeval[,20:50][hdrs_autoeval[,20:50]>4] <- NA

# Mettre la base hdrs_autoeval au format large
large_hdrs_autoeval<- dcast(melt(hdrs_autoeval,id.vars = c("NUMERO", "VISIT")),NUMERO~variable+VISIT)
View(large_hdrs_autoeval)


########### Question 1 ###########
# Validation à J0 : patients tres malades et a J56 : patients moins malades
# Selection des variables HAMD à JO et J56
hamd_J0 <- dplyr::select(dplyr::select(large_hdrs_autoeval,dplyr::starts_with("HAMD"),NUMERO)
                        , ends_with("_J0")
                        , NUMERO)

hamd_J56 <- dplyr::select(dplyr::select(large_hdrs_autoeval,dplyr::starts_with("HAMD"),NUMERO)
                        , ends_with("_J56")
                        , NUMERO)

# Calcul des scores à JO et J56
score_hamd_JO <- rowSums(hamd_J0[, 1:17], na.rm = TRUE)
score_hamd_J56 <- rowSums(hamd_J56[, 1:17], na.rm = TRUE)

# 1 - Analyse d items (diagramme en batons, pour tous les items) (que vaut la mesure)
hist(score_hamd_JO, main = "Score HAMD à J0")
hist(score_hamd_J56, main = "Score HAMD à J56")

hdrs_col <- c("HAMD1", "HAMD2", "HAMD3", "HAMD4", "HAMD5", "HAMD6", "HAMD7", "HAMD8",
              "HAMD9", "HAMD10","HAMD11", "HAMD12", "HAMD13", "HAMD14", "HAMD15", "HAMD17", "HAMD16")

# Sélectionner les colonnes pour J0
hdrs_items_J0 <- hdrs[hdrs$VISIT == "J0", hdrs_col]

# Sélectionner les colonnes pour J56
hdrs_items_J56 <- hdrs[hdrs1$VISIT == "J56", hdrs_col]

# Diagramme en baton 
for (j in hdrs_col) {
  x11()
  barplot(table(hdrs_items_J0[, j], useNA = "ifany"), main = j, col = "lightblue") 
}
for (j in hdrs_col) {
  x11()
  barplot(table(hdrs_items_J56[, j], useNA = "ifany"), main = j, col = "lightblue") 
}

# Verifier si y a un effet plafond

# Verifier si y a un effet plancher

# Verifier si y a des beaucoup de donnees manquantes

# On peut faire une matrice de correlation (pas indispensable)

# 2 - Diagramme des valeurs propres (pour savoir combien y a de dimensions) (que vaut la mesure)
# On fera une analyse factorielle si jamais y a plus d une dimension
# Calcul du Coefficient de crohnback

# 3- Validation concourante et divergente (c est la qu on utilise la scl90) (que mesure l instrument)
# A partir du fichier scl90, on calcul les 7 scores qui correspondent aux 7 dimensions

# Ensuite on correle ces 7 scores avec mon score hdrs (normalement le score hdrs doit correle plus avec la depression qu avec les autres scores) donc validite concourante avec la depression et validite divergente avec le reste

########### Question 2 ###########
# 25:42 dans la video

########### Question 3 ###########

