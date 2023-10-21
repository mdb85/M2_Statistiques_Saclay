##### Devoir de statistiques de base Octobre 2023 #####
##### Groupe N 25

#AGE : Age (en années)
#SEXE : 1=Masculin, 2=Féminin
#TABAC : 1=jamais, 2=autrefois, 3=actuellement
#BMI : poids/(taille^2)
#VITAMINES : 1=oui, souvent, 2=oui, pas souvent, 3=non
#CALORIES : Nombre de calories par jour
#GRAISSES : Grammes de graisse consommés par jour
#FIBRES : Grammes de fibre consommés par jour
#ALCOOL : Nombre de verres d'alcool consommés par semaine
#CHOLESTEROL : Cholesterol consommé (mg par jour).
#BETADIET : beta-carotene consommé (mcg par jour).
#RETDIET : retinol consommé (mcg par jour)
#BETAPLASMA : beta-carotene plasmatique (ng/ml)
#RETPLASMA : Retinol plasmatique (ng/ml)

library(prettyR)
library(Epi)
library(corrplot)

# Question 1 : Décrivez vos variables
tpRetinol <- read.csv2("C:/Users/Mehdi/Desktop/Statistiques/DevoirStatBase/presentationTPretinol.csv")
summary(tpRetinol)
describe(tpRetinol)
table(tpRetinol$sexe, deparse.level=2, useNA = "always")
table(tpRetinol$vitamine, useNA = "always")

# Calcul des medians
age_median <- median(tpRetinol$age)
beta_carotene_conso_median <- median(tpRetinol$betadiet)
retinol_conso_median <- median(tpRetinol$retdiet)
cholesterol_median <- median(tpRetinol$cholesterol)
alcool_median <- median(tpRetinol$alcool)
retinol_plasmatique_median <- median(tpRetinol$retplasma)

# Scinder les variables quantitatives en deux
age_b <- ifelse(tpRetinol$age > age_median, 1, 0)
bmi_b <- ifelse(tpRetinol$bmi > 25 , 1, 0)
tabac_b <- ifelse(tpRetinol$tabac > 2 , 1, 0)
sexe_b <- ifelse(tpRetinol$sexe==2 , 1, 0)
beta_carotene_conso_b <- ifelse(tpRetinol$betadiet > beta_carotene_conso_median, 1, 0)
retinol_conso_b <- ifelse(tpRetinol$retdiet > retinol_conso_median, 1, 0)
cholesterol_b <- ifelse(tpRetinol$cholesterol > cholesterol_median, 1, 0)
alcool_b <- ifelse(tpRetinol$alcool > alcool_median, 1, 0)
retinol_plasmatique_b <- ifelse(tpRetinol$retplasma > retinol_plasmatique_median, 1, 0)

# Question 2 : 
# variables dinteret : concentration retinol plasmatique, age, sexe, BMI, tabac, consommation alimentaire de vitamines, cholesterol, alcool, retinol

# Calcul de correlation
quantitative_var <- c ("retplasma", "age", "bmi", "tabac", "betadiet", 
                       "retdiet", "cholesterol", "alcool")
matrix_correlation <- cor(tpRetinol[, quantitative_var], use = "complete.obs")
corrplot(matrix_correlation, method = "circle")
for (i in quantitative_var) {
  for (j in quantitative_var) {
    result <- cor.test(tpRetinol[, i], tpRetinol[, j], use = "complete.obs")
    print(paste(i, j, result$estimate, result$p.value))
  }
}

# Fonction permettant de réaliser un test de student pour comparer 2 moyennes
Compute_quantitative_stat <- function(name, var_expliquer, var_explicatives) {
  for(i in 1:ncol(age_explicatives)) {
    result <- t.test(var_expliquer~var_explicatives[, i]
                     , var.equal=TRUE, paired = FALSE)
    print(paste(name, colnames(var_explicatives)[i], result$p.value))
  } 
}

# Fonction permettant de réaliser un test du chi 2
Compute_qualitative_stat <- function(name, var_expliquer, var_explicatives) {
  for(i in 1:ncol(age_explicatives)) {
    #rr <- twoby2(1-var_expliquer, 1-var_explicatives[, i])
    result <- chisq.test(var_expliquer, var_explicatives[, i])
    print(paste(name, colnames(var_explicatives)[i], result$p.value))
  } 
}

# Retplasma
retplasma_explicatives <- cbind(age_b, sexe_b, bmi_b, tabac_b, beta_carotene_conso_b, 
           retinol_conso_b, cholesterol_b, alcool_b)
Compute_quantitative_stat("retplasma", tpRetinol$retplasma, retplasma_explicatives)

# Age
qqnorm(tpRetinol$age)
age_explicatives <- cbind(retinol_plasmatique_b, sexe_b, bmi_b, tabac_b, 
                          beta_carotene_conso_b, retinol_conso_b, cholesterol_b, alcool_b)
Compute_quantitative_stat("age", tpRetinol$age, age_explicatives)

# BMI
qqnorm(tpRetinol$bmi)
bmi_explicatives <- cbind(retinol_plasmatique_b, age_b, sexe_b, tabac_b, 
                          beta_carotene_conso_b, retinol_conso_b, cholesterol_b, alcool_b)
Compute_quantitative_stat("bmi", tpRetinol$bmi, bmi_explicatives)

# Betadiet
qqnorm(tpRetinol$betadiet)
betadiet_explicatives <- cbind(retinol_plasmatique_b, age_b, sexe_b, bmi_b, 
                          tabac_b, retinol_conso_b, cholesterol_b, alcool_b)
Compute_quantitative_stat("betadiet", tpRetinol$betadiet, betadiet_explicatives)

# Retinoldiet
qqnorm(tpRetinol$retdiet)
retdiet_explicatives <- cbind(retinol_plasmatique_b, age_b, sexe_b, bmi_b, 
                               tabac_b, beta_carotene_conso_b, cholesterol_b, alcool_b)
Compute_quantitative_stat("retdiet", tpRetinol$retdiet, retdiet_explicatives)

# Cholesterol
qqnorm(tpRetinol$cholesterol)
cholesterol_explicatives <- cbind(retinol_plasmatique_b, age_b, sexe_b, bmi_b, 
                              tabac_b, beta_carotene_conso_b, retinol_conso_b, alcool_b)
Compute_quantitative_stat("cholesterol", tpRetinol$cholesterol, cholesterol_explicatives)

# Alcool
qqnorm(tpRetinol$alcool)
alcool_explicatives <- cbind(retinol_plasmatique_b, age_b, sexe_b, bmi_b, 
                                  tabac_b, beta_carotene_conso_b, retinol_conso_b, cholesterol_b)
Compute_quantitative_stat("alcool", tpRetinol$alcool, alcool_explicatives)

# Sexe
sexe_explicatives <- cbind(retinol_plasmatique_b, age_b, bmi_b, tabac_b, 
                          beta_carotene_conso_b, retinol_conso_b, cholesterol_b, alcool_b)
Compute_qualitative_stat("sexe", sexe_b, sexe_explicatives)

# Tabac
tabac_explicatives <- cbind(retinol_plasmatique_b, age_b, bmi_b, sexe_b, 
                           beta_carotene_conso_b, retinol_conso_b, cholesterol_b, alcool_b)
Compute_qualitative_stat("tabac", tabac_b, tabac_explicatives)

# Question 3 Regression lineaire avec comme variable à expliquer "retinol plasmatique concentration"
# et les autres variables explicatives
regression_linaire <- lm(tpRetinol$retplasma~tpRetinol$age
                         +tpRetinol$sexe
                         +tpRetinol$bmi
                         +tpRetinol$tabac
                         +tpRetinol$betadiet
                         +tpRetinol$retdiet
                         +tpRetinol$cholesterol
                         +tpRetinol$alcool
                         , data=tpRetinol)
summary(regression_linaire)
hist(resid(regression_linaire), col="grxey", main="")

# Rechercher synergies
data_interest <- tpRetinol[, c("retplasma", "age", "sexe", "bmi", "tabac",
                                "betadiet", "retdiet", "cholesterol", "alcool")]
regression_lineaire_synergies <- lm(data_interest$retplasma ~ .^2, data=data_interest)
summary(regression_lineaire_synergies)

# Question 4 Regression logistique
regression_logistique <- glm(retinol_plasmatique_b~tpRetinol$age
                             +tpRetinol$sexe
                             +tpRetinol$bmi
                             +tpRetinol$tabac
                             +tpRetinol$betadiet
                             +tpRetinol$retdiet
                             +tpRetinol$cholesterol
                             +tpRetinol$alcool
                             , data=tpRetinol
                             , family = "binomial")
summary(regression_logistique)
exp(coefficients(regression_logistique)) # calcul des coefficients
exp(confint(regression_logistique)) # calcul des intervalles de confiance
drop1(regression_logistique,.~.,test="Chisq")

# Rechercher synergies
data_interest_logicstic <- data_interest
data_interest_logicstic$retplasma <- retinol_plasmatique_b
regression_logistique_synergies <- glm(data_interest_logicstic$retplasma ~ .^2, 
                                      data=data_interest_logicstic,
                                      family = "binomial")
summary(regression_logistique_synergies)
