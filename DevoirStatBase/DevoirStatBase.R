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

#2.1 Relation entre Retinol plasmatique et age
qqnorm(tpRetinol$retplasma)
t.test(tpRetinol$retplasma~age_b, var.equal=TRUE)

#2.2 Relation entre Retinol plasmatique et sexe
t.test(tpRetinol$retplasma~tpRetinol$sexe, var.equal=TRUE)

#2.3 Relation entre Retinol plasmatique et BMI
t.test(tpRetinol$retplasma~bmi_b, var.equal=TRUE)

#2.4 Relation entre Retinol plasmatique et tabac
t.test(tpRetinol$retplasma~tabac_b, var.equal=TRUE)

#2.5 Relation entre Retinol plasmatique et beta-carotene consommé
t.test(tpRetinol$retplasma~beta_carotene_conso_b, var.equal=TRUE)

#2.6 Relation entre Retinol plasmatique et retinol consommé
t.test(tpRetinol$retplasma~retinol_conso_b, var.equal=TRUE)

#2.7 Relation entre Retinol plasmatique et cholesterol
t.test(tpRetinol$retplasma~cholesterol_b, var.equal=TRUE)

#2.8 Relation entre Retinol plasmatique et alcool
t.test(tpRetinol$retplasma~alcool_b, var.equal=TRUE)

#2.9 Relation entre age et sexe
t.test(tpRetinol$age~tpRetinol$sexe, var.equal=TRUE)

#2.10 Relation entre age et BMI

#2.11 Relation entre age et tabac

#2.12 Relation entre age et beta-carotene consomme

#2.13 Relation entre age et retinol consomme

#2.14 Relation entre age et cholesterol

#2.15 Relation entre age et alcool

#2.16 Relation entre sexe et BMI

#2.17 Relation entre sexe et tabac
twoby2(1-sexe_b, 1-tabac_b)
chisq.test(sexe_b, tabac_b)

#2.18 Relation entre sexe et beta-carotene consomme
twoby2(1-sexe_b, 1-beta_carotene_conso_b)
chisq.test(sexe_b, beta_carotene_conso_b)

#2.19 Relation entre sexe et retinol consomme

#2.20 Relation entre sexe et cholesterol

#2.21 Relation entre sexe et alcool

#2.22 Relation entre BMI et tabac

#2.23 Relation entre BMI et beta-carotene consomme

#2.24 Relation entre BMI et retinol consomme

#2.25 Relation entre BMI et cholesterol

#2.26 Relation entre BMI et alcool

#2.27 Relation entre tabac et beta-carotene consomme

#2.28 Relation entre tabac et retinol consomme

#2.29 Relation entre tabac et cholesterol

#2.30 Relation entre tabac et alcool

#2.31 Relation entre beta-carotene consomme et retinol consomme

#2.32 Relation entre beta-carotene consomme et cholesterol

#2.33 Relation entre beta-carotene consomme et alcool

#2.34 Relation entre retinol consomme consomme et cholesterol

#2.35 Relation entre retinol consomme consomme et alcool

#2.36 Relation entre cholesterol et alcool

# Compute correlation
quantitative_var <- c ("retplasma", "age", "bmi", "tabac", "betadiet", 
                       "retdiet", "cholesterol", "alcool")
matrix_correlation <- cor(tpRetinol[, quantitative_var], use = "complete.obs")
corrplot(matrix_correlation, method = "circle")

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
hist(resid(regression_linaire), col="grey", main="")

# Rechercher synergies
data_interest <- tpRetinol[, c("retplasma", "age", "sexe", "bmi", "tabac",
                                "betadiet", "retdiet", "cholesterol", "alcool")]
regression_lineaire_synergies <- lm(data_interest$retplasma ~ .^2, data=data_interest)
tt <- lm(data_interest$retplasma ~ (data_interest$age
                                     +data_interest$sexe
                                     +data_interest$bmi
                                     +data_interest$tabac
                                     +data_interest$betadiet
                                     +data_interest$retdiet
                                     +data_interest$cholesterol
                                     +data_interest$alcool)^2
                                      , data=data_interest)

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
