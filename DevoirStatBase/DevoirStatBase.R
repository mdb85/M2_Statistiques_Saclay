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

# Scinder les variables quantitatives en deux
age_b <- ifelse(tpRetinol$age > age_median, 1, 0)
bmi_b <- ifelse(tpRetinol$bmi > 25 , 1, 0)
tabac_b <- ifelse(tpRetinol$tabac > 2 , 1, 0)
sexe_b <- ifelse(tpRetinol$sexe==2 , 1, 0)
beta_carotene_conso_b <- ifelse(tpRetinol$betadiet > beta_carotene_conso_median, 1, 0)
retinol_conso_b <- ifelse(tpRetinol$retdiet > retinol_conso_median, 1, 0)
cholesterol_b <- ifelse(tpRetinol$cholesterol > cholesterol_median, 1, 0)
alcool_b <- ifelse(tpRetinol$alcool > alcool_median, 1, 0)

# Question 2 : 
# variables dinteret : concentration retinol plasmatique, age, sexe, BMI, tabac, consommation alimentaire de vitamines, cholesterol, alcool, retinol

#2.1 Relation entre Retinol plasmatique et age
qqnorm(tpRetinol$retplasma)
t.test(tpRetinol$retplasma~age_b, var.equal=TRUE)
cor(tpRetinol$retplasma, tpRetinol$age, use="complete.obs")

#2.2 Relation entre Retinol plasmatique et sexe
t.test(tpRetinol$retplasma~tpRetinol$sexe, var.equal=TRUE)

#2.3 Relation entre Retinol plasmatique et BMI
cor(tpRetinol$retplasma, tpRetinol$sexe, use="complete.obs")
t.test(tpRetinol$retplasma~bmi_b, var.equal=TRUE)

#2.4 Relation entre Retinol plasmatique et tabac
cor(tpRetinol$retplasma, tpRetinol$tabac, use="complete.obs")
t.test(tpRetinol$retplasma~tabac_b, var.equal=TRUE)

#2.5 Relation entre Retinol plasmatique et beta-carotene consommé
cor(tpRetinol$retplasma, tpRetinol$betadiet, use="complete.obs")
t.test(tpRetinol$retplasma~beta_carotene_conso_b, var.equal=TRUE)

#2.6 Relation entre Retinol plasmatique et retinol consommé
cor(tpRetinol$retplasma, tpRetinol$retdiet, use="complete.obs")
t.test(tpRetinol$retplasma~retinol_conso_b, var.equal=TRUE)

#2.7 Relation entre Retinol plasmatique et cholesterol
cor(tpRetinol$retplasma, tpRetinol$cholesterol, use="complete.obs")
t.test(tpRetinol$retplasma~cholesterol_b, var.equal=TRUE)

#2.8 Relation entre Retinol plasmatique et alcool
cor(tpRetinol$retplasma, tpRetinol$alcool, use="complete.obs")
t.test(tpRetinol$retplasma~alcool_b, var.equal=TRUE)

#2.9 Relation entre age et sexe
t.test(tpRetinol$age~tpRetinol$sexe, var.equal=TRUE)

#2.10 Relation entre age et BMI
cor(tpRetinol$age, tpRetinol$bmi, use="complete.obs")

#2.11 Relation entre age et tabac
cor(tpRetinol$age, tpRetinol$tabac, use="complete.obs")

#2.12 Relation entre age et beta-carotene consomme
cor(tpRetinol$age, tpRetinol$betadiet, use="complete.obs")

#2.13 Relation entre age et retinol consomme
cor(tpRetinol$age, tpRetinol$retdiet, use="complete.obs")

#2.14 Relation entre age et cholesterol
cor(tpRetinol$age, tpRetinol$cholesterol, use="complete.obs")

#2.15 Relation entre age et alcool
cor(tpRetinol$age, tpRetinol$alcool, use="complete.obs")

#2.16 Relation entre sexe et BMI
cor(tpRetinol$sexe, tpRetinol$bmi, use="complete.obs")

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
cor(tpRetinol$bmi, tpRetinol$tabac, use="complete.obs")

#2.23 Relation entre BMI et beta-carotene consomme
cor(tpRetinol$bmi, tpRetinol$betadiet, use="complete.obs")

#2.24 Relation entre BMI et retinol consomme
cor(tpRetinol$bmi, tpRetinol$retdiet, use="complete.obs")

#2.25 Relation entre BMI et cholesterol
cor(tpRetinol$bmi, tpRetinol$cholesterol, use="complete.obs")

#2.26 Relation entre BMI et alcool
cor(tpRetinol$bmi, tpRetinol$alcool, use="complete.obs")

#2.27 Relation entre tabac et beta-carotene consomme
cor(tpRetinol$tabac, tpRetinol$betadiet, use="complete.obs")

#2.28 Relation entre tabac et retinol consomme
cor(tpRetinol$tabac, tpRetinol$retdiet, use="complete.obs")

#2.29 Relation entre tabac et cholesterol
cor(tpRetinol$tabac, tpRetinol$cholesterol, use="complete.obs")

#2.30 Relation entre tabac et alcool
cor(tpRetinol$tabac, tpRetinol$alcool, use="complete.obs")

#2.31 Relation entre beta-carotene consomme et retinol consomme
cor(tpRetinol$betadiet, tpRetinol$retdiet, use="complete.obs")

#2.32 Relation entre beta-carotene consomme et cholesterol
cor(tpRetinol$betadiet, tpRetinol$cholesterol, use="complete.obs")

#2.33 Relation entre beta-carotene consomme et alcool
cor(tpRetinol$betadiet, tpRetinol$alcool, use="complete.obs")

#2.34 Relation entre retinol consomme consomme et cholesterol
cor(tpRetinol$retdiet, tpRetinol$cholesterol, use="complete.obs")

#2.35 Relation entre retinol consomme consomme et alcool
cor(tpRetinol$retdiet, tpRetinol$alcool, use="complete.obs")

#2.36 Relation entre cholesterol et alcool
cor(tpRetinol$cholesterol, tpRetinol$alcool, use="complete.obs")

# Question 3

# Question 4