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

# Question 1 : 
tpRetinol <- read.csv2("C:/Users/Mehdi/Desktop/Statistiques/DevoirStatBase/presentationTPretinol.csv")
summary(tpRetinol)
describe(tpRetinol)

# Question 2 : 
# variables dinteret : concentration retinol plasmatique, age, sexe, BMI, tabac, consommation alimentaire de vitamines, cholesterol, alcool, retinol

#2.1 Relation entre Retinol plasmatique et age
qqnorm(tpRetinol$retplasma)
t.test(tpRetinol$retplasma~tpRetinol$age, var.equal=TRUE)

#2.2 Relation entre Retinol plasmatique et sexe
t.test(tpRetinol$retplasma~tpRetinol$sexe, var.equal=TRUE)

#2.3 Relation entre Retinol plasmatique et BMI
t.test(tpRetinol$retplasma~tpRetinol$bmi, var.equal=TRUE)

#2.4 Relation entre Retinol plasmatique et tabac

#2.5 Relation entre Retinol plasmatique et beta-carotene consommé

#2.6 Relation entre Retinol plasmatique et retinol consommé

#2.7 Relation entre Retinol plasmatique et cholesterol

#2.8 Relation entre Retinol plasmatique et alcool


# Question 3

# Question 4