library(readxl)
library(reshape2)

# Importation des fichiers
autoeval <- read_excel("C:/Users/Mehdi/Desktop/Statistiques/DevoirStatAvance/outils autoeval.xls")
View(autoeval)

groupe <- read_excel("C:/Users/Mehdi/Desktop/Statistiques/DevoirStatAvance/outils groupe.xls")
View(groupe)

hdrs <- read_excel("C:/Users/Mehdi/Desktop/Statistiques/DevoirStatAvance/outils hdrs.xls")
View(hdrs)

# Fusionner HAMD16A et HAMD16B afin d'eliminer les NA
hdrs$HAMD16 <- ifelse(is.na(hdrs$HAMD16A),hdrs$HAMD16B,hdrs$HAMD16A)

### Supprimer HAMD16A et HAMD16B de la base hdrs
hdrs <- subset(hdrs, select = c(1:17,20,21))


reshaped_data <- melt(outils_hdrs, id.vars = c("NUMERO", "VISIT"))
View(reshaped_data)
large_outils_hdrs<- dcast(melt(outils_hdrs,id.vars = c("NUMERO", "VISIT")),NUMERO~variable+VISIT)
View(large_outils_hdrs)
