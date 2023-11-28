rm(list=ls())
load("C:/Users/Mehdi/Desktop/Statistiques/DataScience/Prostate.rda")
require(glmnet)
summary(Prostate)

mod = glm(lpsa~., data = Prostate)
summary(mod)











#lambda = exp(seq(log(1.5),log(0.001),len=100))
#nbfolds = 3
y = Prostate[,9]
x = as.matrix(Prostate[,-9])
cvfit = cv.glmnet(x, 
                  y, 
                  family="gaussian",
                  type.measure = "mse", # type de perte qui sert de critere de choix de modele
                  #lambda=lam,  
                  nfolds=3) # nombre de folds
##cvfit = cv.glmnet(x, y, family="gaussian", nfolds=3)

# cvfit$glmnet.fit$beta ==> represente les coeff pour chaque lambda

plot(cvfit$glmnet.fit, "lambda", col=1:8, lwd=2,ylab=~beta)
legend("topright",lty=1, lwd=2, col = 1:8, legend = colnames(Prostate[,-9]))

print(paste(" best lambda = ", round(cvfit$lambda.min, 3), sep=""))

bestbetalasso = cvfit$glmnet.fit$beta[,which.min(cvfit$cvm)]
print(bestbetalasso)
