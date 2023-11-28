rm(list=ls())
require(MASS)
require(glmnet)
data("Pima.te")
data("Pima.tr")

y = Pima.tr[,8]
x = as.matrix(Pima.tr[,-8])

cvfit = cv.glmnet(x, 
                  y, 
                  family="binomial", # regression logistique
                  type.measure = "class", #taux de mauvais classement comme type de measure
                  #lambda=lam, 
                  nfolds=5)
##cvfit = cv.glmnet(x, y, family="gaussian", nfolds=3)

plot(cvfit$glmnet.fit, ylim = c(0, 0.2), "lambda", col=1:7, lwd=2,ylab=~beta)
legend("topright",lty=1, lwd=2, col = 1:7, legend = colnames(Pima.tr[,-8]))

print(paste(" best lambda = ", round(cvfit$lambda.min, 3), sep=""))

## les coefficients de régression qui correspondent aux meilleures valeurs de lambda et alpha
bestbetalasso = cvfit$glmnet.fit$beta[,which.min(cvfit$cvm)]
print(bestbetalasso)

# code pour predire le jeu as.matrix(Pima.te[,-8]) en utilisant lambda.min
lasso_pred  = predict(cvfit$glmnet.fit, s = cvfit$lambda.min, as.matrix(Pima.te[,-8]), type ="class")
mean(lasso_pred!=Pima.te$type)
