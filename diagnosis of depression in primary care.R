data <- read.table("http://www.stat.ucdavis.edu/~azari/sta138/final.dat",header = T)
names(data)

#Establish the model
model1 <- glm(dav~pcs+mcs+beck+pgend+age+educat, data = data, family=binomial(link=logit))
summary (model1)


#Chi-square test model1
anova(model1, test = 'Chisq')



#Establish the null model
nullmodel <- glm(dav~1, data=data, family=binomial(link=logit))
summary(nullmodel)


#Stepwise to get the best fitted model
best_model = step(nullmodel, scope = list(lower = nullmodel, upper = model1), direction = "both")


best_model



#Fit the new model2
model2 <- glm(dav ~ mcs + beck + pgend + age + educat, data = data, family = binomial(link=logit))
summary(model2)


#Confidence of odd ratio
exp(cbind(OR = coef(best_model), confint(best_model)))
library(ResourceSelection)

#homsmer and lemeshow goodness of fit test
hoslem.test(model2$y, fitted(model2), g = 10)



#Residuals and influence measure
library(LogisticDx)

good = dx(best_model)
dx(best_model)

#Pearson Residual
pearson.r = good$Pr
#Deviance Residual
deviance.r = good$dr
#Standardized Residual
std.r = good$sPr
#Change in pearson for each observation
change.pearson = good$dChisq
#Change in LR-test for each observation
change.LR = good$dDev

#Plot the standardized Residuals
hist(std.r, main = "Pearson Standardized Residuals",col = "pink")
hist(change.LR, main = "Change in LR­test G^2 for each observation",col="light blue")

#Residual vs Fitted
plot(best_model$fitted.values, best_model$residuals, main = "Residual VS Fitted value", xlab = "Fitted", ylab = "Residual")
abline(h = 0, col = "orange")

