> getwd()
[1] "/Users/JanetWei/Documents"
> mydata = read.table("property.txt")
> X1 = mydata[,1]
> X2 = mydata[,2]
> X3 = mydata[,3]
> X4 = mydata[,4]
> Y = mydata[,5]
> 
> anova(lm(Y~X1+X2+X3+X4))
> anova(lm(Y~X1+X2+X3))

> anova(lm(Y~X1+X2+X3))
> anova(lm(X4~X1+X2+X3))

> summary(lm(X4~X1+X2+X3))
> summary(lm(Y~X1+X2+X3))

> hist(Y)
> hist(X1)
> hist(X2)
> hist(X3)
> hist(X4)
> 
> library(MASS)
> boxcox(lm(Y~X1+X2+X3+X4))
> boxplot(X1,X2,X3,X4)
> 
> lnY = log(mydata[,5])
> boxcox(lm(lnY~X1+X2+X3+X4))

> plot(data.frame(cbind(lnY,X1,X2,X3,X4)))
> pairs(lnY~X1+X2+X3+X4)

> mean(X1)
> mean(X2)
> mean(X3)
> mean(X4)
> sd(X1)
> sd(X2)
> sd(X3)
> sd(X4)

> mean(scale(Y))
> sd(scale(Y))
> mean(scale(X1))
> mean(scale(X2))
> mean(scale(X3))
> mean(scale(X4))
> sd(scale(X1))
> sd(scale(X2))
> sd(scale(X3))
> sd(scale(X4))
> mean(scale(Y))
> sd(scale(Y))

> cor(X1,X2)
> cor(X1,X3)
> cor(X1,X4)
> cor(X2,X3)
> cor(X2,X4)
> cor(X3,X4)

> summary(lm(X4~X1+X2+X3))
> summary(lm(Y~X1+X2+X3+X4))


> mydata = read.table("fat.txt")
> X1 = mydata[,1]
> X2 = mydata[,2]
> X3 = mydata[,3]
> Y = mydata[,4]

> cor(X1,X2)
> cor(X1,X3)
> cor(X2,X3)

> summary(lm(X3~X1+X2))

> summary(lm(Y~X1+X2))
