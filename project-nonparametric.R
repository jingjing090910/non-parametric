#load the data
setwd("~/Documents")
school = read.csv('school.csv')
#group the data
n = length(read)
meanr = mean(read)
rstandard_dev = sd(read)
meang = mean(grad)
gstandard_dev = sd(grad)
cor(read, grad)
# graphical
par(mfrow=c(1,2))
plot(read, grad, main='Scatterplot of grad vs read',
     xlab='students who were proficient in reading',
     ylab='students who graduated from high school',col =
"red")
boxplot(cbind(read, grad), main='Boxplot for variable read and
grad',
        xlab='', ylab='percentage')
par(mfrow=c(1,2))
qqnorm(read, main='the normal plot for read proficient')
qqline(read)
qqnorm(grad, main='the normal probability plot for graduate
from high school')
qqline(grad,col="red")
#permutation approach
set.seed(1000000)
OBS = lm(read ~ grad)$coefficients[2]
R = 5000
all.perm.slopes = sapply(1:R,function(i){
  the.data = school
  the.data$grad = sample(the.data$grad,n,replace = FALSE)
  slope.i = lm(read ~ grad, data = the.data)$coefficients[2]
  return(slope.i)
  })
lower.p = mean(all.perm.slopes < OBS)
upper.p = mean(all.perm.slopes > OBS)
two.p = mean(abs(all.perm.slopes) > abs(OBS))
all.slope.pval = c(lower.p,upper.p,two.p)
names(all.slope.pval) = c("lower tail","upper tail","two-
tailed")
all.slope.pval
#The histogram for each permutation
hist(all.perm.slopes, xlab='slope', main='histogram of the
permutation')
#95% CI for the permutation approach
alpha = 0.05
ci.percentile = as.numeric(quantile(all.perm.slopes,
c(alpha/2, 1-alpha/2)))
ci.percentile
#center for the CI percentile
mean(ci.percentile)
#the width
diff(ci.percentile)
#Bootstrap method
B = 5000
set.seed(9000000)
bi.boot  = sapply(1:B,function(i){
   boot.data =
school[sample(1:nrow(school),nrow(school),replace = TRUE),]
   boot.slope = lm(grad ~ read, data =
boot.data)$coefficients[2]
   return(boot.slope)
   })
#The histogram for bootstrap
hist(bi.boot, main = "Bootstrap Distribution",xlab =
"Bootstrap Slopes",col = "pink")
# use percentile to obtain CI
alpha = 0.05
ci.percentile = as.numeric(quantile(bi.boot, c(alpha/2, 1-
alpha/2)))
ci.percentile
# center
mean(ci.percentile)
# width
diff(ci.percentile)

